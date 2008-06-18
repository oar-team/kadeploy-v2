%%%
%%%  Copyright © Nicolas Niclausse. 2008
%%%
%%%  Author : Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%  Created: 25 avr 2008 by Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%
%%%  This program is free software; you can redistribute it and/or modify
%%%  it under the terms of the GNU General Public License as published by
%%%  the Free Software Foundation; either version 2 of the License, or
%%%  (at your option) any later version.
%%%
%%%  This program is distributed in the hope that it will be useful,
%%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%  GNU General Public License for more details.
%%%
%%%  You should have received a copy of the GNU General Public License
%%%  along with this program; if not, write to the Free Software
%%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
%%%

-module(chain).
-vc('$Id: chain.erl,v 0.0 2008/04/25 11:32:02 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-include_lib("kernel/include/file.hrl").

-export([write_tar/2]).
-export([slave_init/4]).
-export([start_chain/3]).

-define(chunksize, 10000). %% in bytes

start_chain(Filename,[Next|Nodes],DestDir)->
    {ok, FileInfo} = file:read_file_info(Filename),
    Size=FileInfo#file_info.size,
%%    Iterations = Size div ?chunksize,
%%    LastChunkSize = Size rem ?chunksize,
    {ok, IODevice} = file:open(Filename,[read,raw,binary]),
    NextPid=spawn_link(Next,chain,slave_init,[Nodes,self(),DestDir,self()]),
    read_forward(IODevice,0,?chunksize,Size,NextPid,length(Nodes)+1).

slave_init(AllNodes,Master,DestDir,PredPid)->
    NextPid=case AllNodes of
                [] ->
                    none;
                [Next|Nodes]->
                    spawn_link(Next,chain,slave_init,[Nodes,Master,DestDir,self()])
            end,
    Command="tar zx",
    Port = open_port({spawn, Command},[stream,{cd, DestDir},exit_status,use_stdio,binary]),
    loop(Master,PredPid,NextPid,Port).

loop(Master,PredPid,NextPid,TarPid)->
    receive
        {data,Bin, _From} ->
            send(NextPid,{data, Bin, self()} ),
            port_command(TarPid, Bin),
            loop(Master,PredPid,NextPid,TarPid);
        {eof, _From} ->
            send(NextPid, {eof, self()}),
            true=port_close(TarPid),
            Master ! {done, self()}
    after 15000 ->
            Master ! {timeout,self()}
    end.

send(none,_)  -> ok;
send(Pid,Msg) -> Pid ! Msg.

read_forward(IODevice, Loc, ChunkSize, Size, RemotePid,SlaveNumber) when Loc+ChunkSize < Size->
    Data=file:pread(IODevice, Loc, ChunkSize),
    RemotePid ! {data,Data,self()},
    read_forward(IODevice, Loc+ChunkSize, ChunkSize, Size, RemotePid,SlaveNumber);
read_forward(IODevice, Loc, _ChunkSize, Size, RemotePid, SlaveNumber)->
    Data=file:pread(IODevice, Loc, Size-Loc),
    RemotePid ! {data,Data,self()},
    RemotePid ! {eof,self()},
    file:close(IODevice),
    wait_slaves(SlaveNumber,0).

wait_slaves(Total,Total)->
    erlang:display(["chain finished"]);
wait_slaves(Total,Current) when Current < Total->
    receive
        {done,_Pid } ->
            wait_slaves(Total,Current+1);
        {timeout,Pid} ->
            erlang:display(["timeout from", Pid]),
            wait_slaves(Total,Current+1)
    end.

write_tar(File,Dir)->
    {ok,Bin}=file:read_file(File),
    Command="tar zx",
    Port = open_port({spawn, Command},[stream,{cd, Dir},exit_status,use_stdio,binary]),
    port_command(Port, Bin),
    true = port_close(Port).
