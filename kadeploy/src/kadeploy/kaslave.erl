%%%
%%%  Copyright 2008 © INRIA
%%%
%%%  Author : Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%  Created: 09 jun 2008 by Nicolas Niclausse <nniclaus@sophia.inria.fr>
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

-module(kaslave).
-vc('$Id: avalanche.erl,v 0.0 2007/06/26 07:40:15 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-include("kaconf.hrl").

-export([fdisk_data/3, fdisk_file/3, mkfs/3, mount/3, umount/1,
         unzip_and_exec/3,unzip/2]).
-export([chain_init/6, load_code/1]).

-export([myoscmd/1]).

-export([port_loop/5]).

-export([start/3]).
-export([diststart/3]).
-export([pdiststart/3]).
-export([pstart/3]).

-export([start/5]).
-export([wait/4]).
-export([deploy/5]).
-export([nosplit/1]).
-export([start_slave/7]).
-export([splithalf/1]).

%% @doc We want to be able to use remote nodes without having to
%% install kadeploy modules on it. So we push the kaslave and katools
%% modules code to them at runtime.
load_code(Nodes) ->
    ?LOGF("Loading kaslave module on nodes ~p~n", [Nodes], ?DEB),
    LoadCode = fun(Mod)->
                       {_, Binary, _} = code:get_object_code(Mod),
                       rpc:multicall(Nodes, code, load_binary, [Mod, Mod, Binary], infinity)
               end,
    LoadRes = lists:map(LoadCode, [?MODULE, katools]),
    ?LOGF("Load_code result: ~p ~n", [LoadRes],?DEB),
    ok.

% sequential start
start(Hosts, TimeOut, Args) ->
    start(Hosts, Args, nosplit, false, TimeOut).

% parallel start: use a new erlang process for every start
pstart(Hosts, TimeOut, Args) ->
    start(Hosts, Args, nosplit, true, TimeOut).

%% distributed start (each started beam starts itself half of the
%% remaining nodes to be started.
diststart(Hosts, TimeOut, Args) ->
    start(Hosts, Args,splithalf, false, TimeOut).

%% parallel distributed start (each started beam starts itself half
%% of the remaining nodes to be started. Moreover, use spawn to start
%% a beam.
pdiststart(Hosts, TimeOut, Args) ->
    start(Hosts, Args, splithalf, true, TimeOut).

start(Hosts, Args, Fun, DoSpawn,TimeOut) -> % start beam on host list
    N=length(Hosts),
    erlang:spawn(?MODULE,deploy, [Hosts, Args, Fun, self(), DoSpawn]),
    wait(N,TimeOut,Hosts).

wait(N, Timeout, Hosts)->
    wait(N, Timeout, [], Hosts).

wait(0,_,Good,Bad) -> {Good, Bad};
wait(N,TimeOut,Good,Bad) ->
    ?LOGF("waiting for ~p nodes, Good=~p, Bad=~p~n",[N,Good,Bad],?INFO),
    receive
        {started, From} ->
            wait(N-1, TimeOut, [From|Good], Bad--[From] );
        {notstarted,  {Reason, Host, Name}} ->
            ?LOGF("notstarted : ~p~n",[[Reason,Host,Name]],?DEB),
            wait(N-1,TimeOut, Good, Bad )
    after TimeOut ->
            ?LOGF("timeout waiting for remote beams to start, start with only ~p nodes~n",[length(Good)],?WARN),
            {Good, Bad}
    end.

deploy([],_,_,_,_) ->ok;

deploy([Host | Hosts], Args, Fun, Master, DoSpawn) ->
    {MyHost, HisHosts} = ?MODULE:Fun(Hosts),
    ArgList=[Host, "kaslave",Args,HisHosts,Fun, Master,DoSpawn],
    case DoSpawn of
        true  -> spawn(?MODULE,start_slave,ArgList);
        false -> start_slave(ArgList)
    end,
    deploy(MyHost,Args,Fun, Master, DoSpawn).

start_slave([Host,Name, Args, MyHosts, Fun, Master,DoSpawn]) ->
    start_slave(Host,Name, Args, MyHosts, Fun, Master,DoSpawn).
start_slave(Host,Name, Args, MyHosts, Fun, Master,DoSpawn) ->
    ?LOGF("try to start slave ~p~n",[[Host,Name, Args]],?DEB),
    case  slave_noproxy:start(Host, Name, Args) of
%%     case  slave:start(Host, Name, Args) of
        {ok, Node} ->
            Master ! {started, Host},
            load_code([Node]),
            %% FIXME temporary set for debug
            rpc:cast(Node,application, set_env, [stdlib,debug_level, 6]),
            rpc:cast(Node,?MODULE, deploy, [MyHosts, Args, Fun, Master, DoSpawn]);
        Reason ->
            Master ! {notstarted, {Reason, Host, Name}}
    end.

nosplit(List)     -> {List,[]}.

splithalf([])     -> {[],[]};
splithalf([List]) -> {[List],[]};
splithalf(List) ->
    S = (length(List)+1) div 2,
    lists:split(S, List).


fdisk_file(Device, TmpPath, FdiskFile)->
    ok=create_dir_ifnec(TmpPath),
    FileName=filename:join(TmpPath,FdiskFile),
    Cmd="fdisk "++Device ++" < " ++FileName,
    myoscmd(Cmd ).

fdisk_data(Device, TmpPath, FdiskData)->
    ok=create_dir_ifnec(TmpPath),
    FileName=filename:join(TmpPath,"fdisk-preinstall-tmp.txt"),
    file:write_file(FileName,list_to_binary(FdiskData)),
    Cmd="fdisk "++Device ++" < " ++FileName,
    myoscmd(Cmd ).

mkfs(Device, FSType, Options)->
    Cmd=lists:append(["mkfs -t ",FSType," ",Device," ",Options]),
    myoscmd(Cmd).

mount(Partition, FSType, Mount)->
    ok=create_dir_ifnec(Mount),
    Cmd=lists:append(["mount -t ",FSType," ",Partition," ",Mount]),
    myoscmd(Cmd).

umount(Mount) when is_list(Mount)->
    Cmd="umount "++Mount,
    myoscmd(Cmd).

create_dir_ifnec(DestDir)->
    case file:read_file_info(DestDir) of
        {ok, FileInfo}  ->
            ?LOGF("Info for file ~p: ~p~n",[DestDir,FileInfo],?DEB),
            ok;
        {error, enoent} ->
            ?LOGF("Must create directory ~p on node~p~n",[DestDir,node()],?WARN),
            ok=file:make_dir(DestDir);
        Error ->
            {error,cant_create_dir,Error}
    end.

unzip_and_exec(Bin,DestDir,Script)->
    ok=create_dir_ifnec(DestDir),
    ok = erl_tar:extract({binary,Bin},[{cwd,DestDir},compressed]),
    AbsScript=filename:join([DestDir,Script]),
    myoscmd(AbsScript).

unzip(Bin,DestDir)->
    ?LOGF("Extracting tar in ~p on node ~p~n",[DestDir,node()],?INFO),
    ok=create_dir_ifnec(DestDir),
    case erl_tar:extract({binary,Bin},[{cwd,DestDir},compressed]) of
        ok   -> {ok, ""};
        Else -> Else
    end.

myoscmd(Cmd)->
    ?LOGF("Running command ~p on node~p~n",[Cmd,node()],?INFO),
    Res=os:cmd(Cmd ++" ; echo $?"),
    Tokens=string:tokens(Res,"\n"),
    case lists:nthtail(length(Tokens)-1,Tokens) of
        ["0"] ->
            {ok,Res};
        [ErrNo]->
            {error,list_to_integer(ErrNo),Res} %% @FIXME: remove errno from Res
    end.

chain_init(AllNodes,Command,Master,DestDir,PredPid,Id)->
    NextPid=case AllNodes of
                [] ->
                    none;
                [{_Pid, NextNode}|Nodes]->
                    spawn_link(NextNode,kaslave,chain_init,[Nodes,Command,Master,DestDir,self(),Id+1])
            end,
    ?DebugF("Open command ~p , pwd=~p~n",[Command,DestDir]),
    Port = open_port({spawn, Command},[stream,{cd, DestDir},exit_status,use_stdio,binary]),
    %% use a separate process to handle the port, otherwise, it will
    %% block if the port is running too slow, and will slow down the chain.
    LoopPort=spawn_link(kaslave,port_loop,[Command,DestDir,Master,Port,self()]),
    port_connect(Port,LoopPort),
    unlink(Port),
    loop(Master,PredPid,NextPid,LoopPort,Port,0,[],Id).

port_loop(Command,DestDir,Master,Port,Parent)->
    receive
        {data,Bin}->
            port_command(Port, Bin),
            port_loop(Command,DestDir,Master,Port,Parent);
        {eof, Id} ->
            true=port_close(Port),
            send(Master, {done, self(), node(), Id});
        _Msg -> % useful ??
            port_loop(Command,DestDir,Master,Port,Parent)
    after 120000 -> % @FIXME: hardcoded values is BAD
            send(Master, {port_timeout,self(), node()})
    end.

loop(Master,PredPid,NextPid,TarPid,Port,CurrentSize,Reader,Id)->
    receive
        {readerpid,Pid} ->
             ?DebugF("got reader pid:~p~n",[Pid]),
            %% we must link to the port to do limit the input rate.
%%             port_connect(Port,self()),
            loop(Master,PredPid,NextPid,TarPid, Port,CurrentSize,Pid,Id);
        {data,Bin, _From} ->
            send(NextPid,{data, Bin, self()} ),
            TarPid ! {data, Bin},
            NewSize=CurrentSize+size(Bin),
            case Reader of
                Pid when is_pid(Pid) -> Pid ! {next, self()};
                [] -> skip
            end,
%%             case NewSize rem ?config(logchunksize) == 0 of
%%                 true ->
%%                     mylog(Master,["slave ", Id," chain ",NewSize]);
%%                 _    -> ok
%%             end,
            loop(Master,PredPid,NextPid,TarPid, Port,NewSize,Reader,Id);
        {eof, _From} ->
            send(NextPid, {eof, self()}),
%%             case Reader of
%%                 Pid when is_pid(Pid) -> unlink(Port);
%%                 [] -> skip
%%             end,
            TarPid ! {eof, Id}
    after 300000 -> % @FIXME: hardcoded values is BAD
            send(Master, {chain_timeout,self(), node()})
    end.

send(none,_)  -> ok;
send(Pid,Msg) -> Pid ! Msg.
