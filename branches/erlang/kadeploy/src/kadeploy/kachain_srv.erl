%%%
%%%  Copyright 2008 � INRIA
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

-module(kachain_srv).
-vc('$Id: kadeploy_mgr.erl,v 0.0 2008/06/09 08:50:31 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-behaviour(gen_server).

-include("kaconf.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/4, start_transfert/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([read_forward/5]).

-record(transfert, {
          number,        %% total number of nodes of the tranfert
          status=waiting,%% waiting | started
          master,
          type,          %% tar | dd
          destdir,
          chunksize,     %%
          timeout,       %%
          start_chain,   %% date
          filename,      %% file to transfert
          transfered=[], %% finished nodes
          pending=[]     %% pending nodes
         }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Type::dd|tar, File::filename, NodeNumber::integer,
%%                  From:: pid) -> {ok,Pid} | ignore | {error,Error}
%%
%% @doc Starts a kachain_srv with NodeNumber of nodes the transfert
%% will start when NodeNumber of nodes are registered (see
%% start_transfert)
%%--------------------------------------------------------------------
start_link(Type,File,NodeNumber,From) ->
    gen_server:start_link(?MODULE, {Type,File,NodeNumber,From}, []).

%% @spec start_transfert(Pid::pid, DestDir::string, Node::node) -> ok
%% @doc the given node is ready start the transfert; the transfert
%% will start when all nodes are ready (see start_link)
start_transfert(Pid, DestDir,Node)->
    gen_server:cast(Pid, {transfert, {DestDir,Node,self()}}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init({Type,File,NodeNumber,From}) ->
    ?LOGF("Starting chain server for file ~p and ~p nodes~n",[File,NodeNumber],?INFO),
    State=#transfert{chunksize=?config(chunksize),
                     type=Type,
                     filename=File,
                     number=NodeNumber,
                     timeout=?config(transfert_timeout),
                     master=From},
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({transfert,{DestDir,Node,From}},  T=#transfert{number=Number,pending=P}) ->
    %% FIXME Destdir can be dependant on child ? (dd)
    NewState=T#transfert{pending=[{From,Node}|P], destdir=DestDir},
    case length(NewState#transfert.pending) of
        Number -> %% all nodes are waiting for transfert, go !
            %% timeout for transfert
            erlang:start_timer(T#transfert.timeout,self(),transfert_timeout),
            {noreply, start_chain(NewState) } ;
        _ ->
            {noreply, NewState}
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({done,From, FromNode,Id}, State) ->
    %% @TODO: update state and warn the kadeploy_node process that transfert is done
    ?LOGF("Transfert done for ~p:~p:~p ~n",[From,FromNode,Id],?DEB),
    Pending=case keytake(FromNode, 2, State#transfert.pending) of
                {value, {Pid,_}, TupleList2} ->
                    ?LOGF("Acknowledge ~p for transfert success !~n",[Pid],?DEB),
                    kadeploy_node:transfert_done(Pid),
                    TupleList2;
                false ->
                    ?LOGF("Can't find ~p in pending transfert !~n",[FromNode],?ERR),
                    State#transfert.pending
            end,
    {noreply, State#transfert{pending=Pending}};

handle_info({port_timeout, From, FromNode}, State) ->
    %% @TODO: update state and warn the kadeploy_node process that transfert has failed
    {noreply, State#transfert{}};
handle_info({timeout, _Ref, transfert_timeout}, State)  ->
    %% @FIXME
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

start_chain(T=#transfert{filename=Filename,pending=[{_Pid,NextNode}|Nodes],destdir=DestDir,
                         chunksize=ChunkSize}) ->
    {ok, FileInfo} = file:read_file_info(Filename),
    Size=FileInfo#file_info.size,
    ?LOGF("file size:~p KB~n",[Size/1024],?DEB),
    {Command,DestDirReal}=getcmd(T#transfert.type,DestDir),
    ?LOGF("Command:~p~n",[Command],?DEB),
    NextPid=spawn_link(NextNode,kaslave,chain_init,[Nodes,Command,self(),DestDirReal,self(),1]),
    ?LOG("first slave started, read file and send~n",?INFO),
    Reader=spawn_link(kachain_srv,read_forward,[Filename,ChunkSize,Size,NextPid,length(Nodes)+1]),
    NextPid ! {readerpid, Reader},
    T#transfert{start_chain=now(),status=started}.

getcmd(tar,DestDir) -> {"tar zx",DestDir};
getcmd(dd, DestDir)  -> {"dd of="++DestDir,"./"}. %don't change dir for dd

read_forward(Filename,ChunkSize,Size,NextPid, SlaveNumber)->
    {ok, IODevice} = file:open(Filename,[read,raw,binary]),
    ?LOGF("file ~p opened~n",[Filename],?DEB),
    read_forward(IODevice,0,ChunkSize,Size,NextPid,SlaveNumber).

read_forward(IODevice, Loc, ChunkSize, Size, RemotePid,SlaveNumber) when Loc+ChunkSize < Size->
    {ok, Data}=file:pread(IODevice, Loc, ChunkSize),
    RemotePid ! {data,Data,self()},
    receive
        {next, _RemotePid} -> ok
    after 60000 ->
          throw(timeout)
    end,
    read_forward(IODevice, Loc+ChunkSize, ChunkSize, Size, RemotePid,SlaveNumber);
read_forward(IODevice, Loc, _ChunkSize, Size, RemotePid, _SlaveNumber)->
    {ok, Data}=file:pread(IODevice, Loc, Size-Loc),
    RemotePid ! {data,Data,self()},
    RemotePid ! {eof,self()},
    ?LOG("closing file~n",?DEB),
    file:close(IODevice).

%% @FIXME: only in R12B and up. TODO Update erlang at sophia
keytake(Key, N, L) when is_integer(N), N > 0 ->
    keytake(Key, N, L, []).

keytake(Key, N, [H|T], L) when element(N, H) == Key ->
    {value, H, lists:reverse(L, T)};
keytake(Key, N, [H|T], L) ->
    keytake(Key, N, T, [H|L]);
keytake(_K, _N, [], _L) -> false.