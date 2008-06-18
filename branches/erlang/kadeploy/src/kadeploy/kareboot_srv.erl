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

-module(kareboot_srv).
-vc('$Id: kadeploy_mgr.erl,v 0.0 2008/06/09 08:50:31 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-behaviour(gen_server).

-include("kaconf.hrl").


%% API
-export([start_link/0, reboot/2, kexec/2, reboot_virt/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([handle_reboot/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% @spec reboot(Nodes::List, {Soft::string, Hard::string, Power::string}) -> ok
%% @doc  reboot a list of nodes using an external script (async)
%% should we add a function for type of remote power managememnt ?
%%    (for ex: ipmi, drac,rsa/telnet)
reboot(Node,Methods)->
    gen_server:cast({global, ?MODULE},{reboot, Node, Methods, self()}).

kexec(Node,Args)->
    gen_server:cast({global, ?MODULE},{kexec, Node, Args, self()}).

reboot_virt(Node)->
    gen_server:cast({global, ?MODULE},{reboot_virt, Node, self()}).

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
init([]) ->
    {ok, #state{}}.

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
handle_cast({reboot, Node, {Soft,Hard,Power}, From}, State) ->
    %% TODO: add a maximum number of concurrent process running a reboot command ?
    %% we can use: {links, LinkedPidList} = process_info(self(), links).
    spawn_link(?MODULE,handle_reboot,[From, Node, [{soft,Soft},{hard,Hard},{power,Power}]]),
    {noreply, State};

handle_cast(Msg, State) ->
    ?LOGF("Got message Msg ~p~n",[Msg],?DEB),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
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

handle_reboot(Pid,Node,[{_Type, [] }])->
    ?LOGF("Failed to reboot node ~p!~n",[Node],?ERR),
    kadeploy_node:reboot_failed(Pid);
handle_reboot(Pid,Node,[{Type,NextMethod}|Rest])->
    ?LOGF("Try to boot ~p with cmd ~p~n",[Node,NextMethod],?DEB),
    %% @FIXME add timeout ? or let the node process handle it's timeout ?
    case kaslave:myoscmd(NextMethod++" "++Node) of
        {ok, _Res} ->
            kadeploy_node:reboot_done(Pid);
        {error, _ErrNo, Reason} ->
            ?LOGF("Fail to reboot node ~p with ~p method (Err: ~p), try harder~n",
                  [Node, Type, Reason], ?NOTICE),
            handle_reboot(Pid,Node,Rest)
    end.

