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

-module(kadeploy_mgr).
-vc('$Id: kadeploy_mgr.erl,v 0.0 2008/06/09 08:50:31 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-behaviour(gen_server).


-include("kaconf.hrl").
-include("kaenv.hrl").

%% API
-export([start_link/1, deploy/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          deployment,
          pids,
          good,
          bad,
          env,
          clientpid
          }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).


%% @spec deploy(User::string, Nodes::List,
%%       Env::record(environment) | {anonymous, Directory::string} |
%%       {recorded, Name::string}, Opts::record(deploy_opts) ) ->
%%             { NodesOK::List, BadNodes::List }
%% @doc main method for deploying a given environment on a list of nodes.
%%
%% No nodes, aborts
deploy(User,[],Env,Opts) ->
    ?LOG("No nodes to deploy, abort !",?ERR),
    {[],[]};
%%
deploy(User,Nodes,Env,Opts) ->
    {ok, Pid} = kadeploy_sup:start_mgr_srv({User,Nodes,Env,Opts}),
    %% a timer is used to stop the deployment after #deploy_opts.timeout
    %% here we add another timeout (with one minute more) for the call (is
    %% it really necessary ?)
    TimeOut=Opts#deploy_opts.timeout + 60000,
    gen_server:call(Pid, {deploy}, TimeOut).


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
init({User, Nodes, EnvName, Opts}) ->
    Depl= #deployment{username=User,
                      nodes= Nodes,
                      envname=EnvName,
                      options=Opts, startdate=now()},
    {ok, #state{deployment=Depl}};
init(Args) ->
    ?LOGF("error: init with args~p~n",[Args],?ERR).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({deploy}, From, State=#state{deployment=D}) ->
    Opts=D#deployment.options,
    {ok, Env} = check_rights(D),
    %% start the deployment timer
    erlang:start_timer(Opts#deploy_opts.timeout,self(),deploy_timeout),
    case deploy_setup(D,Env) of
        {[], Bad } ->
            ?LOGF("Only bad nodes, something must be wrong!~p~n",[Bad],?ERR),
            {stop, normal, {[],Bad}, State};
        {Good, Bad} ->
            N=length(Good),
            %% @FIXME: should we try again with bad nodes ?
            ?LOGF("OK, start with ~p nodes~n",[N],?NOTICE),
            %% start the chain server
            {ok, ChainPid} = kadeploy_sup:start_chain_srv(tar, Env#environment.filebase,N),
            %% start a process (fsm) for each (good) node to deploy
            StartChild= fun(Node)->kadeploy_node:start({Node,Env,Opts,ChainPid,self()}) end,
            OutPut= lists:map(StartChild, Good),
            {Pids, BadChilds} = get_good_childs(OutPut),
            %% @FIXME what if retries ?
            %% @FIXME handle badchilds: remove them from chain, ...
            %% @FIXME: improve timeout: we need a global timeout from
            %%         the beginning of the deployment
            ?LOGF("Call wait_nodes with pids ~p (timeout is ~p)~n",[Pids,Opts#deploy_opts.timeout],?DEB),
            %% initialise bad nodes list with 'timeout' reason
            AllBad = Bad ++badnodes(Good,?fail_timeout),
%%             {noreply,State#state{clientpid=From,pids=Pids,good=[],bad=AllBad,env=Env },Opts#deploy_opts.timeout}
            {noreply,State#state{clientpid=From,pids=Pids,good=[],bad=AllBad,env=Env } }
    end;
handle_call(Request, From, State) ->
    ?LOGF("Unknown call ~p from ~p~n",[Request,From],?WARN),
    Reply = error,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?LOGF("Unknown Msg ~p~n",[Msg],?WARN),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
%% @FIXME: handle minnodes (stop as soon as we have minnodes successfully deployed

%% last pid has finished
handle_info({done, FromPid, Host }, State=#state{pids=[Pid],bad=Bad,good=Good}) ->
    ?LOGF("Last Host ~p has been deployed~p~n",[Host],?DEB),
    NewBad = delbadnode(Bad,Host),
    gen_server:reply(State#state.clientpid,{[Host|Good],NewBad}),
    {stop,normal,State};

handle_info({done, FromPid, Host }, State=#state{deployment=Depl,pids=Pids,good=Good,bad=Bad}) ->
    ?LOGF("Host ~p has been deployed~p~n",[Host],?DEB),
    Now=now(),
    NewPids= Pids -- [FromPid],
    NewBad = delbadnode(Bad,Host),
    Opts=Depl#deployment.options,
    {noreply, State#state{pids=NewPids,good=[Host|Good],bad=NewBad}};

handle_info({error, Reason, FromPid,Host }, State=#state{deployment=Depl,pids=Pids,bad=Bad}) ->
    ?LOGF("Client ~p has failed, reason ~p~n",[Host,Reason],?WARN),
    Now=now(),
    NewPids = Pids -- [FromPid],
    NewBad = setreason(Bad,Host,Reason),
    case Pids -- [FromPid] of
        [] ->
            ?LOG("No more nodes, stop ~n",?INFO),
            gen_server:reply(State#state.clientpid,{State#state.good,NewBad}),
            {stop,normal,State};
        NewPids->
            ?LOGF("Wait for Pids~p ~n",[NewPids],?DEB),
            Opts=Depl#deployment.options,
            {noreply,State#state{pids=NewPids,bad=NewBad}}
    end;

handle_info({timeout, _Ref, deploy_timeout}, State=#state{good=Good,bad=Bad})->
    ?LOG("Timeout, abort deployment ~n",?DEB),
    gen_server:reply(State#state.clientpid,{Good,Bad}),
    {stop, normal, State};

handle_info(Msg, State) ->
    ?LOGF("Unknown Msg ~p~n",[Msg],?WARN),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    ?LOGF("Terminate mgr ~p~n",[Reason],?WARN),
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


%% @spec deploy_check(D::record(deployment), MgrPid::pid) ->
%%      {NodesOK::List, BadNodes::List }
check_rights(D=#deployment{username=User}) ->
    %% first, we should get the environment
    {ok, Env} = case is_list(D#deployment.envname) of
                    true ->
                        kaenv:getenv(D#deployment.envname);
                    false -> % already a record(environment)
                        {ok, D#deployment.envname}
                end,
    %% check if the user has access to this environment
    ok = karights:check_env(User, Env),

    %% we should check if the user has rights on each nodes/partition
    ok = karights:check_nodes(User,D#deployment.partition,D#deployment.nodes),
    %% extract kernel from environment if needed (tar format only)
    ok = kaenv:extract_kernel(Env),
    {ok, Env}.

deploy_setup(D=#deployment{options=Opts},Env)
  when Opts#deploy_opts.method==virt ->
    %% @TODO
    {D#deployment.nodes, []};

deploy_setup(D=#deployment{options=Opts,nodes=Nodes},Env)
  when Opts#deploy_opts.method==deployenv ->
    %% @TODO
    {D#deployment.nodes, []};

deploy_setup(D=#deployment{options=Opts},Env)
  when Opts#deploy_opts.method==nfsroot ->
    %% @TODO
    {D#deployment.nodes, []};

deploy_setup(D=#deployment{options=Opts,nodes=Nodes},Env)
  when Opts#deploy_opts.method==currentenv ->
    %% First we need to start a beam on each node
    {ok, TimeOut}=kaconfig:getval(first_check_env_timeout),
    ?LOGF("Try to start remote beams on node (currentenv case), use timeout ~p~n",
         [TimeOut*1000],?NOTICE),
    %% WARN: diststart is much faster for big deployement, but it
    %% requires that ssh access between nodes works
    %% {Good,Bad}=kaslave:pdiststart(Nodes,TimeOut,Opts#deploy_opts.erlang_args),
    %% @FIXME: make it customizable
    {Good,Bad}=kaslave:pstart(Nodes,TimeOut*1000,Opts#deploy_opts.erlang_args),
    {Good,badnodes(Bad,?fail_kaslave)}.

get_good_childs(StartChildOutput)->
    %% @FIXME: temporary hack for test
    Fun = fun({ok, Pid}) -> Pid end,
    {lists:map(Fun,StartChildOutput),[]}.

badnodes(Nodes, Error)->
    Fun = fun(Name) -> {Name, {error, Error}} end,
    lists:map(Fun, Nodes).

delbadnode(BadList, Host)->
    lists:keydelete(Host, 1, BadList).

setreason(BadList, BadHost, Reason)->
    lists:keyreplace(BadHost,1, BadList, {BadHost, {error, Reason}}).