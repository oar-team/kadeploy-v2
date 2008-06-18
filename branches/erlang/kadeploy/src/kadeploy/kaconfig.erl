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

-module(kaconfig).
-vc('$Id: kadeploy_mgr.erl,v 0.0 2008/06/09 08:50:31 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-behaviour(gen_server).


-include("kaconf.hrl").
-include("kaenv.hrl").

%% API
-export([start_link/1,
         reload/0, getnodeconf/1,
         getval/1, getval/2, getval_or_fail/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          nodeconfig,
          nodeconfigfile,
          clustersconfig,
          config,
          configfile
         }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Args) ->
    ?LOGF("Start config server with args ~p~n",[Args],?DEB),
    gen_server:start_link({global, ?MODULE}, ?MODULE, Args, []).

%% @spec getnodeconf(Host::string) -> Config::List or {error, Reason}
getnodeconf(Host) ->
    gen_server:call({global, ?MODULE}, {getnodeconf, Host}).

reload() ->
    gen_server:call({global, ?MODULE}, {reload}).

getval(Key,Config) ->
    ?LOGF("Get config key:~p~n",[Key],?DEB),
    case lists:keysearch(Key,1,Config) of
        {value, {Key, Val }} -> {ok, Val};
        false -> {error, undef}
    end.

getval_or_fail(Key,Config) ->
    case catch getval(Key,Config) of
        {ok, Duke} -> Duke;
        _ -> kadeploy_node:failure({?fail_badconf, Key})
    end.

%% @spec getval(Keys::List || Key::atom) ->
%% @doc ask for global parameter(s)
getval(Keys)->
    gen_server:call({global, ?MODULE}, {getval,Keys}).


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
init({NodeConfFile,GlobalConfFile}) ->
    ?LOGF("Reading node config file ~p~n",[NodeConfFile],?INFO),
    {ok, NodeConfig} = readnodeconf(NodeConfFile),
    {ok, ClusterConfig} = readclusters(NodeConfig,filename:dirname(GlobalConfFile)),
    ?LOGF("Reading global config file ~p~n",[GlobalConfFile],?INFO),
    {ok, GlobalConfig} = readglobalconf(GlobalConfFile),
    {ok, #state{ config=GlobalConfig, configfile=GlobalConfFile,
                 clustersconfig=ClusterConfig,
                 nodeconfig=NodeConfig, nodeconfigfile=NodeConfFile
               }}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({getval, Key}, _From, State) ->
    Reply = getval(Key,State#state.config),
    {reply, Reply, State};

handle_call({getnodeconf, Host}, _From, State) ->
    case gb_trees:lookup(Host,State#state.nodeconfig) of
        {value, HostConf} ->
            ?LOGF("ok, found ~p~n",[HostConf],?DEB),
            %% merge global conf and node conf
            ?LOGF("ok, merge global conf ~p~n",[HostConf],?DEB),
            Conf = HostConf++State#state.config,
            %% merge cluster conf and node conf
            case getval(cluster,Conf) of
                {ok, Cluster}->
                    {ok, ClusterConf} = getval(list_to_atom(Cluster),State#state.clustersconfig),
                    NewConf=Conf++ClusterConf,
                    ?LOGF("ok, merge cluster conf ~p~n",[NewConf],?DEB),
                    {reply, NewConf, State};
                Other ->
                    ?LOGF("No cluster definition for host ~p (~p)~n",[Host,Other],?ERR),
                    {reply, Conf, State}
            end;
        Other ->
            {reply, {error, Other}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
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

%% temporary funs for reading config.

readclusters(NodesConfig,ConfDir) ->
    Nodes = gb_trees:keys(NodesConfig),
    Clusters=getclusters(NodesConfig, Nodes),
    ?LOGF("clusters ~p~n",[Clusters],?CRIT),
    F=fun(A)->
              F=filename:join(ConfDir,"cluster-"++A++".conf"),
              {ok,Conf}=readglobalconf(F),
              {list_to_atom(A),Conf}
    end,
    Config=lists:map(F, Clusters),
    {ok,Config}.

getclusters(NodesConfig, Nodes)-> getclusters(NodesConfig, Nodes,[]).
getclusters(NodesConfig, [],Clusters)-> Clusters;
getclusters(NodesConfig, [Node|Tail],Clusters)->
    {value,Conf} = gb_trees:lookup(Node,NodesConfig),
    case getval(cluster,Conf) of
        {ok,Val} -> getclusters(NodesConfig, Tail,lists:umerge([Val],Clusters));
        _        -> getclusters(NodesConfig, Tail,Clusters)
    end.

readnodeconf(FileName) ->
    case file:open(FileName, [read]) of
        {error, Reason} ->
            {error, Reason};
        {ok , File} ->
            Conf = read_lines(File, fun(A,B)->parse_node(A,B) end, gb_trees:empty()),
            file:close(File),
            {ok, Conf}
    end.

readglobalconf(FileName) ->
    case file:open(FileName, [read]) of
        {error, Reason} ->
            {error, Reason};
        {ok , File} ->
            Conf = read_lines(File, fun(A,B)->parse_global(A,B) end, []),
            file:close(File),
            {ok, Conf}
    end.

read_lines(FD,Fun,InitTable) ->
    read_lines(FD,io:get_line(FD,""),Fun,InitTable).

read_lines(_FD, eof, Fun, Table) ->
    Table;
read_lines(FD, "\n", Fun, Table) -> %skip comment
    read_lines(FD, io:get_line(FD,""),Fun, Table);
read_lines(FD, "#"++Rest, Fun, Table) -> %skip comment
    read_lines(FD, io:get_line(FD,""),Fun,Table);
read_lines(FD, Line, Fun, Table) ->
    CLine=katools:chop(Line),
    ?LOGF("read line ~p~n",[CLine],?DEB),
    NewTable=Fun(CLine, Table),
    read_lines(FD, io:get_line(FD,""),Fun, NewTable).

parse_global(Line,Table)->
    {TmpKey , ValueStr} = katools:split2(Line,$=,strip),
    Key=list_to_atom(TmpKey),
    Value=try_integer(ValueStr),
    [{Key,Value}|Table].

parse_node(Line,Table)->
    [Node, Key | TmpValue] = string:tokens(Line," "),
    ValueStr=katools:join(" ",TmpValue),
    Value=try_integer(ValueStr),
    case gb_trees:lookup(Node,Table) of
        {value,NodeConf} ->
            gb_trees:update(Node,[{list_to_atom(Key), Value}|NodeConf],Table);
        none ->
            gb_trees:insert(Node,[{list_to_atom(Key), Value}],Table)
    end.


try_integer(String)->
    case catch list_to_integer(String) of
        Val when is_integer(Val)->
            Val;
        _ ->String
    end.
