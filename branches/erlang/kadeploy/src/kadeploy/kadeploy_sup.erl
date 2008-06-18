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

-module(kadeploy_sup).
-vc('$Id: kadeploy_sup.erl,v 0.0 2008/06/09 08:38:12 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-behaviour(supervisor).


-include("kaconf.hrl").

%% API
-export([start_link/1, start_chain_srv/3, start_mgr_srv/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(retries, 4).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Args]).

start_mgr_srv(Args)->
    Ref=get_unique_id(),
    ?LOGF("Ref for chain server: ~p~n",[Ref],?CRIT),
    DeployMgr = {Ref, {kadeploy_mgr, start_link, [Args]},
                 temporary, 2000, worker, [kadeploy_mgr]},
    supervisor:start_child(?MODULE,DeployMgr).

start_chain_srv(Type,File,NodeNumber)->
    Ref=get_unique_id(),
    ?LOGF("Ref for chain server: ~p~n",[Ref],?DEB),
    ChainSrv  = {Ref, {kachain_srv, start_link, [Type,File,NodeNumber,self()]},
                 temporary, 2000, worker, [kachain_srv]},
    supervisor:start_child(?MODULE,ChainSrv).

%% FIXME: can exhaust atom space !
get_unique_id() ->
    {A,B,C}=now(),
    L=lists:flatmap( fun(X)-> integer_to_list(X)++"-" end, [A,B,C]),
    list_to_atom(L).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([_Args]) ->
    KaNodes   = {kanodes_sup,{kanodes_sup,start_link,[]},
                 permanent,2000,supervisor,[kanodes_sup]},
    %% @FIXME: filename
    ConfigSrv =  {kaconfig, {kaconfig, start_link,
                             [{"./src/test/nodes.conf","./src/test/deploy.conf"}]},
                 permanent, 2000, worker, [kaconfig]},
    RebootSrv = {kareboot_srv, {kareboot_srv, start_link, []},
                 permanent, 2000, worker, [kareboot_srv]},
    {ok,{{one_for_one,?retries,1}, [KaNodes, ConfigSrv, RebootSrv]}}.

%%====================================================================
%% Internal functions
%%====================================================================

