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

-module(kanodes_sup).
-vc('$Id: kadeploy_sup.erl,v 0.0 2008/06/09 08:38:12 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-behaviour(supervisor).

-include("kaconf.hrl").

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(retries, 4).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec start_child(Args::term) -> FIXME
%% @doc Starts a child process (kadeploy_node)
start_child(Args) ->
    ?LOGF("Starting child with Args~p~n",[Args],?DEB),
    supervisor:start_child(?MODULE,[Args]).

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
init([]) ->
    ?LOG("Starting ~n", ?INFO),
    SupFlags = {simple_one_for_one,0, ?restart_sleep},
    ChildSpec = [
                 {kadeploy_node,{kadeploy_node, start_link, []},
                  temporary,2000,worker,[kadeploy_node]}
                ],
    {ok, {SupFlags, ChildSpec}}.

%%====================================================================
%% Internal functions
%%====================================================================

