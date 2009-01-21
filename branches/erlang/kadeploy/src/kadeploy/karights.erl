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

-module(karights).
-vc('$Id: karights.erl,v 0.0 2008/06/09 10:52:43 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-include("kaconf.hrl").
-include("kaenv.hrl").

%% @ doc everything related to ACL a rights management

%% API
-export([check_nodes/3, check_env/2]).


%% @spec check_nodes(User::String,  Nodes::List) -> ok | { error, Reason }
%% @doc check if a given user has the rights to deploy of the given nodes
check_nodes(User,Partition,Nodes)->
    ok.

%% @spec check_env(User::String,  Env::record(environment) ) -> ok | { error, Reason }
%% @doc Check if a user has the rights to use this environment
check_env(User,Env)->
    ok.
