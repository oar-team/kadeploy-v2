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

-module(katools).
-vc('$Id: katools.erl,v 0.0 2008/06/09 12:48:39 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-include("kaconf.hrl").

-export([debug/3, debug/4, get_val/1, elapsed/2, join/2, clean_str/1,
         chop/1, check_host/3, ip_tostr/1, split2/2, split2/3, ip_hex/1]).

level2int("debug")     -> ?DEB;
level2int("info")      -> ?INFO;
level2int("notice")    -> ?NOTICE;
level2int("warning")   -> ?WARN;
level2int("error")     -> ?ERR;
level2int("critical")  -> ?CRIT;
level2int("emergency") -> ?EMERG.

get_val(Var) ->
    case application:get_env(Var) of
        {ok, Val} ->
            ensure_string(Var, Val);
        undefined -> % undef, application not started, try to get var from stdlib
            case application:get_env(stdlib,Var) of
                undefined -> {undef_var, Var};
                {ok,Val}  -> ensure_string(Var, Val)
            end
    end.

%% ensure atom to string conversion of environnement variable
ensure_string(log_file, Atom) when atom(Atom) ->
    atom_to_list(Atom);
ensure_string(proxy_log_file, Atom) when atom(Atom) ->
    atom_to_list(Atom);
ensure_string(config_file, Atom) when atom(Atom) ->
    atom_to_list(Atom);
ensure_string(_, Other) ->
    Other.

debug(From, Message, Level) ->
    debug(From, Message, [], Level).

debug(From, Message, Args, Level) ->
    Debug_level = ?config(debug_level),
    if
        Level =< Debug_level ->
            error_logger:info_msg("~20s:(~p:~p) "++ Message,
                                  [From, Level, self()] ++ Args);
        true ->
            nodebug
    end.

elapsed({Before1, Before2, Before3}, {After1, After2, After3}) ->
    After  = After1  * 1000000000  + After2  * 1000 + After3/1000,
    Before = Before1 * 1000000000  + Before2 * 1000 + Before3/1000,
    After - Before.

%% A Perl-style join --- concatenates all strings in Strings,
%% separated by Sep.
join(_Sep, []) -> [];
join(Sep, List) when is_list(List)->
    join2(Sep, lists:reverse(List)).
join2(Sep, [First | List]) when is_integer(First)->
    join2(Sep, [integer_to_list(First) | List]);
join2(Sep, [First | List]) when is_float(First)->
    join2(Sep, [float_to_list(First) | List]);
join2(Sep, [First | List]) when is_list(First)->
        lists:foldl(fun(X, Sum) -> X ++ Sep ++ Sum end, First, List).


%% check if a port is open
check_host(PortNo, Host, TimeOut) ->
        case gen_tcp:connect(Host,PortNo,[], TimeOut) of
            {ok,Sock} ->
                gen_tcp:close(Sock),
                ok;
            {error, Reason} ->
                {error, Reason}
        end.

chop(String) -> string:strip(String, right, 10).

%%----------------------------------------------------------------------
%% Func: clean_str/1
%% Purpose: remove "\n" and space at the beginning and at that end of a string
%%----------------------------------------------------------------------
clean_str(String) ->
    Str1 = string:strip(String, both, 10),
    Str2 = string:strip(Str1),
    Str3 = string:strip(Str2, both, 10),
    string:strip(Str3).

ip_tostr(IP)  when is_tuple(IP) ->
    L=lists:map(fun(A)->integer_to_list(A) end,tuple_to_list(IP)),
    join(".",L).

%% split a string in 2 (at first occurence of char)
split2(String,Chr) ->
    split2(String,Chr,nostrip).

split2(String,Chr,strip) -> % split and strip blanks
    {A, B} = split2(String,Chr,nostrip),
    {string:strip(A), string:strip(B)};
split2(String,Chr,nostrip) ->
    case string:chr(String, Chr) of
        0   -> {String,[]};
        Pos -> {string:substr(String,1,Pos-1), string:substr(String,Pos+1)}
    end.

ip_hex(A={IP1,IP2,IP3,IP4})->
    lists:flatten(io_lib:format(lists:flatten(lists:duplicate(4,"~2.16.0B")),tuple_to_list(A))).