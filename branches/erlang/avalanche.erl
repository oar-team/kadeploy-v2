%%%
%%%  Copyright © Nicolas Niclausse. 2007
%%%
%%%  Author : Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%  Created: 26 jun 2007 by Nicolas Niclausse <nniclaus@sophia.inria.fr>
%%%
-module(avalanche).
-vc('$Id: avalanche.erl,v 0.0 2007/06/26 07:40:15 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-export([start/4]).
-export([start/1]).
-export([diststart/1]).
-export([pdiststart/1]).
%-export([start/3]).
-export([pstart/1]).
-export([wait/1]).
-export([deploy/5]).
-export([deploy/6]).
-export([nosplit/1]).
-export([start_slave/8]).
-export([splithalf/1]).

-define(DEFAULT_ARGS,"-connect_all false +K true -rsh oarsh -setcookie glop -pa /home/sophia/nniclausse/sources/erlang").
%-define(DEFAULT_ARGS,"-rsh ssh -setcookie glop -pa /home/sophia/nniclausse/sources/erlang").

nstart(N, Host, Fun, DoSpawn) -> % start N beam on host Host
    start(lists:duplicate(N,Host), ?DEFAULT_ARGS,Fun, DoSpawn).

%start/1
start(oar) ->
    Hosts=get_batch_nodes("OAR_NODEFILE"),
    start(Hosts, ?DEFAULT_ARGS);
start(Hosts) ->
    start(Hosts, ?DEFAULT_ARGS).

% start/2
start(Hosts, Args) ->
    start(Hosts, Args, nosplit, false).

pstart(oar) ->
    Hosts=get_batch_nodes("OAR_NODEFILE"),
    pstart(Hosts, ?DEFAULT_ARGS);
pstart(Hosts) ->
    pstart(Hosts, ?DEFAULT_ARGS).
pstart(Hosts, Args) ->
    start(Hosts, Args, nosplit, true).

%% distributed start (each started beam starts itself half of the
%% remaining nodes to be started.
diststart(oar) ->
    Hosts=get_batch_nodes("OAR_NODEFILE"),
    diststart(Hosts, ?DEFAULT_ARGS);
diststart(Hosts) ->
    diststart(Hosts, ?DEFAULT_ARGS).
diststart(Hosts, Args) ->
    start(Hosts, Args,splithalf, false).

%% distributed start (each started beam starts itself half of the
%% remaining nodes to be started. Moreover, use spawn to start a beam.
pdiststart(oar) ->
    Hosts=get_batch_nodes("OAR_NODEFILE"),
    pdiststart(Hosts, ?DEFAULT_ARGS);
pdiststart(Hosts) ->
    pdiststart(Hosts, ?DEFAULT_ARGS).
pdiststart(Hosts, Args) ->
    start(Hosts, Args, splithalf, true).

start(Hosts, Args, Fun, DoSpawn) -> % start beam on host list
    N=length(Hosts),
    erlang:spawn(?MODULE,deploy, [Hosts, Args, Fun, self(), DoSpawn]),
    {Res,ok} = timer:tc(?MODULE,wait,[N]),
    erlang:display(["done! ", [Res/1000000]]),
    UniqueHosts= lists:map(fun(A)->list_to_atom(A) end, lists:usort(Hosts)),
    List = net_adm:world_list(UniqueHosts),
    erlang:display([" connected beams: ", [integer_to_list(length(List))]]).

wait(0) -> ok;
wait(N) ->
    receive
        {started, From} ->
%            erlang:display(["node started on ", [From, N]]),
            wait(N-1);
        {notstarted, From} ->
            erlang:display(["node NOT started on ", [From, N]]),
            wait(N-1)
    after 15000 ->
            erlang:display(["timeout! ", [N]]),
	    ok
    end.

deploy(Hosts, Args, Fun, Master,DoSpawn)->
    deploy(Hosts, Args, Fun, Master,1, DoSpawn).

deploy([],_,_,_,_,_) ->ok;

deploy([Host | Hosts], Args, Fun, Master, I, DoSpawn) ->
    {MyHost, HisHosts} = ?MODULE:Fun(Hosts),
    ArgList=[Host, "avalanche" ++ integer_to_list(I),
                               I+1+length(MyHost),Args,HisHosts,Fun, Master,DoSpawn],
%    erlang:display(["start slave on host ", [Host]]),
    case DoSpawn of 
        true  -> spawn(?MODULE,start_slave,ArgList);
        false -> start_slave(ArgList)
    end,
    deploy(MyHost,Args,Fun, Master, I+1, DoSpawn).

start_slave([Host,Name, I, Args, MyHosts, Fun, Master,DoSpawn]) ->
    start_slave(Host,Name, I, Args, MyHosts, Fun, Master,DoSpawn).
start_slave(Host,Name, I, Args, MyHosts, Fun, Master,DoSpawn) ->
    case slave:start(Host, Name, Args) of
        {ok, Node} ->
            Master ! {started, Node},
            rpc:cast(Node,?MODULE,deploy, [MyHosts, Args, Fun, Master, I,DoSpawn]);
        Reason ->
            Master ! {notstarted, {Reason, Host, Name}}
    end.

nosplit(List)     -> {List,[]}.

splithalf([])     -> {[],[]};
splithalf([List]) -> {[List],[]};
splithalf(List) ->
    S = (length(List)+1) div 2,
    lists:split(S, List).

get_batch_nodes(Env) ->
    case os:getenv(Env) of
        false ->
            [];
        NodeFile ->
            {ok, Nodes} = file_to_list(NodeFile),
%            lists:map(fun shortnames/1, Nodes)
            Nodes
    end.

file_to_list(FileName) ->
    case file:open(FileName, read) of
        {error, Reason} ->
                        {error, Reason};
        {ok , File} ->
            Lines = read_lines(File),
            file:close(File),
                        {ok, Lines}
    end.

read_lines(FD) ->read_lines(FD,io:get_line(FD,""),[]).

read_lines(_FD, eof, L) ->
    lists:reverse(L);
read_lines(FD, Line, L) ->
    read_lines(FD, io:get_line(FD,""),[chop(Line)|L]).

chop(String) ->
        string:strip(String, right, 10).

