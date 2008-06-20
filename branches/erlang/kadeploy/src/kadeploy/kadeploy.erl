%%%-------------------------------------------------------------------
%%% File    : kadeploy.erl
%%% Author  : Nicolas Niclausse <>
%%% Description :
%%%
%%% Created :  5 Jun 2008 by Nicolas Niclausse <>
%%%-------------------------------------------------------------------
-module(kadeploy).

-behaviour(application).

-include("kaconf.hrl").

%% Application callbacks
-export([start/2, stop/1, stop_all/1, status/1, reload/0]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    ?LOG("Starting kadeploy daemon ~n",?NOTICE),
    error_logger:tty(false),
    LogFile = ?config(log_file),
    ?LOGF("Logfile is~p ~n",[LogFile],?NOTICE),
    case  error_logger:logfile({open, LogFile }) of
        ok ->
            ?LOG("Starting kadeploy application ~n",?NOTICE),
            case kadeploy_sup:start_link(LogFile) of
                {ok, Pid} ->
                    {ok, Pid};
                Error ->
                    ?LOGF("Can't start ! ~p ~n",[Error], ?ERR),
                    Error
            end;
        {error, Reason} ->
            Msg = "Error while opening log file: " ,
            ?LOGF(Msg ++ " ~p ~n",[Reason], ?ERR),
            erlang:display(Msg ++ Reason),
            {error, Reason}
    end.

%% reload configuration
reload()->
    todo.

status(Args)->
    todo.
%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
stop(_State) ->
    ?LOG("Stopping kadeploy application ~n",?NOTICE),
    ok.

stop_all([Node]) ->
    case net_adm:ping(Node) of
        pong ->
            erlang:display("stop kadeploy on node " ++ atom_to_list(Node)),
            rpc:call(Node,application,stop,[kadeploy]),
            slave:stop(Node);
        pang ->
            erlang:display("kadeploy not started on node " ++ atom_to_list(Node))
    end.

%%====================================================================
%% Internal functions
%%====================================================================
