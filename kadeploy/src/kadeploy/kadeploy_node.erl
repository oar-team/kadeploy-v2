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

-module(kadeploy_node).
-vc('$Id: kadeploy_node.erl,v 0.0 2008/06/09 08:55:40 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-behaviour(gen_fsm).

-include("kaconf.hrl").
-include("kaenv.hrl").

-define(init_timeout, 1).

%% FSM with 4 states: first_boot, setup_env, transfert, last_boot

%% API
-export([start_link/1, start/1, transfert_done/1, transfert_failed/1,
         reboot_done/1, reboot_failed/1]).
-export([failure/1]).

%% gen_fsm callbacks
-export([init/1,
         setup_env/2, setup_env/3,
         transfert/2, transfert/3,
         first_boot/2, first_boot/3,
         last_boot/2, last_boot/3,
         handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {
          hostname, %% remote hostname
          master,   %% pid of master process handling the deployment.
          chainsrv, %% pid of chain process
          node,     %% remote erlang node name
          config,
          env,
          retry=0,  %% integer: current retry number (for the current state)
          max_retry, %% integer: max number of retries (for each state)
          system_maxretries=0, %% max retries for system command (fdisk, ...) FIXME:useful ?
          transfert_start, %% date
          transfert_timeout,
          first_boot_timeout,
          last_boot_timeout,
          first_check_interval,
          check_interval,
          check_timeout,
          check_portno,
          options
         }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Args::tuple) -> {ok,Pid} | ignore | {error,Error}
%% @doc Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_fsm:start_link(?MODULE, Args, []).

%% @spec start(Args::tuple) -> {ok, Pid}
%% @doc starts a new kadeploy_node fsm process (call the supervisor)
%% and the start the activity
start(Args)->
    {ok, Pid} = kanodes_sup:start_child(Args),
    gen_fsm:send_event(Pid, {start}),
    {ok, Pid}.

%% @spec: transfert_done(Pid::pid) -> ok
%% @doc: called by the kachain server when the transfert has finished
%% for this node (asynchronous call)
transfert_done(Pid) ->
    ?LOGF("Send transfert done event to~p~n",[Pid],?DEB),
    gen_fsm:send_event(Pid, {transfert_done}).

%% @spec: transfert_failed(Pid::pid) -> ok
%% @doc: called by the kachain server when the transfert has failed
%% for this node (asynchronous call)
transfert_failed(Pid) ->
    ?LOGF("Send transfert failed event to~p~n",[Pid],?DEB),
    gen_fsm:send_event(Pid, {transfert_failed}).

%% @spec: reboot_done(Pid::pid) -> ok
%% @doc: called by the reboot server when the reboot command has been
%% successfully executed for this node (asynchronous call)
reboot_done(Pid) ->
    gen_fsm:send_event(Pid, {reboot_done}).

%% @spec: reboot_failed(Pid::pid) -> ok
%% @doc: called by the reboot server when the reboot command has
%% failed for this node (asynchronous call)
reboot_failed(Pid) ->
    ?LOGF("Send reboot failed event to~p~n",[Pid],?DEB),
    gen_fsm:send_event(Pid, {reboot_failed}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%--------------------------------------------------------------------
init({Host, Env, Opts, ChainSrv,Master}) ->
    ?LOGF("Initialize with Args~p~n",[{Host, Env, Opts,Master}],?DEB),
    RemoteNode = list_to_atom("kaslave@"++Host),
    %% use process dictionnary for failure handling
    put(master,Master),
    put(hostname,Host),
    %% FIXME: get the timeout values from the kaconfig server ?
    State = #state{hostname  = Host,
                   chainsrv  = ChainSrv,
                   master    = Master,
                   node      = RemoteNode,
                   env       = Env,
                   options   = Opts,
                   max_retry            =?config(max_retry),
                   transfert_timeout    =?config(transfert_timeout),
                   first_boot_timeout   =?config(first_boot_timeout),
                   last_boot_timeout    =?config(last_boot_timeout),
                   first_check_interval =?config(first_check_interval),
                   check_interval       =?config(check_interval),
                   check_timeout        =?config(check_timeout),
                   check_portno         =?config(check_portno)},
    case Opts#deploy_opts.method of
        prod_env ->
            {ok, setup_env, State};
        deploy_env ->
            {ok, first_boot, State};
        nfsroot ->
            {ok, first_boot, State};
        virt ->
            {ok, setup_env, State} %TODO
    end.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%%--------------------------------------------------------------------
first_boot({start}, State=#state{options=Opts}) when Opts#deploy_opts.method == deploy_env->
    Config = kaconfig:getnodeconf(State#state.hostname),
    case kaenv:setup_pxe(nogrub,State#state.hostname,Config,deploykernel,[]) of
        ok ->
            kareboot_srv:reboot(State#state.hostname, get_reboot_cmds(Config)),
            %% FIXME: handle failure of reboot server ?
            {next_state, first_boot, State#state{config=Config}, State#state.first_boot_timeout};
        {error, Reason} ->
            ?LOGF("Error setup_pxe ~p~n",[Reason],?ERR),
            %% don't retry setup pxe
            failure(?fail_setup_pxe)
    end;

first_boot({reboot_failed}, State=#state{})->
    ?LOG("reboot failed~n",?DEB),
    reboot_retry(State, first_boot, State#state.first_boot_timeout,?fail_first_boot);

first_boot({reboot_done}, State=#state{})->
    %% the reboot command was succesfully launched. Now we must wait
    %% for the node to finish it's reboot sequence.
    %% start the timer
    ?LOG("First reboot command done, wait for node to reboot~n",?INFO),
    erlang:start_timer(State#state.first_boot_timeout,self(),first_boot_timeout),
    {next_state, first_boot, State, State#state.first_check_interval};

first_boot(timeout, State=#state{})->
    ?LOG("First boot timeout, check host~n",?DEB),
    case katools:check_host(State#state.check_portno,
                            State#state.hostname,
                            State#state.check_timeout) of
        ok ->
            {next_state, setup_env, State#state{retry=0}, 1}; % switch to setup_env state
        {error, _Reason} ->
            {next_state, first_boot, State, State#state.check_interval}
    end;

first_boot(Event, State=#state{})->
    ?LOGF("got event in state first_boot~n",[Event],?NOTICE),
    {next_state, first_boot, State}.


%% @spec setup_env(Event::term, State::record(state)) ->
%%  {next_state, transfert|setup_env, State::record(state), Timeout::integer} |
%%  {stop, Reason}
%% @doc setup a deployment environment

setup_env(timeout, State=#state{options=Opts,config=Config}) when Opts#deploy_opts.method==deploy_env ->
%% just switch from first_boot
    try setup_disk(State#state.config,State,Opts) of
      NewState ->
            ?LOGF("~p: setup_disk done, ask for transfert ~n",[State#state.hostname],?DEB),
            Mount  = getval(mount,Config),
            kachain_srv:start_transfert(NewState#state.chainsrv, Mount, NewState#state.node),
            {next_state, transfert, NewState#state{retry=0}, State#state.transfert_timeout}
    catch throw: {rpc, ErrorType, Error} ->
            retry(State, ErrorType, Error)
    end;

setup_env(timeout, State=#state{options=Opts}) when Opts#deploy_opts.method == prod_env ->
    setup_env({start}, State);
setup_env({start}, State=#state{options=Opts}) when Opts#deploy_opts.method == prod_env ->
    ok = check_erlang_node(State#state.node, State),
    Config = kaconfig:getnodeconf(State#state.hostname),
    try setup_disk(Config,State,Opts) of
        NewState ->
            ?LOGF("~p: setup_disk done, ask for transfert ~n",[State#state.hostname],?DEB),
            Mount = getval(mount,Config),
            kachain_srv:start_transfert(NewState#state.chainsrv, Mount, NewState#state.node),
            {next_state, transfert, NewState#state{retry=0}, State#state.transfert_timeout}
    catch throw: {rpc, ErrorType, Error} ->
            retry(State, ErrorType, Error)
    end;
setup_env(Event, State=#state{})->
    ?LOGF("got event ~p~n",[Event],?WARN),
    {next_state, setup_env, State}.


%% @spec transfert(Event::term, State::record(state)) ->
%%  {next_state,transfert|last_boot, State::record(state), Timeout::integer} |
%%  {stop, Reason}
%% @doc wait for transfert and then execute postinstall and go to last_boot

transfert({transfert_failed}, State=#state{config=Config})->
    Retry = State#state.retry,
    case Retry < State#state.max_retry of
        true ->
            ?LOGF("~p: Retry transfert~n",[State#state.hostname],?INFO),
            Mount = getval(mount,Config),
            kachain_srv:retry_transfert(State#state.chainsrv, Mount, State#state.node),
            {next_state, transfert, State#state{retry=Retry+1}, State#state.transfert_timeout};
        false->
            ?LOG("Max retries reached in state transfert, abort~n",?ERR),
            failure(?fail_transfert)
    end;

transfert({transfert_done}, State=#state{config=Config,transfert_start=TD,options=Opts})->
    TransfertTime=round(katools:elapsed(TD,now())/1000),
    ?LOGF("~p: transfert done in ~p sec, start postinstall~n",[State#state.hostname, TransfertTime],?INFO),
    Now = now(),
    Env = State#state.env,
    DestDir   = getval(post_install_destdir,Config), % destdir ?
    Script    = getval(post_install_script,Config),
    Retries   = getval(post_install_script_retries,Config),
    Mount     = getval(mount,Config),
    Partition = Opts#deploy_opts.partition,
    %% start postinstall
    ok = postinstall(State,Env#environment.filesite,DestDir,Script,Retries),
    disk_umount(State,Mount,-1), %% FIXME: stop if failure ?
    Elapsed=round(katools:elapsed(Now,now())/1000),
    ?LOGF("~p: postinstall done and filesystem unmounted (~p sec)~n",[State#state.hostname, Elapsed],?INFO),
    Grub = case getval(use_nogrub,Config) of
               1 -> nogrub;
               0 -> grub
           end,
    case Opts#deploy_opts.last_boot of
        bios ->
            case kaenv:setup_pxe(Grub,State#state.hostname,Config, {env,Env},Partition) of
                ok ->
                    kareboot_srv:reboot(State#state.hostname,get_reboot_cmds(Config)),
                    {next_state,last_boot,State#state{retry=0},State#state.last_boot_timeout};
                {error, Reason} ->
                    failure(?fail_setup_pxe)
            end;
        kexec ->
            case kaenv:setup_pxe(Grub,State#state.hostname,Config,{env,Env},Partition) of
                ok ->
                    kareboot_srv:kexec(State#state.hostname,[]),
                    {next_state,last_boot,State#state{retry=0},State#state.last_boot_timeout};
                {error, Reason} ->
                    failure(?fail_setup_pxe)
            end;
        virt ->
            kareboot_srv:reboot_virt(State#state.hostname),
             %% use another state ?
            {next_state, last_boot, State#state{retry=0}, State#state.last_boot_timeout}
    end;

transfert(timeout, _State) ->
    ?LOG("transfert timeout~n",?INFO),
    failure(?fail_transfert_timeout).


%% @spec last_boot(Event::term, State::record(state)) ->
%%  {next_state, last_boot, State::record(state), Timeout::integer} |
%%  {stop, Reason}
%% @doc handle last_boot: first wait for the response from the
%% kareboot server, then check the node periodically.
%%
last_boot({reboot_failed}, State=#state{})->
    reboot_retry(State, last_boot, State#state.last_boot_timeout,?fail_last_boot);

last_boot({reboot_done}, State=#state{})->
    %% the reboot command was succesfully launched. Now we must wait
    %% for the node to finish it's reboot sequence.
    %% start the timer
    ?LOGF("Reboot command done, wait for node ~p to reboot~n",[State#state.hostname],?INFO),
    erlang:start_timer(State#state.last_boot_timeout,self(),last_boot_timeout),
    {next_state, last_boot, State, State#state.first_check_interval};

last_boot(timeout, State=#state{hostname=Host})->
    ?LOGF("~p: last boot timeout, check host on port ~p~n",[Host,State#state.check_portno],?DEB),
    case katools:check_host(State#state.check_portno,
                            Host,
                            State#state.check_timeout) of
        ok ->
            ?LOGF("~p: success !!! ~n",[Host],?INFO),
            success(State#state.master,Host),
            {stop,normal,State};
        {error, _Reason} ->
            {next_state, last_boot, State, State#state.check_interval}
    end;

last_boot(Event, State=#state{})->
    ?LOGF("~p: got event in state last_boot~n",[State#state.hostname,Event],?NOTICE),
    {next_state, last_boot, State}.


%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName,
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName,
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
setup_env(Event, _From, State) ->
    ?LOGF("got event ~p in state setup_env~n",[Event],?WARN),
    Reply = ok,
    {reply, Reply, setup_env, State}.

transfert(Event, _From, State) ->
    ?LOGF("got event ~p in state transfert~n",[Event],?WARN),
    Reply = ok,
    {reply, Reply, transfert, State}.

last_boot(Event, _From, State) ->
    ?LOGF("got event ~p in state last_boot~n",[Event],?WARN),
    Reply = ok,
    {reply, Reply, last_boot, State}.

first_boot(Event, _From, State) ->
    ?LOGF("got event ~p in state first_boot~n",[Event],?WARN),
    Reply = ok,
    {reply, Reply, last_boot, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%                                            NextState} |
%%                                          {next_state, NextStateName,
%%                                            NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.


%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({timeout, _Ref, last_boot_timeout}, last_boot, State) ->
    reboot_retry(State,last_boot, State#state.last_boot_timeout,?fail_last_boot_timeout);

handle_info({timeout, _Ref, first_boot_timeout}, first_boot, State) ->
    reboot_retry(State,first_boot, State#state.first_boot_timeout,?fail_first_boot_timeout);

handle_info(Info, StateName, State) ->
    ?LOGF("got message ~p in state ~p~n",[Info,StateName],?WARN),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(normal, _StateName, _State) ->
    ?LOG("normal termination ~n",?INFO),
    ok;
terminate(Reason, StateName, _State) ->
    ?LOGF("terminate in state ~p for reason ~p~n",[StateName,Reason],?WARN),
    failure({?fail_unknown, Reason}),
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @spec setup_disk(Config::List,State::record(state),Opts::record(deploy_opts))
%%                  -> State::record(state)
%% @doc unzip preinstall, fdisk, mkfs and mount
setup_disk(Config,State=#state{hostname=Host},Opts)->
    %% @FIXME: handle fdisktype in environment
    Env = State#state.env,
    FDiskDataFile = getval(fdiskfile,Config),
    Device        = getval(device,Config),
    Mount         = getval(mount,Config),
    TmpDir        = getval(preinstall_tmpdir,Config),
    Partition     = Opts#deploy_opts.partition, %% FIXME: read default in Config ?
    MaxRetries    = State#state.system_maxretries,
    PreInstallTgz = getval(preinstall_archive,Config),
    PartDevice    = set_partition(Device,Partition),
    FileSystem    = Env#environment.filesystem,
    FSOptions     = getval(list_to_atom(FileSystem++"_options"),Config),

    %% first, unzip preinstall files
    ?LOGF("~p: preinstall ...~n",[Host],?DEB),
    preinstall(State,PreInstallTgz,TmpDir,MaxRetries),
    disk_fdisk(State,set_device(Device),TmpDir,FDiskDataFile,-1),
    disk_umount(State,Mount,-1),
    %% @FIXME need to add force option or echo "y" ?
    disk_mkfs(State,PartDevice,FileSystem,FSOptions,MaxRetries),
    case Opts#deploy_opts.clean_tmp of
        true ->
            ?LOGF("~p: format /tmp~n",[Host],?NOTICE),
            TmpPart       = getval(tmp_partition,Config),
            TmpFS         = getval(tmp_fs,Config),
            TmpFSOptions  = getval(list_to_atom(TmpFS++"_options"),Config),
            PartDeviceTmp = set_partition(Device,TmpPart),
            disk_umount(State,PartDeviceTmp,-1),
            disk_mkfs(State,PartDeviceTmp,TmpFS,TmpFSOptions,MaxRetries);
        false ->
            ?LOGF("~p: don't format /tmp~n",[Host],?DEB)
    end,
    disk_mount(State,Mount,PartDevice,Env#environment.filesystem,MaxRetries),
    State#state{config=Config, transfert_start=now()}.

check_erlang_node(RemoteNode,State)->
    ?LOGF("Try to reach ~p~n",[RemoteNode],?NOTICE),
    case net_adm:ping(RemoteNode) of
        pong -> %% remote node is alive
            ?LOGF("Node ~p is alive ~n",[RemoteNode],?DEB),
            ok;
        pang -> %% remote node is not alive, we must reboot it
            ?LOGF("Can't reach ~p~n",[RemoteNode],?WARN),
            failure(?fail_slave_ping)
    end.

success(Master,Hostname)->
    kadeploy_mgr:node_success(Master, {self(), Hostname}).

%% warn: use process dictionnary !!!
failure(Reason)->
    failure(Reason,get(master),get(hostname)).
failure(Reason,Master,Hostname)->
    kadeploy_mgr:node_failure(Master, {Reason, self(), Hostname}),
    exit(normal).

set_device(Device)-> "/dev/"++Device.

set_partition(Device, Partition) when is_integer(Partition)->
    set_partition(Device,integer_to_list(Partition));
set_partition(Device,Partition) when is_list(Partition)->
    set_device(Device) ++ Partition.

disk_mkfs(State, Partition, FStype, FSOpts,Retries)->
    rpc_retry(State#state.node,kaslave,mkfs,[Partition,FStype,FSOpts],
              Retries,?fail_mkfs).

disk_write(State, Device, FileData, FileName, Retries) when is_binary(FileData)->
    rpc_retry(State#state.node,file,write_file,[FileData,FileName],
              Retries,?fail_write).

disk_fdisk(State, Device, TmpPath, FdiskInputFile, Retries)->
    rpc_retry(State#state.node,kaslave,fdisk_file,[Device,TmpPath,FdiskInputFile],
              Retries,?fail_fdisk).

disk_mount(State, Mount, Partition, FSType, Retries) ->
    rpc_retry(State#state.node,kaslave,mount,[Partition,FSType,Mount],
              Retries,?fail_mount).

disk_umount(State, Mount, Retries) ->
    rpc_retry(State#state.node,kaslave,umount,[Mount], Retries,?fail_umount).


preinstall(State,File,DestDir,Retries) ->
    {ok, Bin}= file:read_file(File),
    rpc_retry(State#state.node,kaslave,unzip,[Bin,DestDir], Retries,?fail_preinstall).

postinstall(State,File,DestDir,Script,Retries) ->
    %% script must be called with the PATH (Destdir ? )
    RealScript=Script ++" " ++DestDir,
    prepostinstall(?fail_postinstall,State,File,DestDir,RealScript,Retries).

prepostinstall(ErrorType,State,File,DestDir,Script,Retries) ->
    ?LOGF("~p: prepost: read file ~p (destdir is ~p)~n",[State#state.hostname,File, DestDir],?DEB),
    {ok, Bin}= file:read_file(File),
    rpc_retry(State#state.node,kaslave,unzip_and_exec,[Bin,DestDir,Script],
              Retries,ErrorType).

rpc_retry(Node,Mod,Fun,Args,Retries,ErrorType) when is_list(Retries) ->
    rpc_retry(Node,Mod,Fun,Args,list_to_integer(Retries),ErrorType);
rpc_retry(Node,Mod,Fun,Args,Retries,ErrorType) when is_integer(Retries)->
    Call=rpc:call(Node,Mod,Fun,Args),
    case {Call, Retries} of
        {{ok, Res}, _} ->
            ?LOGF("rpc success, output is ~p~n",[Res],?DEB),
            ok;
        {Error,-1} -> %% negative value mean: continue even if an error occured
            ?LOGF("rpc ~p:~p(~p) failed on node ~p, error: ~p, but continue as requested",
                 [Mod,Fun,Args,Node,Error],?WARN),
            ok;
        {Error,0} ->
            ?LOG("rpc failed~n",?DEB),
            throw({rpc, ErrorType, Error});
        {Error,_} ->
            ?LOGF("Retry rpc  ~p:~p~n",[Mod,Fun],?INFO),
            rpc_retry(Node,Mod,Fun,Args,Retries-1,ErrorType)
    end.

getval(Key,Config)-> kaconfig:getval_or_fail(Key,Config).

%% FIXME: add power off /power on if exists ?
get_reboot_cmds(Conf) ->
    Soft = getval(softboot,Conf),
    Hard = getval(hardboot,Conf),
    {Soft, Hard, ""}.

%% called in setup_env state
retry(State, ErrorType, Error)->
    ?LOGF("catch exception~p, check if we retry~n",[{ErrorType, Error}],?INFO),
    Retry= State#state.retry,
    case Retry < State#state.max_retry of
        true ->
            ?LOG("Retry~n",?INFO),
            {next_state, setup_env, State#state{retry=Retry+1},?init_timeout};
        false->
            ?LOGF("~p: Max retries reached in state setup_env, abort~n",[State#state.hostname],?ERR),
            %% warn the chain server that it should not wait for me:
            kachain_srv:update_nodes(State#state.chainsrv, -1),
            failure({ErrorType, Error})
    end.

%% @spec reboot_retry(State::record(state),StateName:atom, TimeOut::integer, ErrorMessage::string) -> {next_state, StateName::atom, NewState::record(state), StateTimeOut} | exit
reboot_retry(State=#state{options=Opts,config=Config,hostname=Host},StateName,StateTimeout,ErrorMessage)->
    Retry= State#state.retry,
    case Retry < State#state.max_retry of
        true ->
            ?LOGF("~p: Reboot in state ~p has failed, try again (try=~p)",
                  [Host, StateName, Retry+1],?WARN),
            case Opts#deploy_opts.last_boot of
                bios ->
                    kareboot_srv:reboot(Host,get_reboot_cmds(Config)),
                    {next_state,StateName,State#state{retry=Retry+1},StateTimeout};
                kexec -> % FIXME: if kexec has failed switch to classic reboot ?
                    kareboot_srv:kexec(Host,[]);
                virt ->
                    kareboot_srv:reboot_virt(Host),
                    {next_state, StateName, State#state{retry=Retry+1}, StateTimeout}
            end;
        false ->
            case StateName of
                first_boot ->
                    %% warn the chain server that it should not wait for me:
                    kachain_srv:update_nodes(State#state.chainsrv, -1);
                _ ->
                    ok
            end,
            failure(ErrorMessage)
    end.
