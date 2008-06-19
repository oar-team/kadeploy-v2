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

-vc('$Id: kaconf.hrl,v 0.0 2008/06/09 08:59:04 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

%% List of all possible failures of a node's deployment:

-define(fail_unknown, unknown).
-define(fail_badconf, node_badconfig).
-define(fail_kaslave, kaslave_failure).
-define(fail_slave_ping, slave_ping_failure).
-define(fail_setup_pxe, setup_pxe_failure).
-define(fail_last_boot, last_boot_failure).
-define(fail_first_boot, first_boot_failure).
-define(fail_timeout, timeout). %% which case ?
-define(fail_transfert, transfert_failure).
-define(fail_transfert_timeout, transfert_timeout).
-define(fail_last_boot_timeout, last_boot_timeout).
-define(fail_first_boot_timeout, first_boot_timeout).
-define(fail_fdisk, fdisk_failure).
-define(fail_mount, mount_failure).
-define(fail_umount, umount_failure).
-define(fail_write, write_failure).
-define(fail_mkfs, mkfs_failure).
-define(fail_preinstall, preinstall_failure).
-define(fail_postinstall, postinstall_failure).

-record(deploy_opts,
        {
          method     = currentenv, %%  deployenv|nfsroot|currentenv|virt
          erlang_args     , %%  currentenv specs (FIXME)
          last_boot  = bios, %%  bios|kexec|virt
          partition  = 3,    %%  deployment partition
          timeout,  %% global timeout for deployement (in msec)
          minnodes  %% minimum nodes to deploy: stop deployment as soon as
                    %% minodes has been succesfully deployed.
         }).

-record(node,
        {
          name,        %% hostname
          device,      %% hard disk device
          ipv4,        %% IPv4 address
          ipv6,        %% IPv6 address
          mac,         %% mac address
          mgt_ip,      %% for future use: if available
          mgt_type,    %% for future use: ipmi|ipmi2|drac|rsa|...
          reboot_soft, %% command for soft rebooting
          reboot_hard, %% command for hard rebooting
          reboot_power, %% command for electrical power down/up node; optionnal
          console_cmd, %% unused
          deployboot   %% script and args to setup pxe for deployenv
         }).

-record(deployment,
        {
          clientpid, %% process pid of client. we should send the Nodes OK
                     %% and badnodes list to this him
          username,
          nodes,
          envname,
          partition, %% partition number
          options,
          startdate,
          serverpid
         }).


-define(restart_sleep, 2000).

-define(config(Var), katools:get_val(Var)).

-define(LOGF(Msg, Args, Level),
        katools:debug(?MODULE, Msg, Args, Level)).
-define(LOG(Msg, Level),
        katools:debug(?MODULE, Msg, Level)).

%% Debug messages can be completely disabled if DEBUG is not defined
-ifdef(DEBUG).
    -define(TRACE, [{debug, [trace]}]).
    -define(DebugF(Msg, Args),
            katools:debug(?MODULE, Msg, Args, ?DEB)).
    -define(Debug(Msg),
            katools:debug(?MODULE, Msg, ?DEB)).
-else.
    -define(TRACE, []).
    -define(DebugF(Msg, Args), ok).
    -define(Debug(Msg), ok).
-endif.

-define(EMERG, 0). % The system is unusable.
-define(ALERT, 1). % Action should be taken immediately to address the problem.
-define(CRIT, 2).  % A critical condition has occurred.
-define(ERR, 3).   % An error has occurred.
-define(WARN, 4).  % A significant event that may require attention has occurred.
-define(NOTICE, 5).% An event that does not affect system operation has occurred.
-define(INFO, 6).  % An normal operation has occurred.
-define(DEB, 7).   % Debugging info
