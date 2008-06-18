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

-module(katest_deploy).

-compile(export_all).

-include("kaconf.hrl").
-include("kaenv.hrl").
-include_lib("eunit/include/eunit.hrl").

test()->
    ok.

-define(DEBUG_LEVEL,6).

myoscmd_test()->
    myset_env(),
    ?assertMatch({error,2, Reason},kaslave:myoscmd("ls /sdfsdfsd")).

%% too dangerous if test is running as root :)
%% mkfs_test()->
%%         ?assertMatch({error,3, Reason},kaslave:mkfs("/dev/sda","ext3","")).

myoscmd_ok_test()->
    ?assertMatch({ok,Res},kaslave:myoscmd("uptime")).

ip_test()->
    {ok,IP}=inet:getaddr("localhost", inet), %% IPV4
    ?assertMatch("127.0.0.1",katools:ip_tostr(IP)).

unzip_test()->
    {ok, Bin} = file:read_file("./src/test/test.tgz"),
    ?assertMatch({ok,Res},kaslave:unzip_and_exec(Bin,"/tmp/post","erlang/script.sh")).

nmap_test()->
    {ok, Hostname}=inet:gethostname(),
    ?assertMatch(ok,katools:check_host(22,Hostname, 2000)).

nmap_nok_test()->
    {ok, Hostname}=inet:gethostname(),
    ?assertMatch({error,Reason},katools:check_host(22222,Hostname, 2000)).

badnodes_test()->
    Good=[],
    application:start(kadeploy),
    application:set_env(kadeploy,debug_level,?DEBUG_LEVEL),
    Resp=kadeploy_mgr:deploy(root,["badone","badtwo"],"nef2",#deploy_opts{timeout=2000}),
    ?assertMatch({Good,
                  [{"badone", {error,kaslave_failure}},
                   {"badtwo", {error,kaslave_failure}}]},
                 Resp).

config_test()->
    ?LOG("config_test~n",?DEB),
    Host="hyperion",
    Conf=kaconfig:getnodeconf(Host),
    Res=[{ok,"azur"}, {ok,"/tmp"},{ok,"/mnt/dest"}],
    ?assertMatch(Res,[kaconfig:getval(cluster,Conf),
                      kaconfig:getval(tmpdir,Conf),
                      kaconfig:getval(mount,Conf)
                     ]).

pxedata_nogrub_test()->
    Env= #environment{id=765,
                      initrdpath = "/boot/initrd.img-2.6.18-6-amd64",
                      kernelpath = "/boot/vmlinuz-2.6.18-6-amd64",
                      kernelparam = "",
                      filesite="./src/test/test.tgz"},
    Host="hyperion",
    Conf=kaconfig:getnodeconf(Host),
    put(master, self()), % needed for failure call
    put(hostname, Host), % needed for failure call
    Data="PROMPT 1\nDEFAULT bootlabel\nDISPLAY messages\nTIMEOUT 50\n\n label bootlabel\n         KERNEL images_grub/folder_env_765/boot/vmlinuz-2.6.18-6-amd64\n         APPEND initrd=images_grub/folder_env_765/boot/initrd.img-2.6.18-6-amd64 root=/dev/hda3 console=tty0 console=ttyS0,9600n8",
    Res=kaenv:setup_pxe_data(nogrub,Host,Conf,{env,Env},3),
    ?assertMatch(Data,Res).

ip_hex_test()->
    ?assertMatch("8A601528",katools:ip_hex({138,96,21,40})).

pxedata_grub_test()->
    Env= #environment{id=765,
                      initrdpath = "/boot/initrd.img-2.6.18-6-amd64",
                      kernelpath = "/boot/vmlinuz-2.6.18-6-amd64",
                      kernelparam = "",
                      filesite="./src/test/test.tgz"},
    Host="hyperion",
    Conf=kaconfig:getnodeconf(Host),
    put(master, self()), % needed for failure call
    put(hostname, Host), % needed for failure call
    Data="PROMPT 1\nDEFAULT bootlabel\nDISPLAY messages\nTIMEOUT 50\n\n label bootlabel\n         KERNEL images_grub/memdisk\n         APPEND initrd=images_grub/grub_img_env765_hda3-azur",
    Res=kaenv:setup_pxe_data(grub,Host,Conf,{env,Env},3),
    ?assertMatch(Data,Res).

pxe_grub_test()->
    Env= #environment{id=765,
                      initrdpath = "/boot/initrd.img-2.6.18-6-amd64",
                      kernelpath = "/boot/vmlinuz-2.6.18-6-amd64",
                      kernelparam = "",
                      filesite="./src/test/test.tgz"},
    Host="hyperion",
    Conf=kaconfig:getnodeconf(Host),
    put(master, self()), % needed for failure call
    put(hostname, Host), % needed for failure call
    Data=ok,
    Res=kaenv:setup_pxe(grub,Host,Conf,{env,Env},3),
    ?assertMatch(Data,Res).

chain_tar_test()->
    ?LOG("chain_tar_test~n",?DEB),
    {ok, ChainPid} = kadeploy_sup:start_chain_srv(tar, "./src/test/test.tgz", 1),
    ?LOG("ok:~n",?DEB),
    Res=kachain_srv:start_transfert(ChainPid, "/tmp", node()),
    Rep= receive
             {'$gen_event',{transfert_done}} ->
                 ok
         after 3000 ->
                 error
         end,
    ?assertMatch(Rep,Res).

chain_dd_test()->
    {ok, ChainPid} = kadeploy_sup:start_chain_srv(dd, "./src/test/test.dd", 1),
    ?LOG("ok:~n",?DEB),
    Res=kachain_srv:start_transfert(ChainPid, "/tmp/toto.dd", node()),
    Rep= receive
             {'$gen_event',{transfert_done}} ->
                 ok
         after 3000 ->
                 error
         end,
    ?assertMatch(Rep,Res).

deploytimeout_test()->
    myset_env(),
    Good=[],
    {ok, Hostname}=inet:gethostname(),
    Env= #environment{filebase="./src/test/test.tgz",
                      filesite="./src/test/test.tgz"},
    Opts=#deploy_opts{timeout=1,
                      method=deployenv,
                      erlang_args="+A 16 -connect_all false +K true -rsh ssh -setcookie testcookie"},
    Resp=kadeploy_mgr:deploy(root,[Hostname,"localhost"],Env,Opts),
    ?assertMatch({Good,
                  [
                   {Hostname, {error,timeout}},
                   {"localhost", {error,timeout}}
                   ]},
                 Resp).



deployenv_test()->
    myset_env(),
    Good=[],
    Env= #environment{filebase="./src/test/test.tgz",
                      filesite="./src/test/test.tgz"},
    Opts=#deploy_opts{timeout=4000,
                      method=deployenv,
                      erlang_args="+A 16 -connect_all false +K true -rsh ssh -setcookie testcookie"},
    {ok, Hostname}=inet:gethostname(),
    Resp=kadeploy_mgr:deploy(root,[Hostname,"badtwo"],Env,Opts),
    ?assertMatch({Good,
                  [
                   {Hostname, {error,setup_pxe_failure}},
                   {"badtwo", {error,{node_badconfig, Key}}}
                   ]},
                 Resp).

goodnode_test()->
    myset_env(),
    Good=[],
    Env= #environment{filebase="./src/test/test.tgz",
                      filesite="./src/test/test.tgz"},
    Opts=#deploy_opts{timeout=6000,
                      erlang_args="+A 16 -connect_all false +K true -rsh ssh -setcookie testcookie"},

    {ok, Hostname}=inet:gethostname(),
    Resp=kadeploy_mgr:deploy(root,[Hostname,"badtwo"],Env,Opts),
    ?assertMatch({Good,
                  [{"badtwo", {error,kaslave_failure}},
                   {Hostname, {error,{preinstall_failure,Reason}}}
                  ]},
                 Resp).



myset_env()->
    application:set_env(stdlib,debug_level,?DEBUG_LEVEL),
    application:set_env(stdlib,deploy_timeout,600000),
    ssl:start(),
    application:start(sasl),
    application:start(crypto).

