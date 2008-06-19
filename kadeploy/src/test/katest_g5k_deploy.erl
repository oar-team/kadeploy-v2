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

-module(katest_g5k_deploy).

-compile(export_all).

-include("kaconf.hrl").
-include("kaenv.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DEBUG_LEVEL,6).

test()->
    ok.

%% chain_test_()->
%%     Nodes=get_batch_nodes("OAR_NODEFILE"),
%%     NumberOfNodes=length(Nodes),
%%     {ok, ChainPid} = kadeploy_sup:start_chain_srv(tar, "./src/test/bigtest.tgz", NumberOfNodes),
%%     Opts=#deploy_opts{timeout=600000,
%%                       erlang_args="+A 16 -connect_all false +K true -rsh oarsh -setcookie testcookie -pa /home/sophia/nniclausse/sources/erlang/kadeploy-0.0.1/ebin"},
%%     ?LOG("ok:~n",?DEB),
%%     {Good,Bad}=kaslave:pstart(Nodes,15000,Opts#deploy_opts.erlang_args),
%%     Rpc=rpc:multicall(Good, kachain_srv,start_transfert,[ChaindPid,"/tmp",],5000),
%%     lists:foreach(Good),
%%     Fun= fun(Node) ->
%%                  kachain_srv:start_transfert(ChainPid, "/tmp", Node )
%%          end,
%%     lists:foreach
%%     erlang:display([Rep,Res]),
%%     {timeout, 240000,?_assertMatch(Rep,Res)}.

kadeg5k_test_()->
    myset_env(),
    Good=[],
    Nodes=get_batch_nodes("OAR_NODEFILE"),
    application:start(kadeploy),
    application:set_env(kadeploy,debug_level,?DEBUG_LEVEL),
    Env= #environment{id = 843,
                      version = 1,
                      name = "etch-x64-nfs-1.0",
                      description = "https://www.grid5000.fr/index.php/Etch-x64-nfs-1.0",
                      author = "cyril.constantin@loria.fr",
                      filebase = "/grid5000/images/etch-x64-nfs-1.0.tgz",
                      filesite = "/grid5000/postinstalls/etch-x64-nfs-1.0-post.tgz",
                      size = "1000",
                      initrdpath = "/boot/initrd.img-2.6.18-6-amd64",
                      kernelpath = "/boot/vmlinuz-2.6.18-6-amd64",
                      kernelparam = "",
                      fdisktype = 83,
                      filesystem = "ext2",
                      siteid = 1,
                      optsupport = 0,
                      user = "deploy"},
    Opts=#deploy_opts{timeout=600000,
                      partition=3,
                      erlang_args="+A 16 -connect_all false +K true -rsh ssh -setcookie testcookie"},

%%     ?LOGF("Deployenv Result:~p~n",[Resp],?DEB),
    {timeout, 240, ?_assertMatch({Good,
                                  [
                                   {"hyperion", {error,timeout}},
                                   {"badtwo", {error,timeout}}
                                  ]},
                                 kadeploy_mgr:deploy(root,Nodes,Env,Opts))}.

myset_env()->
    application:set_env(stdlib,debug_level,?DEBUG_LEVEL),
    application:set_env(stdlib,deploy_timeout,600000),
    ssl:start(),
    application:start(sasl),
    application:start(crypto).



get_batch_nodes(Env) ->
    case os:getenv(Env) of
        false ->
            [];
        NodeFile ->
            {ok, Nodes} = file_to_list(NodeFile),
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

read_lines(_FD, eof, L) -> lists:reverse(L);
read_lines(FD, Line, L) -> read_lines(FD, io:get_line(FD,""),[katools:chop(Line)|L]).

chop(String) -> string:strip(String, right, 10).
