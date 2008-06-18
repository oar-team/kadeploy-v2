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

-module(kaenv).
-vc('$Id: kaenv.erl,v 0.0 2008/06/09 10:58:01 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-include("kaconf.hrl").
-include("kaenv.hrl").

%% @ doc everything related to environments

%% API
-export([getenv/1, setup_pxe/5, setup_pxe_data/5, extract_kernel/1]).

-define(PXE_HEADER,"PROMPT 1
DEFAULT bootlabel
DISPLAY messages
TIMEOUT 50

 label bootlabel
         KERNEL ").

%% @spec getenv(EnvName::String) -> {ok, record(environment)} | { error, Reason}
getenv(_EnvName)->
    {ok, #environment{} }.

setup_pxe(UseGrub, Hostname, _Config,{deploykernel, Duke},Partition)->
    %% @FIXME Temporary: Use external script to do this
    case kaslave:myoscmd(Duke) of
        {ok, Res} ->
            ?LOGF("PXE ok~p~n",[Res],?DEB);
        {error,ErrNo,Reason}->
            {error,Reason}
    end;

setup_pxe(UseGrub,Hostname, Config, Env, Partition)->
    PXEData=setup_pxe_data(UseGrub,Hostname, Config, Env,Partition),
    PXERep=getval(pxe_rep,Config),
    TFTPDir=getval(tftp_repository,Config),
    {ok,IPtmp}=inet:getaddr(Hostname, inet), %% IPV4
    IPhex=katools:ip_hex(IPtmp),
    Filename=filename:join([TFTPDir,PXERep,IPhex]),
    ?LOGF("writing PXE file (~p) for host ~p",[Filename,Hostname],?NOTICE),
    Data=list_to_binary(PXEData),
    file:write_file(Filename,Data).

setup_pxe_data(UseGrub,Hostname, Config, {env,Env}, Partition)->
    Id=integer_to_list(Env#environment.id),
    TFTPRelPath=getval(tftp_relative_path,Config),
    Device    = getval(device,Config),
    case UseGrub of
        grub ->
            Cluster=getval(cluster,Config),
            lists:append([?PXE_HEADER,TFTPRelPath,"/",
                          "memdisk\n",
                          "         APPEND initrd=",TFTPRelPath,"/grub_img_env",
                          Id,"_",Device,integer_to_list(Partition),"-",Cluster]);
        nogrub ->
            Kernel=Env#environment.kernelpath,
            InitRd=Env#environment.initrdpath,
            KernelParam=getval(kernel_param,Config),
            lists:append([?PXE_HEADER,TFTPRelPath,"/",
                          "folder_env_",Id,Kernel,"\n",
                          "         APPEND initrd=",TFTPRelPath,"/folder_env_",Id,
                          InitRd,
                          " root=/dev/",Device,integer_to_list(Partition)," ",
                          KernelParam])
    end.

extract_kernel(Env) ->
    ok.

getval(Key,Config)-> kaconfig:getval_or_fail(Key,Config).
