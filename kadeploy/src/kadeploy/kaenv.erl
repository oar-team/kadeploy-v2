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
-export([getenv/1, setup_pxe/5, setup_pxe_data/5, extract_kernel/2]).

-define(postinstall_file,"postinstall.tgz").
-define(description_file,"description").
-define(initrdpath_file,"initrd").
-define(kernelpath_file,"kernel").
-define(kernelparam_file,"bootcmdline").
-define(PXE_HEADER,"PROMPT 1
DEFAULT bootlabel
DISPLAY messages
TIMEOUT 50

 label bootlabel
         KERNEL ").

%% @spec getenv(EnvName::String) -> {ok, record(environment)} | { error, Reason}
%% @doc returns an environment record from a name or a directory
%%  https://www.grid5000.fr/mediawiki/index.php/Kadeploy:Environment
getenv({recorded, User, EnvName}) when is_list(EnvName)->
    %% TODO
    {ok, #environment{user=User} };
getenv({anonymous, User, Directory}) when is_list(Directory)->
    ?LOGF("Scanning anonymous env in directory ~p~n",[Directory],?INFO),
    case file:list_dir(Directory) of
        {ok, Filenames} ->
            %% there should be one base.tgz or base.dd file
            SearchBase=fun("base.dd") -> true ;
                          ("base.tgz")-> true ;
                          (_) -> false end ,
            [Base]=lists:filter(SearchBase,Filenames),
            FileBase=filename:absname(filename:join(Directory,Base)),
            ?LOG("Read filebase info~n",?INFO),
            {ok,_} = file:read_file_info(FileBase), % check if exists and is readable
            PostInstall=filename:absname(filename:join(Directory,?postinstall_file)),
            {ok,_} = file:read_file_info(PostInstall), % check if exists and is readable
            ?LOG("Read kernel, initrd and desc~n",?INFO),
            {ok,Description}= file:read_file(filename:join(Directory,?description_file)),
            {ok,KernelPath}= file:read_file(filename:join(Directory,?kernelpath_file)),
            {ok,InitrdPath}= file:read_file(filename:join(Directory,?initrdpath_file)),
            KernelParam = case file:read_file(filename:join(Directory,?kernelparam_file)) of
                              {ok, Bin} -> binary_to_list(Bin);
                              _ -> ""
                          end,
            Md5 = case [X ||  X <- Filenames, X=="md5sum"] of
                      [] -> % no md5file
                          ?LOG("Missing md5sum file, compute md5~n",?INFO),
                          lists:sublist(os:cmd("md5sum  "++FileBase),32);
                      [File] ->
                          {ok, Md5Bin} = file:read_file(filename:join(Directory,File)),
                          %% FIXME: what is the format of md5sum ?
                          %% just the md5 of base.tgz or md5 of all
                          %% files ?
                          lists:sublist(binary_to_list(Md5Bin),32)
                  end,
            ?LOGF("MD5 of ~p is ~p~n",[FileBase,Md5],?NOTICE),
            {ok,#environment{
               filebase=FileBase,
               filesite=PostInstall,
               kernelpath=katools:chop(KernelPath),
               initrdpath=katools:chop(InitrdPath),
               kernelparam=KernelParam,
               user=User,
               md5=Md5,
               id=anonymous,
               description=katools:chop(Description)
              }};
        {error,Reason} ->
            {error,Reason}
    end.

%% @spec setup_pxe(UseGrub::grub|nogrub,Hostname::string,
%%                  Config, Env, Partition::integer) -> ok | {error,Reason}
%% @doc returns an environment record from a name or a directory
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
    ?LOGF("writing PXE file (~p) for host ~p~n",[Filename,Hostname],?NOTICE),
    Data=list_to_binary(PXEData),
    file:write_file(Filename,Data).

%% @spec setup_pxe_data(UseGrub::grub|nogrub,Hostname::string,
%%                     Config, Env, Partition::integer) -> Data::string
%% @doc returns the pxe configuration
setup_pxe_data(UseGrub,Hostname, Config, {env,Env}, Partition)->
    Id=getid(Env),
    TFTPRelPath=getval(tftp_relative_path,Config),
    Device    = getval(device,Config),
    case UseGrub of
        grub ->
            Cluster=getval(cluster,Config),
            %%FIXME: we must create grub image !
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

%% @spec extract_kernel(Env::record(environment),Directory) -> ok | {error, Reason}
%% @doc extract kernel and initrd files to output directory
%% FIXME : dd env ?
extract_kernel(Env, BaseDirectory) ->
    Base=Env#environment.filebase,
    Directory=filename:join([BaseDirectory,"folder_env_"++getid(Env)]),
    Kernel=remove_slash(Env#environment.kernelpath),
    InitRd=remove_slash(Env#environment.initrdpath),
    ok = katools:create_dir_ifnec(Directory),
    %% FIXME: check that extracted files are readable for tftp
    %% FIXME: check timestamp
    case {exists(filename:join(Directory,Kernel)),
                 exists(filename:join(Directory,InitRd))} of
        {true, true} ->
            ?LOG("kernel and initrd already extracted~n",?DEB),
            {ok, exists};
        _ -> % at least one file does not exists, extract everythin
            ?LOGF("kernel and initrd needs to be extracted from archive ~p~n",[Base],?NOTICE),
            kaslave:myoscmd("tar zxf "++Base++" -C "++Directory++" "++Kernel++" "++InitRd)
    end.


remove_slash("/"++Tail)->Tail;
remove_slash(String) -> String.

exists(Filename) ->
    ?LOGF("check if file ~p exists~n",[Filename],?DEB),
    case file:read_file_info(Filename) of
        {ok, _} -> true;
        _       -> false
    end.

getval(Key,Config)-> kaconfig:getval_or_fail(Key,Config).

getid(#environment{ id=Id }) when is_integer(Id) ->
    integer_to_list(Id) ;
%% use md5 as id for pxe env pathname
getid(#environment{id=anonymous,md5=MD5}) ->
    MD5.
