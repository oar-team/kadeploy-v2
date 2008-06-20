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

-vc('$Id: kaenv.hrl,v 0.0 2008/06/09 09:08:28 nniclaus Exp $ ').
-author('nniclaus@sophia.inria.fr').

-record(environment,
        {
          name,
          id,
          version,
          description,
          author,
          filebase,
          filesite,
          size,
          initrdpath,
          kernelpath,
          kernelparam,
          fdisktype  = "83",
          filesystem = "ext3",
          siteid,
          optsupport, %% ??
          md5, %% md5sum of filebase (needed for anonymous env)
          user
         }).
