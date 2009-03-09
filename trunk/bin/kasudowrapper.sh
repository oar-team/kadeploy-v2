#!/bin/bash

#===================
# Globals variables
#===================
PATH=/bin:/sbin:/usr/bin:/usr/sbin

TSTAMP=$( date +%Y%m%d-%H%M-%N )
ROOT_HOMEDIR=$(getent passwd|grep $(echo $UID)|cut -d: -f6)
KADEPLOY_USER_DIR=".kadeploy"
NODES_LIST="__nodes_list"
NL_FNAME="${ROOT_HOMEDIR}/${KADEPLOY_USER_DIR}/${NODES_LIST}"
CLUSTER_NAME=""
CLUSTER_FILES=""

# Makefile Substituted variables
DEPLOYCONFDIR=__SUBST__
DEPLOYDIR=__SUBST__
DEPLOYUSER=__SUBST__
PERL5LIBDEPLOY=__SUBST__

export DEPLOYDIR
export PERL5LIB=${PERL5LIBDEPLOY}/:$PERL5LIB
PREPEND=""


#===========
# Functions
#===========

function die()
{
  echo -e "Fatal : $1\nexiting.\n" && exit 1
}

function warn()
{
  echo -e "Warning : $1.\n"
}

#_______________________________
# split nodes for multi-cluster
function kadeploy_split_nodes()
{
    local nodeslist=$1
    local current_cluster=""
    
    for node in $(cat $nodeslist); do 
      cluster=$(echo $node | cut -f1 -d"." | sed s/[-]*[0-9]*//g)
      #
      # Begin Patch [Acedeyn 01/12/2008
      # If want to know for wich cluster belong the node, we have to check
      # /etc/[<DEPLOY_CONF_DIR>]/deploy_cluster.conf
      # line=(`grep ^$node ${DEPLOYCONFDIR}/deploy_cluster.conf`)
      # cluster=${line[1]}
      #### End Patch
      # ?? next line
      [ "$cluster" != "$current_cluster" ] && current_cluster=$cluster
      echo $node >> ${NL_FNAME}.${cluster}.${TSTAMP}
    done
}

#_______________________________________________
# Check whether kadeploy User's directory exists
function check_kadeploy_user_dir()
{
  local kudir="$ROOT_HOMEDIR/$KADEPLOY_USER_DIR"
  
  if [ ! -d "$kudir" ]; then
    ( mkdir ${kudir} || die "failed to create ${kudir}" )
    ( chmod 755 ${kudir} || die "failed to chmod ${kudir}" )
  fi

  if [ ! -r "$kudir" ]; then
    ( chmod 755 ${kudir} || die "failed to chmod ${kudir}" )
  fi
}

#____________________________________
# Some checks before Kadeploy's call
function check_kadeploy_call()
{
  if [ $(basename $0) == "kadeploy" ]; then
    if $(tty -s) 
    then
      PREPEND=""
    else
      PREPEND="screen -D -m "
      echo -e "Kadeploy not launched from a tty\nDetached in a screen\n "
    fi
  fi
}

#_________________________________________________________________
# Extract cluster name from nodes list sorted by cluster filename
function get_cluster_name()
{
  local file=$1
  CLUSTER_NAME=$(basename $file|sed -e 's/^.*\.\([a-zA-Z0-9\-_]*\)\..*$/\1/g')
}

#________________________________________________________________
# Retrieve all filenames containing nodes list sorted by cluster
function get_list_of_cluster_files()
{
  CLUSTER_FILES=$(ls ${NL_FNAME}.*.${TSTAMP})
}

#_________________________________________________
# Remove temporary files generated for deployment
function clean_tmp_files()
{
  ( rm -f ${NL_FNAME}.*.${TSTAMP} || \
  warn "failed to remove temporary files in ${ROOT_HOMEDIR}/${KADEPLOY_USER_DIR}" )
  ( sudo -u $DEPLOYUSER $DEPLOYDIR/bin/kadeploy --rmnodefilesintmp $USER  || \
  warn "failed to remove temporary files in /tmp" )
}


#=============
# Entry point
#=============

check_kadeploy_call

check_kadeploy_user_dir

if [ -x $DEPLOYDIR/sbin/`basename $0` ]
then
    ( ( sudo -u $DEPLOYUSER $DEPLOYDIR/sbin/`basename $0` "$@" ) || \
      ( die "bad configuration ; please refer to \"kasetup -exportenv\"" ) )
elif [ -x $DEPLOYDIR/bin/`basename $0` ]; then 
    if [ "`basename $0`" != "kadeploy" ]; then
      ( ( ${PREPEND} sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$@" ) || \
	( die "bad configuration ; please refer to \"kasetup -exportenv\"" ) )
    else
      remaining_args=""
      node_list=""
      filename=""
      script=""
      while [ -n "$1" ]; do
        case "$1" in
	  -m|--machine)
	    node_list="$node_list $2"
	    shift
	    shift;;
	  -f|--filename)
	    filename="$2"
	    shift
	    shift;;
	  -k|--keys)
	    keys=1
	    shift;;
	  -s|--script)
	    script="$2"
	    shift
	    shift;;
	  *)
	    remaining_args="$remaining_args $1"
	    shift;;
	esac
      done

	# split nodes into many lists as clusters implied in deployment
	# Check cases whether nodes are provided by a file or a parameter's list
	NLIST="${NL_FNAME}.${TSTAMP}"
	if [ -n "$filename" ]; then
	  cat ${filename}|sort -n|uniq > ${NLIST}
	  # kadeploy_split_nodes $filename
	elif [ -n "$node_list" ]; then
	  echo ${node_list}|tr ' ' '\n'|sort -n|uniq > ${NLIST}
	fi
	if [ -s "$NLIST" ]; then
	  kadeploy_split_nodes ${NLIST} && rm -f ${NLIST}
	else
	  die "Kadeploy called on empty node list"
	  # sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$*"
	  #  exit 0
	fi

	# Call Kadeploy for each cluster found
	get_list_of_cluster_files
	for file in $CLUSTER_FILES; do
	  if [ -f "$file" -a -s "$file" ]; then
	    get_cluster_name $file
	    cluster=$CLUSTER_NAME
	    echo -e "\n==> Launching Kadeploy on $cluster nodes\n"
	    cmd_args="$remaining_args -f ${NL_FNAME}.${cluster}.${TSTAMP} -z $cluster"
	    set -- $cmd_args 
	    echo "${PREPEND} sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` \"$@\" &"
	  fi
	done

	#waiting all instances of kadeploy
	wait

	#copy nodes files in the current directory
	cp /tmp/kadeploy-$USER*.out . 2>/dev/null

	#copy ssh keys in root's authorized_keys
	if [ $keys ]
	then
	    get_list_of_cluster_files
	    for file in $CLUSTER_FILES; do 
		if [ -f "$file" -a -s "$file" ]; then
		  get_cluster_name $file
		  cluster=$CLUSTER_NAME
		  if [ -f ~/.ssh/id_rsa.pub ]; then
		    echo "kaaddkeys -f \"kadeploy-${USER}-${cluster}-nodes_ok.out\" -C ${DEPLOYCONFDIR} -k ~/.ssh/id_rsa.pub"
		  elif [ -f ~/.ssh/id_dsa.pub ]; then
		    echo "kaaddkeys -f \"kadeploy-${USER}-${cluster}-nodes_ok.out\" -C ${DEPLOYCONFDIR} -k ~/.ssh/id_dsa.pub"
		  fi
		fi
	    done
	fi
	
	# Purge temporary generated files
	clean_tmp_files
	
	# Launch a script after the deployment
	( [ -e "$script" -a -x "$script" ] && $script ) || \
	warn "$script not found or not executable"
    fi
fi
