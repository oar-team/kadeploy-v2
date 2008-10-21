#!/bin/bash

# split nodes for multi-cluster
function kadeploy_split_nodes()
{
    f=$1

    current_cluster=""
    rm -f kadeploy_tmp_cluster_*

    cat $f | sort | uniq | while read node
    do
	cluster=`echo $node | cut -f1 -d"." | sed s/[-]*[0-9]*//g`
	[ "$cluster" != "$current_cluster" ] && current_cluster=$cluster
	echo $node >> kadeploy_tmp_cluster_$cluster
    done
}

#DEPLOYDIR=/usr/local/kadeploy/
#DEPLOYUSER=deploy
#PERL5LIBDEPLOY=/usr/share/perl/5.8
DEPLOYDIR=__SUBST__
DEPLOYUSER=__SUBST__
PERL5LIBDEPLOY=__SUBST__


OK=0
export DEPLOYDIR
export PERL5LIB=${PERL5LIBDEPLOY}/:$PERL5LIB

append=""

if [ $(basename $0) = "kadeploy" ]
	then if $(tty -s)
		then
			append=${append}
		else
			append="screen -D -m "
			echo -e "Kadeploy not launched from a tty\nDetached in a screen\n "
	fi
fi



#if [ -x $DEPLOYDIR/bin/`basename $0` ] ; then exec ${append} sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$@" ; $OK=1 ;  fi
#if [ -x $DEPLOYDIR/sbin/`basename $0` ] ; then exec sudo -u $DEPLOYUSER $DEPLOYDIR/sbin/`basename $0` "$@" ; $OK=1 ;  fi
#if [ ! $OK ] ; then echo "kasudowrapper.sh badly configured, use (prefix)/sbin/kasetup -exportenv" ; fi


if [ -x $DEPLOYDIR/bin/`basename $0` ]
then 
    #Specific operations for kadeploy
    if [ "`basename $0`" == "kadeploy" ]
    then
	#parse the command line to find the filename or the node_list
	state=""
	for arg in $@
	do
	    [ "$state" = "node_list" ] && node_list=$node_list" "$arg
	    [ "$state" = "filename" ] && filename=$arg
	    [ "$state" = "script" ] && script=$arg
	    case $arg in
		"-m") state="node_list";;
		"--machine") state="node_list";;
		"-f") state="filename";;
		"--file") state="filename";;
		"-k") keys=1 ; state="";;
		"--keys") keys=1 ; state="";;
		"-s") state="script";;
		"--script") state="script";;
		*) state="";;
	    esac
	done

	#split the nodes if a file is used of if a list is used
	if [ ! -z "$filename" ]
	then
	    kadeploy_split_nodes $filename
	elif [ ! -z "$node_list" ]
	then
	    tmp_file="kadeploy_tmp_node_list"
	    rm -f $tmp_file
	    for node in $node_list
	    do
		echo $node >> $tmp_file
	    done
	    kadeploy_split_nodes $tmp_file
	    rm -f $tmp_file
	else
	    #let kadeploy manage the error
	    sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$*"
	    exit 0
	fi

	#remove -k, -f, -s and -m options
	args=`echo "$*" | sed -e 's/\-k//g' -e 's/\--keys//g' \
	                    -e 's/\-f\ [a-zA-Z0-9.\_-]*//g' -e 's/\--filename\ [a-zA-Z0-9.\_-]*//g' \
	                    -e 's/\-s\ [a-zA-Z0-9.\_-]*//g' -e 's/\--script\ [a-zA-Z0-9.\_-]*//g' \
	                    -e 's/\-m\ [a-zA-Z0-9.\_-]*//g' -e 's/\--machine\ [a-zA-Z0-9.\_-]*//g'`
	for file in kadeploy_tmp_cluster_*
	do
	    if [ -f $file ]
	    then
	        cluster=`echo $file | sed 's/kadeploy\_tmp\_cluster\_//g'`
		echo "Launching Kadeploy on $cluster nodes"
		cmd_args=$args" -f kadeploy_tmp_cluster_"$cluster" -c "$cluster
		set -- $cmd_args 
	        ${append} sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$@" &
	    fi
	done

	#waiting all instances of kadeploy
	wait

	#copy nodes files in the current directory
	cp /tmp/kadeploy-$USER*.out . 2>/dev/null
	#remove nodes files in /tmp
	sudo -u $DEPLOYUSER $DEPLOYDIR/bin/kadeploy --rmnodefilesintmp $USER

	#copy ssh keys in root's authorized_keys
	if [ $keys ]
	then
	    for file in kadeploy_tmp_cluster_*
	    do
		if [ -f $file ]
		then
		    cluster=`echo $file | sed 's/kadeploy\_tmp\_cluster\_//g'`
		    if [ -f ~/.ssh/id_rsa.pub ]
		    then
			kaaddkeys -f "kadeploy-"$USER"-"$cluster"-nodes_ok.out" -k ~/.ssh/id_rsa.pub
		    elif [ -f ~/.ssh/id_dsa.pub ]
		    then 
			kaaddkeys -f "kadeploy-"$USER"-"$cluster"-nodes_ok.out" -k ~/.ssh/id_dsa.pub
		    fi
		fi
	    done
	fi

	rm -f kadeploy_tmp_cluster_*
	#launch a script after the deployment
	[ $script ] && $script
    else
	${append} sudo -u $DEPLOYUSER $DEPLOYDIR/bin/`basename $0` "$@"
    fi
    OK=1
fi

if [ -x $DEPLOYDIR/sbin/`basename $0` ]
then
    sudo -u $DEPLOYUSER $DEPLOYDIR/sbin/`basename $0` "$@"
    OK=1
fi

[ "$OK" -ne "1" ] && echo "kasudowrapper.sh badly configured, use (prefix)/sbin/kasetup -exportenv"
