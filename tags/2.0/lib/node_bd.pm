# Node extension for database
#
package node_bd;

# already solved ? TODO: pb set_state : should change state only for one partition, on one disk etc. !
# TO DO : change name <-> ip_addr (? - ask conf julien)

use lib::node;
use base ("node");   # declare superclasses

use lib::deploy_iolib;

use strict;
use warnings;

sub new {
    my ($class,$name,$deploy_id,$env_name,$disk_dev,$part_nb) = @_;

    ## database modifications ##
    my $base = deploy_iolib::connect();
    my $successful = deploy_iolib::add_node_to_deployment($base,$name,$deploy_id,$env_name,$disk_dev,$part_nb);
    if(!$successful){
	print "ERROR : impossible to add node to deployment\n";
	return 0;
    }
    my $addr = deploy_iolib::node_name_to_ip($base,$name);
    if(!$addr){return 0;}
    deploy_iolib::disconnect($base);
    ## end database modifications ##
    
    my $self = {};
    $self->{name} = $name;
    $self->{deploymentId} = $deploy_id;
    $self->{state} = 0;
    bless ($self, $class);
    # now you can invocate methods...
    $self->set_IP($addr);
    return $self;
}

## state
# 0: initial state, know nothing about the node
# 1: node is available for deployment
# -1: node is not available for deployment
sub get_state {
    my $self = shift;
    $self->SUPER::get_state();
}

sub set_state {
    my $self = shift;
    my $state_db;
    my $nextState = shift;
    my $currentState;
    my $deploy_id = $self->{deploymentId};
    my $error_status = "";
    
    $currentState = $self->get_state();

    if ($currentState == $nextState) {
	return 1;
    }
    
    if ($currentState == 0) { # node never appeared before
	if ($nextState == -1) { # node is not yet there
	    $state_db = 'error';
	    $error_status = node::get_error($self);
	} elsif ($nextState == 1) { # node appeared during first check
	    $state_db = 'deploying';
	}
    } elsif ($currentState == -1) { # node appears during check, and only there
	    $state_db = 'deploying'; # node appears
    } else { # node is there (current state == 1)
	$state_db = 'error'; # node disappears
	$error_status = node::get_error($self);
    }

    my $name = $self->get_name();

    my $base = deploy_iolib::connect();
    deploy_iolib::report_state($base,$deploy_id,$name,$state_db,$error_status);
    deploy_iolib::disconnect($base);

    $self->SUPER::set_state($nextState);
}

1;
