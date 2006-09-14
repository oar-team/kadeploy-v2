package libkadeploy2::remotecopy;
use libkadeploy2::message;
use libkadeploy2::parallelcommand;
use strict;
use warnings;

my $message=libkadeploy2::message::new();
my $conf=libkadeploy2::deployconf::new();
$conf->load();

sub new()
{
    my $self=
    {
	connector        => "unknow",
	login            => "",
	nodelist         => 0,
	source           => "",
	dest             => "",
	timeout          => 0,
	verbose          => 0,
	status           => -1,
    };
    bless $self;    
    return $self;
}

sub set_connector($)            { my $self=shift; my $args=shift; $self->{connector}=$args; }
sub set_login($)                { my $self=shift; my $args=shift; $self->{login}=$args; }
sub set_nodelist($)             { my $self=shift; my $args=shift; $self->{nodelist}=$args; }
sub set_sourcefile($)           { my $self=shift; my $args=shift; $self->{source}=$args; }
sub set_destinationfile($)      { my $self=shift; my $args=shift; $self->{dest}=$args; }
sub set_timeout($)              { my $self=shift; my $args=shift; $self->{timeout}=$args; }
sub set_verbose($)              { my $self=shift; $self->{verbose}=1; }


sub exec()
{
    my $self=shift;
    if ($self->{connector} eq "ssh")
    { $self->exec_catpipessh(); }
    elsif ($self->{connector} eq "rsh")
    { $self->exec_catpipersh(); }
    else
    { $message->message("connector not supported\n"); exit 1; }
}

sub exec_catpipessh()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my $ref_node_list;
    my @node_list;
    my $node;
    my $nodeip;
    my @cmdlist;
    my $refcmdlist;
    my $cmd;
    my $ok=0;
    my $parallelcommand;
    my $ssh_default_args;

    $ssh_default_args=$conf->get("ssh_default_args");
    if (! $ssh_default_args) { $ssh_default_args=""; }

    $ref_node_list=$nodelist->get_nodes();
    @node_list=@$ref_node_list;
    foreach $node (@node_list)
    {	
	$nodeip=$node->get_ip();
	$cmd="cat ".$self->{source}." | ssh $ssh_default_args ".$self->{login}."@".$nodeip." 'cat > ".$self->{dest}."'";
	@cmdlist=(@cmdlist,$cmd);
    }
    $refcmdlist=\@cmdlist;
    $parallelcommand=libkadeploy2::parallelcommand::new();
    $parallelcommand->set_timeout($self->{timeout});
    if ($self->{verbose}) { $parallelcommand->set_verbose(); }
    $ok=$parallelcommand->execparallel($refcmdlist);
    $self->{status}=$ok;
    return $ok;
}

sub exec_catpipersh()
{
    my $self=shift;
    my $nodelist=$self->{nodelist};
    my $ref_node_list;
    my @node_list;
    my $node;
    my $nodeip;
    my @cmdlist;
    my $refcmdlist;
    my $cmd;
    my $ok=0;
    my $parallelcommand;
    my $ssh_default_args;

    $ssh_default_args=$conf->get("ssh_default_args");
    if (! $ssh_default_args) { $ssh_default_args=""; }

    $ref_node_list=$nodelist->get_nodes();
    @node_list=@$ref_node_list;
    foreach $node (@node_list)
    {	
	$nodeip=$node->get_ip();
	$cmd="cat ".$self->{source}." | rsh -l ".$self->{login}." ".$nodeip." 'cat > ".$self->{dest}."'";
	@cmdlist=(@cmdlist,$cmd);
    }
    $refcmdlist=\@cmdlist;
    $parallelcommand=libkadeploy2::parallelcommand::new();
    $parallelcommand->set_timeout($self->{timeout});
    if ($self->{verbose}) { $parallelcommand->set_verbose(); }
    $ok=$parallelcommand->execparallel($refcmdlist);
    $self->{status}=$ok;
    return $ok;
}


sub get_status()
{
    my $self=shift;
    return $self->{status};
}
1;
