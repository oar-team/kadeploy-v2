package libkadeploy2::sudo;

use strict;
use warnings;


sub get_user()
{
    return $ENV{USER};
}

sub get_sudo_user()
{
    return $ENV{SUDO_USER};
}



1;
