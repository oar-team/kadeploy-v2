package libkadeploy2::pathlib;

use strict;
use warnings;
use libkadeploy2::debug;

sub strip_leading_slash($)
{
    my $f=shift;
    
    $f =~ s/^\///;
    return($f);
}

sub strip_leading_dirs($)
{
    my $f=shift;
    
    $f =~ s/.*\/([^\/]*)$/$1/;
    return($f);
}

sub check_multiboot_kernel($)
{
    my $k = shift;
    return($k =~ /\bmboot\.c32\b/);
}

1;
