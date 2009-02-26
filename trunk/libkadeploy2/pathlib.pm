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

sub strip_bar($)
{
    my $f=shift;
    
    $f =~ s/^\/(.*)$/$1/;
    return($f);
}

sub strip_dotdot($)
{
    my $f=shift;
      
    $f =~ s/^[\.\/]*(.*)$/$1/;
    return($f);
}

sub get_leading_dirs($)
{
    my $f=shift;
    
    if ($f =~ m/[^\/]*\/.*/) {
	$f =~ s/(^[\/a-zA-Z0-9\-_]*)\/[^\/]+/$1/;
	return($f);
    } else {
	return ("");
    }
}

sub get_subdir_root($)
{
    my $f=shift;
    
    $f =~ s/^([^\/]+)\/.*$/$1/;
    return($f);
}


sub check_multiboot_kernel($)
{
    my $k = shift;
    
    return($k =~ /\bmboot\.c32\b/);
}

1;
