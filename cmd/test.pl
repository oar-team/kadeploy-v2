#!/usr/bin/perl
use lib::conflib;

$toto = conflib::get_conf(tftp_repository_intel);

print "$toto\n";
