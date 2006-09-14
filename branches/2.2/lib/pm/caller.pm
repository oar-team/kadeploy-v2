package libkadeploy2::caller;

sub print()
{
    my $i = 0;
    my $pack;
    my $file;
    my $line;
    my $subname;
    my $hasargs;
    my $wantarray;

#    print "
#+--------------------------+---------------------------------------+---------+--------+-----------+
#| package                  | subname                               | line    | hasarg | wantarray |
#+--------------------------+---------------------------------------+---------+--------+-----------+
#";
    while (($pack, $file, $line, $subname, $hasargs, $wantarray) = caller($i++)) 
    {
	print STDERR "********** $pack, $subname,$line,$hasargs,$wantarray\n";
#	format STDOUT=
#|@<<<<<<<<<<<<<<<<<<<<<<<<<|@<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<|@<<<<<<<<|@<<<<<<<|@<<<<<<<<<<|
#$pack, $subname,$line,$hasargs,$wantarray
#.
#    write;
    }
#    print "+--------------------------+---------------------------------------+---------+--------+-----------+
#";

}

1;
