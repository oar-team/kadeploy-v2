#!/usr/bin/perl
#Use example
#./grub_conf.pl -o essai.img
#               --title debian --root hda5 --kernel /boot/bzImage --param root=/dev/hda5
#               --title mdk --root hdb7 --kernel /boot/Vmlinuz --param "blablabla"

use Getopt::Long;
use strict;
use warnings;

sub getlen($){
    my $file = shift;
    my @len = ();
    
    my $grub_tmp_file_name = "grub_conf.tmp";
    
    # checks if file exists ! (if path is correct actually...)
    if (-e $file){
	system("ls -lL $file > $grub_tmp_file_name");
	open(TMP,"<$grub_tmp_file_name");
	while (<TMP>){
	    @len = split(/\t+| +/);
	}
	close(TMP);
	system("rm -f $grub_tmp_file_name");
	return $len[4];
    }else{
	print "ERROR : file $file does not exist or path is incorrect\n";
    }
}

if (!@ARGV){
    print "Usage : grub_conf.pl options\nOptions between '[]' are facultative\nThe last four options can be used several times to specify several entries\nPossible options are :\n\t[-o|--output-name image_output_name]   #default is grub.img\n\t[--menu menu_name]                     #default is menu.flopppy\n\t[--floppy-blks floppy_blks]            #default is 720\n\t[--timeout timeout]                    #default is 1\n\t[--default default entry]              #default is 0\n\t[--fallback fallback]                  #default is 1\n\t --title title\n\t --root root\n\t --kernel kernel\n\t --param param\n\n";
	exit 0;
}

#declares option variables and sets default values
my $output="grub.img";
my $floppy_blks = 720;
my $grub_dir="../boot/";

my $timeout = 1;
my $default = 0;
my $fallback = 1;
my $serial_speed = 38400;

my $menu="menu.floppy";

my @title = ();
my @root = ();
my @kernel = ();
my @param = ();

# gets the options
GetOptions('o=s' => \$output,
	   'output-name=s' => \$output,
	   'floppy-blks=i' => \$floppy_blks,
	   'timeout=i' => \$timeout,
	   'default=s' => \$default,
	   'fallback=s' => \$fallback,
	   'menu=s' => \$menu,
	   'title=s' => \@title,
	   'root=s' => \@root,
	   'kernel=s' => \@kernel,
	   'param=s' => \@param
	   );

$output = "/tmp/".$output;
$menu = "/tmp/".$menu;

my $stage1="$grub_dir/stage1";

my $stage2="$grub_dir/stage2";
my $stage2_len=getlen($stage2);

my $menu_len=2;
my $menu_offset=($floppy_blks - $menu_len);

print "* Creating floppy grub menu:";

my $i = 0;
open(MENU,">$menu");
print MENU "#autogen grub conf file\nserial --unit=0 --speed=$serial_speed\nterminal --timeout=0 serial\ntimeout $timeout\ncolor black/red yellow/red\ndefault $default\nfallback $fallback\n";
foreach my $title (@title){
    my $device = $root[$i];    
    my $dev = substr($device, 0, 3);
    my @nb = split(/$dev/, $device);
    my $part = $nb[1] - 1;
    my $letter = chop($dev);

    # to be improved...
    if ($letter eq "a"){
	$letter = "0";
    }elsif ($letter eq "b"){
	$letter = "1";
    }

    if (!$param[$i]){
	print MENU "\ntitle $title\nroot ($dev$letter,$part)\nkernel $kernel[$i] root=/dev/$root[$i]\n";
    }else{
	print MENU "\ntitle $title\nroot ($dev$letter,$part)\nkernel $kernel[$i] root=/dev/$root[$i] $param[$i]\n";
    }
    $i++;
}
close MENU;

print "* Creating blank floppy image of $floppy_blks blocks...\n";
system("dd if=/dev/zero of=$output bs=512 count=$floppy_blks");
print "* Copying grub stage1 starting from block 0\n";
system("dd if=$stage1 of=$output bs=512 conv=notrunc");
print "* Copying grub stage2 starting from block 1\n";
system("dd if=$stage2 of=$output bs=512 seek=1 conv=notrunc");
print "* Copying grub menu starting from block $menu_offset\n";
system("dd if=$menu of=$output bs=512 seek=$menu_offset count=$menu_len conv=notrunc");

print "\n$output has been generated.\n";
