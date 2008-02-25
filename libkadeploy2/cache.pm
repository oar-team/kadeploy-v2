package libkadeploy2::cache;

# use strict;
# use warnings;

use libkadeploy2::debug;

## ================
## Global variables
## ================
## Tftp chroot
my $_tftpdirectory;
## Directory path of Kadeploy environment files cache
my $_cachedirectory;
## days count after which files are removed from cache
my $_expirydelay    = 30;
## Array of files in cache
my @_filesincache   = ();


## =========
## Functions
## =========

## Returns true if cache is empty
sub empty_cache()
{
   if ( ! @_filesincache ) { return 1; }
   else { return 0; }
}

## Returns the directory path used for cached files
sub get_cache_directory()
{
    return $_cachedirectory;      
}

## Returns cache directory path relative to n subdirectories from tftpdirectory
sub get_cache_directory_tftprelative($)
{
    my $nsubdirs = shift;
    
    my $relativepath = $_cachedirectory;
    $relativepath =~ s:^$_tftpdirectory/::;
    
    for (my $a = 1; $a <= $nsubdirs; $a++)
    {
	$relativepath = "../" . $relativepath;
    }
    return $relativepath;
}

## Initialize the cache : get TFTP root path + check cache directory
sub init_cache($)
{
    $_tftpdirectory = shift;
    ## Strip tailing /
    $_tftpdirectory =~ s/\/$//;
    $_cachedirectory = $_tftpdirectory . "/" . "cache";
    
    if ( ! libkadeploy2::cache::test_cache_directory() ) { return 0; }
    return 1;
}

## Test if cache directory exists 
## and create it if needed
sub test_cache_directory()
{
    if (! -d $_cachedirectory) 
    { 
	my $success =  mkdir $_cachedirectory, 0755;
        if ($success) { return 1; }
        else 
        {
	    libkadeploy2::debug::debugl(3, "$0: mkdir failed on $_cachedirectory\n");
	    return 0;
	}
    }
    elsif ( ! -r $_cachedirectory || ! -w $_cachedirectory || ! -x $_cachedirectory ) 
    {
	@failed = grep !(chmod 0755, $_), $_cachedirectory;
	if ( @failed )
	{
	    libkadeploy2::debug::debugl(3, "$0 : chmod failed on $_cachedirectory\n");
	    exit 0;
	}
	else { return 1; }
    }
    else { return 1; }
}

## Read cache directory and returns files list
sub read_files_in_cache()
{
    # print "cache::read_files_in_cache() \n";
    
    if ( ! opendir CACHEHANDLE, $_cachedirectory )
    {
	libkadeploy2::debug::debugl(3, "$0 : opendir failed on $_cachedirectory");
	exit 0;
    }
    
    @_filesincache = grep !/^\.\.?$/, readdir CACHEHANDLE;
    closedir CACHEHANDLE;
    return 1;
}

## Search a file in cache
sub already_in_cache($)
{
    if ( empty_cache() ) { return 0; }
    
    my $searchedfile = shift;
    # print "cache::already_in_cache : _filesincache = @_filesincache\n";
    
    if ( grep /$searchedfile/, "@_filesincache" ) { return 1; }
    else { return 0; }
}

## Put files in cache if they're not already in
sub put_in_cache_from_archive($$$)
{
    my $ref_files   = shift;
    my @files = @{$ref_files};
    my $archive = shift;
    my $strip   = shift;
    my $cachemodified = 0;
    my $agearchive = 0;
    my $agecachefile = 0;

    if ( empty_cache() ) { read_files_in_cache(); }
    
    ## Clean cache from oldest files
    libkadeploy2::cache::clean_cache();
    
    ## Securely add eventually new files
    foreach my $archivefile ( @files )
    {
	my $cachefile = $archivefile;
	# Strip leading directories from filename
	$cachefile =~ s/.*\/([^\/]*)$/$1/;
	$agearchive = ( -M $archive );
	$agecachefile = ( -C $_cachedirectory."/".$cachefile );
        # print $archive." : ".$agearchive." | ".$_cachedirectory."/".$cachefile." : ".$agecachefile."\n";
	if ( ( ! already_in_cache($cachefile) ) || ( $agearchive < $agecachefile ) )
	{
	    # print "cache::put_in_cache_from_archive :  adding " . $archivefile . "\n";
	    my $islink = 1;
	    while ($islink) 
	    {
		libkadeploy2::debug::system_wrapper("tar -C $_cachedirectory --strip $strip -xzf $archive $archivefile");
		$file = readlink $_cachedirectory."/".$archivefile;
		if ( ! $file ) { $islink = 0; }
	    }
	    $cachemodified = 1;
	}
	# else : maj estampille temporelle dernier acces (-A) / touch / ben non : c automatique
    }
    
    ## If cache modified, reload it
    if ( $cachemodified ) 
    { 
	print "cache::put_in_cache_from_archive : reload cache\n";
	read_files_in_cache();
	$cachemodified = 0;
    }
    
    return 1;
}

## Remove oldest files from cache
sub clean_cache()
{
    my $cachemodified = 0;
    
    foreach my $currentfile ( @_filesincache )
    {
	my $f = $_cachedirectory . "/" . $currentfile;
	my $ageoffile = ( -A $f );
	# print "cache::clean_cache() : currentfile = " . $currentfile . " -A = " . $ageoffile . "\n";	
	if ( $ageoffile > $_expirydelay )
	{
	    libkadeploy2::debug::system_wrapper("rm -f $_cachedirectory/$currentfile");
	    $cachemodified = 1;
	}
    }
    
    if ( $cachemodified ) 
    { 
	read_files_in_cache();
	$cachemodified = 0;
    }
    
    return 1;
}

## Remove all files from cache
sub purge_cache()
{
    libkadeploy2::debug::system_wrapper("rm -rf $_cachedirectory/*");
    read_files_in_cache();

    return 1;
}

1;
