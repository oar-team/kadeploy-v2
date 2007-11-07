## debug functions
package libkadeploy2::debug;

use strict;
use warnings;


# Debug level
# 0 : extreme verbose debug
# 1 : verbose debug
# 2 : normal debug
# 3 : light debug
# 4 : no debug
#
# We use KADEPLOY_DEBUG_LEVEL to propagate the debug level value (should use Exporter)

my $current_debug_level;

sub debugl($$) {
    if ($ENV{KADEPLOY_DEBUG_LEVEL}) {
	$current_debug_level = $ENV{KADEPLOY_DEBUG_LEVEL};
    } else {
	$current_debug_level = 1;
    }
    my $msg_debug_level = shift;
    my $msg = shift;

    if ($msg_debug_level >= $current_debug_level) {
	print($msg);
    }
}

sub system_wrapper($) {
    if ($ENV{KADEPLOY_DEBUG_LEVEL}) {
	$current_debug_level = $ENV{KADEPLOY_DEBUG_LEVEL};
    } else {
	$current_debug_level = 1;
    }
    my $cmd = shift;
    my $ret;

    if ($current_debug_level <= 1) {
	$ret = system($cmd);
    } else {
	$ret = system($cmd." &>/dev/null");
    }
    return $ret;
}

sub exec_wrapper($) {
    if ($ENV{KADEPLOY_DEBUG_LEVEL}) {
	$current_debug_level = $ENV{KADEPLOY_DEBUG_LEVEL};
    } else {
	$current_debug_level = 1;
    }
    my $cmd = shift;

    if ($current_debug_level <= 1) {
	exec($cmd);
    } else {
	exec($cmd." &>/dev/null");
    }
}

#End of the module
1;
