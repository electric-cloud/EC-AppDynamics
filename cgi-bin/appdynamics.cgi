#!/bin/sh

exec "$COMMANDER_HOME/bin/ec-perl" -x "$0" "${@}"
 
################################
#!perl
# appdynamics.cgi -
#
# Get/set EC-AppDynamics configuration info for UI
#
# The following special keyword indicates that the "cleanup" script should
# scan this file for formatting errors, even though it doesn't have one of
# the expected extensions.
# CLEANUP: CHECK
#
# Copyright (c) 2007-2012 Electric Cloud, Inc.
# All rights reserved
################################

use strict;
use warnings;
use Getopt::Long;
use File::Spec;
use File::Temp;
use ElectricCommander;
use ElectricCommander::PropMod;
use ElectricCommander::PropDB;
use CGI qw(:standard);

use constant {
    SUCCESS => 0,
    ERROR   => 1,
};

# used for output redirection
our $tmpOut = "";
our $tmpErr = "";
our $oldout;
our $olderr;

################################
# main - Main program for the application.
#
# Arguments:
#   -
#
# Returns:
#   0 - Success
#   1 - Error
#
################################
sub main() {

    # globals
    $::cg = CGI->new();
    $::opts = $::cg->Vars;
    $::ec = new ElectricCommander();
    $::ec->abortOnError(0);

    # make sure no libraries print to STDOUT
    saveOutErr();

    # Check for required arguments.
    if (!defined $::opts->{cmd} || $::opts->{cmd} eq "") {
        retError("error: cmd is required parameter");
    }

    # ---------------------------------------------------------------
    # Dispatch operation
    # ---------------------------------------------------------------
    for ($::opts->{cmd})
    {
        # modes
        /getCfgList/i and do   { getCfgList(); last; };
    }
    retError("unknown command $::opts->{cmd}");

    exit SUCCESS;
}

################################
# xmlQuote - Quote special characters such as & to generate well-formed XML character data.
#
# Arguments:
#   string - String whose contents should be quoted.
#
# Returns:
#   string - Quoted string
#
################################
sub xmlQuote($) {
    my ($string) = @_;

    $string =~ s/&/&amp;/g;
    $string =~ s/</&lt;/g;
    $string =~ s/>/&gt;/g;
    $string =~ s{([\0-\x{08}\x{0b}\x{0c}\x{0e}-\x{1f}])}{
    sprintf("%%%02x", ord($1))}ge;
    return $string;
}

################################
# getCfgList - Return the list of configurations from OpenStack
#
# Arguments:
#   -
#
# Returns:
#   0 - Success
#   1 - Error
#
################################
sub getCfgList {

    my $gcfg = new ElectricCommander::PropDB($::ec,"/projects/@PLUGIN_NAME@/appdynamics_cfgs");

    my %cfgs = $gcfg->getRows();
    # print results as XML block
    my $xml = "";
    $xml .= "<cfgs>\n";
    foreach my $cfg (keys  %cfgs) {
        my $url = $gcfg->getCol("$cfg/service_url");
        $xml .= "  <cfg>\n";
        $xml .= "     <name>$cfg</name>\n";
        $xml .= "     <url>" . xmlQuote($url) . "</url>\n";
        $xml .= "  </cfg>\n";
    }
    $xml .= "</cfgs>\n";
    
    printXML($xml);
    exit SUCCESS;
}

################################
# retError - Return an error message
#
# Arguments:
#   msg - Message to return
#
# Returns:
#   0 - Success
#   1 - Error
#
################################
sub retError {
    my ($msg) = @_;

    printXML("<error>$msg</error>\n");
    exit ERROR;
}

################################
# printXML - Print the XML block, add stdout, stderr
#
# Arguments:
#   xml - xml string to print
#
# Returns:
#   -
#
################################
sub printXML {
    my ($xml) = @_;

    my ($out,$err) = retrieveOutErr();
    print $::cg->header("text/html");
    print "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
    print "<response>\n";
    print "$xml\n";
    print "<stdout>" . xmlQuote($out) . "</stdout>\n";
    print "<stderr>" . xmlQuote($err) . "</stderr>\n";
    print "</response>";
}

################################
# saveOutErr - Redirect stdout/stderr to files so that any spurious output from commands does not end up on the return to the cgi caller
#
# Arguments:
#   -
#
# Returns:
#   -
#
################################
sub saveOutErr {
    # temporarily save STDOUT/STDERR to files
    open $::oldout, ">&STDOUT"  or die "Can't dup STDOUT: $!";
    open $::olderr, ">&STDERR"  or die "Can't dup STDERR: $!";
    close STDOUT;
    open STDOUT, '>', \$::tmpOut or die "Can't open STDOUT: $!";
    close STDERR;
    open STDERR, '>', \$::tmpErr or die "Can't open STDOUT: $!";

}

################################
# retrieveOutErr - Reset stdout/sterr back to normal and return the contents of the temp files
#
# Arguments:
#   -
#
# Returns:
#   tmpOut - Content of out file
#   tmpErr - Content of error file
#
################################
sub retrieveOutErr {
    # reconnect to normal STDOUT/STDERR
    open STDOUT, ">&", $::oldout or die "can't reinstate $!";
    open STDERR, ">&", $::olderr or die "can't reinstate $!";
    return ($::tmpOut, $::tmpErr);
}

main();






