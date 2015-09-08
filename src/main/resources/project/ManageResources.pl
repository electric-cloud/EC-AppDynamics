=head1 NAME

EC-AppDynamics, ManageResources procedure
Dynamically manages resources of a business application based on data from 
AppDynamics Controller

=head1 DESCRIPTION

This script provides decision property 'resource_control' to for later 
stages (subworkflows) that handles additional resources 

Copyright (c) 2015 Electric Cloud, Inc.
All rights reserved
=cut


use ElectricCommander::PropMod qw(/myProject/scripts);
use restDriver;
use JSON;
use strict;
use 'EC::Plugin::Core';

print "ManageResources procedure\n";
$| = 1;

sub getProperty {
    my ($ec, $name, $mandatory, $default) = @_;
    my $ret = $ec->getProperty($name)->findvalue('//value')->string_value;

    if(!$ret && $mandatory) { die "Missing mandatory parameter '$name'."; }    
    
    return $ret || $default;
}

my $ec = ElectricCommander->new();
$ec->abortOnError(0);

# Get FP
my $max_calls_per_minute    = getProperty($ec, "max_calls_per_minute", 1);
my $min_calls_per_minute    = getProperty($ec, "min_calls_per_minute", 1);
my $debug = getProperty($ec, "debug", 0);

my $summary = "";

# Process received data, get app status
my $call_per_minute = getProperty($ec, '/myJob/call_per_minute', 0);

if (($call_per_minute > $max_calls_per_minute) || ($call_per_minute < $min_calls_per_minute)) {
    if ($call_per_minute > $max_calls_per_minute) {
        # Increase resources
        $ec->setProperty('/myJob/resource_control', '1');
    }
    if ($call_per_minute < $min_calls_per_minute) {
        # Decrease resources
        $ec->setProperty('/myJob/resource_control', '-1');
    }
}
else {
    # Nothing to change
    $ec->setProperty('/myJob/resource_control', '0');
}


print "Done\n";
