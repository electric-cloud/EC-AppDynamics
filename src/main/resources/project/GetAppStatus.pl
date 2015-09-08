=head1 NAME

EC-AppDynamics, GetAppStatus procedure
Retrieves status of application from AppDynamics Controller

=head1 DESCRIPTION

This script produce status property for later stages

Copyright (c) 2015 Electric Cloud, Inc.
All rights reserved
=cut

use ElectricCommander::PropMod qw(/myProject/scripts);
use restDriver;
use JSON;
use strict;
use 'EC::Plugin::Core';

print "GetAppStatus procedure\n";
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
my $applicationName = getProperty($ec, "applicationName", 1);
my $tierName = getProperty($ec, "tierName", 1);
my $nodeName = getProperty($ec, "nodeName", 1);
my $debug = getProperty($ec, "debug", 0);

# Parameters for the connection
my $contentType = 'application/json';
my $headers_ = 'Accept: application/json';
my $filePath = '';
my $requestType = 'POST';
my $response_outpp = '/myJob/response';

my $pathUrl = '/controller/rest/' . $tierName . '?output=JSON';

callRest($connection_config, $pathUrl, $contentType, $headers_, 
    $formContent, $filePath, $requestType, $response_outpp, $proj);

my $summary = "";

# Process received data, get app status
my $appd_resp = getProperty($ec, '/myJob/response', 0);

if ($appd_resp =~ m/callsPerMinute(.*)"(\d+)/sm)) {
    # $2 is a value of callsPerMinute
    $ec->setProperty('/myJob/call_per_minute', $2); 
}
else {
    # Error handling
    $summary = 'Error retrieve data form Controller. Step fails!\n';
    print $summary;

    $ec->setProperty('/myJobStep/outcome', 'error');
    $ec->setProperty("summary", $summary);
}


print "Done\n";
