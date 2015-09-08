###########################################################################
# Package
#    restDriver.pm
#
# Dependencies
#    -none
#
# Purpose
#    Perl script to create a REST client and make API calls.
#
#
# Copyright (c) 2015 Electric Cloud, Inc.
# All rights reserved
###########################################################################
    
# -------------------------------------------------------------------------
# Includes
# -------------------------------------------------------------------------
use strict;    
use warnings;
use ElectricCommander;
use ElectricCommander::PropMod;
use LWP 5.825;#5.831;#6.02;
use JSON;
use ElectricCommander::PropDB;

my $ec = new ElectricCommander();
#load module from property
ElectricCommander::PropMod::loadPerlCodeFromProperty($ec,"/myProject/XML::Twig");
local $| = 1;
#############################################################################
# main - Makes an REST API call and prints the server's response
#
# Arguments:
#   -none
#
# Returns:
#   -nothing
#
#############################################################################
# sub main {
sub callRest {


    $ec->abortOnError(0);
    my $browser = LWP::UserAgent->new;
    
    # Constants
    my $whitespace = q{ };
    my $empty = q{};
    my $slash = q{/};
    my $colon = q{:};
    
    # Params

    # $connection_config = 'AppDynamics'
    # $pathUrl = '' ('v1.1')
    # $contentType = 'application/json'
    # $headers_ = 'Accept: application/json'
    # $formContent = {json: "json", ...}
    # $filePath = '' (Source path)
    # $requestType = GET/POST/etc.
    # $response_outpp - '/myProject/resp_out'

    my ($connection_config, $pathUrl, $contentType, $headers_, 
        $formContent, $filePath, $requestType, $response_outpp, $proj) = @_;

    my @headers     = split("\n",$headers_);
    my @credentials = split($slash,($ec->getProperty( "credentials" ))->findvalue('//value')->string_value);

    my $username            = $empty;
    my $password            = $empty;
    my $format              = $empty;
    my $controllerHostName  = $empty;    
    my $controllerPort      = $empty;    
    my $accountName         = $empty;    
    my $accountAccessKey    = $empty;    
    
    my $pluginKey = 'EC-AppDynamics';
    my $xpath = $ec->getPlugin($pluginKey);
    my $pluginName = $xpath->findvalue('//pluginVersion')->value;    
    
    if (length($connection_config))
    {
        print "Loading config $connection_config\n";
        
        print "\$proj is '$proj'\n";
        my $cfg  = new ElectricCommander::PropDB( $ec,
            "/projects/$proj/appdynamics_cfgs" );

        my %vals = $cfg->getRow($connection_config);

        # Check if configuration exists
        unless ( keys(%vals) ) {
            die "Configuration [$connection_config] does not exist\n";
        }

        $controllerHostName = $vals{controllerHostName};
        $controllerPort = $vals{controllerPort};
        $accountName = $vals{accountName};
        $accountAccessKey = $vals{accountAccessKey};
        
        # Get user/password out of credential named in $opts->{credential}
        my $xpath = $ec->getFullCredential($vals{credential});
        $username = $xpath->findvalue("//userName");
        $password = $xpath->findvalue("//password");
    }

    my $urlText = $empty;
    # Validate Port
    if($port eq $empty){        
        $urlText = $baseUrl.$slash.$pathUrl;
    } else {
        $urlText = $baseUrl.$colon.$port.$slash.$pathUrl;
    }
    # Validate URL
    if($urlText !~ m/^(https?:\/\/|)([A-Za-z0-9._-]+(:\d+)*)([\/?].*)?$/ixsmg){
        $ec->setProperty("outcome","error");
        print "Invalid URL: ".$urlText."\n";
        print "Please check the provided URL.\n"; return;
    }

    # Set the allowed HTTP methods
    my %methods_allowed = map {$_ => 1} qw/GET POST PUT PATCH DELETE HEAD OPTIONS/;

    # Default to GET
    $methods_allowed{$requestType} or $requestType = 'GET';

    # Request Method
    my $url = URI->new($urlText);
    my $req = HTTP::Request->new($requestType => $url);
    
    if ($username ne $empty and $password ne $empty) { $req->authorization_basic($username, $password);}

    # Set headers
    my $size = @headers;
    if($size>0){
        foreach my $row (@headers){
            my ($key,$value) = split($whitespace,$row);
            $req->header(qq{$key} => qq{$value});
        }
    }
    
    ## Set Request Content type
    if ($contentType ne $empty) { $req->content_type($contentType);}

    # Set Request Content
    if ($filePath ne $empty) {

        my $content = open(my $fin, $filePath);
        if (!defined($content)) {
            print "Could not open $filePath!\n";

            return 1;
        }

        $req->content(<$fin>);
    }
    elsif ($formContent ne $empty) {

        $req->content($formContent);
    }
    
    # Print Request Parameters
    print "Creating Request...\n";
    my $sreq = $req->as_string;
    $sreq =~ s/Authorization: .*\n/Authorization: [PROTECTED]\n/m;
    print "> Request:\n".$sreq;

    # Perform Request
    print "Sending Request to Server...\n";
    my $response = $browser->request($req);
    
    # Check for errors
    print "----------------------------------------------------------------------------------------------------\n";
    print "Response Received.\nChecking for errors...\n";    
    if ($response->is_error) {
        $ec->setProperty("outcome","error");        
        if ($response->status_line ne $empty) { print "\nStatus: ".$response->status_line."\n";}
        print ("ERROR! The server was unable to fulfil the request.\nPlease check your parameters.");
        return;
    }
    
    # Print Response
    print "    No errors found.\nProceed to print response.\n";
    print "> Response\n";
    my @res = split("\n",$response->as_string); 
    $size = @res;
    my $response_line = shift(@res);
    while(($size>1) and ($response_line !~ m/(^{).*|(^\[).*|(^<).*/ixsmg)) {
        if($response_line =~ m/Content-Type:\s(.*)\/([A-Za-z0-9-.]*)/ixsmg){$format=$2;}
        print $response_line."\n";
        $response_line = shift(@res);
        $size--;
    }
    my $response_body = $response->content();
    
    # Response Format Validation
    if($format eq "xml") {
        my $twig = XML::Twig->new( pretty_print => 'indented' );#'nsgmls', 'nice','indented' 'record'or 'record_c'
        $twig->parse($response_body);
        $twig->root->print;
    } elsif($format eq "json" or $format eq "javascript") {
        my $json = JSON->new->allow_nonref;
        my $perl_scalar = $json->decode($response_body);
        print $json->pretty->encode( $perl_scalar );        
    } else {
        print $response_body;
    }
     
    # debug 
    print "\$response_outpp is '$response_outpp'\n";
    # Validate property storage
    if($response_outpp ne $empty) { $ec->setProperty($response_outpp, $response_body);}
    return;
}

1;
