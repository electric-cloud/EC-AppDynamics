@::gMatchers = (
  {
        id =>           "StandardError",
        pattern =>      q{\((.*)(Error)\)},
        action =>       q{&addSimpleError("StandardError", "$1 $2");setProperty("outcome", "error" );updateSummary();},
  },  
  {
        id =>           "RequestType",
        pattern =>      q{^(POST|DELETE|PUT|GET)},
        action =>       q{
                            my $desc = ((defined $::gProperties{"summary"}) ? $::gProperties{"summary"} : '');
                            $desc .= "Request Type: $1";
                            setProperty("summary", $desc . "\n");
                        },
  },
  {
        id =>           "Accept",
        pattern =>      q{Accept:\s(.*)},
        action =>       q{
                            my $desc = ((defined $::gProperties{"summary"}) ? $::gProperties{"summary"} : '');
                            $desc .= "Accept: $1";
                            setProperty("summary", $desc . "\n");
                        },
  },  
  {
        id =>           "Status",
        pattern =>      q{Status:\s(.*)},
        action =>       q{
                            my $desc = ((defined $::gProperties{"summary"}) ? $::gProperties{"summary"} : '');
                            $desc .= "HTTP Status Code: $1";
                            setProperty("summary", $desc . "\n");
                        },
  },
  {
        id =>           "InvalidURL",
        pattern =>      q{Invalid URL:\s(.*)},
        action =>       q{
                            my $desc = ((defined $::gProperties{"summary"}) ? $::gProperties{"summary"} : '');
                            $desc .= "Invalid URL: $1";
                            setProperty("summary", $desc . "\n");
                        },
  },
);
