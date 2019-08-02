#!/usr/bin/perl -w
#
 
use strict;
use LWP::UserAgent;

my $browser = LWP::UserAgent->new;

my $response = $browser->post(
			      'http://lbccontrol/ui/3.php',
			      ['play' => 'false',
			       'import' => 'true',
			       'import_x' => 'true',
			       'userfile' => ["/home/lbceng/WRS/src/R_rfastextra.ob"]
			       ],
			      'Content_Type' => 'form-data');

die "Error: ", $response->status_line
 unless $response->is_success;

# print "$response->content";
# while( my ($k, $v) = each %$response ) {
#    print "key: $k, value: $v.\n";
# }
