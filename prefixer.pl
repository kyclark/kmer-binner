#!/usr/bin/env perl

use common::sense;
use autodie;
use Getopt::Long;
use Pod::Usage;

main();

# --------------------------------------------------
sub main {
    my $prefix = '';
    my ($help, $man_page);
    GetOptions(
        'prefix=s' => \$prefix,
        'help'     => \$help,
        'man'      => \$man_page,
    ) or pod2usage(2);

    if ($help || $man_page) {
        pod2usage({
            -exitval => 0,
            -verbose => $man_page ? 2 : 1
        });
    }; 

    unless ($prefix =~ /\S+/) {
        pod2usage('No prefix');
    }

    while (<>) {
        print $prefix . $_;
    }
}

__END__

# --------------------------------------------------

=pod

=head1 NAME

prefixer.pl - put a common prefix on all lines

=head1 SYNOPSIS

  prefixer.pl -p prefix /path/to/file

Options:

  --help   Show brief help and exit
  --man    Show full documentation

=head1 DESCRIPTION

Adds a prefix to the strings in a file.

=head1 AUTHOR

Ken Youens-Clark E<lt>kyclark@email.arizona.eduE<gt>.

=head1 COPYRIGHT

Copyright (c) 2015 kyclark

This module is free software; you can redistribute it and/or
modify it under the terms of the GPL (either version 1, or at
your option, any later version) or the Artistic License 2.0.
Refer to LICENSE for the full license text and to DISCLAIMER for
additional warranty disclaimers.

=cut
