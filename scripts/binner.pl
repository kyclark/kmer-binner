#!/usr/bin/env perl

$| = 1;

use strict;
use warnings;
use feature 'say';
use autodie;
use Cwd 'cwd';
use Data::Dump 'dump';
use File::Basename qw(basename fileparse dirname);
use File::Find::Rule;
use File::Path qw(make_path);
use File::Spec::Functions;
use Getopt::Long;
use Pod::Usage;
use Readonly;

Readonly my $BIN_SIZE => 5; # 4 ^ 5 = 1025 < max num. open files

my %FHS;
main();

# --------------------------------------------------
sub main {
    my $out_dir    = '';
    my $files_list = '';
    my $quiet      = 0;
    my ($help, $man_page);

    GetOptions(
        'o|out-dir=s' => \$out_dir,
        'help'        => \$help,
        'man'         => \$man_page,
    ) or pod2usage(2);

    if ($help || $man_page) {
        pod2usage({
            -exitval => 0,
            -verbose => $man_page ? 2 : 1
        });
    }

    unless ($out_dir) {
        pod2usage('Missing output dir');
    }

    unless (-d $out_dir) {
        make_path($out_dir);
    }

    while (my $line = <>) {
        chomp $line;

        my ($kmer, $count) = split /\s/, $line;
        next if index($kmer, 'N') > 0;
        my $bin  = substr($kmer, 0, $BIN_SIZE);
        my $out  = fh($bin, $out_dir);
        # need to put a bogus header for FASTA format
        say $out ">1\n", substr($kmer, $BIN_SIZE);
    }
}

# --------------------------------------------------
sub fh {
    my ($file_name, $out_dir) = @_;

    if (!defined $FHS{ $file_name }) {
        open my $fh,  '>', catfile($out_dir, $file_name);
        $FHS{ $file_name } = $fh;
    }

    return $FHS{ $file_name };
}

# --------------------------------------------------

=pod

=head1 NAME

binner.pl

=head1 SYNOPSIS

  jellyfish dump data.jf | binner.pl -o /path/to/bins

  Required Arguments:

    -o|--out-dir   Directory to write the binned k-mers

  Options:

    --help         Show brief help and exit
    --man          Show full documentation

=head1 DESCRIPTION

Bins the output of "jellyfish dump" for sorting and combining.

=head1 AUTHOR

Ken Youens-Clark E<lt>kclark@gmail.comE<gt>.

=head1 COPYRIGHT

Copyright (c) 2015 Hurwitz Lab

This module is free software; you can redistribute it and/or
modify it under the terms of the GPL (either version 1, or at
your option, any later version) or the Artistic License 2.0.
Refer to LICENSE for the full license text and to DISCLAIMER for
additional warranty disclaimers.

=cut
