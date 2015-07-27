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
#use Math::Combinatorics;

Readonly my $BIN_SIZE => 5; # 4 ^ 5 = 1025 < max num. open files

my %FHS;
main();

# --------------------------------------------------
sub main {
    my $in_dir     = '';
    my $out_dir    = '';
    my $files_list = '';
    my $kmer_size  = 20;
    my $quiet      = 0;
    my ($help, $man_page);

    GetOptions(
        'o|out-dir=s' => \$out_dir,
        'i|in-dir:s'  => \$in_dir,
        'f|files:s'   => \$files_list,
        'k|kmer:i'    => \$kmer_size,
        'q|quiet'     => \$quiet,
        'help'        => \$help,
        'man'         => \$man_page,
    ) or pod2usage(2);

    if ($help || $man_page) {
        pod2usage({
            -exitval => 0,
            -verbose => $man_page ? 2 : 1
        });
    }

    if ($in_dir && $files_list) {
        pod2usage('Please just one input source (dir or file)');
    }

    unless ($in_dir || $files_list) {
        pod2usage('No input source (-f|-i)');
    }

    my @files;
    if ($in_dir) {
        unless (-d $in_dir) {
            pod2usage("Bad input dir ($in_dir)");
        }

        @files = File::Find::Rule->file()->in($in_dir);

        unless (@files) {
            pod2usage("No files found in '$in_dir'");
        }
    }
    else {
        @files = grep { -e $_ } split(/\s*,\s*/, $files_list);

        unless (@files) {
            pod2usage("No good files found in '$files_list'");
        }
    }

    unless (-d $out_dir) {
        make_path($out_dir);
    }

    my $report = sub { $quiet || say @_ };
    my $i;
    for my $file (@files) {
        my $basename = basename($file);

        $report->(sprintf("%5d: %s", ++$i, $basename));

        open my $fh, '<', $file;
        local $/ = '>';

        my $file_out_dir = catfile($out_dir, $basename);

        if (-d $file_out_dir) {
            if (my @existing = File::Find::Rule->file()->in($file_out_dir)) {
                unlink @existing;
            }
        }
        else {
            make_path($file_out_dir);
        }

        while (my $fasta = <$fh>) {
            chomp $fasta;
            next unless $fasta;

            my ($id, @seq) = split /\n/, $fasta;
            my $seq        = join '', @seq;
            my $num_kmers  = length($seq) + 1 - $kmer_size;

            next unless $num_kmers > 0;

            for my $pos (0 .. $num_kmers - 1) {
                my $kmer = substr($seq, $pos, $kmer_size);
                my $bin  = substr($kmer, 0, $BIN_SIZE);
                my $out  = fh($bin, $file_out_dir);
                say $out substr($kmer, $BIN_SIZE);
            }
        }

        for my $fh (values %FHS) {
            close $fh;
        }
        %FHS = ();
    }

    $report->('Done.');
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

  binner.pl [-f /path/to/file] [-i /path/to/fasta] -o /path/to/kmers

  Required Arguments:

    -i|--in-dir    Input directory of FASTA files
    -o|--out-dir   Directory to write the binned k-mers

  Options:

    -k|--kmer      Size of the kmers (default "20")
    --help         Show brief help and exit
    --man          Show full documentation

=head1 DESCRIPTION

For each FASTA file in the "in-dir," split the sequences into k-mers
and then bin by the first 5 nucleotides.

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
