#!/usr/bin/env perl

$| = 1;

use strict;
use warnings;
use feature 'say';
use autodie;
use Data::Dump 'dump';
use Cwd 'cwd';
use File::Basename qw(basename fileparse dirname);
use File::Path qw(make_path);
use File::Find::Rule;
use File::Spec::Functions;
use Getopt::Long;
use Math::Combinatorics;
use Pod::Usage;
use Readonly;
use DBI;

Readonly my $BIN_SIZE => 5; # 4 ^ 5 = 1025 < max num. open files

my %FHS;
main();

# --------------------------------------------------
sub main {
    my $in_dir    = '';
    my $out_dir   = '';
    my $kmer_size = 20;
    my ($help, $man_page);

    GetOptions(
        'i|in-dir=s'  => \$in_dir,
        'o|out-dir:s' => \$out_dir,
        'k|kmer:i'    => \$kmer_size,
        'help'        => \$help,
        'man'         => \$man_page,
    ) or pod2usage(2);

    if ($help || $man_page) {
        pod2usage({
            -exitval => 0,
            -verbose => $man_page ? 2 : 1
        });
    }

    unless (-d $in_dir) {
        pod2usage("Bad input dir ($in_dir)");
    }

    my @files = File::Find::Rule->file()->in($in_dir);

    unless (@files) {
        pod2usage("No files found in '$in_dir'");
    }

    unless (-d $out_dir) {
        make_path($out_dir);
    }

    my $i;
    for my $file (@files) {
        my $basename = basename($file);
        printf "%5d: %s\n", ++$i, $basename;

        open my $fh, '<', $file;
        local $/ = '>';

        my $file_out_dir = catfile($out_dir, $basename);

        unless (-d $file_out_dir) {
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

    say "Done.";
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

  binner.pl -o kmer-out -l location-out -i input.fasta 

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
