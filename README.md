# kmer-binner

Consolidate and unique the k-mers of many FASTA files.

The Perl version basically works.

        $ perl binner.pl -i /dir/to/fasta -o /dir/to/bins
        $ sh sort.sh /dir/to/bins
        $ cd sorted
        $ cat * > all.kmers
        $ jellyfish count ... all.kmers
        $ PROFIT!

Working on a Haskell version:

  $ runghc Binner.hs /dir/to/fasta
