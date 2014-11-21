FeatureSquish
=============

Remove features at random from a data set for great justice.

Installation
------------

It should just install with cabal. You will probably want to install
the [Haskell Platform](https://www.haskell.org/platform/).

    cabal install --bindir=/path/to/where/you/want/the/bin

If you have [nix](http://nixos.org/nix/) you can use `nix-shell` to
run the cabal install, and it will make sure any extra dependencies
have been installed.

Usage
-----

Given a dataset you call the program as follows

    FeatureSquish dataset output_directory iterations prob_of_removal+

Where `dataset` is the filename of the data, `prob_of_removal` is the
probability that any feature will be removed from the dataset, and
`output_directory` is where `FeatureSquish` should write the new
datasets with missing features to. `iterations` is the number of
datasets to generate with each probability of removal. Multiple
probabilities of removal may be specified.
