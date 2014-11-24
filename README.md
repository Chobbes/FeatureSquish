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

    FeatureSquish dataset output_directory output_directory_csv iterations prob_of_removal+

Where `dataset` is the filename of the data, `prob_of_removal` is the
probability that any feature will be removed from the dataset, and
`output_directory` is where `FeatureSquish` should write the new
datasets with missing features to (`output_directory_csv` is where CSV
files should go). `iterations` is the number of datasets to generate
with each probability of removal. Multiple probabilities of removal
may be specified.

For example

    FeatureSquish example.train missing_data missing_data_csv 3 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9

Will generate a directory with the following structure:

    missing_data/
    ├── 0.1
    │   ├── example_1.train
    │   ├── example_2.train
    │   └── example_3.train
    ├── 0.2
    │   ├── example_1.train
    │   ├── example_2.train
    │   └── example_3.train
    ├── 0.3
    │   ├── example_1.train
    │   ├── example_2.train
    │   └── example_3.train
    ├── 0.4
    │   ├── example_1.train
    │   ├── example_2.train
    │   └── example_3.train
    ├── 0.5
    │   ├── example_1.train
    │   ├── example_2.train
    │   └── example_3.train
    ├── 0.6
    │   ├── example_1.train
    │   ├── example_2.train
    │   └── example_3.train
    ├── 0.7
    │   ├── example_1.train
    │   ├── example_2.train
    │   └── example_3.train
    ├── 0.8
    │   ├── example_1.train
    │   ├── example_2.train
    │   └── example_3.train
    └── 0.9
        ├── example_1.train
        ├── example_2.train
        └── example_3.train

Where each sub directory contains several copies of example.train with features removed with the given probability. An identical directory will be generated for the CSV files.
