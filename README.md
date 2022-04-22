# ITEA: Interaction-Transformation Evolutionary Algorithm (v1.0)

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://github.com/folivetti/ITEA/blob/master/LICENSE)

**ITEA** is a fast and simple mutation-based Evolutionary Algorithm developed in Haskell. Check out the API [documentation](https://folivetti.github.io/ITEA/) if you want to extend the code.

## Dependencies

For Haskell-only:

- BlAS
- LAPACK
- GSL

For Python wrapper:

- Numpy
- Pandas
- Scikit-Learn

## Installation

1. Clone the repository with `git clone https://github.com/folivetti/ITEA.git`.

### Using Haskell stack

2. Install the Haskell Stack tool following the instructions at [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/).

3. Run `install_stack.sh`

### Using ghcup

2. Run `install_ghcup.sh` (this will also install ghcup)

### Using nix flake

2. Run `install_nix.sh`

## Running

In order to run the algorithm, first create the training and test set files as a comma separated values without a header (see `datasets` folder for some examples) and then create a config file for the experiment (see `configs` folder for some examples).

The config file is split into three sections where you can set different hyperparameters:

```
[IO]
train = path and name of the training set
test  = path and name of the test set
task  = Regression
log   = PartialLog "path and name of the output file"

[Mutation]
exponents      = (-3, 3)
termlimit      = (2,15)
nonzeroexps    = 10
transfunctions = [Id, Sin, Cos, Tanh, SqrtAbs, Log, Exp]
measures       = ["RMSE", "NMSE", "MAE", "R^2"]

[Algorithm]
npop      = 1000
ngens     = 1000
algorithm = ITEA

[Constraints]
penalty = NoPenalty
shapes  = []
domains = Nothing
```

The `task` parameter can be set to `Regression` or `Classification`, `transfunctions` accepts a list of transformation functions supported (see `src/IT/Eval.hs` block "Transformation Functions"), `measures` accepts a list of error (minimization) functions to use in the report generator (see `src/IT/Metrics.hs` blocks "Regression measures" and "Classification measures"). 
The `penalty` option can be `NoPenalty`, `Len <double value>` or `Shape <double value>`. The `shapes` option is a list of shape constraints, see `src/IT/Shape.hs` for a list of choices. `domains` is either `Nothing` or `Just [min_x0 ... max_x0, ...]` a list of interval of each variable domain.

Run the algorithm with the command:

```
stack run config <conf-file> 
```

where <conf-file> is the path and name of the config file.

As an alternative you can use the python wrapper as illustrated in `example.py`.

## Interaction-Transformation

Interaction-Transformation (IT) is a representation proposed in [1](https://www.sciencedirect.com/science/article/pii/S0020025516308635?casa_token=NSH9KVyjs84AAAAA:tDVSPVS8P15nHb8rZvLiW4klNp-nVew1QsKwsxz2YhpxZu2oyhUBJvkufKB8VK8Q6hJIaDr87oo) to avoid some redundancy in the search space of Symbolic Regression.

## Cite

de França, F. O., & Aldeia, G. S. I. (2020). Interaction-Transformation Evolutionary Algorithm for Symbolic Regression. *Evolutionary Computation*, 1-25.

Bibtex:

    @article{10.1162/evco_a_00285,
        author = {de Franca, F. O. and Aldeia, G. S. I.},
        title = "{Interaction-Transformation Evolutionary Algorithm for Symbolic Regression}",
        journal = {Evolutionary Computation},
        pages = {1-25},
        year = {2020},
        month = {12},    
        issn = {1063-6560},
        doi = {10.1162/evco_a_00285},
        url = {https://doi.org/10.1162/evco\_a\_00285},
        eprint = {https://direct.mit.edu/evco/article-pdf/doi/10.1162/evco\_a\_00285/1888497/evco\_a\_00285.pdf},
    }

    
## Experiments Results

Notice that the results in this repository are not the same as those in the referenced paper due to constant improvements to the source code (that sometimes fails). The original results for ITEA and every other algorithm in the paper are available at [experiments-ITEA-paper](https://github.com/gAldeia/experiments-ITEA-paper).
    
## Contact

Maintained by Fabrício Olivetti de França (folivetti at ufabc.edu.br)

## Acknowledgments

This project is supported by Fundação de Amparo à Pesquisa do Estado de São Paulo (FAPESP), grant number 2018/14173-8.

## License

GNU GPLv3, see [LICENSE](LICENSE)
