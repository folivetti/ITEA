# ITEA: Interaction-Transformation Evolutionary Algorithm

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://github.com/folivetti/ITEA/blob/master/LICENSE)

**ITEA** is a fast and simple mutation-based Evolutionary Algorithm developed in Haskell. Check out the API [documentation](https://folivetti.github.io/ITEA/) if you want to extend the code.

## Installation

1. Install the Haskell Stack tool following the instructions at [https://docs.haskellstack.org/en/stable/README/](https://docs.haskellstack.org/en/stable/README/).

2. Clone the repository with `git clone https://github.com/folivetti/ITEA.git`.

3. In the repository root directory run `stack build` (and wait patiently).

## Running

In order to run the algorithm, first create the training and test set files as a comma separated values without a header (see `datasets` folder for some examples) and then create a config file for the experiment (see `configs` folder for some examples).

The config file is split into three sections where you can set different hyperparameters:

```
[Dataset]
train = path and name of the training set
test  = path and name of the test set

[Mutation]
exponents      = (-3, 3) 
termlimit      = (2, 15)
nonzeroexps    = 10
transfunctions = FAll

[Algorithm]
npop  = 100
ngens = 1000
log   = PartialLog "path and name of the output file"
```

The `transfunctions` parameter accepts the values `FLinear` (only the `id` function), `FNonLinear` (`exp, log, abs.sqrt` functions), `FTrig` (`sin, cos, tanh` functions) and `FAll` in order to use every transformation function currently available.

Run the algorithm with the command:

```
stack run config <conf-file> 
```

where <conf-file> is the path and name of the config file.

## Interaction-Transformation

Interaction-Transformation (IT) is a representation proposed in \cite{de2018greedy} to avoid some redundancy in the search space.
Given the definitions of a transformation function $t: \mathbb{R} \rightarrow \mathbb{R}$, as any univariate function, and an interaction function $p: \mathbb{R}^{d} \rightarrow \mathbb{R}$ for a $d$-dimensional space and described as:

$$p(\mathbf{x}) = \prod_{i=1}^{d}{x_i^{k_i}},$$

with $k_i \in \mathbb{Z}$ called the **strength** of the interaction. We can define an IT expression for regression as a function with the form:

$$
f(\mathbf{x}) = w_0 + \sum_{i}{w_i \cdot (t_i \circ p_i) (\mathbf{x})},
$$

where $w_i \in \mathbb{R}$ is the $i$-th coefficient of a linear combination, hereafter referred to as **weight**.


## Cite

de França, Fabrício Olivetti, and Guilherme Seidyo Imai Aldeia. "Interaction-Transformation Evolutionary Algorithm for Symbolic Regression." arXiv preprint arXiv:1902.03983 (2019).

Bibtex:

    @article{de2019interaction,
      title={Interaction-Transformation Evolutionary Algorithm for Symbolic Regression},
      author={de Fran{\c{c}}a, Fabr{\'\i}cio Olivetti and Aldeia, Guilherme Seidyo Imai},
      journal={arXiv preprint arXiv:1902.03983},
      year={2019}
    }
    
## Contact

Maintained by Fabrício Olivetti de França (folivetti at ufabc.edu.br)

## Acknowledgments

This project is supported by Funda\c{c}\~{a}o de Amparo \`{a} Pesquisa do Estado de S\~{a}o Paulo (FAPESP), grant number 2018/14173-8.

## License

GNU GPLv3, see [LICENSE](LICENSE)
