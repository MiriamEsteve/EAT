# EAT: Efficiency Analysis Trees

[Efficiency Analysis Trees](https://www.sciencedirect.com/science/article/pii/S0957417420306072) is an alghoritm by which a production frontier is obtained through and adaptation of regression trees based on CART. The generation of production frontiers falls within the field of efficiency analysis, of which some concepts must be known:

* A **production frontier** is a boundary defined for those feasible combinations of input and output that are efficient.
* A **DMU** (**D**ecision **M**aking **U**nits) is an observation of the dataset whose efficiency is to be assessed.
* A specific DMU is **efficient** when is located at the production frontier and it has room for improvement regarding its inputs or outputs when it is in the area below the frontier.

The `EAT` algorithm must be conceived as a modeling of the response variable (output) in order to know its most efficient levels for each of the different regions of the input space that are generated. Thus, subspaces with homogeneous DMUs (since they must share the characteristics of said subspace) are delimited and the maximun expected output for that subspace is provided. In this way, the `EAT` predictor results in a monotonic increasing frontier with a stepped form where each of these steps corresponds to a node of the tree which contains observations with efficient  and non-efficient output levels.

Although the model training has only been described for a single response variable so far, multiple output scenarios are also available. Additionally, modeling of the response variable(s) can be done using individual CARTs for regression or using Random Forest. On the other hand, the `EAT` object can be plotted as a tree structure to illustrate the relationship between the predictors and the predicted variable and, in the case of a single input and a single output (`y ~ x`), a representation of the frontier is acceptable. Finally, a ranking of variables can be obtained, giving the possibility of making a feature selection.


```r
library(eat)
d <- data("PISAindex")
print(5 + 5)
```

```
## [1] 10
```

```r
print(6 + 6)
```

```
## [1] 12
```

```r
print(1 + 1)
```

```
## [1] 2
```
