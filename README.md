
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adeckstats

<!-- badges: start -->
<!-- badges: end -->

The goal of adeckstats is to …

## Installation

You can install the development version of adeckstats like so:

``` r
devtools::install_github('norepoforyousucker')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(adeckstats)
adeckstats::adeck_binarysearch(c(1,2,3),2)
#> The searched key 2 is found in index 2 of the target vector
#> [1] 2
```

—- $SPECIAL TOPICS$ ——

A look at special topics I am learning and using to develop this
package. This is the ultimate intersection of learning and development.

—- $ALGORITHMS$ ——

## Search Algorithms

The aim is to do one a day and master algorithms, starting with search
algorithms.

### Binary Search Algorithm

``` r
adeckstats::adeck_binarysearch(c(1,2,3,4,5), 1)
#> The searched key 1 is found in index 1 of the target vector
#> [1] 1
```

Time and Space Complexity

Time Complexity:

Drivers are Input size (n), Basic operations (e.g. comparisons in a
sorting algorithm), and Asymptotic analysis (behavior of the algorithm
as n approaches infinity).

Calculating Time complexity involves:

- Identify n
- count the basic operations
- combine operations (add for sequential ops and product for nest ops)
- Use asymptotic notation focusing on the term that grows fastest and n
  increases

Space Complexity:

The binary search algorithms halves the search space and this algorithm
really sank in when i understood this benefit.
