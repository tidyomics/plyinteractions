# plyinteractions <img src="man/figures/logo.png" align="right" alt="" width="180" />

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/js2264/plyinteractions/branch/devel/graph/badge.svg)](https://app.codecov.io/gh/js2264/plyinteractions?branch=devel)
<!-- badges: end -->

`plyinteractions` provides a consistent interface for importing genomic 
interactions from `pairs` and `bedpe` files into `GInteractions` objects 
in `R` and for manipulating them using a tidy grammar. 

[`plyranges`](https://www.bioconductor.org/packages/release/bioc/html/plyranges.html)
operates on genomic ranges (e.g. implemented as `GRanges` 
objects in `Bioconductor`) and introduces a tidy grammar for manipulating 
them. Genomic interactions (implemented as `GInteractions` 
objects in `Bioconductor`) are more complex than genomic ranges in that each 
observation (row) corresponds to *a pair of two genomic ranges*, each one 
with its own metadata. `plyinteractions` extends `plyranges` syntax to 
manipulate genomic interactions in `R` using `dplyr` verbs and tidy operations. 

The grammar of tidy genomic data transformation defined in 
[`plyranges`](https://www.bioconductor.org/packages/release/bioc/html/plyranges.html) 
and available for `GInteractions` currently supports: 

- `dplyr` verbs (for `GInteractions` and `GroupedGInteractions`): 

  - Group genomic interactions with `group_by`; 
  - Summarize grouped genomic interactions with `summarize`; 
  - Tally/count grouped genomic interactions with `tally` and `count`; 
  - Modify genomic interactions with `mutate`; 
  - Subset genomic interactions with `filter` using
  [`<data-masking>`](https://rlang.r-lib.org/reference/args_data_masking.html) 
  and logical expressions; 
  - Pick out any columns from the associated metadata with `select` 
  using [`<tidy-select>` arguments](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html); 
  - Subset using indices with `slice`;
  - Order genomic interactions with `arrange` using categorical/numerical 
  variables. 


- `plyranges` verbs (for `PinnedGInteractions` and `AnchoredPinnedGInteractions`): 

  - Stretch specific anchors of genomic interactions to a given width with `stretch`;
  - `anchor_*` functions to control how stretching is performed; 
  - Shift specific anchors of genomic interactions with `shift`;
  - Obtain flanking `GRanges` from specific anchors of genomic interactions with `flank`.

***Note:*** In the genomic interaction field, the *"anchor"* term typically 
refers to the two genomic loci brought together into an *interaction*. In 
`plyranges`, the term `anchor` is used to specify which "part" of a genomic 
locus is fixed (e.g. "5p", 3p", "center") and, incidently, which one can be modified
by `plyranges` verbs.  

For more details on `GInteractions` "anchors" vs. `plyranges` `anchor`ing 
system, read 
[this section]([#pinned-and-anchored-ginteractions](https://tidyomics.github.io/plyinteractions/articles/plyinteractions.html#pinned-and-anchored-ginteractions)) from our vignette. 

- Overlapping operations (for `GInteractions` and `PinnedGInteractions`): 

  - `find_overlaps`
  - `count_overlaps`
  - `filter_by_overlaps` and `filter_by_non_overlaps`
  - `join_overlap_left`


## Installation

`plyinteractions` can be currently be installed from GitHub:

```r
BiocManager::install("tidyomics/plyinteractions")
```

## Using `plyinteractions`

Read `vignette("plyinteractions")` for more details. 

## Code of Conduct

Please note that this project is released with a
[Contributor Code of
Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Acknowledgments 

`plyinteractions` package is heavily based on `plyranges`. It adapts a 
number of functions and methods defined in that package, and would not have 
been developed without the seminal work from Stuart Lee, Dianne Cook and Michael 
Lawrence: 

- Lee, Stuart, Dianne Cook, and Michael Lawrence. 2019. “Plyranges: A Grammar of Genomic Data Transformation.” Genome Biology 20 (1): 4. https://doi.org/10.1186/s13059-018-1597-8.

This package is largely inspired by the `tidyverse`:

- Wickham H, François R, Henry L, Müller K, Vaughan D (2023). _dplyr: A Grammar of Data Manipulation_. R package version 1.1.2, <https://CRAN.R-project.org/package=dplyr>.
- Henry L, Wickham H (2023). _rlang: Functions for Base Types and Core R and 'Tidyverse' Features_. R package version 1.1.1, <https://CRAN.R-project.org/package=rlang>.
