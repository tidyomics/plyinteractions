#' plyinteractions: a grammar of data manipulation for genomic interactions
#'
#' plyinteractions is a dplyr-like API to the 
#' GInteractions infrastructure in Bioconductor.
#'
#' plyinteractions provides a consistent interface for importing and
#' wrangling genomic interactions from a variety of sources. The package 
#' defines a grammar of genomic interactions manipulation through a set of 
#' verbs. These verbs can be used to construct human-readable analysis 
#' pipelines based on `GInteractions`.
#' 
#'  - Group genomic interactions with `group_by`; 
#'  - Summarize grouped genomic interactions with `summarize`; 
#'  - Tally/count grouped genomic interactions with `tally` and `count`; 
#'  - Modify genomic interactions with `mutate`; 
#'  - Subset genomic interactions with `filter` using
#'  [`<data-masking>`](https://rlang.r-lib.org/reference/args_data_masking.html) 
#'  and logical expressions; 
#'  - Pick out any columns from the associated metadata with `select` 
#'  using [`<tidy-select>` arguments](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html); 
#'  - Subset using indices with `slice`;
#'  - Order genomic interactions with `arrange` using categorical/numerical 
#'  variables. 
#'
#'    For more details on the features of plyinteractions, read the vignette:
#'    `browseVignettes(package = "plyinteractions")`
"_PACKAGE"
