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
#'  - Group genomic interactions with \code{\link[=dplyr-group_by]{group_by}}; 
#'  - Summarize grouped genomic interactions with \code{\link[=dplyr-summarize]{summarize}}; 
#'  - Tally/count grouped genomic interactions with \code{\link[=dplyr-count]{tally}} and \code{\link[=dplyr-count]{count}}; 
#'  - Modify genomic interactions with \code{\link[=dplyr-mutate]{mutate}}; 
#'  - Subset genomic interactions with \code{\link[=dplyr-filter]{filter}} using
#'  [`<data-masking>`](https://rlang.r-lib.org/reference/args_data_masking.html) 
#'  and logical expressions; 
#'  - Pick out any columns from the associated metadata with \code{\link[=dplyr-select]{select}}
#'  using [`<tidy-select>` arguments](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html); 
#'  - Subset using indices with \code{\link[=dplyr-slice]{slice}};
#'  - Order genomic interactions with \code{\link[=dplyr-arrange]{arrange}} using categorical/numerical 
#'  variables. 
#'
#'    For more details on the features of plyinteractions, read the vignette:
#'    `browseVignettes(package = "plyinteractions")`
"_PACKAGE"
