#' Subset a GInteractions with tidyverse-like `filter`
#'
#' @param .data a GInteractions object
#' @param ... <data-masking> Expressions that return a logical value, 
#' and are defined in terms of the variables in .data. If multiple 
#' expressions are included, they are combined with the & operator. 
#' Only rows for which all conditions evaluate to TRUE are kept.
#'
#' @return a GInteractions object.
#'
#' @importFrom dplyr filter
#' 
#' @rdname ginteractions-filter
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 1 10 chr1 1 10
#' chr1 2 10 chr2 1 10
#' chr3 3 10 chr3 1 10
#' chr4 4 10 chr4 1 10
#' chr5 5 10 chr5 1 10",
#' col.names = c("seqnames1", "start1", "end1", "seqnames2", "start2", "end2")) |> 
#'   as_ginteractions() |> 
#'   mutate(cis = seqnames1 == seqnames2, score = runif(5)*100, gc = runif(5))
#' gi
#' 
#' ####################################################################
#' # 1. Filter metadata columns from GInteractions by condition
#' ####################################################################
#' 
#' gi |> filter(gc > 0.1)
#' gi |> filter(gc > 0.1, score > 50)
#' gi |> filter(cis)
#' 
#' ####################################################################
#' # 2. On-the-fly calculations
#' ####################################################################
#' 
#' gi
#' gi |> filter(start1 >= start2 + 3)
#' gi |> filter(score * gc > score * 0.5)
#' 
#' @export
filter.GInteractions <- function(.data, ...) {

    quosures <- rlang::enquos(...)

    ## Put each quosure in an environment with the required generic 
    scoped_quosures <- .scope_quos(quosures) 

    ## tidyeval quosures in a scoped env. This takes care of all the tidy eval. 
    overscope <- .overscope_ginteractions(.data)
    evaled_quosures <- vector("list", length(scoped_quosures))
    for (i in seq_along(scoped_quosures)) {
        quo <- scoped_quosures[[i]]
        evaled_quosures[[i]] <- eval_tidy(quo, data = overscope)
    }
    sub <- Reduce(`&`, evaled_quosures)
    sub <- sub & !is.na(sub)

    ## Filter appropriate rows
    .data <- .data[sub]

    .data

}
