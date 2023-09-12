#' Summarize GInteractions per group
#'  
#' @name dplyr-summarize
#' @aliases dplyr-summarise
#' @rdname dplyr-summarize
#'
#' @param .data a (grouped) GInteractions object
#' @param ... <data-masking> Name-value pairs of summary functions. 
#' The name will be the name of the variable in the result.
#'
#' @return a \code{S4Vectors::\link[S4Vectors:DataFrame-class]{DataFrame()}}
#' object: 
#' 
#' - The rows come from the underlying `group_keys()`.
#' - The columns are a combination of the grouping keys and the summary 
#' expressions that you provide.
#' - GInteractions class is **not** preserved, as a call to `summarize` 
#' fundamentally creates a new data frame
#' 
#' @importFrom S4Vectors rbind cbind
#' @importFrom dplyr summarise 
#' @importFrom dplyr summarize 
#' @importFrom rlang !!! enquos
#' @importFrom dplyr bind_cols bind_rows
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 11 20 chr1 21 30 + +
#' chr1 11 20 chr1 51 55 + +
#' chr1 11 30 chr1 51 55 - -
#' chr1 11 30 chr2 51 60 - -",
#' col.names = c(
#'   "seqnames1", "start1", "end1", 
#'   "seqnames2", "start2", "end2", "strand1", "strand2")
#' ) |> 
#'   as_ginteractions() |> 
#'   mutate(score = runif(4), type = c('cis', 'cis', 'cis', 'trans'))
#' 
#' ####################################################################
#' # 1. Summarize a single column
#' ####################################################################
#' 
#' gi
#' 
#' gi |> group_by(type) |> summarize(m = mean(score))
#' 
#' gi |> group_by(strand1) |> summarize(m = mean(score))
#' 
#' df <- gi |> 
#'   group_by(strand1) |> 
#'   summarize(m = mean(score), n = table(seqnames2))
#' df
#' 
#' df$n
#' 
#' ####################################################################
#' # 2. Summarize by multiple columns
#' ####################################################################
#' 
#' gi |> 
#'   group_by(strand1, seqnames2) |> 
#'   summarise(m = mean(score), n = table(type))
#' 
#' @export
summarise.GroupedGInteractions <- function(.data, ...) {

    quosures <- rlang::enquos(..., .named = TRUE)

    ## Put each quosure in an environment with the required generic 
    scoped_quosures <- .scope_quos(quosures) 
    names(scoped_quosures) <- names(quosures)

    ## tidyeval quosures in a scoped env. This takes care of all the tidy eval. 
    overscope <- .overscope_groupedginteractions(.data)
    evaled_quosures <- vector("list", length(scoped_quosures))
    names(evaled_quosures) <- names(scoped_quosures)
    for (i in seq_along(scoped_quosures)) {
        quo <- scoped_quosures[[i]]
        evaled_quosures[[i]] <- eval_tidy(quo, data = overscope)
    }

    ## If evaluated quosures are lists, make sure to keep them
    is_list <- vapply(
        evaled_quosures, function(.) is(., "List") || is(., "list"), logical(1)
    )
    if (any(is_list)) {
        nr <- .data@n
        for (i in which(is_list)) {
            ## If scalar, repeat the scalar as many times as the number of groups
            if (length(evaled_quosures[[i]]) == 1) {
                evaled_quosures[[i]] <- as(
                    rep(evaled_quosures[[i]], nr), "CompressedList"
                )
            ## Otherwise, 
            } else {
                if (all(lengths(evaled_quosures[[i]]) == 
                    length(evaled_quosures[[i]][[1]]))
                ) {
                    stopifnot(length(evaled_quosures[[i]]) == nr)
                    evaled_quosures[[i]] <- as(
                        BiocGenerics::Reduce(
                            S4Vectors::pc, evaled_quosures[[i]]
                        ), 
                        "CompressedList"
                    )
                }
            }
        } 
    }

    ## If evaluated quosures are table, convert to lists
    is_table <- vapply(
        evaled_quosures,
        function(.) is(., "table"), 
        logical(1)
    )
    if (any(is_table)) {
        nr <- .data@n
        for (i in which(is_table)) {
            stopifnot(nrow(evaled_quosures[[i]]) == nr)
            evaled_quosures[[i]] <- apply(
                evaled_quosures[[i]], 1, function(x) list(x)[[1]], 
                simplify = FALSE
            ) |> as("CompressedList")
        } 
    }

    ## Aggregate results
    summarized_df <- DataFrame(evaled_quosures)
    rownames(summarized_df) <- NULL
    res <- cbind(
        group_keys(.data), 
        summarized_df
    )
    res[order(res[, group_vars(.data)]), ]

}

#' @rdname dplyr-summarize
#' @export
summarize.GroupedGInteractions <- summarise.GroupedGInteractions
