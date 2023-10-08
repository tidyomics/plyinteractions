#' GInteractions grouping metadata
#'
#' @name group-group_data
#' @rdname group-group_data
#' 
#' @param .data,.tbl,x a GInteractions object
#' @param ... Ignored.
#' 
#' @return a GInteractions object.
#'
#' @importFrom tidyselect eval_select
#' @importFrom rlang syms
#' @importFrom methods new
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 11 20 chr1 21 30
#' chr1 11 20 chr1 51 55
#' chr1 11 30 chr1 51 55
#' chr1 11 30 chr2 51 60",
#' col.names = c(
#'     "seqnames1", "start1", "end1", 
#'     "seqnames2", "start2", "end2")
#' ) |> 
#'   as_ginteractions() |> 
#'   mutate(type = c('cis', 'cis', 'cis', 'trans'), score = runif(4))
#' 
#' ggi <- gi |> group_by(end1)
#' ggi
#' group_data(ggi)
#' group_keys(ggi)
#' group_rows(ggi)
#' group_indices(ggi)
#' group_vars(ggi)
#' groups(ggi)
#' group_size(ggi)
#' n_groups(ggi)
#' @include tbl_vars.R
NULL

#' @importFrom dplyr group_data
#' @importFrom methods as
#' @importFrom dplyr group_data
#' @rdname group-group_data
#' @export
group_data.GroupedGInteractions <- function(.data) {
    S4Vectors::DataFrame(
        .data@group_keys, 
        .rows = methods::as(unname(S4Vectors::split(
            seq_len(length(.data@delegate)),
            .data@group_indices
        )), "IntegerList")
    )
}

#' @importFrom dplyr group_keys
#' @rdname group-group_data
#' @export
group_keys.GroupedGInteractions <- function(.tbl, ...) .tbl@group_keys 

#' @importFrom dplyr group_indices
#' @rdname group-group_data
#' @export
group_indices.GroupedGInteractions <- function(.data, ...) {
    .data@group_indices
}

#' @importFrom dplyr group_vars
#' @rdname group-group_data
#' @export
group_vars.GInteractions <- function(x) character(0)
#' @rdname group-group_data
#' @export
group_vars.GroupedGInteractions <- function(x) colnames(x@group_keys) 

#' @importFrom dplyr groups
#' @rdname group-group_data
#' @export
groups.GroupedGInteractions <- function(x) syms(colnames(x@group_keys))

#' @importFrom dplyr group_size
#' @rdname group-group_data
#' @export
group_size.GroupedGInteractions <- function(x) lengths(group_rows(x))

#' @importFrom dplyr n_groups
#' @rdname group-group_data
#' @export
n_groups.GroupedGInteractions <- function(x) nrow(group_data(x))
