#' Slice a GInteractions rows by their index
#'
#' @param .data a GInteractions object
#' @param ... Integer indicating rows to keep.
#'
#' @importFrom rlang enquos
#' 
#' @return a GInteractions object.
#' 
#' @name dplyr-slice
#' @rdname dplyr-slice
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 1 10 chr1 1 10
#' chr2 1 10 chr2 1 10
#' chr3 1 10 chr3 1 10
#' chr4 1 10 chr4 1 10
#' chr5 1 10 chr5 1 10",
#' col.names = c(
#'     "seqnames1", "start1", "end1", 
#'     "seqnames2", "start2", "end2")
#' ) |> 
#'   as_ginteractions()
#'   
#' ####################################################################
#' # 1. Slice a GInteractions
#' ####################################################################
#' 
#' gi |> slice(1, 2, 3)
#' gi |> slice(-3)
#' gi |> slice(1:2, 5:4)
#' @export
slice.GInteractions <- function(.data, ...) {

    quosures <- rlang::quos(..., .ignore_empty = "all") 
    
    # Check that provided quosures are all numeric
    .check_indices(quosures)

    # Parse all indices
    idx <- lapply(quosures, rlang::eval_tidy) |> Reduce(f = union, x = _)

    # Slice rows
    return(.data[idx])

}
