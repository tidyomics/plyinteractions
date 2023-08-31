#' Select columns within GInteractions metadata columns
#'
#' @param .data a GInteractions object
#' @param ... Integer indicating rows to keep.
#'
#' @return a GInteractions object.
#'
#' @importFrom tidyselect eval_select
#' 
#' @rdname ginteractions-select
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 1 10 chr1 1 10
#' chr2 1 10 chr2 1 10
#' chr3 1 10 chr3 1 10
#' chr4 1 10 chr4 1 10
#' chr5 1 10 chr5 1 10",
#' col.names = c("seqnames1", "start1", "end1", "seqnames2", "start2", "end2")) |> 
#'   as_ginteractions() |> 
#'   mutate(score = runif(5)*100, cis = TRUE, gc = runif(5))
#'   
#' ####################################################################
#' # 1. Select metadata columns from GInteractions by index
#' ####################################################################
#' 
#' gi |> select(2, 1)
#' gi |> select(-3)
#' 
#' ####################################################################
#' # 2. Select metadata columns from GInteractions by name
#' ####################################################################
#' 
#' gi |> select(gc, score)
#' 
#' ####################################################################
#' # 3. Select metadata columns from GInteractions with <tidy-select>
#' ####################################################################
#' 
#' gi |> select(contains('s'))
#' gi |> select(matches('^s'))
#' 
#' @export
select.GInteractions <- function(.data, ...) {

    # Recover available names from mcols
    available_names <- colnames(S4Vectors::mcols(.data))
    names(available_names) <- available_names

    # Tidy-select over available columns
    pos <- try(
        tidyselect::eval_select(rlang::expr(c(...)), 
        available_names
    ), silent = TRUE)

    # Abort if query not found
    if (is(pos, "try-error")) {
        stop(geterrmessage(), call. = FALSE)
    }

    mcols(.data) <- mcols(.data)[ , pos, drop = FALSE]
    names(mcols(.data)) <- names(pos)
    return(.data)

}
