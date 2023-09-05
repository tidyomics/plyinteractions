#' Rename columns from a GInteractions with tidyverse-like `rename`
#'  
#' @name ginteractions-rename
#' @rdname ginteractions-rename
#' 
#' @importFrom tidyselect eval_rename
#' 
#' @param .data a GInteractions object
#' @param ... <tidy-select> Use `new_name = old_name` to 
#' rename selected variables.
#'
#' @return a GInteractions object.
#'
#' @examples
#' gi <- read.table(text = "
#' chr1 10 20 chr1 50 51
#' chr1 10 50 chr2 30 40",
#' col.names = c("chr1", "start1", "end1", "chr2", "start2", "end2")) |> 
#'   as_ginteractions(seqnames1 = chr1, seqnames2 = chr2) |> 
#'   mutate(type = c('cis', 'trans'), score = runif(2))
#'   
#' ####################################################################
#' # 1. Rename metadata columns to a GInteractions object
#' ####################################################################
#' 
#' gi |> rename(interaction_type = type, GC = score)
#' 
#' @export
rename.GInteractions <- function(.data, ...) {

    ## Check that old names exist in mcols
    quosures <- rlang::enquos(..., .named = FALSE)
    old_colnames <- lapply(quosures, rlang::get_expr) |> unlist() 
    if (any(old_colnames %in% c(
        "seqnames1", "start1", "end1", "width1", "strand1", 
        "seqnames2", "start2", "end2", "width2", "strand2"
    ))) {
        stop("Core GInteractions columns cannot be renamed.", call. = FALSE)
    }
    if (!all(old_colnames %in% colnames(mcols(.data)))) {
        stop("Old column names not found. Use `new_name = old_name` syntax.", call. = FALSE)
    }

    ## Rename with `eval_rename`
    loc <- tidyselect::eval_rename(
        rlang::expr(c(...)), as.data.frame(mcols(.data))
    )
    names <- colnames(mcols(.data))
    names[loc] <- names(loc)
    colnames(mcols(.data)) <- names
    
    .data

}
