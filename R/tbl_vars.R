#' @importFrom dplyr tbl_vars
#' @export
tbl_vars.GInteractions <- function(x) c(
    "seqnames1", "start1", "end1", "width1", "strand1", 
    "seqnames2", "start2", "end2", "width2", "strand2", 
    names(mcols(x))
)
