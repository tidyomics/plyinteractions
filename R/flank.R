#' Generate flanking regions from pinned anchors of a GInteractions object 
#' with plyranges
#'  
#' @param x a PinnedGInteractions object
#' @param width The width of the flanking region relative to the ranges in x. 
#' Either an integer vector of length 1 or an integer vector the same length as 
#' x. The width can be negative in which case the flanking region is reversed.
#' @return A PinnedGInteractions object
#' 
#' @name plyranges-flank
#' @rdname plyranges-flank
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
#' # 1. Simple flanking
#' ####################################################################
#' 
#' gi 
#' 
#' gi |> pin_by("first") |> flank_left(-2) 
#' 
#' gi |> pin_by("second") |> flank_upstream(4)
#' 
#' ####################################################################
#' # 2. Chained flanking of each set of anchors
#' ####################################################################
#' 
#' gi |> 
#'   pin_by("first") |> flank_left(2) |> 
#'   pin_by("second") |> flank_right(2)
NULL

#' @importFrom plyranges flank_downstream
#' @importFrom plyranges flank_upstream
#' @importFrom plyranges flank_right
#' @importFrom plyranges flank_left
#' @rdname plyranges-flank
#' @export
flank_downstream <- function(x, width) UseMethod("flank_downstream")
#' @rdname plyranges-flank
#' @export
flank_downstream.Ranges <- function(x, width) plyranges::flank_downstream(x, width)
#' @rdname plyranges-flank
#' @export
flank_downstream.PinnedGInteractions <- function(x, width) {
    pinned <- pinned_anchors(x) 
    flanked_pinned <- flank_downstream(pinned, width) 
    x <- replace_anchors(x, value = flanked_pinned)
    x
}

#' @rdname plyranges-flank
#' @export
flank_upstream <- function(x, width) UseMethod("flank_upstream")
#' @rdname plyranges-flank
#' @export
flank_upstream.Ranges <- function(x, width) plyranges::flank_upstream(x, width)
#' @rdname plyranges-flank
#' @export
flank_upstream.PinnedGInteractions <- function(x, width) {
    pinned <- pinned_anchors(x) 
    flanked_pinned <- flank_upstream(pinned, width) 
    x <- replace_anchors(x, value = flanked_pinned)
    x
}

#' @rdname plyranges-flank
#' @export
flank_right <- function(x, width) UseMethod("flank_right")
#' @rdname plyranges-flank
#' @export
flank_right.Ranges <- function(x, width) plyranges::flank_right(x, width)
#' @rdname plyranges-flank
#' @export
flank_right.PinnedGInteractions <- function(x, width) {
    pinned <- pinned_anchors(x) 
    flanked_pinned <- flank_right(pinned, width) 
    x <- replace_anchors(x, value = flanked_pinned)
    x
}

#' @rdname plyranges-flank
#' @export
flank_left <- function(x, width) UseMethod("flank_left")
#' @rdname plyranges-flank
#' @export
flank_left.Ranges <- function(x, width) plyranges::flank_left(x, width)
#' @rdname plyranges-flank
#' @export
flank_left.PinnedGInteractions <- function(x, width) {
    pinned <- pinned_anchors(x) 
    flanked_pinned <- flank_left(pinned, width) 
    x <- replace_anchors(x, value = flanked_pinned)
    x
}
