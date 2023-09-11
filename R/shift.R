#' Shift pinned anchors of a GInteractions object with plyranges
#'  
#' @param x a PinnedGInteractions object
#' @param shift The amount to move the genomic interval in the Ranges object 
#' by. Either a non-negative integer vector of length 1 or an integer vector 
#' the same length as x.
#' @return A PinnedGInteractions object
#' 
#' @name plyranges-shift
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
#' # 1. Simple shifting
#' ####################################################################
#' 
#' gi 
#' 
#' gi |> pin_by("first") |> shift_left(15)
#' 
#' gi |> pin_by("second") |> shift_downstream(10)
#' 
#' ####################################################################
#' # 2. Chained shifting of each set of anchors
#' ####################################################################
#' 
#' gi |> 
#'   pin_by("first") |> shift_downstream(20) |> 
#'   pin_by("second") |> shift_upstream(20)
NULL

#' @importFrom plyranges shift_downstream
#' @importFrom plyranges shift_upstream
#' @importFrom plyranges shift_right
#' @importFrom plyranges shift_left
#' @rdname plyranges-shift
#' @export
shift_downstream <- function(x, shift) UseMethod("shift_downstream")
#' @rdname plyranges-shift
#' @export
shift_downstream.Ranges <- function(x, shift) plyranges::shift_downstream(x, shift)
#' @rdname plyranges-shift
#' @export
shift_downstream.PinnedGInteractions <- function(x, shift) {
    pinned <- pinned_anchors(x) 
    shifted_pinned <- shift_downstream(pinned, shift) 
    x <- replace_anchors(x, value = shifted_pinned)
    x
}

#' @rdname plyranges-shift
#' @export
shift_upstream <- function(x, shift) UseMethod("shift_upstream")
#' @rdname plyranges-shift
#' @export
shift_upstream.Ranges <- function(x, shift) plyranges::shift_upstream(x, shift)
#' @rdname plyranges-shift
#' @export
shift_upstream.PinnedGInteractions <- function(x, shift) {
    pinned <- pinned_anchors(x) 
    shifted_pinned <- shift_upstream(pinned, shift) 
    x <- replace_anchors(x, value = shifted_pinned)
    x
}

#' @rdname plyranges-shift
#' @export
shift_right <- function(x, shift) UseMethod("shift_right")
#' @rdname plyranges-shift
#' @export
shift_right.Ranges <- function(x, shift) plyranges::shift_right(x, shift)
#' @rdname plyranges-shift
#' @export
shift_right.PinnedGInteractions <- function(x, shift) {
    pinned <- pinned_anchors(x) 
    shifted_pinned <- shift_right(pinned, shift) 
    x <- replace_anchors(x, value = shifted_pinned)
    x
}

#' @rdname plyranges-shift
#' @export
shift_left <- function(x, shift) UseMethod("shift_left")
#' @rdname plyranges-shift
#' @export
shift_left.Ranges <- function(x, shift) plyranges::shift_left(x, shift)
#' @rdname plyranges-shift
#' @export
shift_left.PinnedGInteractions <- function(x, shift) {
    pinned <- pinned_anchors(x) 
    shifted_pinned <- shift_left(pinned, shift) 
    x <- replace_anchors(x, value = shifted_pinned)
    x
}
