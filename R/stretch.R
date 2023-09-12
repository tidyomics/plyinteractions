#' Stretch pinned anchors of a GInteractions object with plyranges
#'  
#' @param x a PinnedGInteractions object
#' @param extend The amount to alter the width of a Ranges object by. Either 
#' an integer vector of length 1 or an integer vector the same length as x.
#' @return A PinnedGInteractions object
#' 
#' @name plyranges-stretch
#' @rdname plyranges-stretch
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
#' # 1. Simple stretching
#' ####################################################################
#' 
#' gi 
#' 
#' gi |> pin_by("first") |> anchor_start() |> stretch(15)
#' 
#' gi |> pin_by("second") |> anchor_center() |> stretch(10)
#' 
#' gi |> pin_by("second") |> anchor_3p() |> stretch(20)
#' 
#' ####################################################################
#' # 2. Chained stretching of each set of anchors
#' ####################################################################
#' 
#' gi |> 
#'   pin_by("first") |> anchor_start() |> stretch(20) |> 
#'   pin_by("second") |> stretch(20)
#' 
#' @importFrom plyranges stretch
#' @export
stretch.AnchoredPinnedGInteractions <- function(x, extend) {
    pinned <- pinned_anchors(x)
    stretched_pinned <- stretch(pinned, extend) 
    x <- replace_anchors(x, value = stretched_pinned)
    unanchor(x)
}

#' @rdname plyranges-stretch
#' @export
stretch.PinnedGInteractions <- function(x, extend) {
    pinned <- pinned_anchors(x) 
    stretched_pinned <- stretch(pinned, extend) 
    x <- replace_anchors(x, value = stretched_pinned)
    x
}
