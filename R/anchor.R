#' Manage GInteractions anchors with plyranges
#'  
#' @param x A PinnedGInteractions object
#' @return 
#' - `anchor_*` functions return an AnchoredPinnedGInteractions object.
#' - `anchor` returns a character string indicating where the pinned
#' anchors are anchored at. 
#' - `unanchor` removes the anchoring for a AnchoredPinnedGInteractions object.
#' @name plyranges-anchor
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
#' # 1. Anchoring pinned genomic interactions with plyranges
#' ####################################################################
#' 
#' gi |> pin("second") |> anchor_end()
#' 
#' @importFrom plyranges anchor
NULL

.is_pinned <- function(x) {
    if (!is(x, 'PinnedGInteractions')) 
        stop("GInteractions object is not pinned.")
    invisible(pin(x))
}

.is_anchored <- function(gr) {
    if (!is(gr, 'AnchoredPinnedGInteractions')) 
        stop("Pinned interaction anchors are not anchored")
    invisible(gr@anchor)
}

#' @name plyranges-anchor
#' @export
anchor.AnchoredPinnedGInteractions <- function(x) {
    x@anchor
}

#' @name plyranges-anchor
#' @export
unanchor.AnchoredPinnedGInteractions <- function(x) {
    x@delegate
}

#' @name plyranges-anchor
#' @export
anchor_start.PinnedGInteractions <- function(x) {
    new("AnchoredPinnedGInteractions", x, pin = pin(x), anchor = "start")
}

#' @name plyranges-anchor
#' @export
anchor_end.PinnedGInteractions <- function(x) {
    new("AnchoredPinnedGInteractions", x, pin = pin(x), anchor = "end")
}

#' @name plyranges-anchor
#' @export
anchor_center.PinnedGInteractions <- function(x) {
    new("AnchoredPinnedGInteractions", x, pin = pin(x), anchor = "center")
}

#' @name plyranges-anchor
#' @export
anchor_3p.PinnedGInteractions <- function(x) {
    new("AnchoredPinnedGInteractions", x, pin = pin(x), anchor = "3p")
}

#' @name plyranges-anchor
#' @export
anchor_5p.PinnedGInteractions <- function(x) {
    new("AnchoredPinnedGInteractions", x, pin = pin(x), anchor = "5p")
}

#' @name plyranges-anchor
#' @export
anchor_start.AnchoredPinnedGInteractions <- function(x) {
    new("AnchoredPinnedGInteractions", unanchor(x), pin = pin(x), anchor = "start")
}

#' @name plyranges-anchor
#' @export
anchor_end.AnchoredPinnedGInteractions <- function(x) {
    new("AnchoredPinnedGInteractions", unanchor(x), pin = pin(x), anchor = "end")
}

#' @name plyranges-anchor
#' @export
anchor_center.AnchoredPinnedGInteractions <- function(x) {
    new("AnchoredPinnedGInteractions", unanchor(x), pin = pin(x), anchor = "center")
}

#' @name plyranges-anchor
#' @export
anchor_3p.AnchoredPinnedGInteractions <- function(x) {
    new("AnchoredPinnedGInteractions", unanchor(x), pin = pin(x), anchor = "3p")
}

#' @name plyranges-anchor
#' @export
anchor_5p.AnchoredPinnedGInteractions <- function(x) {
    new("AnchoredPinnedGInteractions", unanchor(x), pin = pin(x), anchor = "5p")
}
