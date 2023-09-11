#' Pin GInteractions by anchors set (anchors1 or anchors2). 
#'
#' @param x a GInteractions object
#' @param anchors Anchors to pin on ("first" or "second")
#' 
#' @return a PinnedGInteractions object.
#'
#' @rdname ginteractions-pin
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 11 20 chr1 21 30
#' chr1 11 20 chr1 51 55
#' chr1 11 30 chr1 51 55
#' chr1 11 30 chr2 51 60",
#' col.names = c("seqnames1", "start1", "end1", "seqnames2", "start2", "end2")) |> 
#'   as_ginteractions() |> 
#'   mutate(type = c('cis', 'cis', 'cis', 'trans'), score = runif(4))
#' 
#' ####################################################################
#' # 1. Pin and modify anchors
#' ####################################################################
#' 
#' gi |> pin("first")
#' 
#' gi |> pin_by("first")
#' 
#' gi |> pin_first()
#' 
#' gi |> pin_anchors1()
#' 
#' ####################################################################
#' # 2. Pin by second anchors
#' ####################################################################
#' 
#' gi |> pin("second")
#' 
#' gi |> pin_by("second")
#' 
#' gi |> pin_second()
#' 
#' gi |> pin_anchors2()
#' 
#' ####################################################################
#' # 3. Unpin
#' ####################################################################
#' 
#' gi |> pin("second") |> unpin()
#' 
#' @export
setGeneric("pin", function(x, anchors) standardGeneric("pin"))

#' @rdname ginteractions-pin
#' @export
setMethod("pin", signature(x = "GroupedGInteractions", anchors = "character"), function(x, anchors) {
    methods::new("PinnedGInteractions", ungroup(x), anchors)
})

#' @rdname ginteractions-pin
#' @export
setMethod("pin", signature(x = "GroupedGInteractions", anchors = "numeric"), function(x, anchors) {
    methods::new("PinnedGInteractions", ungroup(x), as.character(anchors))
})

#' @rdname ginteractions-pin
#' @export
setMethod("pin", signature(x = "GInteractions", anchors = "character"), function(x, anchors) {
    methods::new("PinnedGInteractions", x, anchors)
})

#' @rdname ginteractions-pin
#' @export
setMethod("pin", signature(x = "GInteractions", anchors = "numeric"), function(x, anchors) {
    if (!anchors %in% c(1, 2)) stop("`anchors` can only be set to `1` or `2`")
    methods::new("PinnedGInteractions", x, as.character(anchors))
})

#' @rdname ginteractions-pin
#' @export
setMethod("pin", signature(x = "PinnedGInteractions", anchors = "missing"), function(x, anchors) {
    x@pin
})

#' @rdname ginteractions-pin
#' @export
setMethod("pin", signature(x = "PinnedGInteractions", anchors = "character"), function(x, anchors) {
    methods::new("PinnedGInteractions", unpin(x), anchors)
})

#' @rdname ginteractions-pin
#' @export
setMethod("pin", signature(x = "PinnedGInteractions", anchors = "numeric"), function(x, anchors) {
    if (!anchors %in% c(1, 2)) stop("`anchors` can only be set to `1` or `2`")
    methods::new("PinnedGInteractions", unpin(x), as.character(anchors))
})

#' @rdname ginteractions-pin
#' @export
setMethod("pin", signature(x = "AnchoredPinnedGInteractions", anchors = "character"), function(x, anchors) {
    methods::new("PinnedGInteractions", unpin(unanchor(x)), anchors)
})

#' @rdname ginteractions-pin
#' @export
setMethod("pin", signature(x = "AnchoredPinnedGInteractions", anchors = "numeric"), function(x, anchors) {
    methods::new("PinnedGInteractions", unpin(unanchor(x)), as.character(anchors))
})


#' @export
#' @rdname ginteractions-pin
setGeneric("pin_by", function(x, anchors) pin(x, anchors))

#' @rdname ginteractions-pin
#' @export
pin_first <- function(x) pin(x, 1)

#' @rdname ginteractions-pin
#' @export
pin_second <- function(x) pin(x, 2)

#' @rdname ginteractions-pin
#' @export
pin_anchors1 <- function(x) pin(x, 1)

#' @rdname ginteractions-pin
#' @export
pin_anchors2 <- function(x) pin(x, 2)

#' @rdname ginteractions-pin
#' @export
setGeneric("unpin", function(x) standardGeneric("unpin"))

#' @rdname ginteractions-pin
#' @export
setMethod("unpin", signature(x = "AnchoredPinnedGInteractions"), function(x) {
    unpin(unanchor(x))
})

#' @rdname ginteractions-pin
#' @export
setMethod("unpin", signature(x = "PinnedGInteractions"), function(x) {
    x@delegate
})

#' @rdname ginteractions-pin
#' @export
setMethod("unpin", signature(x = "GInteractions"), function(x) {
    x
})

#' @rdname ginteractions-pin
#' @export
setGeneric("pinned_anchors", function(x) standardGeneric("pinned_anchors"))

#' @rdname ginteractions-pin
#' @export
setMethod("pinned_anchors", signature(x = "PinnedGInteractions"), function(x) {
    id <- switch(pin(x), "1" = "first", "2" = "second")
    InteractionSet::anchors(unpin(x), type = id)
})

#' @rdname ginteractions-pin
#' @export
setMethod("pinned_anchors", signature(x = "AnchoredPinnedGInteractions"), function(x) {
    pinned_anchors(unanchor(x))
})
