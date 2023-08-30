#' Internal GInteractions setters
#' 
#' @rdname ginteractions-setters
#' @importFrom S4Vectors first<-
setReplaceMethod("first", "GInteractions", function(x, value) {
    out <- InteractionSet::GInteractions(
        value, 
        InteractionSet::anchors(x, "second")
    )
    mcols(out) <- mcols(x)
    out
})

#' @rdname ginteractions-setters
#' @importFrom S4Vectors second<-
setReplaceMethod("second", "GInteractions", function(x, value) {
    out <- InteractionSet::GInteractions(
        InteractionSet::anchors(x, "first"),
        value
    )
    mcols(out) <- mcols(x)
    out
})

#' @rdname ginteractions-setters
#' @export
setGeneric("set_seqnames1", function(x, value) standardGeneric("set_seqnames1"))
#' @rdname ginteractions-setters
#' @export
setGeneric("set_seqnames2", function(x, value) standardGeneric("set_seqnames2"))
#' @rdname ginteractions-setters
#' @export
setGeneric("set_start1", function(x, value) standardGeneric("set_start1"))
#' @rdname ginteractions-setters
#' @export
setGeneric("set_start2", function(x, value) standardGeneric("set_start2"))
#' @rdname ginteractions-setters
#' @export
setGeneric("set_end1", function(x, value) standardGeneric("set_end1"))
#' @rdname ginteractions-setters
#' @export
setGeneric("set_end2", function(x, value) standardGeneric("set_end2"))
#' @rdname ginteractions-setters
#' @export
setGeneric("set_width1", function(x, value) standardGeneric("set_width1"))
#' @rdname ginteractions-setters
#' @export
setGeneric("set_width2", function(x, value) standardGeneric("set_width2"))
#' @rdname ginteractions-setters
#' @export
setGeneric("set_strand1", function(x, value) standardGeneric("set_strand1"))
#' @rdname ginteractions-setters
#' @export
setGeneric("set_strand2", function(x, value) standardGeneric("set_strand2"))
#' @rdname ginteractions-setters
#' @export
setMethod("set_seqnames1", signature("GInteractions", "factor"), function(x, value) {
    GenomeInfoDb::seqnames(S4Vectors::first(x)) <- value
    x
})
#' @rdname ginteractions-setters
#' @export
setMethod("set_seqnames2", signature("GInteractions", "factor"), function(x, value) {
    GenomeInfoDb::seqnames(S4Vectors::second(x)) <- value
    x
})
#' @rdname ginteractions-setters
#' @export
setMethod("set_start1", signature("GInteractions", "numeric"), function(x, value) {
    BiocGenerics::start(S4Vectors::first(x)) <- value
    x
})
#' @rdname ginteractions-setters
#' @export
setMethod("set_start2", signature("GInteractions", "numeric"), function(x, value) {
    BiocGenerics::start(S4Vectors::second(x)) <- value
    x
})
#' @rdname ginteractions-setters
#' @export
setMethod("set_end1", signature("GInteractions", "numeric"), function(x, value) {
    BiocGenerics::end(S4Vectors::first(x)) <- value
    x
})
#' @rdname ginteractions-setters
#' @export
setMethod("set_end2", signature("GInteractions", "numeric"), function(x, value) {
    BiocGenerics::end(S4Vectors::second(x)) <- value
    x
})
#' @rdname ginteractions-setters
#' @export
setMethod("set_width1", signature("GInteractions", "numeric"), function(x, value) {
    BiocGenerics::width(S4Vectors::first(x)) <- value
    x
})
#' @rdname ginteractions-setters
#' @export
setMethod("set_width2", signature("GInteractions", "numeric"), function(x, value) {
    BiocGenerics::width(S4Vectors::second(x)) <- value
    x
})
#' @rdname ginteractions-setters
#' @export
setMethod("set_strand1", signature("GInteractions", "character"), function(x, value) {
    BiocGenerics::strand(S4Vectors::first(x)) <- value
    x
})
#' @rdname ginteractions-setters
#' @export
setMethod("set_strand2", signature("GInteractions", "character"), function(x, value) {
    BiocGenerics::strand(S4Vectors::second(x)) <- value
    x
})
