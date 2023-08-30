#' Internal GInteractions getters
#' 
#' @rdname ginteractions-getters
#' @export
setGeneric("seqnames1", function(x) standardGeneric("seqnames1"))
#' @rdname ginteractions-getters
#' @export
setGeneric("seqnames2", function(x) standardGeneric("seqnames2"))
#' @rdname ginteractions-getters
#' @export
setGeneric("start1", function(x) standardGeneric("start1"))
#' @rdname ginteractions-getters
#' @export
setGeneric("start2", function(x) standardGeneric("start2"))
#' @rdname ginteractions-getters
#' @export
setGeneric("end1", function(x) standardGeneric("end1"))
#' @rdname ginteractions-getters
#' @export
setGeneric("end2", function(x) standardGeneric("end2"))
#' @rdname ginteractions-getters
#' @export
setGeneric("width1", function(x) standardGeneric("width1"))
#' @rdname ginteractions-getters
#' @export
setGeneric("width2", function(x) standardGeneric("width2"))
#' @rdname ginteractions-getters
#' @export
setGeneric("strand1", function(x) standardGeneric("strand1"))
#' @rdname ginteractions-getters
#' @export
setGeneric("strand2", function(x) standardGeneric("strand2"))
#' @rdname ginteractions-getters
#' @export
setMethod("seqnames1", signature("GInteractions"), function(x) {
    GenomeInfoDb::seqnames(S4Vectors::first(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("seqnames2", signature("GInteractions"), function(x) {
    GenomeInfoDb::seqnames(S4Vectors::second(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("start1", signature("GInteractions"), function(x) {
    BiocGenerics::start(S4Vectors::first(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("start2", signature("GInteractions"), function(x) {
    BiocGenerics::start(S4Vectors::second(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("end1", signature("GInteractions"), function(x) {
    BiocGenerics::end(S4Vectors::first(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("end2", signature("GInteractions"), function(x) {
    BiocGenerics::end(S4Vectors::second(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("width1", signature("GInteractions"), function(x) {
    BiocGenerics::width(S4Vectors::first(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("width2", signature("GInteractions"), function(x) {
    BiocGenerics::width(S4Vectors::second(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("strand1", signature("GInteractions"), function(x) {
    BiocGenerics::strand(S4Vectors::first(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("strand2", signature("GInteractions"), function(x) {
    BiocGenerics::strand(S4Vectors::second(x))
})
