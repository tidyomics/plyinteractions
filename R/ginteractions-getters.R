#' Internal GInteractions getters
#' 
#' @name ginteractions-getters
#' @param x a GInteractions object
#' @return One of the core GInteractions fields (e.g. seqnames1, start1, ...)
NULL

#' @rdname ginteractions-getters
setGeneric("seqnames1", function(x) standardGeneric("seqnames1"))
#' @rdname ginteractions-getters
setGeneric("seqnames2", function(x) standardGeneric("seqnames2"))
#' @rdname ginteractions-getters
setGeneric("start1", function(x) standardGeneric("start1"))
#' @rdname ginteractions-getters
setGeneric("start2", function(x) standardGeneric("start2"))
#' @rdname ginteractions-getters
setGeneric("end1", function(x) standardGeneric("end1"))
#' @rdname ginteractions-getters
setGeneric("end2", function(x) standardGeneric("end2"))
#' @rdname ginteractions-getters
setGeneric("width1", function(x) standardGeneric("width1"))
#' @rdname ginteractions-getters
setGeneric("width2", function(x) standardGeneric("width2"))
#' @rdname ginteractions-getters
setGeneric("strand1", function(x) standardGeneric("strand1"))
#' @rdname ginteractions-getters
setGeneric("strand2", function(x) standardGeneric("strand2"))
#' @importFrom GenomeInfoDb seqnames
#' @rdname ginteractions-getters
setMethod("seqnames1", signature("GInteractions"), function(x) {
    GenomeInfoDb::seqnames(S4Vectors::first(x))
})
#' @rdname ginteractions-getters
setMethod("seqnames2", signature("GInteractions"), function(x) {
    GenomeInfoDb::seqnames(S4Vectors::second(x))
})
#' @importFrom BiocGenerics start
#' @rdname ginteractions-getters
setMethod("start1", signature("GInteractions"), function(x) {
    BiocGenerics::start(S4Vectors::first(x))
})
#' @rdname ginteractions-getters
setMethod("start2", signature("GInteractions"), function(x) {
    BiocGenerics::start(S4Vectors::second(x))
})
#' @importFrom BiocGenerics end
#' @rdname ginteractions-getters
setMethod("end1", signature("GInteractions"), function(x) {
    BiocGenerics::end(S4Vectors::first(x))
})
#' @rdname ginteractions-getters
setMethod("end2", signature("GInteractions"), function(x) {
    BiocGenerics::end(S4Vectors::second(x))
})
#' @importFrom BiocGenerics width
#' @rdname ginteractions-getters
setMethod("width1", signature("GInteractions"), function(x) {
    BiocGenerics::width(S4Vectors::first(x))
})
#' @rdname ginteractions-getters
setMethod("width2", signature("GInteractions"), function(x) {
    BiocGenerics::width(S4Vectors::second(x))
})
#' @importFrom BiocGenerics strand
#' @rdname ginteractions-getters
setMethod("strand1", signature("GInteractions"), function(x) {
    BiocGenerics::strand(S4Vectors::first(x))
})
#' @rdname ginteractions-getters
setMethod("strand2", signature("GInteractions"), function(x) {
    BiocGenerics::strand(S4Vectors::second(x))
})
