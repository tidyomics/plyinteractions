#' Internal GInteractions getters
#' 
#' @name ginteractions-getters
#' @param x a GInteractions object
#' @return One of the core GInteractions fields (e.g. seqnames1, start1, ...)
#' @examples 
#' gi <- data.frame(
#'   seqnames1 = 'chr1', start1 = 1, end1 = 10, 
#'   seqnames2 = 'chr1', start2 = 1, end2 = 10
#' ) |> as_ginteractions()
#' seqnames1(gi)
#' seqnames2(gi)
#' start1(gi)
#' start2(gi)
#' end1(gi)
#' end2(gi)
#' width1(gi)
#' width2(gi)
#' strand1(gi)
#' strand2(gi)
NULL

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
#' @importFrom GenomeInfoDb seqnames
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
#' @importFrom BiocGenerics start
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
#' @importFrom BiocGenerics end
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
#' @importFrom BiocGenerics width
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
#' @importFrom BiocGenerics strand
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
