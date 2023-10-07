#' Enhanced GInteractions getters
#' 
#' @name ginteractions-getters
#' @param x a GInteractions object
#' @param name The pattern or name of a column stored in 
#' the GInteractions metadata (mcols). 
#' @return One of the core GInteractions fields (e.g. seqnames1, start1, ...)
#' or one of the metadata columns when using `$`. 
#' Note that auto-completion works with `$`. 
#' @examples 
#' gi <- data.frame(
#'   seqnames1 = 'chr1', start1 = 1, end1 = 10, 
#'   seqnames2 = 'chr1', start2 = 2, end2 = 20
#' ) |> as_ginteractions() |> mutate(type = 'cis')
#' anchors1(gi)
#' anchors2(gi)
#' seqnames1(gi)
#' seqnames2(gi)
#' start1(gi)
#' start2(gi)
#' end1(gi)
#' end2(gi)
#' width1(gi)
#' width2(gi)
#' ranges1(gi)
#' ranges2(gi)
#' strand1(gi)
#' strand2(gi)
#' gi$type
NULL

#' @importFrom utils .DollarNames
.DollarNames.GInteractions <- function(x, pattern = "") 
    grep(pattern, names(mcols(x, use.names = FALSE)), value = TRUE)

#' @rdname ginteractions-getters
#' @export 
setMethod("$", "GInteractions",
    function(x, name) mcols(x, use.names=FALSE)[[name]]
)

#' @rdname ginteractions-getters
#' @export
setMethod("anchors1", signature("GInteractions"), function(x) {
    S4Vectors::first(x)
})
#' @rdname ginteractions-getters
#' @export
setMethod("anchors2", signature("GInteractions"), function(x) {
    S4Vectors::second(x)
})
#' @importFrom GenomeInfoDb seqnames
#' @rdname ginteractions-getters
#' @export
setMethod("seqnames1", signature("GInteractions"), function(x) {
    GenomeInfoDb::seqnames(anchors1(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("seqnames2", signature("GInteractions"), function(x) {
    GenomeInfoDb::seqnames(anchors2(x))
})
#' @importFrom BiocGenerics start
#' @rdname ginteractions-getters
#' @export
setMethod("start1", signature("GInteractions"), function(x) {
    BiocGenerics::start(anchors1(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("start2", signature("GInteractions"), function(x) {
    BiocGenerics::start(anchors2(x))
})
#' @importFrom BiocGenerics end
#' @rdname ginteractions-getters
#' @export
setMethod("end1", signature("GInteractions"), function(x) {
    BiocGenerics::end(anchors1(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("end2", signature("GInteractions"), function(x) {
    BiocGenerics::end(anchors2(x))
})
#' @importFrom BiocGenerics width
#' @rdname ginteractions-getters
#' @export
setMethod("width1", signature("GInteractions"), function(x) {
    BiocGenerics::width(anchors1(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("width2", signature("GInteractions"), function(x) {
    BiocGenerics::width(anchors2(x))
})
#' @importFrom BiocGenerics strand
#' @rdname ginteractions-getters
#' @export
setMethod("strand1", signature("GInteractions"), function(x) {
    BiocGenerics::strand(anchors1(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("strand2", signature("GInteractions"), function(x) {
    BiocGenerics::strand(anchors2(x))
})
#' @importFrom IRanges ranges
#' @rdname ginteractions-getters
#' @export
setMethod("ranges1", signature("GInteractions"), function(x) {
    IRanges::ranges(anchors1(x))
})
#' @rdname ginteractions-getters
#' @export
setMethod("ranges2", signature("GInteractions"), function(x) {
    IRanges::ranges(anchors2(x))
})
