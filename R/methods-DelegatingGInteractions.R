#' @importFrom InteractionSet GInteractions
#' @importFrom S4Vectors DataFrame
#' @importFrom S4Vectors Rle
#' @importFrom utils capture.output
#' @importFrom methods show
#' @importFrom InteractionSet GInteractions
#' @importFrom methods setMethod initialize
#' @title Methods for DelegatingGInteractions objects
#' @name delegating-ginteractions-methods
#' @keywords internal
#' @return One of the core GInteractions fields (e.g. seqnames1, start1, ...)
NULL 

#' @rdname delegating-ginteractions-methods
setMethod("anchors1", "DelegatingGInteractions",
    function(x) anchors1(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("ranges1", "DelegatingGInteractions",
    function(x) ranges1(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("seqnames1", "DelegatingGInteractions",
    function(x) seqnames1(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("start1", "DelegatingGInteractions",
    function(x) start1(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("end1", "DelegatingGInteractions",
    function(x) end1(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("width1", "DelegatingGInteractions",
    function(x) width1(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("strand1", "DelegatingGInteractions",
    function(x) strand1(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("anchors2", "DelegatingGInteractions",
    function(x) anchors2(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("ranges2", "DelegatingGInteractions",
    function(x) ranges2(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("seqnames2", "DelegatingGInteractions",
    function(x) seqnames2(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("start2", "DelegatingGInteractions",
    function(x) start2(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("end2", "DelegatingGInteractions",
    function(x) end2(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("width2", "DelegatingGInteractions",
    function(x) width2(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("strand2", "DelegatingGInteractions",
    function(x) strand2(x@delegate)
)
#' @rdname delegating-ginteractions-methods
setMethod("anchors", "DelegatingGInteractions", function(x) anchors(x@delegate))
#' @rdname delegating-ginteractions-methods
setMethod("regions", "DelegatingGInteractions", function(x) regions(x@delegate))
#' @rdname delegating-ginteractions-methods
setMethod("seqinfo", "DelegatingGInteractions", function(x) seqinfo(x@delegate))
#' @rdname delegating-ginteractions-methods
setMethod("mcols", "DelegatingGInteractions", function(x) mcols(x@delegate))
#' @rdname delegating-ginteractions-methods
setMethod("show", "DelegatingGInteractions", function(object) { 
    show(object@delegate)
})
