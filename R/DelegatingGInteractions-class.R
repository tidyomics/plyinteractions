#' DelegatingGInteractions class
#' @rdname delegating-ginteractions-class
#' @include ginteractions-getters.R
#' @param x DelegatingGInteractions object
setClass("DelegatingGInteractions",
    slots = list(delegate="GInteractions"),
    contains=c("GInteractions", "VIRTUAL")
)

#' @rdname delegating-ginteractions-class
setMethod("anchors1", "DelegatingGInteractions",
    function(x) anchors1(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("ranges1", "DelegatingGInteractions",
    function(x) ranges1(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("seqnames1", "DelegatingGInteractions",
    function(x) seqnames1(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("start1", "DelegatingGInteractions",
    function(x) start1(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("end1", "DelegatingGInteractions",
    function(x) end1(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("width1", "DelegatingGInteractions",
    function(x) width1(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("strand1", "DelegatingGInteractions",
    function(x) strand1(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("anchors2", "DelegatingGInteractions",
    function(x) anchors2(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("ranges2", "DelegatingGInteractions",
    function(x) ranges2(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("seqnames2", "DelegatingGInteractions",
    function(x) seqnames2(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("start2", "DelegatingGInteractions",
    function(x) start2(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("end2", "DelegatingGInteractions",
    function(x) end2(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("width2", "DelegatingGInteractions",
    function(x) width2(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("strand2", "DelegatingGInteractions",
    function(x) strand2(x@delegate)
)
#' @rdname delegating-ginteractions-class
setMethod("anchors", "DelegatingGInteractions", function(x) anchors(x@delegate))
#' @rdname delegating-ginteractions-class
setMethod("regions", "DelegatingGInteractions", function(x) regions(x@delegate))
#' @rdname delegating-ginteractions-class
setMethod("seqinfo", "DelegatingGInteractions", function(x) seqinfo(x@delegate))
