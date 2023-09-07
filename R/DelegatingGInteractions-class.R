#' DelegatingGInteractions class
#' @rdname delegating-ginteractions-class
#' @include ginteractions-getters.R
#' @keywords internal
#' @param x,object DelegatingGInteractions object
#' @return A DelegatingGInteractions object 
#' or one of the core GInteractions fields (e.g. seqnames1, start1, ...)
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
#' @rdname delegating-ginteractions-class
setMethod("mcols", "DelegatingGInteractions", function(x) mcols(x@delegate))
#' @rdname delegating-ginteractions-class
setMethod("show", "DelegatingGInteractions", function(object) { 
    groups <- colnames(object@group_keys)
    groups <- paste(groups, collapse = ", ")
    output <- c("", utils::capture.output(show(object@delegate)))
    output[1] <- output[2]
    output[2] <- paste("Groups:", groups, paste0("[", object@n, "]"))
    cat(output, sep = "\n")
})
