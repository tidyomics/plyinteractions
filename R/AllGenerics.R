#' @rdname replace_anchors
#' @export
setGeneric("replace_anchors", 
    function(x, id, value) standardGeneric("replace_anchors")
)




#' @rdname ginteractions-pin
#' @export
setGeneric("pin", function(x, anchors) standardGeneric("pin"))
#' @export
#' @rdname ginteractions-pin
setGeneric("pin_by", function(x, anchors) pin(x, anchors))
#' @rdname ginteractions-pin
#' @export
setGeneric("pinned_anchors", function(x) standardGeneric("pinned_anchors"))
#' @rdname ginteractions-pin
#' @export
setGeneric("unpin", function(x) standardGeneric("unpin"))





#' @rdname ginteractions-annotate
#' @export
setGeneric("annotate", function(x, y, by) standardGeneric("annotate"))
#' @rdname ginteractions-annotate
#' @export
setGeneric("annotate_directed", 
    function(x, y, by) standardGeneric("annotate_directed")
)





#' @rdname ginteractions-getters
#' @export
setGeneric("anchors1", function(x) standardGeneric("anchors1"))
#' @rdname ginteractions-getters
#' @export
setGeneric("anchors2", function(x) standardGeneric("anchors2"))
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
setGeneric("ranges1", function(x) standardGeneric("ranges1"))
#' @rdname ginteractions-getters
#' @export
setGeneric("ranges2", function(x) standardGeneric("ranges2"))




#' @rdname ginteractions-setters
setGeneric("set_seqnames1", function(x, value) standardGeneric("set_seqnames1"))
#' @rdname ginteractions-setters
setGeneric("set_seqnames2", function(x, value) standardGeneric("set_seqnames2"))
#' @rdname ginteractions-setters
setGeneric("set_start1", function(x, value) standardGeneric("set_start1"))
#' @rdname ginteractions-setters
setGeneric("set_start2", function(x, value) standardGeneric("set_start2"))
#' @rdname ginteractions-setters
setGeneric("set_end1", function(x, value) standardGeneric("set_end1"))
#' @rdname ginteractions-setters
setGeneric("set_end2", function(x, value) standardGeneric("set_end2"))
#' @rdname ginteractions-setters
setGeneric("set_width1", function(x, value) standardGeneric("set_width1"))
#' @rdname ginteractions-setters
setGeneric("set_width2", function(x, value) standardGeneric("set_width2"))
#' @rdname ginteractions-setters
setGeneric("set_strand1", function(x, value) standardGeneric("set_strand1"))
#' @rdname ginteractions-setters
setGeneric("set_strand2", function(x, value) standardGeneric("set_strand2"))