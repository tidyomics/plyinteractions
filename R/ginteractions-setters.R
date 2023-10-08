#' Internal GInteractions setters
#' 
#' @name ginteractions-setters
#' @param x a GInteractions object
#' @param value a value passed to the corresponding field
#' @return A modified GInteractions
#' @keywords internal
NULL 

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
setMethod("set_seqnames1", 
    signature("GInteractions", "factor"), function(x, value) {
        GenomeInfoDb::seqnames(S4Vectors::first(x)) <- value
        x
    }
)
#' @rdname ginteractions-setters
setMethod("set_seqnames2", 
    signature("GInteractions", "factor"), function(x, value) {
        GenomeInfoDb::seqnames(S4Vectors::second(x)) <- value
        x
    }
)
#' @rdname ginteractions-setters
setMethod("set_start1", 
    signature("GInteractions", "numeric"), function(x, value) {
        BiocGenerics::start(S4Vectors::first(x)) <- value
        x
    }
)
#' @rdname ginteractions-setters
setMethod("set_start2", 
    signature("GInteractions", "numeric"), function(x, value) {
        BiocGenerics::start(S4Vectors::second(x)) <- value
        x
    }
)
#' @rdname ginteractions-setters
setMethod("set_end1", 
    signature("GInteractions", "numeric"), function(x, value) {
        BiocGenerics::end(S4Vectors::first(x)) <- value
        x
    }
)
#' @rdname ginteractions-setters
setMethod("set_end2", 
    signature("GInteractions", "numeric"), function(x, value) {
        BiocGenerics::end(S4Vectors::second(x)) <- value
        x
    }
)
#' @rdname ginteractions-setters
setMethod("set_width1", 
    signature("GInteractions", "numeric"), function(x, value) {
        BiocGenerics::width(S4Vectors::first(x)) <- value
        x
    }
)
#' @rdname ginteractions-setters
setMethod("set_width2", 
    signature("GInteractions", "numeric"), function(x, value) {
        BiocGenerics::width(S4Vectors::second(x)) <- value
        x
    }
)
#' @importFrom plyranges set_width
#' @rdname ginteractions-setters
setMethod("set_width1", 
    signature("AnchoredPinnedGInteractions", "numeric"), 
    function(x, value) {
        if (pin(x) == 1) {
            replace_anchors(
                x, value = plyranges::mutate(pinned_anchors(x), width = value)
            )
        } else {
            replace_anchors(
                x, id = 1, value = plyranges::mutate(anchors1(x), width = value)
            )
        }
    }
)
#' @rdname ginteractions-setters
setMethod("set_width2", 
    signature("AnchoredPinnedGInteractions", "numeric"), 
    function(x, value) {
        if (pin(x) == 2) {
            replace_anchors(
                x, value = plyranges::mutate(pinned_anchors(x), width = value)
            )
        } else {
            replace_anchors(
                x, id = 2, value = plyranges::mutate(anchors2(x), width = value)
            )
        }
    }
)
#' @rdname ginteractions-setters
setMethod("set_strand1", 
    signature("GInteractions", "character"), function(x, value) {
        BiocGenerics::strand(S4Vectors::first(x)) <- value
        x
    }
)
#' @rdname ginteractions-setters
setMethod("set_strand2", 
    signature("GInteractions", "character"), function(x, value) {
        BiocGenerics::strand(S4Vectors::second(x)) <- value
        x
    }
)
