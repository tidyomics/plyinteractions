#' @importFrom InteractionSet GInteractions
#' @importFrom S4Vectors DataFrame
#' @importFrom S4Vectors Rle
#' @importFrom utils capture.output
#' @importFrom methods show
#' @include AllGenerics.R
#' @keywords internal
NULL

setClass("DelegatingGInteractions",
    slots = c(
        delegate="GInteractions"
    ),
    contains=c("GInteractions", "VIRTUAL")
)

setClass("GroupedGInteractions",
    slot = c(
        group_keys = "DFrame", 
        group_indices = "Rle",
        n = "integer"
    ),
    contains = c("DelegatingGInteractions")
)

setClass("PinnedGInteractions",
    slot = c(
        pin = "integer"
    ),
    contains = c("DelegatingGInteractions")
)

setClass("AnchoredPinnedGInteractions",
    slot = c(
        anchor = "character"
    ),
    contains = c("PinnedGInteractions")
)
