#' @importFrom InteractionSet GInteractions
#' @importFrom S4Vectors DataFrame
#' @importFrom S4Vectors Rle
#' @importFrom utils capture.output
#' @importFrom methods show
#' @include PinnedGInteractions-class.R
setClass("AnchoredPinnedGInteractions",
    slot = c(
        anchor = "character"
    ),
    contains = c("PinnedGInteractions")
)

#' @importFrom InteractionSet GInteractions
#' @importFrom methods setMethod initialize
setMethod("initialize", "AnchoredPinnedGInteractions", function(
    .Object, delegate = InteractionSet::GInteractions(), pin, anchor
) {
    stopifnot(
        pin %in% c("anchors1", "first", "1", "anchors2", "second", "2")
    )
    pin <- switch(as.character(pin), 
        "anchors1" = 1L, 
        "first" = 1L, 
        "1" = 1L, 
        "anchors2" = 2L, 
        "second" = 2L, 
        "2" = 2L
    )
    .Object@delegate <- delegate
    .Object@pin <- pin
    .Object@anchor <- anchor
    .Object
})

setMethod("show", "AnchoredPinnedGInteractions", function(object) { 
    output <- c("", utils::capture.output(show(unpin(unanchor(object)))))
    output[1] <- gsub("^GInteractions", "AnchoredPinnedGInteractions", output[2])
    output[2] <- paste0(
        "Pinned on: anchors", object@pin, 
        " | Anchored by: ", object@anchor
    )
    cat(output, sep = "\n")
})
