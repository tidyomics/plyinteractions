#' @importFrom InteractionSet GInteractions
#' @importFrom S4Vectors DataFrame
#' @importFrom S4Vectors Rle
#' @importFrom utils capture.output
#' @importFrom methods show
#' @include DelegatingGInteractions-class.R
setClass("PinnedGInteractions",
    slot = c(
        pin = "integer"
    ),
    contains = c("DelegatingGInteractions")
)

#' @importFrom InteractionSet GInteractions
#' @importFrom methods setMethod initialize
setMethod("initialize", "PinnedGInteractions", function(
    .Object, delegate = InteractionSet::GInteractions(), pin
) {
    stopifnot(
        pin %in% c("anchors1", "first", "1", "anchors2", "second", "2")
    )
    pin <- switch(pin, 
        "anchors1" = 1L, 
        "first" = 1L, 
        "1" = 1L, 
        "anchors2" = 2L, 
        "second" = 2L, 
        "2" = 2L
    )
    .Object@delegate <- delegate
    .Object@pin <- pin
    .Object
})

setMethod("show", "PinnedGInteractions", function(object) { 
    output <- c("", utils::capture.output(show(object@delegate)))
    output[1] <- gsub("^GInteractions", "PinnedGInteractions", output[2])
    output[2] <- paste0("Pinned on: anchors", object@pin)
    cat(output, sep = "\n")
})
