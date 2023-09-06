#' @importFrom InteractionSet GInteractions
#' @importFrom S4Vectors DataFrame
#' @importFrom S4Vectors Rle
#' @importFrom utils capture.output
#' @importFrom methods show
setClass("GroupedGInteractions",
    slot = c(
        group_keys = "DFrame", 
        group_indices = "Rle",
        n = "integer"
    ),
    contains = c("DelegatingGInteractions")
)

#' @importFrom methods setMethod initialize
setMethod("initialize", "GroupedGInteractions",
    function(.Object, delegate = GRanges(), group_keys = DataFrame(), group_indices = Rle(), n = integer()) {
        .Object@delegate <- delegate
        .Object@group_keys <- group_keys
        .Object@group_indices <- group_indices
        .Object@n <- n
        .Object
    }
)

setMethod("show", "GroupedGInteractions", function(object) { 
    groups <- colnames(object@group_keys)
    groups <- paste(groups, collapse = ", ")
    output <- c("", utils::capture.output(show(object@delegate)))
    output[1] <- output[2]
    output[2] <- paste("Groups:", groups, paste0("[", object@n, "]"))
    cat(output, sep = "\n")
})
