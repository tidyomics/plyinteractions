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
    function(
        .Object, delegate = GRanges(), group_keys = DataFrame(), 
        group_indices = Rle(), n = integer()
    ) {
        .Object@delegate <- delegate
        .Object@group_keys <- group_keys
        .Object@group_indices <- group_indices
        .Object@n <- n
        .Object
    }
)

#' @export
#' @keywords internal
group_by.GroupedGInteractions <- function(.data, ..., .add = FALSE) {
    new_groups <- rlang::enquos(...)
    if (.add) new_groups <- c(groups(.data), new_groups)
    group_by(.data@delegate, !!!new_groups)
}

