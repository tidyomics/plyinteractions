#' @importFrom InteractionSet GInteractions
#' @importFrom methods setMethod initialize
setMethod("initialize", "GroupedGInteractions",
    function(
        .Object, 
        delegate = InteractionSet::GInteractions(), 
        group_keys = DataFrame(), 
        group_indices = Rle(), n = integer()
    ) {
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
    output[1] <- gsub("^GInteractions", "GroupedGInteractions", output[2])
    output[2] <- paste("Groups:", groups, paste0("[", object@n, "]"))
    cat(output, sep = "\n")
})

#' @export
#' @keywords internal
group_by.GroupedGInteractions <- function(.data, ..., .add = FALSE) {
    new_groups <- rlang::enquos(...)
    if (.add) new_groups <- c(groups(.data), new_groups)
    group_by(.data@delegate, !!!new_groups)
}
