#' @importFrom InteractionSet GInteractions
#' @importFrom S4Vectors DataFrame
#' @importFrom S4Vectors Rle
#' @importFrom utils capture.output
#' @importFrom methods show
#' @importFrom methods representation
#' @importFrom methods prototype
#' @include AllGenerics.R
#' @keywords internal
NULL

setClass("DelegatingGInteractions",
    representation(
        delegate="GInteractions"
    ),
    prototype(
        delegate = GInteractions(), 
        elementMetadata=DataFrame(), 
        metadata = list()
    ), 
    contains=c("GInteractions", "VIRTUAL")
)

setClass("GroupedGInteractions",
    representation(
        group_keys = "DataFrame", 
        group_indices = "Rle",
        n = "integer"
    ),
    prototype(
        group_keys = DataFrame(), 
        group_indices = Rle(),
        n = 0L
    ), 
    contains = c("DelegatingGInteractions"), 
)

setValidity("GroupedGInteractions", function(object) {
    errors <- character()
    if (nrow(object@group_keys) != object@n) {
        msg <- "`n` should be equal to the number of rows in `group_keys`"
        errors <- c(errors, msg)
    }
    if (length(object@group_indices) != length(object@delegate)) {
        msg <- "`group_indices` and GInteractions lengths should be identical"
        errors <- c(errors, msg)
    }
    group_names <- colnames(object@group_keys)
    check_valid_groups <- !(group_names %in% tbl_vars(object))
    if (any(check_valid_groups)) {
        msg <- paste("Invalid groups slot:",
            paste(group_names[check_valid_groups], collapse = ","),
            "not found in data.")
        errors <- c(errors, msg)
    }
    if (length(errors) == 0) TRUE else errors
})

setClass("PinnedGInteractions",
    representation(
        pin = "integer"
    ),
    prototype(
        pin = 1L
    ), 
    contains = c("DelegatingGInteractions")
)

setValidity("PinnedGInteractions", function(object) {
    errors <- character()
    if (length(object@pin) != 1) {
        msg <- "`pin` should be either `1L` or `2L`"
        errors <- c(errors, msg)
    }
    if (any(!object@pin %in% c(1L, 2L))) {
        msg <- "`pin` should be either `1L` or `2L`"
        errors <- c(errors, msg)
    }
    if (length(errors) == 0) TRUE else errors
})

setClass("AnchoredPinnedGInteractions",
    representation(
        anchor = "character"
    ),
    prototype(
        anchor = 'center'
    ), 
    contains = c("PinnedGInteractions")
)

setValidity("AnchoredPinnedGInteractions", function(object) {
    errors <- character()
    if (!object@anchor %in% c(1, 2)) {
        msg <- "`anchor` should be one of 'start', 'end', 'center', '5p' or '3p'"
        errors <- c(errors, msg)
    }
    if (length(errors) == 0) TRUE else errors
})
