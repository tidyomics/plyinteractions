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

setValidity("GroupedGInteractions",
    function(object)
    {
        if (nrow(object@group_keys) != object@n)
            return("`n` should be equal to the number of rows in `group_keys`")
        if (length(object@group_indices) != length(object@delegate))
            return("`group_indices` and GInteractions lengths should be identical")
        if (any(!colnames(object@group_keys) %in% 
            colnames(as_tibble(object@delegate)))
        )
            return("Some column names in `group_keys` are not found in the GInteractions object")
        TRUE
    }
)

setClass("PinnedGInteractions",
    slot = c(
        pin = "integer"
    ),
    contains = c("DelegatingGInteractions")
)

setValidity("PinnedGInteractions",
    function(object)
    {
        if (!object@pin %in% c(1, 2))
            return("`pin` should be either `1L` or `2L`")
        TRUE
    }
)

setClass("AnchoredPinnedGInteractions",
    slot = c(
        anchor = "character"
    ),
    contains = c("PinnedGInteractions")
)

setValidity("AnchoredPinnedGInteractions",
    function(object)
    {
        if (!object@anchor %in% c(1, 2))
            return("`anchor` should be one of 'start', 'end', 'center', '5p' or '3p'")
        TRUE
    }
)

