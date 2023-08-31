.check_allowed_names <- function(quosures, valid_names) {
    if (length(quosures) > 0) {
        valid_args <- names(quosures) %in% valid_names
        if (any(!valid_args)) {
            stop(paste(
                "Named arguments can only be",
                paste(valid_names, collapse = ", ")
            ), call. = FALSE)
        }
    }
}

.check_protected_names <- function(quosures, protected_names) {
    if (length(quosures) > 0) {
        invalid_args <- names(quosures) %in% protected_names
        if (any(invalid_args)) {
        stop(paste0("Core GInteractions columns (",
                paste(protected_names, collapse = ", "), 
                ") cannot be accessed yet."),
            call. = FALSE)
        }
    }
}

#' @importFrom methods is

.check_indices <- function(quosures) {
    if (length(quosures) > 0) {
        res <- lapply(quosures, function(quo) {
            if(!methods::is(eval_tidy(quo), "numeric")) stop(
                "Provided slicing indices are not numeric."
            )
        })
    }
}
