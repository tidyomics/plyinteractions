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
        stop(paste("GInteractions columns named ",
                paste(protected_names, collapse = ", "), 
                "cannot be modified with `mutate`."),
            call. = FALSE)
        }
    }
}
