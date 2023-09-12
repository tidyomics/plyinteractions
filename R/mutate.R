#' Mutate columns from a GInteractions object
#'  
#' @name dplyr-mutate
#' @rdname dplyr-mutate
#' 
#' @param .data a GInteractions object
#' @param ... Optional named arguments specifying which the columns in .data
#' to create/modify.
#'
#' @return a GInteractions object.
#'
#' @importFrom tibble as_tibble
#' @importFrom rlang enquos
#' @importFrom rlang `:=`
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 10 20 chr1 50 51
#' chr1 10 50 chr2 30 40",
#' col.names = c("chr1", "start1", "end1", "chr2", "start2", "end2")) |> 
#'   as_ginteractions(seqnames1 = chr1, seqnames2 = chr2)
#'   
#' ####################################################################
#' # 1. Add metadata columns to a GInteractions object
#' ####################################################################
#' 
#' gi |> 
#'   mutate(type = c('cis', 'trans'), score = runif(2)) |> 
#'   mutate(type2 = type)
#' 
#' ####################################################################
#' # 2. More complex, nested or inplace changes
#' ####################################################################
#' 
#' gi |> 
#'   mutate(type = c('cis', 'trans'), score = runif(2)) |> 
#'   mutate(type2 = type) |> 
#'   mutate(count = c(1, 2), score = count * 2, new_col = paste0(type2, score))
#' 
#' ####################################################################
#' # 3. Core GInteractions columns can also be modified
#' ####################################################################
#' 
#' gi |> 
#'   mutate(start1 = 1, end1 = 10, width2 = 30, strand2 = c('-', '+'))
#' 
#' # Note how the core columns are modified sequentially 
#' 
#' gi |> 
#'   mutate(start1 = 1, end1 = 10)
#' 
#' gi |> 
#'   mutate(start1 = 1, end1 = 10, width1 = 50)
#' 
#' ####################################################################
#' # 4. Evaluating core GInteractions columns
#' ####################################################################
#' 
#' gi |> 
#'   mutate(
#'     score = runif(2), 
#'     cis = seqnames1 == seqnames2, 
#'     distance = ifelse(cis, start2 - end1, NA)
#'   )
#' @export
mutate.GInteractions <- function(.data, ...) {
    
    quosures <- rlang::enquos(..., .named = TRUE)

    ## Put each quosure in an environment with the required generic 
    scoped_quosures <- .scope_quos(quosures) 
    names(scoped_quosures) <- names(quosures)

    ## Check that all quosures are named 
    col_names <- names(scoped_quosures)
    if (any(col_names %in% "")) {
        stop("mutate must have name-variable pairs as input", call. = FALSE)
    }

    ## tidyeval quosures in a scoped env. This takes care of all the tidy eval. 
    overscope <- .overscope_ginteractions(.data)
    evaled_quosures <- vector("list", length(scoped_quosures))
    names(evaled_quosures) <- names(scoped_quosures)
    for (i in seq_along(scoped_quosures)) {
        quo <- scoped_quosures[[i]]
        evaled_quosures[[i]] <- eval_tidy(quo, data = overscope)
        new_col <- names(scoped_quosures)[[i]]
        rlang::env_bind(overscope, !!new_col := evaled_quosures[[i]])
    }

    ## Replace appropriate columns
    .data <- .mutate_core(.data, evaled_quosures)
    .data <- .mutate_mcols(.data, evaled_quosures)

    .data
}

#' @importFrom methods selectMethod
.mutate_core <- function(.data, .mutated) {
    all_cols <- names(.mutated)
    core_cols <- all_cols[all_cols %in% c(
        "seqnames1", "start1", "end1", "width1", "strand1", 
        "seqnames2", "start2", "end2", "width2", "strand2"
    )]
    if (length(core_cols == 0)) {
        .data
    }

    for (col in core_cols) {
        modifier <- match.fun(paste0("set_", col))
        .data <- modifier(.data, .mutated[[col]])
    }
    .data
}

.mutate_mcols <- function(.data, .mutated) {
    all_cols <- names(.mutated)
    only_mcols <- !(all_cols %in% c(
        "seqnames1", "start1", "end1", "width1", "strand1", 
        "seqnames2", "start2", "end2", "width2", "strand2"
    ))
    .mutated <- .mutated[only_mcols]
    update_cols <- all_cols[only_mcols]

    matches_mcols <- match(update_cols, names(mcols(.data)))
    idx_mcols <- !is.na(matches_mcols)

    if (any(idx_mcols)) {
        mcols(.data)[matches_mcols[idx_mcols]] <- .mutated[idx_mcols]
    }

    if (!all(idx_mcols)) {
        if (is.null(mcols(.data))) {
        mcols(.data) <- S4Vectors::DataFrame(.mutated[!idx_mcols])
        } else {
        mcols(.data) <- S4Vectors::DataFrame(list(mcols(.data), 
                                                    .mutated[!idx_mcols]))
        }
    }
    .data
}
