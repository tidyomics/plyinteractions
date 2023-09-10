#' Count or tally GInteractions per group
#'  
#' @name ginteractions-count
#'
#' @param x A grouped GInteractions object
#' @param ... <[`data-masking`][rlang::args_data_masking]> Variables to group
#'   by.
#' @param wt <[`data-masking`][rlang::args_data_masking]> Frequency weights.
#'   Can be `NULL` or a variable:
#'
#'   * If `NULL` (the default), counts the number of rows in each group.
#'   * If a variable, computes `sum(wt)` for each group.
#' 
#' @param sort If `TRUE`, will show the largest groups at the top.
#' @param name The name of the new column in the output.
#'
#' @return a \code{S4Vectors::\link[S4Vectors:DataFrame-class]{DataFrame()}}
#' object, with an added column with the count/tablly per group. 
#' 
#' @importFrom dplyr count 
#' @importFrom dplyr tally 
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 11 20 chr1 21 30 + +
#' chr1 11 20 chr1 51 55 + +
#' chr1 11 30 chr1 51 55 - -
#' chr1 11 30 chr2 51 60 - -",
#' col.names = c(
#'   "seqnames1", "start1", "end1", 
#'   "seqnames2", "start2", "end2", "strand1", "strand2")
#' ) |> 
#'   as_ginteractions() |> 
#'   mutate(score = runif(4), type = c('cis', 'cis', 'cis', 'trans'))
#' 
#' ####################################################################
#' # 1. Tally groups
#' ####################################################################
#' 
#' gi 
#' 
#' gi |> group_by(strand1) |> tally()
#' 
#' gi |> group_by(type) |> tally()
#' 
#' ####################################################################
#' # 2. Count per groups
#' ####################################################################
#' 
#' gi |> count(type)
#' 
#' gi |> group_by(type) |> count()
#' 
#' gi |> group_by(type) |> count(strand1)
#' 
#' @importFrom rlang local_options
#' @importFrom rlang enquo
#' @importFrom rlang sym
#' @importFrom rlang is_call
#' @importFrom rlang quo_get_expr
#' @importFrom rlang warn
#' @importFrom rlang quo
#' @importFrom rlang quo_is_null
#' @importFrom rlang inform
#' @importFrom rlang expr
#' @importFrom dplyr desc
#' @export
tally.GroupedGInteractions <- function(x, wt = NULL, sort = FALSE, name = NULL) {

    ## Check provided name against gorup names
    name <- check_n_name(name, group_vars(x))

    ## Prepare tally strategy
    n <- tally_n(x, {{ wt }})

    ## Tally groups
    rlang::local_options(dplyr.summarise.inform = FALSE)
    out <- summarise(x, !!name := !!n)

    ## Sort by group by default
    if (sort) {
        arrange(out, dplyr::desc(!!rlang::sym(name)))
    } else {
        out
    }

}

#' @rdname ginteractions-count
#' @export
count.GInteractions <- function(x, ..., wt = NULL, sort = FALSE, name = NULL) {

    ## Add new groups before counting
    if (!missing(...)) {
        out <- group_by(x, ..., .add = TRUE)
    } else {
        out <- x
    }

    ## count through weighted tally
    tally(out, wt = !!rlang::enquo(wt), sort = sort, name = name)

}

tally_n <- function(x, wt) {
    wt <- rlang::enquo(wt)
    if (rlang::is_call(rlang::quo_get_expr(wt), "n", n = 0)) {
        # Provided only by dplyr 1.0.0. See #5349 for discussion.
        rlang::warn(c(
            "`wt = n()` is deprecated",
            i = "You can now omit the `wt` argument"
        ))
        wt <- rlang::quo(NULL)
    }
    if (rlang::quo_is_null(wt)) {
        group_size(x)
    } else {
        rlang::expr(sum(!!wt, na.rm = TRUE))
    }
}

check_n_name <- function(
    name,
    vars,
    arg = rlang::caller_arg(name),
    call = rlang::caller_env()
) {
    if (is.null(name)) {
        name <- n_name(vars)
        if (name != "n") {
            rlang::inform(c(
                paste0(
                    "Storing counts in `", name, 
                    "`, as `n` already present in input"
                ),
                i = "Use `name = \"new_name\"` to pick a new name."
            ))
        }
    } else {
        stopifnot(!is.na(name))
        stopifnot(name != "")
    }
    name
}

n_name <- function(x) {
    name <- "n"
    while (name %in% x) name <- paste0("n", name)
    name
}
