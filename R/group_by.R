#' Group GInteractions by columns
#'
#' @param .data,x a (Grouped)GInteractions object
#' @param ... Column(s) to group by. 
#' @param .add When FALSE, the default, group_by() will override existing
#' groups. To add to the existing groups, use .add = TRUE.
#' 
#' @return a GInteractions object.
#'
#' @importFrom tidyselect eval_select
#' @importFrom rlang syms
#' @importFrom methods new
#' 
#' @name dplyr-group_by
#' @rdname dplyr-group_by
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 11 20 chr1 21 30
#' chr1 11 20 chr1 51 55
#' chr1 11 30 chr1 51 55
#' chr1 11 30 chr2 51 60",
#' col.names = c("seqnames1", "start1", "end1", "seqnames2", "start2", "end2")) |> 
#'   as_ginteractions() |> 
#'   mutate(type = c('cis', 'cis', 'cis', 'trans'), score = runif(4))
#' 
#' ####################################################################
#' # 1. Group by core column
#' ####################################################################
#' 
#' gi |> group_by(end1)
#' 
#' gi |> group_by(end1, end2) |> group_data()
#' 
#' ####################################################################
#' # 2. Group by metadata column
#' ####################################################################
#' 
#' gi |> group_by(type) |> group_data()
#' 
#' ####################################################################
#' # 3. Combine core and metadata column grouping
#' ####################################################################
#' 
#' gi |> group_by(end1, type)
#' gi |> group_by(end1, type) |> group_data()
#' 
#' ####################################################################
#' # 4. Create a new column and group by this new variable
#' ####################################################################
#' 
#' gi |> group_by(class = c(1, 2, 1, 2))
#' 
#' ####################################################################
#' # 5. Replace or add groups to a GroupedGInteractions
#' ####################################################################
#' 
#' ggi <- gi |> group_by(class = c(1, 2, 1, 2))
#' ggi |> group_data()
#' ggi |> group_by(type) |> group_data()
#' ggi |> group_by(type, .add = TRUE) |> group_data()
#' 
#' ####################################################################
#' # 6. Ungroup GInteractions
#' ####################################################################
#' 
#' ggi <- gi |> group_by(type, class = c(1, 2, 1, 2))
#' ggi
#' ungroup(ggi, type)
#' ungroup(ggi, class)
#' 
#' @include tbl_vars.R
#' @include group_data.R
#' @export
group_by.GInteractions <- function(.data, ..., .add = FALSE) {
    
    new_groups <- rlang::enquos(...)

    ## Return GInteractions if no new groups
    if (length(new_groups) == 0) return(.data)
    
    ## Check if we need to mutate, i.e. if quosure is a call
    new_groups <- rlang::quos_auto_name(new_groups)
    update_groups <- Filter(rlang::quo_is_call, new_groups)
    if (length(update_groups) > 0) {
        .data <- mutate(.data, !!!update_groups)
    }

    ## Check that queried groups exist
    existing_columns <- c(
        "seqnames1", "start1", "end1", "width1", "strand1", 
        "seqnames2", "start2", "end2", "width2", "strand2", 
        names(mcols(.data))
    )
    check_names <- !(names(new_groups) %in% existing_columns)
    if (any(check_names)) {
        stop(paste0("Column `", 
            names(new_groups)[check_names],
            "` is unknown")
        )
    }
    
    ## Group data
    group_df <- select(
        .data, !!!rlang::syms(names(new_groups)), .drop_ranges = TRUE
    )
    unique <- BiocGenerics::unique(group_df)
    inx <- Rle(BiocGenerics::match(group_df, unique))
    n <- nrow(unique)
    methods::new("GroupedGInteractions", .data, unique, inx, n)

}

#' @rdname dplyr-group_by
#' @importFrom dplyr ungroup
#' @export
ungroup.GInteractions <- function(x, ...) {
    ungroups <- enquos(...)
    ungroups <- rlang::quos_auto_name(ungroups)
    if (length(ungroups) == 0L) {
        return(x@delegate)
    } else {
        gvars <- group_vars(x)
        names(gvars) <- gvars
        groups_update <- tidyselect::eval_select(rlang::expr(-c(...)), gvars)
        if (length(groups_update) == 0) {
        return(x@delegate)
        }
        
        groups_update <- syms(names(groups_update))
        group_by(x@delegate, !!!groups_update)
    }
}
