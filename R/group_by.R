#' Group GInteractions by columns
#'
#' @param .data a GInteractions object
#' @param ... Column(s) to group by. 
#' 
#' @return a GInteractions object.
#'
#' @importFrom tidyselect eval_select
#' @importFrom rlang syms
#' @importFrom methods new
#' 
#' @rdname ginteractions-group_by
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 11 20 chr1 21 30
#' chr1 11 20 chr1 51 50
#' chr1 11 30 chr1 51 50
#' chr1 11 30 chr2 51 60",
#' col.names = c("seqnames1", "start1", "end1", "seqnames2", "start2", "end2")) |> 
#'   as_ginteractions() |> 
#'   mutate(type = c('cis', 'cis', 'cis', 'trans'), score = runif(4))
#' 
#' ####################################################################
#' # 1. Group by code column
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
#' @include tbl_vars.R
#' @include group_data.R
#' @export
group_by.GInteractions <- function(.data, ...) {
    
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