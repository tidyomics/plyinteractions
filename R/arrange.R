#' Arrange a GInteractions by a column
#'
#' @param .data a GInteractions object
#' @param ... <data-masking> Variables, or functions of variables. 
#' Use dplyr::desc() to sort a variable in descending order.
#'
#' @return a GInteractions object.
#'
#' @rdname ginteractions-arrange
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 1 10 chr1 1 10
#' chr1 2 10 chr2 1 10
#' chr3 3 10 chr3 1 10
#' chr4 4 10 chr4 1 10
#' chr5 5 10 chr5 1 10",
#' col.names = c("seqnames1", "start1", "end1", "seqnames2", "start2", "end2")) |> 
#'   as_ginteractions() |> 
#'   mutate(cis = seqnames1 == seqnames2, score = runif(5)*100, gc = runif(5))
#' gi
#' 
#' ####################################################################
#' # 1. Arrange GInteractions by a numerical column
#' ####################################################################
#' 
#' gi |> arrange(gc)
#' 
#' ####################################################################
#' # 2. Arrange GInteractions by a logical column
#' ####################################################################
#' 
#' gi |> arrange(cis)
#' 
#' ####################################################################
#' # 3. Arrange GInteractions by a factor
#' ####################################################################
#' 
#' gi |> 
#'   mutate(rep = factor(c("rep1", "rep2", "rep1", "rep2", "rep1"))) |> 
#'   arrange(rep)
#' 
#' ####################################################################
#' # 4. Combine sorting variables
#' ####################################################################
#' 
#' gi |> 
#'   mutate(rep = factor(c("rep1", "rep2", "rep1", "rep2", "rep1"))) |> 
#'   arrange(dplyr::desc(rep), score)
#' 
#' @export
arrange.GInteractions <- function(.data, ...) {

    quosures <- rlang::enquos(...)

    ## Put each quosure in an environment with the required generic 
    scoped_quosures <- .scope_quos(quosures) 

    ## tidyeval quosures in a scoped env. This takes care of all the tidy eval. 
    overscope <- .overscope_ginteractions(.data)
    evaled_quosures <- vector("list", length(scoped_quosures))
    for (i in seq_along(scoped_quosures)) {
        quo <- scoped_quosures[[i]]
        evaled_quosures[[i]] <- eval_tidy(quo, data = overscope)
    }

    if (length(evaled_quosures) == 1L) {
        o <- order(evaled_quosures[[1]])
    } else {
        o <- Reduce(order, evaled_quosures)
    }

    ## Order appropriate rows
    .data <- .data[o]

    .data

}
