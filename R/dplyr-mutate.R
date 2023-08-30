#' Mutate columns from a GInteractions object
#'
#' @param .data a GInteractions object
#' @param ... Optional named arguments specifying which the columns in .data
#' to create/modify.
#'
#' @return a GInteractions object.
#'
#' @importFrom tibble as_tibble
#' @importFrom rlang `:=`
#' @importFrom dplyr mutate
#' 
#' @rdname ginteractions-mutate
#' 
#' @examples
#' gi <- read.table(text = "
#' chr1 10 20 chr1 50 51
#' chr1 10 50 chr1 30 40",
#' col.names = c("chr1", "start1", "end1", "chr2", "start2", "end2")) |> 
#'   as_ginteractions(seqnames1 = chr1, seqnames2 = chr2)
#'   
#' gi |> 
#'   mutate(type = 'cis', score = runif(2)) |> 
#'   mutate(type2 = type)
#' @export
mutate.GInteractions <- function(.data, ...) {

    quosures <- rlang::quos(..., .ignore_empty = "all") 
    
    # Check that named arguments are not protected columns (core GI columns)
    #       %%%% Modification of protected columns would require to 
    #       %%%% 1. Parse core GI as a tibble (can be long for large GIs)
    #       %%%% 2. Compare against seqinfo, ... 
    #        
    #       %%%% Alternatively, could rely on plyranges ... 
    protected_names <- c("seqnames1", "start1", "end1", "strand1", 
        "seqnames2", "start2", "end2", "strand2")
    .check_protected_names(quosures, protected_names)

    # Mutate metadata columns using dplyr::mutate
    quosures_mcols <- quosures[!names(quosures) %in% protected_names]
    mcols <- tibble::as_tibble(S4Vectors::mcols(.data))
    mcols <- dplyr::mutate(mcols, ...)
    S4Vectors::mcols(.data) <- mcols
    return(.data)

}
