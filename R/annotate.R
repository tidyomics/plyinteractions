#' Annotate both anchors of a GInteractions
#'
#' For each interaction in a `GInteractions` object, `annotate` returns 
#' the pairs of annotations from the `GRanges` object it overlaps with. 
#' 
#' @param x a GInteractions object
#' @param y a GRanges object to extract annotations from
#' @param by Column name from `y` to use to extract annotations
#' 
#' @return a GInteractions object with two extra metadata columns named 
#' `by.1` and `by.2`.
#'
#' @name ginteractions-annotate
#' 
#' @examples
#' ####################################################################
#' # 1. Basic example
#' ####################################################################
#' 
#' gi <- read.table(text = "  
#'     chr1 11 20 - chr1 21 30 + 
#'     chr1 21 30 + chr2 51 60 +",  
#'     col.names = c("seqnames1", "start1", "end1", "strand1", "seqnames2", "start2", "end2", "strand2")
#' ) |> as_ginteractions() 
#' 
#' gr <- GenomicRanges::GRanges(c("chr1:20-30:+", "chr2:55-65:+")) |>
#'     plyranges::mutate(id = 1:2)
#' 
#' annotate(gi, gr, by = 'id')
#' 
#' annotate_directed(gi, gr, by = 'id')
#' 
#' ####################################################################
#' # 2. Match loops with tiled genomic bins
#' ####################################################################
#' 
#' data(GM12878_HiCCUPS)
#' loops <- GM12878_HiCCUPS |> 
#'     pin_by('first') |> 
#'     anchor_center() |> 
#'     mutate(width1 = 500) |> 
#'     pin_by('second') |> 
#'     anchor_center() |> 
#'     mutate(width2 = 500)
#' 
#' genomic_bins <- GenomeInfoDb::getChromInfoFromUCSC(
#'     'hg19', assembled.molecules.only = TRUE, as.Seqinfo = TRUE
#' ) |> 
#'     GenomicRanges::tileGenome(tilewidth = 10000) |> 
#'     unlist() |> 
#'     plyranges::mutate(binID = seq_len(plyranges::n()))
#' 
#' annotate(loops, genomic_bins, by = 'binID') |> 
#'     select(starts_with('binID'))
#' 
#' ####################################################################
#' # 3. Annotate interactions by a set of regulatory elements
#' ####################################################################
#' 
#' data(ce10_ARCC)
#' data(ce10_REs)
#' annotate(ce10_ARCC, ce10_REs, by = 'annot') |> 
#'    count(annot.1, annot.2) |> 
#'    as.data.frame() |> 
#'    dplyr::arrange(desc(n))
NULL

#' @rdname ginteractions-annotate
#' @export
setGeneric("annotate", function(x, y, by) standardGeneric("annotate"))

#' @rdname ginteractions-annotate
#' @export
setMethod(
    "annotate", 
    signature(x = "GInteractions", y = "GRanges", by = "character"), 
    function(x, y, by) {

        cols <- colnames(mcols(x))
        
        # If `by` column does not exist in `y`, abort
        if(!by %in% colnames(mcols(y))) {
            stop("`by` argument doesn't match any existing column from `y`")
        }

        # If `by` column already exists in `x`, rename it
        .exists <- FALSE 
        if (by %in% cols) {
            .exists <- TRUE
            colnames(mcols(x))[cols == by] <- paste0(by, '.x')
            cols <- colnames(mcols(x))
        }
        col1 <- paste0(by, ".1")
        col2 <- paste0(by, ".2")
        if (.exists) {
            col1 <- paste0(by, ".y.1")
            col2 <- paste0(by, ".y.2")
        }
        x <- x |> pin_by("first") 
        x <- x |> join_overlap_left(y) 
        x <- x |> rename(!!col1 := !!by) 
        x <- x |> pin_by("second") 
        x <- x |> join_overlap_left(y) 
        x <- x |> rename(!!col2 := !!by)
        x
    
    }
)

#' @rdname ginteractions-annotate
#' @export
setGeneric("annotate_directed", function(x, y, by) standardGeneric("annotate_directed"))

#' @rdname ginteractions-annotate
#' @export
setMethod(
    "annotate_directed", 
    signature(x = "GInteractions", y = "GRanges", by = "character"), 
    function(x, y, by) {

        cols <- colnames(mcols(x))
        
        # If `by` column does not exist in `y`, abort
        if(!by %in% colnames(mcols(y))) {
            stop("`by` argument doesn't match any existing column from `y`")
        }

        # If `by` column already exists in `x`, rename it
        .exists <- FALSE 
        if (by %in% cols) {
            .exists <- TRUE
            colnames(mcols(x))[cols == by] <- paste0(by, '.x')
            cols <- colnames(mcols(x))
        }
        col1 <- paste0(by, ".1")
        col2 <- paste0(by, ".2")
        if (.exists) {
            col1 <- paste0(by, ".y.1")
            col2 <- paste0(by, ".y.2")
        }
        x <- x |> pin_by("first") 
        x <- x |> join_overlap_left_directed(y) 
        x <- x |> rename(!!col1 := !!by) 
        x <- x |> pin_by("second") 
        x <- x |> join_overlap_left_directed(y) 
        x <- x |> rename(!!col2 := !!by)
        x
    
    }
)
