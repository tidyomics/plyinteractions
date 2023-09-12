#' Filter GInteractions overlapping with a GRanges
#'
#' @section Pinned `GInteractions`:  
#' 
#' When using `filter_by_overlaps()` with a `PinnedGInteractions` object, 
#' only the pinned anchors are used to check for overlap with `y`. 
#' This is equivalent to specifying `use.region="both"` in 
#' \code{\href{https://bioconductor.org/packages/release/bioc/vignettes/InteractionSet/inst/doc/interactions.html#27_Overlap_methods}{InteractionSet::countOverlaps()}}.
#' 
#' @param x A (Pinned)GInteractions object
#' @param y A GRanges object
#' @param maxgap,minoverlap See \code{?\link[GenomicRanges]{countOverlaps}} 
#' in the \pkg{GenomicRanges} package for a description of these arguments
#'
#' @importFrom plyranges filter_by_overlaps
#' @importFrom plyranges filter_by_non_overlaps
#' @importFrom IRanges subsetByOverlaps
#' 
#' @return An integer vector of same length as x.
#'
#' @name ginteractions-filter-overlaps
#' 
#' @examples
#' gi <- read.table(text = "  
#'     chr1 11 20 - chr1 21 30 + 
#'     chr1 11 20 - chr1 51 55 + 
#'     chr1 21 30 - chr1 51 55 + 
#'     chr1 21 30 - chr2 51 60 +",  
#'     col.names = c("seqnames1", "start1", "end1", "strand1", "seqnames2", "start2", "end2", "strand2")
#' ) |> as_ginteractions() |> mutate(id = 1:4, type = 'gi')
#' 
#' gr <- GenomicRanges::GRanges(c("chr1:20-30:+", "chr2:55-65:-")) |> plyranges::mutate(id = 1:2, type = 'gr')
#' 
#' gi
#' 
#' gr
#' 
#' ####################################################################
#' # 1. Filter GInteractions overlapping with a subject GRanges
#' ####################################################################
#' 
#' filter_by_overlaps(gi, gr)
#' 
#' filter_by_non_overlaps(gi, gr)
#' 
#' ####################################################################
#' # 2. Filter PinnedGInteractions overlapping with a subject GRanges
#' ####################################################################
#' 
#' gi |> pin_by("first") |> filter_by_overlaps(gr)
#' 
#' gi |> pin_by("first") |> filter_by_non_overlaps(gr)
#' 
#' gi |> pin_by("second") |> filter_by_overlaps(gr)
#' 
#' gi |> pin_by("second") |> filter_by_non_overlaps(gr)
NULL

#' @rdname ginteractions-filter-overlaps
#' @export
filter_by_overlaps.PinnedGInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L
) {

    IRanges::subsetByOverlaps( 
        unpin(x), 
        y, 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        ignore.strand = TRUE, 
        use.region = switch(
            as.character(pin(x)), 
            "1" = "first",
            "2" = "second"
        )
    )

}

#' @rdname ginteractions-filter-overlaps
#' @export
filter_by_overlaps.GInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L, suffix = c(".x", ".y")
) {

    IRanges::subsetByOverlaps( 
        x, 
        y, 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        ignore.strand = TRUE, 
        use.region = 'both'
    )
    
}

#' @rdname ginteractions-filter-overlaps
#' @export
filter_by_non_overlaps.PinnedGInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L
) {

    IRanges::subsetByOverlaps( 
        unpin(x), 
        y, 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        ignore.strand = TRUE, 
        use.region = switch(
            as.character(pin(x)), 
            "1" = "first",
            "2" = "second"
        ), 
        invert = TRUE
    )

}

#' @rdname ginteractions-filter-overlaps
#' @export
filter_by_non_overlaps.GInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L, suffix = c(".x", ".y")
) {

    IRanges::subsetByOverlaps( 
        x, 
        y, 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        ignore.strand = TRUE, 
        use.region = 'both', 
        invert = TRUE
    )
    
}
