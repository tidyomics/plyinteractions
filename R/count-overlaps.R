#' Count overlaps between a query GInteractions and a GRanges
#'
#' @section Pinned `GInteractions`:  
#' 
#' When using `count_overlaps()` with a `PinnedGInteractions` object, 
#' only the pinned anchors are used to check for overlap with `y`. 
#' This is equivalent to specifying `use.region="both"` in 
#' \code{\href{https://bioconductor.org/packages/release/bioc/vignettes/InteractionSet/inst/doc/interactions.html#27_Overlap_methods}{InteractionSet::countOverlaps()}}.
#' 
#' @param x A (Pinned)GInteractions object
#' @param y A GRanges object
#' @param maxgap,minoverlap See \code{?\link[GenomicRanges]{countOverlaps}} 
#' in the \pkg{GenomicRanges} package for a description of these arguments
#'
#' @importFrom plyranges count_overlaps
#' @importFrom plyranges count_overlaps_directed
#' @importFrom InteractionSet countOverlaps
#' 
#' @return An integer vector of same length as x.
#'
#' @name ginteractions-count-overlaps
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
#' # 1. Count overlaps between GInteractions and a subject GRanges
#' ####################################################################
#' 
#' count_overlaps(gi, gr)
#' 
#' count_overlaps_directed(gi, gr)
#' 
#' ####################################################################
#' # 2. Count overlaps between PinnedGInteractions and a subject GRanges
#' ####################################################################
#' 
#' gi |> pin_by("first") |> count_overlaps(gr)
#' 
#' gi |> pin_by("second") |> count_overlaps(gr)
#' 
#' gi |> pin_by("first") |> count_overlaps_directed(gr)
#' 
#' gi |> pin_by("second") |> count_overlaps_directed(gr)
NULL

#' @rdname ginteractions-count-overlaps
#' @export
count_overlaps.PinnedGInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L
) {

    InteractionSet::countOverlaps( 
        query = unpin(x), 
        subject = y, 
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

#' @rdname ginteractions-count-overlaps
#' @export
count_overlaps.GInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L, suffix = c(".x", ".y")
) {

    InteractionSet::countOverlaps( 
        query = x, 
        subject = y, 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        ignore.strand = TRUE, 
        use.region = 'both'
    )
    
}

#' @rdname ginteractions-count-overlaps
#' @export
count_overlaps_directed.PinnedGInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L, suffix = c(".x", ".y")
) {

    InteractionSet::countOverlaps( 
        query = unpin(x), 
        subject = y, 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        ignore.strand = FALSE, 
        use.region = switch(
            as.character(pin(x)), 
            "1" = "first",
            "2" = "second"
        )
    )

}

#' @rdname ginteractions-count-overlaps
#' @export
count_overlaps_directed.GInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L, suffix = c(".x", ".y")
) {

    InteractionSet::countOverlaps( 
        query = x, 
        subject = y, 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        ignore.strand = FALSE, 
        use.region = 'both'
    )
    
}
