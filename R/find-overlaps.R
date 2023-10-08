#' Find overlaps between a query GInteractions and a GRanges
#'
#' @section Rationale:  
#' 
#' `find_overlaps()` will search for any overlap between `GInteractions` 
#' in `x` and `GRanges` in `y`. It will return a `GInteractions` 
#' object of length equal to the number of times `x` overlaps `y`. 
#' This `GInteractions` will have additional metadata columns 
#' corresponding to the metadata from `y`. `find_overlaps_directed()` 
#' takes the strandness of each object into account. 
#' 
#' @section Pinned `GInteractions`:  
#' 
#' When using `find_overlaps()` with a `PinnedGInteractions` object, 
#' only the pinned anchors are used to check for overlap with `y`. 
#' This is equivalent to specifying `use.region="both"` in 
#' `InteractionSet::findOverlaps()`.
#' 
#' @param x A (Pinned)GInteractions object
#' @param y A GRanges object
#' @param maxgap,minoverlap See \code{?\link[GenomicRanges]{findOverlaps}} 
#' in the \pkg{GenomicRanges} package for a description of these arguments
#' @param suffix Suffix to add to metadata 
#' columns (character vector of length 2, default to `c(".x", ".y")`).
#'
#' @importFrom GenomicRanges granges
#' @importFrom plyranges find_overlaps
#' @importFrom plyranges find_overlaps_directed
#' @importFrom InteractionSet findOverlaps
#' 
#' @return a GInteractions object with rows corresponding to the GInteractions 
#' in `x` that overlap `y`.
#'
#' @name ginteractions-find-overlaps
#' 
#' @examples
#' gi <- read.table(text = "  
#'     chr1 11 20 - chr1 21 30 + 
#'     chr1 11 20 - chr1 51 55 + 
#'     chr1 21 30 - chr1 51 55 + 
#'     chr1 21 30 - chr2 51 60 +",  
#'     col.names = c(
#'         "seqnames1", "start1", "end1", "strand1", 
#'         "seqnames2", "start2", "end2", "strand2"
#'     )
#' ) |> as_ginteractions() |> mutate(id = 1:4, type = 'gi')
#' 
#' gr <- GenomicRanges::GRanges(
#'     c("chr1:20-30:+", "chr2:55-65:-")
#' ) |> plyranges::mutate(id = 1:2, type = 'gr')
#' 
#' gi
#' 
#' gr
#' 
#' ####################################################################
#' # 1. Find overlaps between GInteractions and a subject GRanges
#' ####################################################################
#' 
#' find_overlaps(gi, gr)
#' 
#' find_overlaps_directed(gi, gr)
#' 
#' ####################################################################
#' # 2. Find overlaps between PinnedGInteractions and a subject GRanges
#' ####################################################################
#' 
#' gi |> pin_by("first") |> find_overlaps(gr)
#' 
#' gi |> pin_by("second") |> find_overlaps(gr)
#' 
#' gi |> pin_by("first") |> find_overlaps_directed(gr)
#' 
#' gi |> pin_by("second") |> find_overlaps_directed(gr)
NULL

#' @rdname ginteractions-find-overlaps
#' @export
find_overlaps.PinnedGInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L, suffix = c(".x", ".y")
) {

    hits <- InteractionSet::findOverlaps( 
        query = y, 
        subject = unpin(x), 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        select = 'all', 
        ignore.strand = TRUE, 
        use.region = switch(
            as.character(pin(x)), 
            "1" = "first",
            "2" = "second"
        )
    )
    left <- unpin(x)[S4Vectors::subjectHits(hits), ]
    right <- y[S4Vectors::queryHits(hits), ]
    mcols(left) <- .mcols_overlaps_update(left, right, suffix)
    return(left)

}

#' @rdname ginteractions-find-overlaps
#' @export
find_overlaps.GInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L, suffix = c(".x", ".y")
) {

    hits <- InteractionSet::findOverlaps( 
        query = y, 
        subject = x, 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        select = 'all', 
        ignore.strand = TRUE, 
        use.region = 'both'
    )
    left <- x[S4Vectors::subjectHits(hits), ]
    right <- y[S4Vectors::queryHits(hits), ]
    mcols(left) <- .mcols_overlaps_update(left, right, suffix)
    return(left)
    
}

#' @rdname ginteractions-find-overlaps
#' @export
find_overlaps_directed.PinnedGInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L, suffix = c(".x", ".y")
) {

    hits <- InteractionSet::findOverlaps( 
        query = y, 
        subject = unpin(x), 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        select = 'all', 
        ignore.strand = FALSE, 
        use.region = switch(
            as.character(pin(x)), 
            "1" = "first",
            "2" = "second"
        )
    )
    left <- unpin(x)[S4Vectors::subjectHits(hits), ]
    right <- y[S4Vectors::queryHits(hits), ]
    mcols(left) <- .mcols_overlaps_update(left, right, suffix)
    return(left)

}

#' @rdname ginteractions-find-overlaps
#' @export
find_overlaps_directed.GInteractions <- function(
    x, y, maxgap = -1L, minoverlap = 0L, suffix = c(".x", ".y")
) {

    hits <- InteractionSet::findOverlaps( 
        query = y, 
        subject = x, 
        maxgap = maxgap, 
        minoverlap = minoverlap,
        type = 'any', 
        select = 'all', 
        ignore.strand = FALSE, 
        use.region = 'both'
    )
    left <- x[S4Vectors::subjectHits(hits), ]
    right <- y[S4Vectors::queryHits(hits), ]
    mcols(left) <-  .mcols_overlaps_update(left, right, suffix)
    return(left)
    
}

.mcols_overlaps_update <- function(
    left, right, suffix, return_data_frame = FALSE
) {

    left_names <- names(mcols(left))
    right_names <- names(mcols(right))
    common_name <- intersect(left_names, right_names)
    lname_inx <- left_names %in% common_name
    rname_inx <- right_names %in% common_name
    if (any(lname_inx)) {
        names(mcols(left))[lname_inx] <- paste0(
            left_names[lname_inx], suffix[1]
        )
    }

    if (any(rname_inx)) {
        names(mcols(right))[rname_inx] <- paste0(
            right_names[rname_inx], suffix[2]
        )
    }

    if (!is.null(mcols(left))) {
        additional_mcols <- mcols(left)
    } else {
        additional_mcols <- NULL
    }

    if (!is.null(mcols(right))) {
        if (is.null(additional_mcols)) {
        additional_mcols <- mcols(right)
        } else {
        additional_mcols <- cbind(additional_mcols, mcols(right))
        }
    }

    if (return_data_frame) {
        if (is(left, "GenomicRanges")) {
        ranges_df <- DataFrame(granges.x = GenomicRanges::granges(left),
                                granges.y = GenomicRanges::granges(right))
        } else {
        ranges_df <- DataFrame(ranges.x = ranges(left),
                                ranges.y = ranges(right))
        }
        names(ranges_df) <- paste0(gsub("\\..*", "" , names(ranges_df)), suffix)
        if (!is.null(additional_mcols)) {
        additional_mcols <- cbind(ranges_df, additional_mcols)
        } else {
        return(ranges_df)
        }
    }

    additional_mcols
}
