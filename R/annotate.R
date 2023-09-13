#' Annotate both anchors of a GInteractions
#'
#' @param x a GInteractions object
#' @param y a GRanges object to extract annotations from
#' @param label Column name from `y` to use to extract annotations
#' 
#' @return a GInteractions object with two extra metadata columns named 
#' `annotation_first` and `annotation_second`.
#'
#' @name ginteractions-annotate
#' @rdname ginteractions-annotate
#' 
#' @examples
#' data(GM12878_HiCCUPS, package = 'plyinteractions')
#' annots <- AnnotationHub::AnnotationHub()[['AH75190']]
#' annots <- lapply(annots, function(x) {seqlevelsStyle(x) <- "NCBI" ; x})
#' 
#' annotate(GM12878_HiCCUPS, annots$fullGenome, label = 'theRegion') |> 
#'    count(annotation_first, annotation_second) |> 
#'    as.data.frame() |> 
#'    dplyr::arrange(desc(n))
#' 
#' annotate(GM12878_HiCCUPS, annots$codingGenome, label = 'theRegion') |> 
#'    count(annotation_first, annotation_second) |> 
#'    as.data.frame() |> 
#'    dplyr::arrange(desc(n))
#' @export
setGeneric("annotate", function(x, y, label, ...) standardGeneric("annotate"))

#' @rdname ginteractions-annotate
#' @export
setMethod(
    "annotate", 
    signature(x = "GInteractions", y = "GRanges", label = "character"), 
    function(x, y, label) {

        cols <- colnames(mcols(x))
        pin_by(x, "first") |> 
            join_overlap_left(y) |> 
            rename(annotation_first = !!label) |>
            pin_by("second") |> 
            join_overlap_left(y) |> 
            rename(annotation_second = !!label) |> 
            select(cols, annotation_first, annotation_second)

    }
)
