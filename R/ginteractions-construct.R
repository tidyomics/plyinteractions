#' Construct a GInteractions object from a tibble, DataFrame or data.frame
#'
#' @description The `as_ginteractions` function looks for column names in 
#' .data called seqnames\{1,2\}, start\{1,2\}, end\{1,2\}, and strand\{1,2\} 
#' in order to construct a GInteractions object. 
#' By default other columns in .data are placed into the mcols (metadata
#' columns) slot of the returned object.
#'
#' @param .data A [data.frame()], [DataFrame()] or `tibble()` to
#' construct a GInteractions object from.
#' @param ... Optional named arguments specifying which the columns in .data
#' containin the core components a GInteractions object.
#' @param keep.extra.columns TRUE or FALSE (the default). If TRUE, the columns 
#' in df that are not used to form the genomic ranges of the returned GRanges 
#' object are then returned as metadata columns on the object. Otherwise, they 
#' are ignored.
#' @param starts.in.df.are.0based TRUE or FALSE (the default). If TRUE, then 
#' the start positions of the genomic ranges in df are considered to be 
#' 0-based and are converted to 1-based in the returned GRanges object. 
#'
#' @return a GInteractions object.
#' 
#' @seealso \code{\link[InteractionSet:GInteractions-class]{InteractionSet::GInteractions()}}
#'
#' @importFrom rlang quos
#' @importFrom rlang quo_name
#' @importFrom rlang eval_tidy
#' @importFrom InteractionSet GInteractions
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors mcols
#' @importFrom S4Vectors mcols<-
#' 
#' @rdname ginteractions-construct
#' 
#' @examples
#' ####################################################################
#' # 1. GInteractions from bedpe files imported into a data.frame
#' ####################################################################
#' 
#' bedpe <- read.table(text = "
#' chr1 100 200 chr1 5000 5100 bedpe_example1 30 + -
#' chr1 1000 5000 chr1 3000 3800 bedpe_example2 100 + -",
#' col.names = c(
#'   "chrom1", "start1", "end1", 
#'   "chrom2", "start2", "end2", "name", "score", "strand1", "strand2"))
#' bedpe |> 
#'   as_ginteractions(seqnames1 = chrom1, seqnames2 = chrom2)
#' 
#' ####################################################################
#' # 2. GInteractions from standard pairs files imported into a data.frame
#' ####################################################################
#' 
#' # Note how the pairs are 0-based and no "end" field is provided 
#' # (the standard pairs file format does not have "end" fields)
#' # We can provide width1 and width2 to fix this problem. 
#' 
#' pairs <- read.table(text = "
#' pair1 chr1 10000 chr1 20000 + +
#' pair2 chr1 50000 chr1 70000 + +
#' pair3 chr1 60000 chr2 10000 + +
#' pair4 chr1 30000 chr3 40000 + -", 
#' col.names = c(
#'   "pairID", "chr1", "pos1", "chr2", "pos2", "strand1", "strand2")
#' )
#' pairs |> 
#'   as_ginteractions(
#'     seqnames1 = chr1, start1 = pos1, width1 = 1000, 
#'     seqnames2 = chr2, start2 = pos2, width2 = 1000, 
#'     starts.in.df.are.0based = TRUE
#'   )
#' 
#' ####################################################################
#' # 3. GInteractions from data.frame with extra fields
#' ####################################################################
#' 
#' df <- read.table(text = "
#' chr1 100 200 chr1 5000 5100
#' chr1 1000 5000 chr1 3000 3800",
#' col.names = c("chr1", "start1", "end1", "chr2", "start2", "end2"))
#' df |> 
#'   as_ginteractions(seqnames1 = chr1, seqnames2 = chr2)
#' 
#' df <- read.table(text = "
#' chr1 100 200 chr1 5000 5100
#' chr1 1000 5000 chr1 3000 3800",
#' col.names = c("chr1", "start1", "end1", "chr2", "start2", "end2"))
#' df |> 
#'   as_ginteractions(
#'     seqnames1 = chr1, seqnames2 = chr2, strand1 = '+', strand2 = '-'
#'   )
#' 
#' data.frame(type = "cis", count = 3) |> 
#'   as_ginteractions(
#'     seqnames1 = 'chr1', start1 = 1, end1 = 10,
#'     seqnames2 = 'chr1', start2 = 40, end2 = 50
#'   )
#' 
#' ####################################################################
#' # 4. GInteractions from a real like pairs files
#' ####################################################################
#' 
#' pairsf <- system.file('extdata', 'pairs.gz', package = 'plyinteractions')
#' pairs <- read.table(pairsf, comment.char = '#', header = FALSE)
#' head(pairs)
#' pairs |> 
#'   as_ginteractions(
#'     seqnames1 = V2, start1 = V3, width1 = 1, strand1 = V6, 
#'     seqnames2 = V4, start2 = V5, width2 = 1, strand2 = V7,
#'     starts.in.df.are.0based = TRUE
#'   )
#' @export
as_ginteractions <- function(
    .data, ..., keep.extra.columns = TRUE, starts.in.df.are.0based = FALSE
) UseMethod("as_ginteractions")

#' @export
as_ginteractions.default <- function(
    .data, ..., keep.extra.columns = TRUE, starts.in.df.are.0based = FALSE
) {
    as_ginteractions.data.frame(
        as.data.frame(.data), ..., keep.extra.columns, starts.in.df.are.0based
    )
}

#' @export
as_ginteractions.data.frame <- function(
    .data, ..., keep.extra.columns = TRUE, starts.in.df.are.0based = FALSE
) {

    quosures <- rlang::quos(...)
    col_names <- names(.data)

    # Check that named arguments are only here to define bare GI, not mcols
    valid_names <- c("seqnames1", "start1", "end1", "width1", "strand1", 
        "seqnames2", "start2", "end2", "width2", "strand2")
    .check_allowed_names(quosures, valid_names)

    # Parse quosures
    if (length(quosures) > 0) {
        rd <- lapply(quosures, rlang::eval_tidy,  data = .data)
    } else {
        rd <- NULL
    }

    # Check that all required fields are found: 
    required_names <- c("seqnames1", "start1", "seqnames2", "start2")
    for (name in required_names) {
        if (!(any(name %in% c(names(rd), names(.data))))) {
            stop(name, "column is required for GInteractions.", call. = FALSE)
        }
    }
    
    # Look for end*/width* fields
    for (name in c('end1', 'end2')) {
        if (!(any(name %in% c(names(rd), names(.data))))) {
            if (!(any(
                    gsub("end", "width", name) %in% c(names(rd), names(.data))
                ))) {
                stop(
                    "Please provide end/width information.", 
                    call. = FALSE
                )
            }
        }
    }

    # If strands are not provided, add "*" by default
    for (name in c("strand1", "strand2")) {
        if (!(any(name %in% c(names(rd), names(.data))))) {
            rd[[name]] <- "*"
        }
    }

    # First generate quos for core parts of class
    core_gi <- rlang::quos(
        seqnames1 = .data$seqnames1, start1 = .data$start1, end1 = .data$end1, 
        width1 = .data$width1, strand1 = .data$strand1,
        seqnames2 = .data$seqnames2, start2 = .data$start2, end2 = .data$end2, 
        width2 = .data$width2, strand2 = .data$strand2
    )

    # Then modify the core_gi using rd
    gi <- .gi_construct(.data, rd, col_names, core_gi, starts.in.df.are.0based)

    # Add extra columns from .data 
    if (keep.extra.columns) {
        return(.make_mcols(.data, gi, col_names, quosures, core_gi))
    }
    gi
}

.gi_construct <- function(
    .data, rd, col_names, core_gi, starts.in.df.are.0based
) {

    ## Check that all required columns are found, either in .data or ...
    match_cols_i <- names(core_gi) %in% col_names
    match_quosures_i <- names(core_gi) %in% names(rd)
    if (sum(c(match_cols_i, match_quosures_i)) < 8) {
        stop("Unable to construct GInteractions from .data. 
            Specify the column name used for each required field: 
            seqnames1, start1, end/wdith1, strand1, 
            seqnames2, start2, end/wdith2, strand2",
                call. = FALSE)
    }

    ## Build GInteractions
    remain_cols <- match_cols_i & !match_quosures_i
    remain_core <- core_gi[remain_cols]
    if (length(remain_core) > 0) {
        gi <- lapply(core_gi[match_cols_i], rlang::eval_tidy, data = .data)
        gi <- c(gi, rd[names(rd) %in% names(core_gi)])
    } else {
        gi <- rd[names(rd) %in% names(core_gi)]
    }

    if (starts.in.df.are.0based) 
        gi[['start1']] <- gi[['start1']] + 1
    if (!is.null(gi[['width1']]) & is.null(gi[['end1']])) 
        gi[['end1']] <- gi[['start1']] + gi[['width1']] - 1
    an1 <- GenomicRanges::GRanges(
        seqnames = gi$seqnames1, strand = gi$strand1, range = IRanges::IRanges(
            start = gi$start1, end = gi$end1
        )
    )
    if (starts.in.df.are.0based) 
        gi[['start2']] <- gi[['start2']] + 1
    if (!is.null(gi[['width2']]) & is.null(gi[['end2']])) 
        gi[['end2']] <- gi[['start2']] + gi[['width2']] - 1
    an2 <- GenomicRanges::GRanges(
        seqnames = gi$seqnames2, strand = gi$strand2, range = IRanges::IRanges(
            start = gi$start2, end = gi$end2
        )
    )
    gi <- InteractionSet::GInteractions(an1, an2)

    return(gi)

}

.make_mcols <- function(.data, gi, col_names, quosures, core) {
    old_cols <- unlist(lapply(quosures, rlang::quo_name))
    remain_cols <- !(col_names %in% c(old_cols, names(core)))
    if (any(remain_cols)) {
        S4Vectors::mcols(gi) <- .data[, remain_cols, drop = FALSE]
        names(S4Vectors::mcols(gi)) <- col_names[remain_cols]
    }
    gi
}
