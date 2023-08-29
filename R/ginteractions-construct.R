#' Construct a GInteractions object from a tibble, DataFrame or data.frame
#'
#' @description The `as_ginteractions` function looks for column names in 
#' .data called seqnames\{1,2\}, start\{1,2\}, end\{1,2\}, and strand\{1,2\} 
#' in order to construct a GInteractions object. 
#' By default other columns in .data are placed into the mcols (metadata
#' columns) slot of the returned object.
#'
#' @param .data a [data.frame()], [DataFrame()] or `tibble()` to
#' construct a GInteractions object from.
#' @param ... Optional named arguments specifying which the columns in .data
#' containin the core components a GInteractions object.
#' @param keep_mcols place the remaining columns into the metadata columns slot
#' (default=TRUE).
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
#' # 1. GInteractions from standard pairs files imported into a data.frame
#' ####################################################################
#' 
#' pairs <- read.table(text = "
#' EAS139:136:FC706VJ:2:2104:23462:197393 chr1 10000 chr1 20000 + +
#' EAS139:136:FC706VJ:2:8762:23765:128766 chr1 50000 chr1 70000 + +
#' EAS139:136:FC706VJ:2:2342:15343:9863 chr1 60000 chr2 10000 + +
#' EAS139:136:FC706VJ:2:1286:25:275154 chr1 30000 chr3 40000 + -", 
#' col.names = c("readID", "chr1", "pos1", "chr2", "pos2", "strand1", "strand2"))
#' as_ginteractions(
#'   pairs, seqnames1 = chr1, start1 = pos1, end1 = pos1, 
#'   seqnames2 = chr2, start2 = pos2, end2 = pos2
#' )
#' 
#' ####################################################################
#' # 2. GInteractions from bedpe files imported into a data.frame
#' ####################################################################
#' 
#' bedpe <- read.table(text = "
#' chr1	100	200	chr1	5000	5100	bedpe_example1	30	+	-
#' chr1	1000	5000	chr1	3000	3800	bedpe_example2	100	+	-",
#' col.names = c(
#'   "chrom1", "start1", "end1", 
#'   "chrom2", "start2", "end2", "name", "score", "strand1", "strand2"))
#' as_ginteractions(bedpe, seqnames1 = chrom1, seqnames2 = chrom2)
#' 
#' ####################################################################
#' # 3. GInteractions from data.frame with extra fields
#' ####################################################################
#' 
#' df <- read.table(text = "
#' chr1	100	200	chr1	5000	5100
#' chr1	1000	5000	chr1	3000	3800",
#' col.names = c("chr1", "start1", "end1", "chr2", "start2", "end2"))
#' as_ginteractions(df, seqnames1 = chr1, seqnames2 = chr2)
#' 
#' df <- read.table(text = "
#' chr1	100	200	chr1	5000	5100
#' chr1	1000	5000	chr1	3000	3800",
#' col.names = c("chr1", "start1", "end1", "chr2", "start2", "end2"))
#' as_ginteractions(df, seqnames1 = chr1, seqnames2 = chr2, strand1 = '+', strand2 = '-')
#' 
#' df <- data.frame(type = "cis", count = 3)
#' as_ginteractions(
#'   df, 
#'   seqnames1 = 'chr1', start1 = 1, end1 = 10,
#'   seqnames2 = 'chr1', start2 = 40, end2 = 50
#' )
#' @export
as_ginteractions <- function(.data, ..., keep_mcols = TRUE) UseMethod("as_ginteractions")

#' @export
as_ginteractions.default <- function(.data, ..., keep_mcols = TRUE) {
    as_ginteractions.data.frame(as.data.frame(.data), ...)
}

#' @export
as_ginteractions.data.frame <- function(.data, ..., keep_mcols = TRUE) {

    # Check and parse quosures
    dots <- rlang::quos(...)
    col_names <- names(.data)
    valid_names <- c("seqnames1", "start1", "end1", 
        "seqnames2", "start2", "end2", "strand1", "strand2")
    .check_names(dots, valid_names)
    if (length(dots) > 0) {
        rd <- lapply(dots, rlang::eval_tidy,  data = .data)
    } else {
        rd <- NULL
    }

    # Check that all required fields are found: 
    required_names <- c("seqnames1", "start1", "end1", 
        "seqnames2", "start2", "end2")
    for (name in required_names) {
        if (!(any(name %in% names(rd)) | any(name %in% names(.data)))) {
            stop(paste0(name, " column is required for GInteractions."), call. = FALSE)
        }
    }

    # If strands are not provided, add "*" by default
    for (name in c("strand1", "strand2")) {
        if (!(any(name %in% names(rd)) | any(name %in% names(.data)))) {
            rd[[name]] <- "*"
        }
    }

    # GInteractions constructor generate quos for core parts of class
    core_gi <- rlang::quos(
        seqnames1 = .data$seqnames1, start1 = .data$start1, end1 = .data$end1, 
        strand1 = .data$strand1,
        seqnames2 = .data$seqnames2, start2 = .data$start2, end2 = .data$end2, 
        strand2 = .data$strand2
    )
    gi <- .gi_construct(.data, rd, col_names, core_gi)

    if (keep_mcols) {
        return(.make_mcols(.data, gi, col_names, dots, core_gi))
    }
    gi
}

.check_names <- function(dots, valid_names) {
    if (length(dots) > 0) {
        valid_args <- names(dots) %in% valid_names
        if (any(!valid_args)) {
        stop(paste("Named arguments must be ",
                    paste(valid_names, collapse = ", "), "."),
            .call = FALSE)
        }
    }
}

.gi_construct <- function(.data, rd, col_names, core_gi) {

    ## Check that all required columns are found, either in .data or ...
    match_cols_i <- names(core_gi) %in% col_names
    match_dots_i <- names(core_gi) %in% names(rd)
    if (sum(c(match_cols_i, match_dots_i)) < 8) {
        stop("Unable to construct GInteractions from .data. 
            Specify the column name used for each required field: 
            seqnames1, start1, end1, strand1, seqnames2, start2, end2, strand2",
                call. = FALSE)
    }

    ## Build GInteractions
    remain_cols <- match_cols_i & !match_dots_i
    remain_core <- core_gi[remain_cols]
    if (length(remain_core) > 0) {
        gi <- lapply(core_gi[match_cols_i], rlang::eval_tidy, data = .data)
        gi <- c(gi, rd[names(rd) %in% names(core_gi)])
    } else {
        gi <- rd[names(rd) %in% names(core_gi)]
    }

    an1 <- GenomicRanges::GRanges(
        seqnames = gi$seqnames1, strand = gi$strand1, range = IRanges::IRanges(
            start = gi$start1, end = gi$end1
        )
    )
    an2 <- GenomicRanges::GRanges(
        seqnames = gi$seqnames2, strand = gi$strand2, range = IRanges::IRanges(
            start = gi$start2, end = gi$end2
        )
    )
    gi <- InteractionSet::GInteractions(an1, an2)

  return(gi)

}

.make_mcols <- function(.data, gi, col_names, dots, core) {
    old_cols <- unlist(lapply(dots, rlang::quo_name))
    remain_cols <- !(col_names %in% c(old_cols, names(core)))
    if (any(remain_cols)) {
        S4Vectors::mcols(gi) <- .data[, remain_cols, drop = FALSE]
        names(S4Vectors::mcols(gi)) <- col_names[remain_cols]
    }
    gi
}
