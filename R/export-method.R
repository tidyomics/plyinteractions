#' @title GInteractions export methods
#' 
#' @name export-methods
#' @rdname export-methods
#' @aliases export
#' @aliases export,GInteractions,missing,character-method
#' @description 
#' 
#' Export methods to save a GInteractions object
#' @param object A GInteractions object 
#' @param prefix Prefix used when generating output file(s).
#' @param format File format. Available: `cool` and `HiC-Pro`.
#' @param ... Extra arguments to use when exporting to `cool`. 
#' Can be `metadata <string>` or `chunksize <integer>`.
#' @return Path to saved files
#' @importFrom BiocIO export
#' @examples
#' gi <- read.table(text = "
#' chr1 11 20 chr1 21 30 + +
#' chr1 11 20 chr1 51 55 + +
#' chr1 11 30 chr1 51 55 - -
#' chr1 11 30 chr2 51 60 - -",
#' col.names = c(
#'   "seqnames1", "start1", "end1", 
#'   "seqnames2", "start2", "end2", "strand1", "strand2")
#' ) |> 
#'   as_ginteractions() |> 
#'   mutate(score = runif(4), type = c('cis', 'cis', 'cis', 'trans'))
#' 
#' ################################################################
#' ## 1. Export GInteractions 
#' ################################################################
#' 
#' export(gi, prefix = 'subset_chrII', format = 'cool')
#' export(hic["II"], prefix = 'subset_chrII', format = 'HiC-Pro')
NULL

#' @exportMethod export 
#' @rdname export-methods

setMethod("export", signature(object = "GInteractions", con = "missing", format = "character"),
    function(object, prefix, format, ...) {

        params <- list(...)

        if (format == 'HiC-Pro') {

            out_regions <- paste0(prefix, "_regions.bed")
            out_matrix <- paste0(prefix, "_matrix.mtx")
            b <- bins(object)
            gi <- interactions(object)
            .f <- .writeHicpro(b, gi, out_regions, out_matrix)

            ## -- Return HicproFile object
            res <- HicproFile(path = out_matrix, bed = out_regions)
            return(res)

        }

        else if (format == 'cool') {

            out_cool <- paste0(prefix, ".cool")
            re <- regions(object)
            b <- as.data.frame(bins(object))
            gi <- interactions(object)

            ## -- Estimate chunksize for pixels
            if ('chunksize' %in% names(params)) {
                chunksize <- params[['chunksize']]
            } else {
                chunksize <- ceiling(sqrt(length(gi)))
                chunksize <- max(1000, chunksize)
            }

            ## -- Get metadata
            if ('metadata' %in% names(params)) {
                metadata <- params[['metadata']]
            } else {
                metadata <- "{}"
            }

            ## -- Create info 
            info <- list(
                "bin-size"= resolution(object),
                "bin-type"= "fixed",
                "creation-date"= gsub(' ', 'T', as.character(Sys.time())),
                "format"= "HDF5::Cooler",
                "format-url"= "https://github.com/open2c/cooler",
                "format-version"= "3",
                "generated-by"= paste0('HiCExperiment-', packageVersion('HiCExperiment')),
                "genome-assembly"= "unknown",
                "metadata" = metadata,
                "nbins"= nrow(b),
                "nchroms"= length(GenomeInfoDb::seqlevels(re)),
                "nnz"= length(interactions(object)),
                "storage-mode"= "symmetric-upper",
                "sum"= sum(scores(object, 'count'))
            )

            ## -- Create Chroms table
            chroms <- data.frame(
                seqnames = GenomeInfoDb::seqlevels(re), 
                seqlengths = GenomeInfoDb::seqlengths(re)
            )
            colnames(chroms) <- c("name", "length")

            ## -- Create Bins table
            bins <- data.frame(
                chrom = as.character(b$seqnames), 
                start = as.integer(b$start - 1),
                end = as.integer(b$end)
            )
            if ('weight' %in% colnames(b)) {
                bins$weight <- b$weight
            }
            else {
                bins$weight <- 1
            }
            bins <- bins[, c("chrom", "end", "start", "weight")]

            ## -- Create Pixels table
            pixels <- data.frame(gi$bin_id1, gi$bin_id2, gi$count)
            colnames(pixels) <- c("bin1_id", "bin2_id", "count")

            ## -- Aggregate all data 
            .f <- .writeCool(info, bins, chroms, pixels, chunksize, out_cool)

            ## -- Return CoolFile object
            res <- CoolFile(.f)
            return(res)

        }

    }
)
