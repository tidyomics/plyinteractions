## Ripped from InteractionSet to add the strand{12} to the print
setMethod("show", "GInteractions", function(object){
    .showGInteractions(
        object, margin="  ", print.seqinfo=TRUE, print.classinfo=TRUE
    )
})

#' @importFrom GenomeInfoDb seqinfo
.showGInteractions <- function(
    x, margin="", print.seqinfo=FALSE, print.classinfo=FALSE
) {
    lx <- length(x)
    nr <- length(InteractionSet::regions(x))
    nc <- ncol(mcols(x))
    if (is.null(nc)) {
        nc <- 0L
    }
    cat(class(x), " object with ",
        lx, " ", ifelse(lx == 1L, "interaction", "interactions"), " and ",
        nc, " metadata ", ifelse(nc == 1L, "column", "columns"),
        ":\n", sep="")
    out <- S4Vectors::makePrettyMatrixForCompactPrinting(
        x, .makeNakedMatFromGInteractions
    )
    if (print.classinfo) { 
        .COL2CLASS <- c(
            seqnames1 = "Rle", ranges1 = "IRanges", strand1 = "Rle", "   "="", 
            seqnames2="Rle", ranges2="IRanges", strand2 = "Rle"
        )
        classinfo <- S4Vectors::makeClassinfoRowForCompactPrinting(
            x, .COL2CLASS
        )
        classinfo[,"   "] <- ""
        stopifnot(identical(colnames(classinfo), colnames(out)))
        out <- rbind(classinfo, out)
    }

    if (nrow(out) != 0L) {
        rownames(out) <- paste0(margin, rownames(out))
    }

    print(out, quote=FALSE, right=TRUE, max=length(out))
    if (print.seqinfo) {
        cat(margin, "-------\n", sep="")
        ncr <- ncol(mcols(InteractionSet::regions(x)))
        if (is.null(ncr)) {
            ncr <- 0L
        }
        cat(
            margin, "regions: ", nr, " ranges and ", ncr, 
            " metadata ", ifelse(ncr==1L, "column", "columns"), "\n", sep=""
        )
        cat(
            margin, "seqinfo: ", 
            summary(GenomeInfoDb::seqinfo(x)), "\n", sep=""
        )
    }
}

#' @importFrom InteractionSet anchors
#' @importFrom InteractionSet regions
#' @importFrom S4Vectors showAsCell
.makeNakedMatFromGInteractions <- function(x) {
    lx <- length(x)
    nc <- ncol(mcols(x))
    if (is.null(nc)) {
        nc <- 0L
    }
    ans <- cbind(
        .pasteAnchor(InteractionSet::anchors(x, type="first"), append="1"),
        "   "=rep.int("---", lx),
        .pasteAnchor(InteractionSet::anchors(x, type="second"), append="2")
    )
    if (nc > 0L) {
        tmp <- do.call(
            data.frame, 
            c(lapply(mcols(x), S4Vectors::showAsCell), list(check.names=FALSE))
        )
        ans <- cbind(ans, `|`=rep.int("|", lx), as.matrix(tmp))
    }
    ans
}

#' @importFrom GenomeInfoDb seqnames
#' @importFrom IRanges ranges
#' @importFrom BiocGenerics strand
.pasteAnchor <- function(x, append) {
    out <- cbind(
        as.character(GenomeInfoDb::seqnames(x)), 
        S4Vectors::showAsCell(IRanges::ranges(x)), 
        as.character(BiocGenerics::strand(x))
    )
    colnames(out) <- paste0(c("seqnames", "ranges", "strand"), append)
    out
}
