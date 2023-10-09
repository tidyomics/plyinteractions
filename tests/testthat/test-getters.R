test_that("getter functions work", {
    
    expect_identical(
        seqnames1(gi), 
        new("Rle", values = structure(1L, levels = c("chr1", "chr2"), class = "factor"), 
            lengths = 4L, elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        start1(gi), 
        c(11L, 11L, 11L, 11L)
    )
    expect_identical(
        end1(gi), 
        c(20L, 20L, 30L, 30L)
    )
    expect_identical(
        width1(gi), 
        c(10L, 10L, 20L, 20L)
    )
    expect_identical(
        anchors1(gi), 
        new("GRanges", seqnames = new("Rle", values = structure(1L, levels = c("chr1", 
        "chr2"), class = "factor"), lengths = 4L, elementMetadata = NULL, 
            metadata = list()), ranges = new("IRanges", start = c(11L, 
        11L, 11L, 11L), width = c(10L, 10L, 20L, 20L), NAMES = NULL, 
            elementType = "ANY", elementMetadata = NULL, metadata = list()), 
            strand = new("Rle", values = structure(1:2, levels = c("+", 
            "-", "*"), class = "factor"), lengths = c(2L, 2L), elementMetadata = NULL, 
                metadata = list()), seqinfo = new("Seqinfo", seqnames = c("chr1", 
            "chr2"), seqlengths = c(NA_integer_, NA_integer_), is_circular = c(NA, 
            NA), genome = c(NA_character_, NA_character_)), elementMetadata = new("DFrame", 
                rownames = NULL, nrows = 4L, elementType = "ANY", elementMetadata = NULL, 
                metadata = list(), listData = structure(list(), names = character(0))), 
            elementType = "ANY", metadata = list())
    )
    expect_identical(
        strand1(gi), 
        new("Rle", values = structure(1:2, levels = c("+", "-", "*"), class = "factor"), 
            lengths = c(2L, 2L), elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        ranges1(gi), 
        new("IRanges", start = c(11L, 11L, 11L, 11L), width = c(10L, 
        10L, 20L, 20L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list())
    )
    expect_identical(
        seqnames2(gi), 
        new("Rle", values = structure(1:2, levels = c("chr1", "chr2"), class = "factor"), 
            lengths = c(3L, 1L), elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        start2(gi), 
        c(21L, 51L, 51L, 51L)
    )
    expect_identical(
        end2(gi), 
        c(30L, 55L, 55L, 60L)
    )
    expect_identical(
        width2(gi), 
        c(10L, 5L, 5L, 10L)
    )
    expect_identical(
        anchors2(gi), 
        new("GRanges", seqnames = new("Rle", values = structure(1:2, levels = c("chr1", 
        "chr2"), class = "factor"), lengths = c(3L, 1L), elementMetadata = NULL, 
            metadata = list()), ranges = new("IRanges", start = c(21L, 
        51L, 51L, 51L), width = c(10L, 5L, 5L, 10L), NAMES = NULL, elementType = "ANY", 
            elementMetadata = NULL, metadata = list()), strand = new("Rle", 
            values = structure(1:2, levels = c("+", "-", "*"), class = "factor"), 
            lengths = c(2L, 2L), elementMetadata = NULL, metadata = list()), 
            seqinfo = new("Seqinfo", seqnames = c("chr1", "chr2"), seqlengths = c(NA_integer_, 
            NA_integer_), is_circular = c(NA, NA), genome = c(NA_character_, 
            NA_character_)), elementMetadata = new("DFrame", rownames = NULL, 
                nrows = 4L, elementType = "ANY", elementMetadata = NULL, 
                metadata = list(), listData = structure(list(), names = character(0))), 
            elementType = "ANY", metadata = list())
    )
    expect_identical(
        strand2(gi), 
        new("Rle", values = structure(1:2, levels = c("+", "-", "*"), class = "factor"), 
        lengths = c(2L, 2L), elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        ranges2(gi), 
        new("IRanges", start = c(21L, 51L, 51L, 51L), width = c(10L, 
        5L, 5L, 10L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list())
    )

})
