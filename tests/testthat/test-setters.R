test_that("setter functions work", {
    
    expect_identical(
        set_seqnames1(gi, factor("chr2", c("chr1", "chr2"))) |> seqnames1(), 
        new("Rle", values = structure(2L, levels = c("chr1", "chr2"), class = "factor"), 
            lengths = 4L, elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        set_seqnames2(gi, factor("chr2", c("chr1", "chr2"))) |> seqnames2(), 
        new("Rle", values = structure(2L, levels = c("chr1", "chr2"), class = "factor"), 
            lengths = 4L, elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        set_start1(gi, 1) |> start1(), 
        c(1L, 1L, 1L, 1L)
    )
    expect_identical(
        set_start2(gi, 2) |> start2(), 
        c(2L, 2L, 2L, 2L)
    )
    expect_identical(
        set_end1(gi, 10) |> end1(), 
        c(10L, 10L, 10L, 10L)
    )
    expect_identical(
        set_end2(gi, 200) |> end2(), 
        c(200L, 200L, 200L, 200L)
    )
    expect_identical(
        set_width1(gi, 10) |> width1(), 
        c(10L, 10L, 10L, 10L)
    )
    expect_identical(
        set_width2(gi, 200) |> width2(), 
        c(200L, 200L, 200L, 200L)
    )
    expect_identical(
        set_strand1(gi, c('-', '+', '-', '*')) |> strand1(), 
        new("Rle", values = structure(c(2L, 1L, 2L, 3L), levels = c("+", 
        "-", "*"), class = "factor"), lengths = c(1L, 1L, 1L, 1L), elementMetadata = NULL, 
            metadata = list())
    )
    expect_identical(
        set_strand2(gi, "*") |> strand2(), 
        new("Rle", values = structure(3L, levels = c("+", "-", "*"), class = "factor"), 
            lengths = 4L, elementMetadata = NULL, metadata = list())
    )

})
