test_that("setter functions work", {
    
    set_seqnames1(gi, factor("chr2", c("chr1", "chr2"))) |> expect_no_error()
    set_seqnames2(gi, factor("chr2", c("chr1", "chr2"))) |> expect_no_error()
    set_start1(gi, 1) |> expect_no_error()
    set_start2(gi, 2) |> expect_no_error()
    set_end1(gi, 100) |> expect_no_error()
    set_end2(gi, 100) |> expect_no_error()
    set_width1(gi, 10) |> expect_no_error()
    set_width2(gi, 10) |> expect_no_error()
    set_strand1(gi, '*') |> expect_no_error()
    set_strand2(gi, c('-', '+', '-', '*')) |> expect_no_error()

})
