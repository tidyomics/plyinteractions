test_that("getter functions work", {
    
    seqnames1(gi) |> expect_s4_class("Rle")
    start1(gi) |> expect_type("integer")
    end1(gi) |> expect_type("integer")
    width1(gi) |> expect_type("integer")
    anchors1(gi) |> expect_s4_class("GRanges")
    strand1(gi) |> expect_s4_class("Rle")
    ranges1(gi) |> expect_s4_class("IRanges")
    seqnames2(gi) |> expect_s4_class("Rle")
    start2(gi) |> expect_type("integer")
    end2(gi) |> expect_type("integer")
    width2(gi) |> expect_type("integer")
    anchors2(gi) |> expect_s4_class("GRanges")
    strand2(gi) |> expect_s4_class("Rle")
    ranges2(gi) |> expect_s4_class("IRanges")

})
