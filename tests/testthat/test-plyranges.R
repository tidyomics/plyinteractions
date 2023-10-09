test_that("plyranges functions work", {
    
    ## Flanking
    gi |> flank_left(2) |> expect_error()
    gi |> anchors1() |> flank_left(2) |> expect_s4_class("GRanges")
    gi |> anchors1() |> 
        flank_upstream(2) |> flank_downstream(2) |> 
        flank_left(2) |> flank_right(-2) |> 
        expect_s4_class('GRanges')
    expect_identical(
        gi |> anchors1() |> 
            flank_upstream(2) |> flank_downstream(2) |> 
            flank_left(2) |> flank_right(-2), 
        new("GRanges", seqnames = new("Rle", values = structure(1L, levels = c("chr1", 
            "chr2"), class = "factor"), lengths = 4L, elementMetadata = NULL, 
                metadata = list()), ranges = new("IRanges", start = c(9L, 
            9L, 27L, 27L), width = c(2L, 2L, 2L, 2L), NAMES = NULL, elementType = "ANY", 
                elementMetadata = NULL, metadata = list()), strand = new("Rle", 
                values = structure(1:2, levels = c("+", "-", "*"), class = "factor"), 
                lengths = c(2L, 2L), elementMetadata = NULL, metadata = list()), 
                seqinfo = new("Seqinfo", seqnames = c("chr1", "chr2"), seqlengths = c(NA_integer_, 
                NA_integer_), is_circular = c(NA, NA), genome = c(NA_character_, 
                NA_character_)), elementMetadata = new("DFrame", rownames = NULL, 
                    nrows = 4L, elementType = "ANY", elementMetadata = NULL, 
                    metadata = list(), listData = structure(list(), names = character(0))), 
                elementType = "ANY", metadata = list()),  
        tolerance = 1e-4
    )
    expect_identical(
        gi |> pin_by("first") |> 
            flank_upstream(2) |> flank_downstream(2) |> 
            flank_left(2) |> flank_right(-2) |> 
            pin_by("second") |> flank_downstream(2), 
        new("PinnedGInteractions", pin = 2L, delegate = new("GInteractions", 
            anchor1 = c(1L, 1L, 4L, 4L), anchor2 = c(2L, 3L, 5L, 6L), 
            regions = new("GRanges", seqnames = new("Rle", values = structure(1:2, levels = c("chr1", 
            "chr2"), class = "factor"), lengths = c(5L, 1L), elementMetadata = NULL, 
                metadata = list()), ranges = new("IRanges", start = c(9L, 
            31L, 56L, 27L, 49L, 49L), width = c(2L, 2L, 2L, 2L, 2L, 2L
            ), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
                metadata = list()), strand = new("Rle", values = structure(1:2, levels = c("+", 
            "-", "*"), class = "factor"), lengths = c(3L, 3L), elementMetadata = NULL, 
                metadata = list()), seqinfo = new("Seqinfo", seqnames = c("chr1", 
            "chr2"), seqlengths = c(NA_integer_, NA_integer_), is_circular = c(NA, 
            NA), genome = c(NA_character_, NA_character_)), elementMetadata = new("DFrame", 
                rownames = NULL, nrows = 6L, elementType = "ANY", elementMetadata = NULL, 
                metadata = list(), listData = structure(list(), names = character(0))), 
                elementType = "ANY", metadata = list()), NAMES = NULL, 
            elementMetadata = new("DFrame", rownames = NULL, nrows = 4L, 
                elementType = "ANY", elementMetadata = NULL, metadata = list(), 
                listData = list(score = c(0.113703411305323, 0.622299404814839, 
                0.609274732880294, 0.623379441676661), type = c("cis", 
                "cis", "cis", "trans"))), metadata = list())), 
        tolerance = 1e-4
    )

    ## Shift
    gi |> shift_left(2) |> expect_error()
    gi |> pin_by("first") |> 
        shift_upstream(2) |> shift_downstream(2) |> 
        shift_left(2) |> shift_right(2) |> 
        pin_by("second") |> shift_downstream(2) |> 
        expect_s4_class('PinnedGInteractions')
    expect_identical(
        gi |> pin_by("first") |> 
            shift_upstream(2) |> shift_downstream(2) |> 
            shift_left(2) |> shift_right(2) |> 
            pin_by("second") |> shift_downstream(2), 
        new("PinnedGInteractions", pin = 2L, delegate = new("GInteractions", 
            anchor1 = c(1L, 1L, 4L, 4L), anchor2 = c(2L, 3L, 5L, 6L), 
            regions = new("GRanges", seqnames = new("Rle", values = structure(1:2, levels = c("chr1", 
            "chr2"), class = "factor"), lengths = c(5L, 1L), elementMetadata = NULL, 
                metadata = list()), ranges = new("IRanges", start = c(11L, 
            23L, 53L, 11L, 49L, 49L), width = c(10L, 10L, 5L, 20L, 5L, 
            10L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
                metadata = list()), strand = new("Rle", values = structure(1:2, levels = c("+", 
            "-", "*"), class = "factor"), lengths = c(3L, 3L), elementMetadata = NULL, 
                metadata = list()), seqinfo = new("Seqinfo", seqnames = c("chr1", 
            "chr2"), seqlengths = c(NA_integer_, NA_integer_), is_circular = c(NA, 
            NA), genome = c(NA_character_, NA_character_)), elementMetadata = new("DFrame", 
                rownames = NULL, nrows = 6L, elementType = "ANY", elementMetadata = NULL, 
                metadata = list(), listData = structure(list(), names = character(0))), 
                elementType = "ANY", metadata = list()), NAMES = NULL, 
            elementMetadata = new("DFrame", rownames = NULL, nrows = 4L, 
                elementType = "ANY", elementMetadata = NULL, metadata = list(), 
                listData = list(score = c(0.113703411305323, 0.622299404814839, 
                0.609274732880294, 0.623379441676661), type = c("cis", 
                "cis", "cis", "trans"))), metadata = list())), 
        tolerance = 1e-4
    )
    gi |> anchors1() |> shift_left(2) |> expect_s4_class("GRanges")
    gi |> anchors1() |> 
        shift_upstream(2) |> shift_downstream(2) |> 
        shift_left(2) |> shift_right(2) |> 
        expect_s4_class('GRanges')
    expect_identical(
        gi |> anchors1() |> 
            shift_upstream(2) |> shift_downstream(2) |> 
            shift_left(2) |> shift_right(2), 
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
            elementType = "ANY", metadata = list())    )
    
    ## Stretch
    gi |> stretch(2) |> expect_error()
    expect_identical(
        pgi |> stretch(2) |> anchors2(), 
        new("GRanges", seqnames = new("Rle", values = structure(1:2, levels = c("chr1", 
        "chr2"), class = "factor"), lengths = c(3L, 1L), elementMetadata = NULL, 
            metadata = list()), ranges = new("IRanges", start = c(20L, 
        50L, 50L, 50L), width = c(12L, 7L, 7L, 12L), NAMES = NULL, elementType = "ANY", 
            elementMetadata = NULL, metadata = list()), strand = new("Rle", 
            values = structure(1:2, levels = c("+", "-", "*"), class = "factor"), 
            lengths = c(2L, 2L), elementMetadata = NULL, metadata = list()), 
            seqinfo = new("Seqinfo", seqnames = c("chr1", "chr2"), seqlengths = c(NA_integer_, 
            NA_integer_), is_circular = c(NA, NA), genome = c(NA_character_, 
            NA_character_)), elementMetadata = new("DFrame", rownames = NULL, 
                nrows = 4L, elementType = "ANY", elementMetadata = NULL, 
                metadata = list(), listData = structure(list(), names = character(0))), 
            elementType = "ANY", metadata = list())    )
    expect_identical(
        apgi |> pin_by(1) |> anchor_3p() |> stretch(2) |> anchors1(), 
        new("GRanges", seqnames = new("Rle", values = structure(1L, levels = c("chr1", 
        "chr2"), class = "factor"), lengths = 4L, elementMetadata = NULL, 
            metadata = list()), ranges = new("IRanges", start = c(9L, 
        9L, 11L, 11L), width = c(12L, 12L, 22L, 22L), NAMES = NULL, elementType = "ANY", 
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
})
