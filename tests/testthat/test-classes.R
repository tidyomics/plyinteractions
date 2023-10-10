test_that("classes work", {
    
    ## GInteractions
    df <- read.table(text = "
        chr1 11 20 chr1 21 30 + +
        chr1 11 20 chr1 51 55 + +
        chr1 11 30 chr1 51 55 - -
        chr1 11 30 chr2 51 60 - -",
        col.names = c(
            "seqnames1", "start1", "end1", 
            "seqnames2", "start2", "end2", 
            "strand1", "strand2"
        )
    ) |> 
        dplyr::mutate(score = runif(4), type = c('cis', 'cis', 'cis', 'trans'))
    df |> as_ginteractions(test = strand1) |> expect_error()
    df[, seq(1, 4)] |> as_ginteractions() |> expect_error()
    df[, c(1, 2, 4, 5)] |> as_ginteractions() |> expect_error()
    expect_identical(
        df[1:2, seq(1, 6)] |> as_ginteractions(), 
        new("GInteractions", anchor1 = c(1L, 1L), anchor2 = 2:3, regions = new("GRanges", 
        seqnames = new("Rle", values = structure(1L, levels = "chr1", class = "factor"), 
            lengths = 3L, elementMetadata = NULL, metadata = list()), 
        ranges = new("IRanges", start = c(11L, 21L, 51L), width = c(10L, 
        10L, 5L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list()), strand = new("Rle", values = structure(3L, levels = c("+", 
        "-", "*"), class = "factor"), lengths = 3L, elementMetadata = NULL, 
            metadata = list()), seqinfo = new("Seqinfo", seqnames = "chr1", 
            seqlengths = NA_integer_, is_circular = NA, genome = NA_character_), 
        elementMetadata = new("DFrame", rownames = NULL, nrows = 3L, 
            elementType = "ANY", elementMetadata = NULL, metadata = list(), 
            listData = structure(list(), names = character(0))), 
        elementType = "ANY", metadata = list()), NAMES = NULL, elementMetadata = new("DFrame", 
        rownames = NULL, nrows = 2L, elementType = "ANY", elementMetadata = NULL, 
        metadata = list(), listData = structure(list(), names = character(0))), 
        metadata = list())
    )
    gi <- df |> as_ginteractions() |> expect_s4_class("GInteractions")
    expect_identical(
        df |> as_ginteractions(starts.in.df.are.0based = TRUE) |> start1(), 
        c(12L, 12L, 12L, 12L)
    )
    expect_identical(
        df |> as_ginteractions(keep.extra.columns = FALSE) |> mcols() |> colnames(), 
        character(0)
    )
    show(gi) |> expect_no_error()

    ## PinnedGInteractions
    gi |> 
        pin_by("first") |> 
        expect_s4_class("PinnedGInteractions")
    gi |> 
        pin_first() |> 
        expect_s4_class("PinnedGInteractions")
    gi |> 
        pin_anchors1() |> 
        expect_s4_class("PinnedGInteractions")
    gi |> 
        pin_by("second") |> 
        expect_s4_class("PinnedGInteractions")
    gi |> 
        pin_second() |> 
        expect_s4_class("PinnedGInteractions")
    gi |> 
        pin_anchors2() |> 
        expect_s4_class("PinnedGInteractions")
    gi |> 
        pin_by(1) |> 
        expect_s4_class("PinnedGInteractions")
    expect_identical(
        gi |> pin_by(2) |> shift_left(5) |> start2(), 
        c(16L, 46L, 46L, 46L)
    )
    expect_identical(
        gi |> pin_by(1) |> shift_upstream(10) |> start1(), 
        c(1L, 1L, 21L, 21L)
    )
    pgi <- gi |> 
        pin_by(2) |> 
        expect_s4_class("PinnedGInteractions")
    expect_identical(
        pgi |> pin_by("first") |> shift_upstream(10) |> start1(), 
        c(1L, 1L, 21L, 21L)
    )
    expect_identical(
        pinned_anchors(pgi), 
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
    unpin(pgi) |> expect_s4_class("GInteractions")
    unpin(pgi) |> pin() |> expect_error()
    expect_identical(
        pin(pgi), 
        2L
    )
    pgi |> pin_by("firstsecond") |> expect_error()
    pgi |> pin_by(c(1, 2)) |> expect_error()
    show(pgi) |> expect_no_error()

    ## AnchoredPinnedGInteractions

    pgi |> anchor_start() |> expect_s4_class("AnchoredPinnedGInteractions")
    pgi |> anchor_end() |> expect_s4_class("AnchoredPinnedGInteractions")
    pgi |> anchor_center() |> expect_s4_class("AnchoredPinnedGInteractions")
    pgi |> anchor_3p() |> expect_s4_class("AnchoredPinnedGInteractions")
    expect_identical(
        pgi |> anchor_3p() |> anchor(), 
        "3p"
    )
    expect_identical(
        pgi |> anchor_3p() |> stretch(10) |> ranges2(), 
        new("IRanges", start = c(11L, 41L, 51L, 51L), width = c(20L, 
            15L, 15L, 20L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
                metadata = list())
    )
    apgi <- pgi |> anchor_5p() |> expect_s4_class("AnchoredPinnedGInteractions")
    apgi |> unanchor() |> expect_s4_class("PinnedGInteractions")
    apgi |> unanchor() |> anchor() |> expect_error()
    expect_identical(
        apgi |> anchor(), 
        "5p"
    )
    apgi |> unpin() |> expect_s4_class("GInteractions")
    apgi |> unpin() |> pin() |> expect_error()
    apgi |> pin_by("first") |> expect_s4_class("PinnedGInteractions")
    expect_identical(
        apgi |> pin_by("first") |> stretch(100) |> ranges1(), 
        new("IRanges", start = c(-39L, -39L, -39L, -39L), width = c(110L, 
            110L, 120L, 120L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
                metadata = list())
    )
    expect_identical(
        apgi |> anchor_start() |> stretch(100) |> ranges2(), 
        new("IRanges", start = c(21L, 51L, 51L, 51L), width = c(110L, 
        105L, 105L, 110L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list())
    )
    expect_identical(
        apgi |> anchor_end() |> stretch(100) |> ranges2(), 
        new("IRanges", start = c(-79L, -49L, -49L, -49L), width = c(110L, 
        105L, 105L, 110L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list())
    )
    expect_identical(
        apgi |> anchor_center() |> stretch(100) |> ranges2(), 
        new("IRanges", start = c(-29L, 1L, 1L, 1L), width = c(110L, 105L, 
        105L, 110L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list())
    )
    expect_identical(
        apgi |> anchor_3p() |> stretch(100) |> ranges2(), 
        new("IRanges", start = c(-79L, -49L, 51L, 51L), width = c(110L, 
        105L, 105L, 110L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list())
    )
    expect_identical(
        apgi |> anchor_5p() |> stretch(100) |> ranges2(), 
        new("IRanges", start = c(21L, 51L, -49L, -49L), width = c(110L, 
        105L, 105L, 110L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list())
    )
    gi |> anchor_5p() |> expect_error()
    show(apgi) |> expect_no_error()

    ## GroupedGInteractions

    gi |> group_by(strand1) |> expect_s4_class("GroupedGInteractions")
    gi |> group_by(type) |> expect_s4_class("GroupedGInteractions")
    ggi <- gi |> group_by(group = c(1, 1, 2, 2)) |> expect_s4_class("GroupedGInteractions")
    expect_identical(
        group_by(ggi, type, .add = TRUE) |> count(), 
        new("DFrame", rownames = NULL, nrows = 3L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                group = c(1, 2, 2), type = c("cis", "cis", "trans"), 
                n = c(2L, 1L, 1L)))
    )
    expect_identical(
        group_by(ggi, type, .add = TRUE) |> n_groups(), 
        3L
    )
    expect_identical(
        group_by(ggi, type, .add = FALSE) |> n_groups(), 
        2L
    )
    expect_identical(
        group_by(ggi, type, .add = TRUE) |> ungroup(group) |> n_groups(), 
        2L
    )
    pgi |> group_by(type) |> expect_s4_class("GroupedGInteractions")
    apgi |> group_by(type) |> expect_s4_class("GroupedGInteractions")
    ggi |> ungroup() |> expect_s4_class("GInteractions")
    gi |> ungroup() |> expect_error("GInteractions")
    show(ggi) |> expect_no_error()

    ## DelegatingGInteractions
    expect_identical(
        anchors1(apgi), 
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
        ranges1(apgi), 
        new("IRanges", start = c(11L, 11L, 11L, 11L), width = c(10L, 
        10L, 20L, 20L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list())
    )
    expect_identical(
        seqnames1(apgi), 
        new("Rle", values = structure(1L, levels = c("chr1", "chr2"), class = "factor"), 
        lengths = 4L, elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        start1(apgi), 
        c(11L, 11L, 11L, 11L)
    )
    expect_identical(
        end1(apgi), 
        c(20L, 20L, 30L, 30L)
    )
    expect_identical(
        width1(apgi), 
        c(10L, 10L, 20L, 20L)
    )
    expect_identical(
        strand1(apgi), 
        new("Rle", values = structure(1:2, levels = c("+", "-", "*"), class = "factor"), 
        lengths = c(2L, 2L), elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        anchors2(apgi), 
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
        ranges2(apgi), 
        new("IRanges", start = c(21L, 51L, 51L, 51L), width = c(10L, 
        5L, 5L, 10L), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list())
    )
    expect_identical(
        seqnames2(apgi), 
        new("Rle", values = structure(1:2, levels = c("chr1", "chr2"), class = "factor"), 
            lengths = c(3L, 1L), elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        start2(apgi), 
        c(21L, 51L, 51L, 51L)
    )
    expect_identical(
        end2(apgi), 
        c(30L, 55L, 55L, 60L)
    )
    expect_identical(
        width2(apgi), 
        c(10L, 5L, 5L, 10L)
    )
    expect_identical(
        strand2(apgi), 
        new("Rle", values = structure(1:2, levels = c("+", "-", "*"), class = "factor"), 
            lengths = c(2L, 2L), elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        anchors(apgi), 
        list(first = new("GRanges", seqnames = new("Rle", values = structure(1L, levels = c("chr1", 
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
            elementType = "ANY", metadata = list()), second = new("GRanges", 
            seqnames = new("Rle", values = structure(1:2, levels = c("chr1", 
            "chr2"), class = "factor"), lengths = c(3L, 1L), elementMetadata = NULL, 
                metadata = list()), ranges = new("IRanges", start = c(21L, 
            51L, 51L, 51L), width = c(10L, 5L, 5L, 10L), NAMES = NULL, 
                elementType = "ANY", elementMetadata = NULL, metadata = list()), 
            strand = new("Rle", values = structure(1:2, levels = c("+", 
            "-", "*"), class = "factor"), lengths = c(2L, 2L), elementMetadata = NULL, 
                metadata = list()), seqinfo = new("Seqinfo", seqnames = c("chr1", 
            "chr2"), seqlengths = c(NA_integer_, NA_integer_), is_circular = c(NA, 
            NA), genome = c(NA_character_, NA_character_)), elementMetadata = new("DFrame", 
                rownames = NULL, nrows = 4L, elementType = "ANY", elementMetadata = NULL, 
                metadata = list(), listData = structure(list(), names = character(0))), 
            elementType = "ANY", metadata = list()))
    )
    expect_identical(
        regions(apgi), 
        new("GRanges", seqnames = new("Rle", values = structure(1:2, levels = c("chr1", 
        "chr2"), class = "factor"), lengths = c(5L, 1L), elementMetadata = NULL, 
            metadata = list()), ranges = new("IRanges", start = c(11L, 
        21L, 51L, 11L, 51L, 51L), width = c(10L, 10L, 5L, 20L, 5L, 10L
        ), NAMES = NULL, elementType = "ANY", elementMetadata = NULL, 
            metadata = list()), strand = new("Rle", values = structure(1:2, levels = c("+", 
        "-", "*"), class = "factor"), lengths = c(3L, 3L), elementMetadata = NULL, 
            metadata = list()), seqinfo = new("Seqinfo", seqnames = c("chr1", 
        "chr2"), seqlengths = c(NA_integer_, NA_integer_), is_circular = c(NA, 
        NA), genome = c(NA_character_, NA_character_)), elementMetadata = new("DFrame", 
            rownames = NULL, nrows = 6L, elementType = "ANY", elementMetadata = NULL, 
            metadata = list(), listData = structure(list(), names = character(0))), 
            elementType = "ANY", metadata = list())
    )
    expect_identical(
        seqinfo(apgi), 
        new("Seqinfo", seqnames = c("chr1", "chr2"), seqlengths = c(NA_integer_, 
        NA_integer_), is_circular = c(NA, NA), genome = c(NA_character_, 
        NA_character_))
    )
    expect_identical(
        mcols(apgi)[,2], 
        c("cis", "cis", "cis", "trans")
    )
    show(apgi) |> expect_no_error()

})
