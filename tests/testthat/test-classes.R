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
    df[, seq(1, 6)] |> as_ginteractions() |> expect_s4_class("GInteractions")
    gi <- df |> as_ginteractions() |> expect_s4_class("GInteractions")
    df |> as_ginteractions(starts.in.df.are.0based = TRUE) |> expect_s4_class("GInteractions")
    df |> as_ginteractions(keep.extra.columns = FALSE) |> expect_s4_class("GInteractions")
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
    pgi <- gi |> 
        pin_by(2) |> 
        expect_s4_class("PinnedGInteractions")
    pgi |> pin_by("first") |> expect_s4_class("PinnedGInteractions")
    pinned_anchors(pgi) |> expect_s4_class("GRanges")
    unpin(pgi) |> expect_s4_class("GInteractions")
    pin(pgi) |> expect_type('integer')
    pgi |> pin_by("firstsecond") |> expect_error()
    show(pgi) |> expect_no_error()

    ## AnchoredPinnedGInteractions

    pgi |> anchor_start() |> expect_s4_class("AnchoredPinnedGInteractions")
    pgi |> anchor_end() |> expect_s4_class("AnchoredPinnedGInteractions")
    pgi |> anchor_center() |> expect_s4_class("AnchoredPinnedGInteractions")
    pgi |> anchor_3p() |> expect_s4_class("AnchoredPinnedGInteractions")
    apgi <- pgi |> anchor_5p() |> expect_s4_class("AnchoredPinnedGInteractions")
    apgi |> unanchor() |> expect_s4_class("PinnedGInteractions")
    expect_identical(apgi |> anchor(), "5p")
    apgi |> unpin() |> expect_s4_class("GInteractions")
    apgi |> pin_by("first") |> expect_s4_class("PinnedGInteractions")
    apgi |> anchor_start() |> expect_s4_class("AnchoredPinnedGInteractions")
    apgi |> anchor_end() |> expect_s4_class("AnchoredPinnedGInteractions")
    apgi |> anchor_center() |> expect_s4_class("AnchoredPinnedGInteractions")
    apgi |> anchor_3p() |> expect_s4_class("AnchoredPinnedGInteractions")
    apgi |> anchor_5p() |> expect_s4_class("AnchoredPinnedGInteractions")
    gi |> anchor_5p() |> expect_error()
    show(apgi) |> expect_no_error()

    ## GroupedGInteractions

    gi |> group_by(strand1) |> expect_s4_class("GroupedGInteractions")
    gi |> group_by(type) |> expect_s4_class("GroupedGInteractions")
    ggi <- gi |> group_by(group = c(1, 1, 2, 2)) |> expect_s4_class("GroupedGInteractions")
    expect_identical(group_by(ggi, type, .add = TRUE) |> n_groups(), 3L)
    expect_identical(group_by(ggi, type, .add = FALSE) |> n_groups(), 2L)
    expect_identical(group_by(ggi, type, .add = TRUE) |> ungroup(group) |> n_groups(), 2L)
    pgi |> group_by(type) |> expect_s4_class("GroupedGInteractions")
    apgi |> group_by(type) |> expect_s4_class("GroupedGInteractions")
    ggi |> ungroup() |> expect_s4_class("GInteractions")
    gi |> ungroup() |> expect_error("GInteractions")
    show(ggi) |> expect_no_error()

})
