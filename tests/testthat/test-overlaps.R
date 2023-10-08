test_that("plyranges functions work", {
    
    ## Find
    expect_identical(
        find_overlaps(gi, gr) |> length(), 
        5L
    )
    expect_identical(
        find_overlaps_directed(gi, gr) |> length(), 
        3L
    )
    expect_identical(
        gi |> pin_by("first") |> find_overlaps(gr) |> length(), 
        4L
    )
    expect_identical(
        gi |> pin_by("first") |> find_overlaps_directed(gr) |> length(), 
        2L
    )

    ## Count
    expect_identical(
        count_overlaps(gi, gr), 
        c(1L, 1L, 1L, 2L)
    )
    expect_identical(
        count_overlaps_directed(gi, gr), 
        c(1L, 1L, 0L, 1L)
    )
    expect_identical(
        gi |> pin_by("first") |> count_overlaps(gr), 
        c(1L, 1L, 1L, 1L)
    )
    expect_identical(
        gi |> pin_by("first") |> count_overlaps_directed(gr), 
        c(1L, 1L, 0L, 0L)
    )

    ## Filter
    expect_identical(
        filter_by_overlaps(gi, gr) |> length(), 
        4L
    )
    expect_identical(
        filter_by_non_overlaps(gi, gr) |> length(), 
        0L
    )
    expect_identical(
        gi |> pin_by("second") |> filter_by_overlaps(gr) |> length(), 
        2L
    )
    expect_identical(
        gi |> pin_by("second") |> filter_by_non_overlaps(gr) |> length(), 
        2L
    )

    ## Join
    expect_identical(
        join_overlap_left(gi, gr) |> mcols() |> colnames(), 
        c('score', 'type.x', 'id', 'type.y')
    )
    expect_identical(
        join_overlap_left(gi, gr) |> length(), 
        5L
    )
    expect_identical(
        gi |> pin_by(2) |> join_overlap_left(gr) |> mcols() |> colnames(), 
        c('score', 'type.x', 'id', 'type.y')
    )
    expect_identical(
        gi |> pin_by(2) |> join_overlap_left(gr) |> length(), 
        4L
    )
    expect_identical(
        join_overlap_left_directed(gi, gr) |> length(), 
        4L
    )
    expect_identical(
        gi |> pin_by(2) |> join_overlap_left_directed(gr) |> length(), 
        4L
    )

})
