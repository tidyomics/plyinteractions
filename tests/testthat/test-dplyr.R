test_that("dplyr functions work", {
    
    ## arrange

    expect_identical(
        gi |> mutate(idx = seq(1, length(gi))) |> 
            arrange(score) |> 
            strand1(), 
        S4Vectors::Rle(factor(c('+', '-', '+', '-'), c('+', '-', '*')))
    )

    ## tally/count

    ggi |> count(strand1) |> expect_no_error()
    ggi |> tally() |> expect_no_error()
    gi |> count(strand1) |> expect_s4_class("DataFrame")
    gi |> tally() |> expect_error()

    ## filter

    expect_identical(gi |> filter(strand1 == '-') |> length(), 2L)
    ggi |> filter(strand1 == '+') |> expect_error()

    ## mutate

    gi |> mutate(strand1 = '-') |> expect_s4_class("GInteractions")
    gi |> mutate(xxx = 1) |> expect_s4_class("GInteractions")
    expect_identical(
        mutate(gi, xxx = IRanges::RleList(c(1, 2), c(3, 4)))$xxx,
        IRanges::RleList(c(1, 2), c(3, 4), c(1, 2), c(3, 4))
    )
    ggi |> mutate(strand1 = '-') |> expect_error()

    ## rename

    expect_error(gi |> rename(strand1 = strand2))
    expect_error(gi |> rename(yyy = xxx))
    expect_identical(
        gi |> rename(xx = type) |> as_tibble() |> colnames(), 
        c("seqnames1", "start1", "end1", "width1", "strand1", "seqnames2", 
        "start2", "end2", "width2", "strand2", "score", "xx")
    )

    ## select
    expect_identical(
        gi |> select(type) |> as_tibble() |> colnames(), 
        c("seqnames1", "start1", "end1", "width1", "strand1", "seqnames2", 
        "start2", "end2", "width2", "strand2", "type")
    )
    expect_identical(
        gi |> select(type, .drop_ranges = TRUE) |> as_tibble() |> colnames(), 
        c("type")
    )
    expect_error(gi |> select(strand1))
    expect_identical(
        gi |> select(strand1, .drop_ranges = TRUE) |> as_tibble() |> colnames(), 
        c("strand1")
    )

    ## slice
    gi |> slice(1:3) |> expect_s4_class("GInteractions")
    gi |> slice(1:5) |> expect_error()
    gi |> slice('error') |> expect_error()

    ## summarize
    ggi |> summarize(m = mean(score))
    ggi |> summarise(m = mean(score))

})

test_that("dplyr group functions work", {

    group_data(ggi) |> expect_s4_class("DataFrame")
    group_keys(ggi) |> expect_s4_class("DataFrame")
    expect_identical(group_indices(ggi), S4Vectors::Rle(c(1L, 1L, 2L, 2L)))
    expect_identical(group_vars(ggi), "group")
    expect_identical(groups(ggi), list(rlang::sym("group")))
    expect_identical(group_size(ggi), c(2L, 2L))
    expect_identical(n_groups(ggi), 2L)

})
