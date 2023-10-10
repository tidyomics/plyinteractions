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
    expect_identical(
        ggi |> count(group), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                group = c(1, 2), n = c(2L, 2L)))
    )
    expect_identical(
        ggi |> count(type), 
        new("DFrame", rownames = NULL, nrows = 3L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                group = c(1, 2, 2), type = c("cis", "cis", "trans"), 
                n = c(2L, 1L, 1L)))
    )
    expect_identical(
        ggi |> tally(), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                group = c(1, 2), n = c(2L, 2L)))
    )
    expect_identical(
        ggi |> count(strand1), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                group = c(1, 2), strand1 = new("Rle", values = structure(1:2, levels = c("+", 
                "-", "*"), class = "factor"), lengths = c(1L, 1L), elementMetadata = NULL, 
                    metadata = list()), n = c(2L, 2L)))
    )
    expect_identical(
        gi |> count(strand1), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                strand1 = new("Rle", values = structure(1:2, levels = c("+", 
                "-", "*"), class = "factor"), lengths = c(1L, 1L), elementMetadata = NULL, 
                    metadata = list()), n = c(2L, 2L)))
    )
    expect_identical(
        ggi |> count(strand1, wt = score), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                group = c(1, 2), strand1 = new("Rle", values = structure(1:2, levels = c("+", 
                "-", "*"), class = "factor"), lengths = c(1L, 1L), elementMetadata = NULL, 
                    metadata = list()), n = c(0.736002816120163, 1.23265417455696
                ))), 
        tolerance = 1e-4
    )
    expect_identical(
        gi |> count(strand1, wt = score), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                strand1 = new("Rle", values = structure(1:2, levels = c("+", 
                "-", "*"), class = "factor"), lengths = c(1L, 1L), elementMetadata = NULL, 
                    metadata = list()), n = c(0.736002816120163, 1.23265417455696
                ))), 
        tolerance = 1e-4
    )
    expect_identical(
        ggi |> count(strand1, wt = score, sort = TRUE), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                group = c(2, 1), strand1 = new("Rle", values = structure(2:1, levels = c("+", 
                "-", "*"), class = "factor"), lengths = c(1L, 1L), elementMetadata = NULL, 
                    metadata = list()), n = c(1.23265417455696, 0.736002816120163
                ))), 
        tolerance = 1e-4
    )
    expect_identical(
        gi |> count(strand1, wt = score, sort = TRUE), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                strand1 = new("Rle", values = structure(2:1, levels = c("+", 
                "-", "*"), class = "factor"), lengths = c(1L, 1L), elementMetadata = NULL, 
                    metadata = list()), n = c(1.23265417455696, 0.736002816120163
                ))), 
        tolerance = 1e-4
    )

    ## filter

    expect_identical(
        gi |> filter(strand1 == '-') |> ranges1(), 
        new("IRanges", start = c(11L, 11L), width = c(20L, 20L), NAMES = NULL, 
            elementType = "ANY", elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        gi |> filter(strand1 == '+') |> ranges1(), 
        new("IRanges", start = c(11L, 11L), width = c(10L, 10L), NAMES = NULL, 
            elementType = "ANY", elementMetadata = NULL, metadata = list())
    )
    ggi |> filter(strand1 == '+') |> expect_error()

    ## mutate

    expect_identical(
        gi |> mutate(strand1 = '-') |> strand1(), 
        new("Rle", values = structure(2L, levels = c("+", "-", "*"), class = "factor"), 
        lengths = 4L, elementMetadata = NULL, metadata = list())
    )
    expect_identical(
        gi |> mutate(xxx = 1) |> mcols() |> subset(, c(2, 3)), 
        new("DFrame", rownames = NULL, nrows = 4L, elementType = "ANY", 
        elementMetadata = NULL, metadata = list(), listData = list(
            type = c("cis", "cis", "cis", "trans"), xxx = c(1, 1, 
            1, 1)))
    )
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
    expect_identical(
        gi |> slice(1:3) |> start1(), 
        c(11L, 11L, 11L)
    )
    gi |> slice(1:5) |> expect_error()
    gi |> slice('error') |> expect_error()

    ## summarize
    expect_identical(
        ggi |> summarize(m = mean(score)), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
        elementMetadata = NULL, metadata = list(), listData = list(
            group = c(1, 2), m = c(0.368001408060081, 0.616327087278478
            ))), 
        tolerance = 1e-4
    )
    expect_identical(
        ggi |> summarize(m = table(type)), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                group = c(1, 2), m = new("CompressedIntegerList", elementType = "integer", 
                    elementMetadata = NULL, metadata = list(), unlistData = c(cis = 2L, 
                    trans = 0L, cis = 1L, trans = 1L), partitioning = new("PartitioningByEnd", 
                        end = c(2L, 4L), NAMES = c("1", "2"), elementType = "ANY", 
                        elementMetadata = NULL, metadata = list()))))
    )
    expect_identical(
        ggi |> summarize(m = table(type), n = table(group)), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                group = c(1, 2), m = new("CompressedIntegerList", elementType = "integer", 
                    elementMetadata = NULL, metadata = list(), unlistData = c(cis = 2L, 
                    trans = 0L, cis = 1L, trans = 1L), partitioning = new("PartitioningByEnd", 
                        end = c(2L, 4L), NAMES = c("1", "2"), elementType = "ANY", 
                        elementMetadata = NULL, metadata = list())), 
                n = new("CompressedIntegerList", elementType = "integer", 
                    elementMetadata = NULL, metadata = list(), unlistData = c(`1` = 2L, 
                    `2` = 0L, `1` = 0L, `2` = 2L), partitioning = new("PartitioningByEnd", 
                        end = c(2L, 4L), NAMES = c("1", "2"), elementType = "ANY", 
                        elementMetadata = NULL, metadata = list()))))
    )

})

test_that("dplyr group functions work", {

    expect_identical(
        group_data(ggi), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
        elementMetadata = NULL, metadata = list(), listData = list(
            group = c(1, 2), .rows = new("CompressedIntegerList", 
                elementType = "integer", elementMetadata = NULL, 
                metadata = list(), unlistData = 1:4, partitioning = new("PartitioningByEnd", 
                    end = c(2L, 4L), NAMES = NULL, elementType = "ANY", 
                    elementMetadata = NULL, metadata = list()))))
    )
    expect_identical(
        group_keys(ggi), 
        new("DFrame", rownames = NULL, nrows = 2L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                group = c(1, 2)))
    )
    expect_identical(
        group_indices(ggi), 
        S4Vectors::Rle(c(1L, 1L, 2L, 2L))
    )
    expect_identical(
        group_vars(ggi), 
        "group"
    )
    expect_identical(
        groups(ggi), 
        list(rlang::sym("group"))
    )
    expect_identical(
        group_size(ggi), 
        c(2L, 2L)
    )
    expect_identical(
        n_groups(ggi), 
        2L
    )

})
