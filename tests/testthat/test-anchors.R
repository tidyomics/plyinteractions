test_that("anchors functions work", {

    expect_identical(
        gi |> replace_anchors(2, value = anchors1(gi)) |> anchors2(), 
        anchors1(gi)
    )
    expect_identical(
        pgi |> replace_anchors(value = anchors1(gi)) |> anchors2(), 
        anchors1(pgi)
    )
    expect_identical(
        pgi |> unpin() |> replace_anchors(2, value = anchors1(gi)) |> anchors2(), 
        anchors1(gi)
    )

    expect_identical(
        annotate(gi, gr, by = "type") |> mcols(),
        new("DFrame", rownames = NULL, nrows = 4L, elementType = "ANY", 
        elementMetadata = NULL, metadata = list(), listData = list(
            score = c(0.113703411305323, 0.622299404814839, 0.609274732880294, 
            0.623379441676661), type.x = c("cis", "cis", "cis", "trans"
            ), id.x = c(1L, 1L, 1L, 1L), type.y.1 = c("gr", "gr", 
            "gr", "gr"), id.y = c(1L, NA, NA, 2L), type.y.2 = c("gr", 
            NA, NA, "gr"))), 
        tolerance = 1e-4
    )

    expect_identical(
        annotate_directed(gi, gr, by = "type") |> mcols(),
        new("DFrame", rownames = NULL, nrows = 4L, elementType = "ANY", 
            elementMetadata = NULL, metadata = list(), listData = list(
                score = c(0.113703411305323, 0.622299404814839, 0.609274732880294, 
                0.623379441676661), type.x = c("cis", "cis", "cis", "trans"
                ), id.x = c(1L, 1L, NA, NA), type.y.1 = c("gr", "gr", 
                NA, NA), id.y = c(1L, NA, NA, 2L), type.y.2 = c("gr", 
                NA, NA, "gr"))), 
        tolerance = 1e-4
    )
    
})

