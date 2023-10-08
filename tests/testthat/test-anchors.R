test_that("anchors functions work", {

    gi |> replace_anchors(2, value = anchors1(gi)) |> expect_s4_class("GInteractions")
    pgi |> replace_anchors(value = anchors1(gi)) |> expect_s4_class("GInteractions")
    apgi |> replace_anchors(value = anchors1(gi)) |> expect_s4_class("GInteractions")

    annotate(gi, gr, by = "type")
    annotate_directed(gi, gr, by = "type")
})

