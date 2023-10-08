test_that("plyranges functions work", {
    
    ## Flanking
    gi |> flank_left(2) |> expect_error()
    gi |> pin_by("first") |> 
        flank_upstream(2) |> flank_downstream(2) |> 
        flank_left(2) |> flank_right(-2) |> 
        pin_by("second") |> flank_downstream(2) |> 
        expect_s4_class('PinnedGInteractions')
    
    gi |> anchors1() |> flank_left(2) |> expect_s4_class("GRanges")
    gi |> anchors1() |> 
        flank_upstream(2) |> flank_downstream(2) |> 
        flank_left(2) |> flank_right(-2) |> 
        expect_s4_class('GRanges')

    ## Shift
    gi |> shift_left(2) |> expect_error()
    gi |> pin_by("first") |> 
        shift_upstream(2) |> shift_downstream(2) |> 
        shift_left(2) |> shift_right(2) |> 
        pin_by("second") |> shift_downstream(2) |> 
        expect_s4_class('PinnedGInteractions')
    
    gi |> anchors1() |> shift_left(2) |> expect_s4_class("GRanges")
    gi |> anchors1() |> 
        shift_upstream(2) |> shift_downstream(2) |> 
        shift_left(2) |> shift_right(2) |> 
        expect_s4_class('GRanges')
    
    ## Stretch
    gi |> stretch(2) |> expect_error()
    pgi |> stretch(2) |> expect_s4_class('PinnedGInteractions')
    apgi |> stretch(2) |> expect_s4_class('PinnedGInteractions')
    
})
