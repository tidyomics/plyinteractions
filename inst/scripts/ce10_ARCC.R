#' ## code to prepare `ce10-ARCC` dataset goes here
download.file('https://genome.cshlp.org/content/suppl/2022/01/17/gr.275669.121.DC1/Supplemental_TableS2.xlsx', 'ce10_ARCC.xlsx')
ce10_ARCC <- readxl::read_xlsx('ce10_ARCC.xlsx', sheet = 2) |> 
    select(location1, location2) |> 
    tidyr::separate(location1, into = c("seqnames1", "start1", "end1"), sep = ':|-') |>
    tidyr::separate(location2, into = c("seqnames2", "start2", "end2"), sep = ':|-') |>
    mutate(across(starts_with(c('start', 'end')), as.numeric)) |> 
    as_ginteractions()
usethis::use_data(ce10_ARCC, overwrite = TRUE)
