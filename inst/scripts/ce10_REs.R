#' @importFrom tidyr separate
#' ## code to prepare `ce10_REs` dataset goes here
download.file('https://elifesciences.org/download/aHR0cHM6Ly9jZG4uZWxpZmVzY2llbmNlcy5vcmcvYXJ0aWNsZXMvMzczNDQvZWxpZmUtMzczNDQtZmlnMi1kYXRhMS12Mi50eHQ-/elife-37344-fig2-data1-v2.txt?_hash=jnh09dk%2F9t%2BIseamB5NWBCgtLxFmYQ%2BPJIOMmxucAww%3D', 'ce10_REs.txt')
ce10_REs <- read.delim('ce10_REs.txt', sep = '\t') |> 
    select(1, 2, 3, 7) |> 
    plyranges::as_granges(seqnames = chrom_ce10, start = start_ce10, end = end_ce10, keep_mcols = TRUE)
usethis::use_data(ce10_REs, overwrite = TRUE)
