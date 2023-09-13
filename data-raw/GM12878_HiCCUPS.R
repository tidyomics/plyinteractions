## code to prepare `GM12878_HiCCUPS` dataset goes here
download.file('https://ftp.ncbi.nlm.nih.gov/geo/series/GSE63nnn/GSE63525/suppl/GSE63525_GM12878_primary%2Breplicate_HiCCUPS_looplist.txt.gz', 'GM12878_HiCCUPS.txt.gz')
GM12878_HiCCUPS <- read.delim('GM12878_HiCCUPS.txt.gz', sep = '\t') |> 
    as_ginteractions(
        seqnames1 = chr1, start1 = x1, end1 = x2, 
        seqnames2 = chr2, start2 = y1, end2 = y2, keep.extra.columns = TRUE)
usethis::use_data(GM12878_HiCCUPS, overwrite = TRUE)
