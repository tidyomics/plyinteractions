set.seed(1234)
gi <- read.table(text = "
    chr1 11 20 chr1 21 30 + +
    chr1 11 20 chr1 51 55 + +
    chr1 11 30 chr1 51 55 - -
    chr1 11 30 chr2 51 60 - -",
    col.names = c(
    "seqnames1", "start1", "end1", 
    "seqnames2", "start2", "end2", "strand1", "strand2")
) |> 
    as_ginteractions() |> 
    mutate(score = runif(4), type = c('cis', 'cis', 'cis', 'trans'))
pgi <- gi |> pin_by(2) 
apgi <- pgi |> anchor_5p()
ggi <- gi |> group_by(group = c(1, 1, 2, 2))
gr <- GenomicRanges::GRanges(c("chr1:20-30:+", "chr2:55-65:-")) |> 
    plyranges::mutate(id = 1:2, type = 'gr')
