# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(plyinteractions)

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

test_check("plyinteractions")
