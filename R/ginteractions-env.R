#' @importFrom S4Vectors as.env
setMethod("as.env", "GInteractions", function(x, enclos, tform = identity) {
    
    ## This is tricky, normally S4Vectors gets accessors from the 
    ## package defining the class (ie. accessors defined in InteractionSet
    ## for GInteractions). But `seqnames1`, `strand1`, ... are NOT 
    ## accessors defined in InteractionSet. So I have to trick 
    ## S4Vectors into making the correct nested envs. 
    ## 
    ## Parent: has mcols 
    ## env child: has core GI columns and its accessors

    parent <- S4Vectors::as.env(
        S4Vectors::mcols(x, use.names=FALSE), enclos, tform = tform
    )
    env <- .makeFixedColumnEnv(x, parent, tform)
    env$.. <- x
    env
})

.makeFixedColumnEnv <- function(x, parent, tform = identity) {
    env <- new.env(parent=parent)
    pvnEnv <- getNamespace("plyinteractions")
    nms <- c("seqnames1", "start1", "end1", "width1", "strand1", 
        "seqnames2", "start2", "end2", "width2", "strand2")
    lapply(nms, function(nm) {
        accessor <- get(nm, pvnEnv, mode="function")
        makeActiveBinding(
            sym = nm, 
            fun = function() {
                val <- tform(accessor(x))
                rm(list=nm, envir=env)
                assign(nm, val, env)
                val
            },
            env = env
        )
    })
    env
}

.overscope_ginteractions <- function(x, envir = parent.frame()) {
    env <- S4Vectors::as.env(x, envir)
    rlang::new_data_mask(env, top = parent.env(env))
}

.overscope_groupedginteractions <- function(x, envir = parent.frame()) {
    env <- S4Vectors::as.env(
        x@delegate, 
        envir, 
        tform = function(col) unname(S4Vectors::splitAsList(col, x@group_indices))
    )
    rlang::new_data_mask(env, top = parent.env(env))
}
