# Mostly copied and adapted from plyranges package
# Lee, Stuart, Cook, Dianne, Lawrence, Michael (2019). 
# “plyranges: a grammar of genomic data transformation.” Genome Biol., 20(1), 4.

# This allows tidy functions (e.g. mutate, select, ...) to work for 
# GInteractions. The trick was to tweak S4Vectors::as.env when 
# passing `GInteractions` objects to it. 

.scope_quos <- function(quos) {
    scoped_quosures <- rlang::quos()
    for (i in seq_along(quos)) {
        scoped_quosures[[i]] <- rlang::quo_set_env(
            quos[[i]],
            env = .scope_ginteractions(
                rlang::quo_get_env(quos[[i]]), .plyinteractions_generics()
            )
        )
    }
    return(scoped_quosures)
}

.scope_ginteractions <- function(env, generics) {
    tail <- env
    nms <- character(0)
    # recurse through parent environments until we get to the empty env
    while (!identical(tail, rlang::empty_env())) {
        env_nms <- rlang::env_names(tail)
        nms <- unique(c(nms, intersect(names(generics), env_nms)))
        tail <- rlang::env_parent(tail)
    }
    nms <- setdiff(nms, rlang::env_names(rlang::global_env()))
    generics <- generics[nms]

    child <- rlang::child_env(env, !!!generics)
    child
}

#' @importFrom methods getGeneric
#' @importFrom methods getGenerics
.plyinteractions_generics <- function(
    pkgs = c("BiocGenerics", "IRanges", "S4Vectors", "plyinteractions")
) {
    pkgs <- lapply(pkgs, asNamespace)
    generics <- methods::getGenerics(pkgs)
    fn <- mapply(getGeneric,
        f = generics@.Data,
        package = generics@package,
        SIMPLIFY = FALSE
    )
    Filter(function(x) {
        fun <- try(x@default, silent = TRUE)
        if (is(fun, "try-error")) FALSE
        !is.primitive(fun)
    },
    fn)
}

.overscope_ginteractions <- function(x, envir = parent.frame()) {
    
    ## This is tricky, normally S4Vectors gets accessors from the 
    ## package defining the class (ie. accessors defined in InteractionSet
    ## for GInteractions). But `seqnames1`, `strand1`, ... are NOT 
    ## accessors defined in InteractionSet. So I have to trick 
    ## S4Vectors into making the correct nested envs. 
    ## 
    ## Parent: has mcols 
    ## env child: has core GI columns and its accessors

    parent <- S4Vectors::as.env(
        S4Vectors::mcols(x, use.names=FALSE), 
        envir, 
        tform = identity
    )
    env <- new.env(parent=parent)
    pvnEnv <- getNamespace("plyinteractions")
    nms <- c("seqnames1", "start1", "end1", "width1", "strand1", 
        "seqnames2", "start2", "end2", "width2", "strand2")
    lapply(nms, function(nm) {
        accessor <- get(nm, pvnEnv, mode="function")
        makeActiveBinding(nm, function() {
                val <- identity(accessor(x))
                rm(list=nm, envir=env)
                assign(nm, val, env)
                val
            },
            env
        )
    })
    env$.. <- x
    rlang::new_data_mask(env, top = parent.env(env))

}
