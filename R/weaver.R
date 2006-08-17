## Adapted from utils package code by Friedrich Leisch

weaver <- function()
{
    list(setup = weaverLatexSetup,
         runcode = weaverRuncode,
         writedoc = utils:::RweaveLatexWritedoc,
         finish = weaverLatexFinish,
         checkopts = utils:::RweaveLatexOptions)
}

weaverLatexSetup <-
    function(file, syntax,
             output=NULL, quiet=FALSE, debug=FALSE, echo=TRUE,
             eval=TRUE, split=FALSE, stylepath=TRUE, pdf=TRUE, eps=TRUE,
             use.cache=TRUE)
{
    if (!quiet)
      cat("Working dir:", getwd(), "\n")
    log_debug(paste("Working dir:", getwd()))
    res <- utils:::RweaveLatexSetup(file, syntax, output=output, quiet=quiet,
                                    debug=debug, echo=echo, eval=eval,
                                    split=split, stylepath=stylepath, pdf=pdf,
                                    eps=eps)
    res$options[["use.cache"]] <- use.cache
    res$options[["cache"]] <- FALSE
    ## to be on the safe side: see if defaults pass the check
    res$options <- utils:::RweaveLatexOptions(res$options)
    res
}


resetStorage <- function(fun) {
    storage <- environment(fun)
    storage[["hashDeps"]] <- new.env(parent=emptyenv())
    storage[["sym2hash"]] <- new.env(parent=emptyenv())
}
    

weaverRemoveOrphans <- function(object, options) {
    if(!options$use.cache || !options$cache)
      return(NULL)
    chunk <- options$label
    cachedir <- file.path(CACHE_DIR, getRversion(), chunk)
    curhashes <- sort(ls(environment(cache_expr)$hashDeps))
    expPat1 <- paste(".*\\", CACHE_EXT, "$", sep="")
    expPat2 <- paste("\\", CACHE_EXT, sep="")
    hashfiles <- list.files(cachedir, pattern=expPat1)
    hashfiles <- sort(sub(expPat2, "", hashfiles))
    orphans <- hashfiles[!hashfiles %in% curhashes]
    if (length(orphans)) {
        if (!object$quiet)
          cat("     Removing orphaned cache files:\n")
        for (orph in orphans) {
            if (!object$quiet)
              cat(paste("       ", orph, ".RData", sep=""), "\n")
            orph <- paste(cachedir, "/", orph, CACHE_EXT, sep="")
            tryCatch(file.remove(orph), error=function(e) NULL)
        }
    }
}


removeOrphanedCacheFiles <- function(verbose=TRUE) {
    curhashes <- sort(ls(environment(cache_expr)$hashDeps))
    cachedir <- file.path(CACHE_DIR, getRversion())
    expPat1 <- paste(".*\\", CACHE_EXT, "$", sep="")
    expPat2 <- paste("\\", CACHE_EXT, sep="")
    hashfiles <- list.files(cachedir, pattern=expPat1)
    hashfiles <- sort(sub(expPat2, "", hashfiles))
    orphans <- hashfiles[!hashfiles %in% curhashes]
    if (length(orphans)) {
        if (verbose)
          cat("Removing orphaned cache files:\n")
        for (orph in orphans) {
            if (verbose)
              cat(paste(orph, ".RData", sep=""), "\n")
            orph <- paste(cachedir, "/", orph, CACHE_EXT, sep="")
            tryCatch(file.remove(orph), error=function(e) NULL)
        }
    }
}


weaverLatexFinish <- function(object, error=FALSE) {
    if (!error)
      removeOrphanedCacheFiles(verbose=!object$quiet)
    resetStorage(cache_expr)
    utils:::RweaveLatexFinish(object, error)
}


## Ask Friedrich if he would consider adding an argument that allows
## passing the function to use for RweaveEvalWithOpt.
weaverRuncode <- function(object, chunk, options)
{
    if(!(options$engine %in% c("R", "S"))){
        return(object)
    }

    if(!object$quiet){
        cat(formatC(options$chunknr, width=2), ":")
        if(options$echo) cat(" echo")
        if(options$eval){
            if(options$print) cat(" print")
            if(options$term) cat(" term")
            cat("", options$results)
            if(options$fig){
                if(options$eps) cat(" eps")
                if(options$pdf) cat(" pdf")
            }
        }
        if(!is.null(options$label))
            cat(" (label=", options$label, ")", sep="")
        cat("\n")
    }

    chunkprefix <- utils:::RweaveChunkPrefix(options)

    if(options$split){
        chunkout <- object$chunkout[[chunkprefix]]
        if(is.null(chunkout)){
            chunkout <- file(paste(chunkprefix, "tex", sep="."), "w")
            if(!is.null(options$label))
                object$chunkout[[chunkprefix]] <- chunkout
        }
    }
    else
        chunkout <- object$output

    SweaveHooks(options, run=TRUE)

    chunkexps <- try(parse(text=chunk), silent=TRUE)
    utils:::RweaveTryStop(chunkexps, options)
    openSinput <- FALSE
    openSchunk <- FALSE

    if(length(chunkexps)==0)
        return(object)

    for(nce in 1:length(chunkexps))
    {
        ce <- chunkexps[[nce]]
        dce <- deparse(ce, width.cutoff=0.75*getOption("width"))
        if(object$debug)
            cat("\nRnw> ", paste(dce, collapse="\n+  "),"\n")
        if(options$echo){
            if(!openSinput){
                if(!openSchunk){
                    cat("\\begin{Schunk}\n",
                        file=chunkout, append=TRUE)
                    openSchunk <- TRUE
                }
                cat("\\begin{Sinput}",
                    file=chunkout, append=TRUE)
                openSinput <- TRUE
            }
            cat("\n", getOption("prompt"),
                paste(dce,
                      collapse=paste("\n", getOption("continue"), sep="")),
                file=chunkout, append=TRUE, sep="")
        }

        # tmpcon <- textConnection("output", "w")
        # avoid the limitations (and overhead) of output text connections
        tmpcon <- file()
        sink(file=tmpcon)
        err <- NULL
        if(options$eval) err <- weaverEvalWithOpt(ce, options, object$quiet)
        cat("\n") # make sure final line is complete
        sink()
        output <- readLines(tmpcon)
        close(tmpcon)
        ## delete empty output
        if(length(output)==1 & output[1]=="") output <- NULL

        utils:::RweaveTryStop(err, options)

        if(object$debug)
            cat(paste(output, collapse="\n"))

        if(length(output)>0 & (options$results != "hide")){

            if(openSinput){
                cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
                openSinput <- FALSE
            }
            if(options$results=="verbatim"){
                if(!openSchunk){
                    cat("\\begin{Schunk}\n",
                        file=chunkout, append=TRUE)
                    openSchunk <- TRUE
                }
                cat("\\begin{Soutput}\n",
                    file=chunkout, append=TRUE)
            }

            output <- paste(output,collapse="\n")
            if(options$strip.white %in% c("all", "true")){
                output <- sub("^[[:space:]]*\n", "", output)
                output <- sub("\n[[:space:]]*$", "", output)
                if(options$strip.white=="all")
                    output <- sub("\n[[:space:]]*\n", "\n", output)
            }
            cat(output, file=chunkout, append=TRUE)
            remove(output)

            if(options$results=="verbatim"){
                cat("\n\\end{Soutput}\n", file=chunkout, append=TRUE)
            }
        }
    }
    ## we've eval'd the entire chunk, clean orphans from
    ## the cache dir
    weaverRemoveOrphans(object, options)

    if(openSinput){
        cat("\n\\end{Sinput}\n", file=chunkout, append=TRUE)
    }

    if(openSchunk){
        cat("\\end{Schunk}\n", file=chunkout, append=TRUE)
    }

    if(is.null(options$label) & options$split)
        close(chunkout)

    if(options$split & options$include)
        cat("\\input{", chunkprefix, "}\n", sep="",
            file=object$output, append=TRUE)

    if(options$fig && options$eval){
        if(options$eps){
            postscript(file=paste(chunkprefix, "eps", sep="."),
                       width=options$width, height=options$height,
                       paper="special", horizontal=FALSE)

            err <- try({SweaveHooks(options, run=TRUE);
                        eval(chunkexps, envir=.GlobalEnv)})
            dev.off()
            if(inherits(err, "try-error")) stop(err)
        }
        if(options$pdf){
            pdf(file=paste(chunkprefix, "pdf", sep="."),
                width=options$width, height=options$height)

            err <- try({SweaveHooks(options, run=TRUE);
                        eval(chunkexps, envir=.GlobalEnv)})
            dev.off()
            if(inherits(err, "try-error")) stop(err)
        }
        if(options$include)
            cat("\\includegraphics{", chunkprefix, "}\n", sep="",
                file=object$output, append=TRUE)
    }
    return(object)
}


weaverEvalWithOpt <- function (expr, options, quiet=TRUE){
    if(options$eval){
        label <- options$label
        if(options$use.cache && options$cache)
          expr <- substitute(cache_expr(e, chunk.name=n, quiet=q),
                             list(e=expr, n=label, q=quiet))
        res <- try(.Internal(eval.with.vis(expr, .GlobalEnv, baseenv())),
                   silent=TRUE)
        if(inherits(res, "try-error")) return(res)
        if(options$print | (options$term & res$visible))
            print(res$value)
    }
    return(res)
}

