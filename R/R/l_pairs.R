
#' @title Scatterplot Matrix in Loon
#'   
#' @description Function creates a scatterplot matrix using loon's scatterplot 
#'   widgets
#'   
#' @param data a data.frame with numerical data to create the scatterplot matrix
#' @param showHistoram show histograms of each varible or not. Default is FALSE
#' @param showSerialAxes show serialAxes or not. Default is FALSE
#' @template param_parent
#' @param ... named arguments to modify the scatterplot states
#' 
#' @return a list with scatterplot handles
#' 
#' @seealso \code{\link{l_plot}}
#' 
#' @export
#' 
#' @examples
#' p <- l_pairs(iris[,-5], color=iris$Species)

l_pairs <- function(data, showHistoram = FALSE, showSerialAxes = FALSE, parent=NULL, ...) {

    args <- list(...)
    if(!identical(class(data), "data.frame")) { # use of identical to deal with tibbles
        data <- as.data.frame(data)
    }

    if (is.null(args[['linkingGroup']])) {
        args[['linkingGroup']] <- deparse(substitute(data))
    }

    
    args[['x']] <- NULL
    args[['y']] <- NULL

    if (dim(data)[2] <= 2) {
        args[['x']] <- data
        args[['parent']] <- parent
        return(do.call(l_plot, args))
    }

    args[['showLabels']] <- FALSE
    args[['showScales']] <- FALSE
    args[['swapAxes']] <- FALSE
    
    new.toplevel <- FALSE
    if(is.null(parent)) {
        new.toplevel <- TRUE
        parent <- l_toplevel()
        tktitle(parent) <- paste("loon scatterplot matrix for",
                                 deparse(substitute(data)), "data")
    }
    
    child <- as.character(tcl('frame', l_subwin(parent, 'pairs')))

    ## parent for individual scatterplots
    args[['parent']] <- child
    
    nvar <- dim(data)[2]
    pair <- utils::combn(nvar, 2)
    varnames <- names(data)

    ## combn returns the variable combinations for the scatterplot
    ## matrix. The scatterplot arrangements is as follows
    ##
    ##      1      2      3      4
    ##  1  [1]   (2,1)  (3,1)  (4,1)
    ##  2         [2]   (3,2)  (4,2)
    ##  3                [3]   (4,3)
    ##  4                       [4]
    ##
    ##
    ## pair is
    ##  1  1  1  2  2  3
    ##  2  3  4  3  4  4
    
    cellExpand <- nvar - 1
    text_adjustValue <- 1
    scatter_adjustValue <- 0
    
    if (showHistoram) {
        hist_args <- args
        hist_args[['showStackedColors']] <- TRUE
        hist_args[['showOutlines']] <- FALSE
        histograms <- list()
        # The first half are top hists, the second half are right hists
        for(i in 1:(2*nvar)){
            if (i <= nvar) {
                hist_args[['x']] <- as.numeric(data[, i])
                hist_args[['xlabel']] <- varnames[i]
                # top level histograms  
                hist_args[['swapAxes']] <- FALSE
                ix <- 0
                iy <- i-1
            } else {
                hist_args[['x']] <- as.numeric(data[, i - nvar])
                hist_args[['xlabel']] <- varnames[i - nvar]
                # right level histograms  
                hist_args[['swapAxes']] <- TRUE
                ix <- i - nvar
                iy <- nvar
            }
            histograms[[i]] <- do.call(l_hist, hist_args)
            names(histograms)[i] <- paste('x',ix,'y',iy, sep='')
        }
        # throw errors
        if (any(sapply(histograms, function(p) {is(p, 'try-error')}))) {
            if(new.toplevel) tkdestroy(parent)
            stop("histogram could not be created.")
        }
        
        # resize
        sapply(histograms, function(h) {
            tkconfigure(paste(h,'.canvas',sep=''), width=50, height=50)
        })
        
        # grid layout
        lapply(seq_len(2*nvar), 
               function(i){
                   if(i <= nvar) {
                       tkgrid(histograms[[i]], row = 0, column = (i-1), sticky="nesw") 
                   } else {
                       tkgrid(histograms[[i]], row = i - nvar, column = nvar, sticky="nesw") 
                   }
               }
        )

        cellExpand <- nvar
        text_adjustValue <- 0
        scatter_adjustValue <- 1
    }

    if (showSerialAxes) {
        seriel_args <- args
        seriel_args[['data']] <- data
        seriel_args[['showScales']] <- NULL
        seriel_args[['swapAxes']] <- NULL
        seriel_args[['axesLayout']] <- "parallel"
        spaceSpan <- floor(nvar/2)
        serialAxes <- do.call(l_serialaxes, seriel_args)
        tkconfigure(paste(serialAxes,'.canvas',sep=''), 
                    width= spaceSpan * 50, 
                    height = spaceSpan * 50)
        tkgrid(serialAxes, 
               row = cellExpand - spaceSpan + 1, column = 0, 
               rowspan = spaceSpan, columnspan = spaceSpan,
               sticky="nesw") 
    }
    
    scatterplots <- vector(mode="list", dim(pair)[2])
    
    ## create first plot
    for (i in 1:dim(pair)[2]) {
        ix <- pair[2,i]; iy <- pair[1,i]
        args[['x']] <- data[,ix]
        args[['y']] <- data[,iy]
        args[['xlabel']] <- varnames[ix]
        args[['ylabel']] <- varnames[iy]
        scatterplots[[i]] <- do.call(l_plot, args)
        names(scatterplots)[i] <- paste('x',ix,'y',iy, sep='')
    }
    
    if (any(sapply(scatterplots, function(p) {is(p, 'try-error')}))) {
        if(new.toplevel) tkdestroy(parent)
        stop("scatterplot matrix could not be created.")
    }
    
    ## resize the min canvas size
    sapply(scatterplots, function(p) {
               tkconfigure(paste(p,'.canvas',sep=''), width=50, height=50)
           })
    ## grid layout
    apply(rbind(unlist(scatterplots), pair - 1), 2, function(obj) {
        tkgrid(obj[1], 
               row= as.numeric(obj[2]) + scatter_adjustValue, 
               column = as.numeric(obj[3]), 
               sticky="nesw")
    })
    
    ## Column and Row wheight such that the cells expand
    for (i in seq(0, cellExpand)) {
        tkgrid.columnconfigure(child, i, weight = 1)
        tkgrid.rowconfigure(child, i, weight = 1)
    }
    ## Add Variable Label
    maxchar <- max(sapply(names(data), nchar))
    strf <- paste("%-", maxchar,'s', sep='')
    for (i in 1:nvar) {
        lab <- as.character(tcl('label', as.character(l_subwin(child,'label')),
                                text= sprintf(strf, names(data)[i])))
        tkgrid(lab, row = i - text_adjustValue, column = i - 1)
    }
    
    if(new.toplevel) {
        tkpack(child, fill="both", expand=TRUE)
    }

    plotsHash <- vector(mode="list", dim(pair)[2])
    for (i in 1:dim(pair)[2]) {
        ix <- pair[2,i]
        iy <- pair[1,i]
        
        tmpX <- which(pair[2,] == ix)
        shareX <- tmpX[tmpX != i]
        
        tmpY <- which(pair[1,] == iy)
        shareY <- tmpY[tmpY != i]
        if(showHistoram) {
            plotsHash[[paste("x_",scatterplots[i],sep="")]] <- c(scatterplots[shareX], histograms[pair[2,i]])
            plotsHash[[paste("y_",scatterplots[i],sep="")]] <- scatterplots[shareY]
            plotsHash[[paste("y_hist",scatterplots[i],sep="")]] <- histograms[pair[1,i] + nvar]
        } else {
            plotsHash[[paste("x_",scatterplots[i],sep="")]] <- scatterplots[shareX]
            plotsHash[[paste("y_",scatterplots[i],sep="")]] <- scatterplots[shareY]
        }
    }
    
    ## Make bindings for scatter synchronizing zoom and pan
    busy <- FALSE

    synchronizeBindings <- function(W) {
        #print(paste(W, ', busy', busy))
        if (!busy) {
            busy <<- TRUE
            class(W) <- "loon"
            zoomX <- W['zoomX']; zoomY <- W['zoomY']
            panX <- W['panX']; panY <- W['panY']
            deltaX <- W['deltaX']; deltaY <- W['deltaY']
            
            lapply(plotsHash[[paste("x_",W,sep="")]], function(p) {
                l_configure(p, zoomX=zoomX, panX=panX, deltaX=deltaX)
            })
            lapply(plotsHash[[paste("y_",W,sep="")]], function(p) {
                l_configure(p, zoomY=zoomY, panY=panY, deltaY=deltaY)
            })
            if (showHistoram) {
                lapply(plotsHash[[paste("y_hist",W,sep="")]], function(p) {
                    l_configure(p, zoomX=zoomY, panX=panY, deltaX=deltaY)
                }) 
            }
            busy <<- FALSE
            tcl('update', 'idletasks')
            ##assign("busy", FALSE, envir=parent.env(environment()))
        }
    }
    
    lapply(scatterplots, function(p) {
        tcl(p, 'systembind', 'state', 'add',
            c('zoomX', 'panX', 'zoomY', 'panY', 'deltaX', 'deltaY'),
            synchronizeBindings)
    })
        
    # forbidden scatter plots
    lapply(scatterplots, function(p) {
               tcl(p, 'systembind', 'state', 'add',
                   c('showLabels', 'showScales', 'swapAxes'),
                   undoStateChanges)
           })

    plots <- scatterplots
    if (showHistoram) {
        # forbidden histogram states (both top and right)
        lapply(histograms, function(h) {
            tcl(h, 'systembind', 'state', 'add',
                c('showLabels', 'showScales', 'swapAxes'),
                undoStateChanges)
        })
        
        plots<- c(plots, histograms)
    }
    if(showSerialAxes) {
        plots <- c(plots, list(serialAxes = serialAxes))
    }
    
    ## beware undoStateChanges and synchronizeBindings from garbage collector
    callbackFunctions$state[[paste(child,"synchronize", sep="_")]] <- synchronizeBindings
    callbackFunctions$state[[paste(child,"undo", sep="_")]] <- undoStateChanges
    
    structure(
        plots,
        class = c("l_pairs", "loon")
    )
}


#'@export
names.l_pairs <- function(x) {attr(x, "names")}

#' @export
l_cget.l_pairs <- function(target, state) {
    
    plotNames <- names(target)
    plots <- lapply(plotNames, 
                    function(plotName) {
                        target[[plotName]]
                        
                    })
    values <- lapply(plots, l_cget, state)
    
    values
    
}


#' @export
l_configure.l_pairs <- function(target, ...) {
    
    args <- list(...)
    states <- names(args)
    if (is.null(states) || any("" %in% states))
        stop("configuration needs key=value pairs")
    
    plotNames <- names(target)
    plots <- lapply(plotNames, 
                    function(plotName) {
                        target[[plotName]]
        
    })
    for (state in states) {
        
        switch(
            state,
            linkingGroup = lapply(plots, l_configure, 
                                  linkingGroup = args$linkingGroup, sync = "pull"),
            selected = stop("not implemented yet"),
            stop("state ", state, " not implemented")
        )
    }
    
    target
}

## forbidden states
undoStateChanges <- function(W) {
    warning("showLabels, showScales, and swapAxes can not be changed for scatterplot matrix.")
    l_configure(W, showLabels=FALSE, showScales=FALSE, swapAxes=FALSE)
}
