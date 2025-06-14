

.withTclImg <- FALSE
.withCorrectTclImg <- TRUE # OS X might load system Img package
.withCimgScale <- TRUE


#' @import tcltk
.onLoad <- function(libname, pkgname) {
#
#     # if (as.character(tcl('set', 'tcl_version'))!="8.6") {
#     #     stop("Tcl version 8.6 required.")
#     #     ## tclRequire('TclOO') > done by pkgIndex.tcl
#     #     ## .Tcl("
#     #     ##      proc lmap {_var list body} {
#     #     ##          upvar 1 $_var var
#     #     ##          set res {}
#     #     ##          foreach var $list {lappend res [uplevel 1 $body]}
#     #     ##          set res
#     #     ##      }
#     #     ##")
#     # }
#     version <- as.character(tcltk::tclVersion())
#     major <- as.integer(strsplit(version, "\\.")[[1]][1])
#     minor <- as.integer(strsplit(version, "\\.")[[1]][2])
#
#     if ((major == 8) && (minor < 6)) {
#         stop(sprintf("Tcl version >= 8.6 required; found %s", version), call. = FALSE)
#     }
#
#     # if (!(major == 8 && minor >= 6)) {
#     #     stop(sprintf("Tcl version 8.6 required; found %s", version), call. = FALSE)
#     # }
#     #
#     # if (major >= 9) {
#     #     stop(sprintf("Tcl version < 9.0 required; found %s; Tcl version >= 9.0 coming", version), call. = FALSE)
#     # }


    ## Load Tcl package
    libdir <- file.path(find.package(package = pkgname), "tcl")

    ## sometimes there are issues with devtools and testthat
    if (!dir.exists(libdir))
        libdir <- sub("tcl$", "inst/tcl", libdir)
    # cat(paste0("libdir = ", libdir, "\n"))

    tcl('lappend','auto_path',libdir)


    if (FALSE) {
        ## image resizing function in C
        .Tcl(paste('load "',
                   system.file("libs",.Platform$r_arch,
                               paste("ImgscaleTea",.Platform$dynlib.ext,sep=''),
                               package = pkgname, lib.loc = libname),
                   '"', sep=''))
    }

    ## load Img tk extension
    sysname <- Sys.info()[1]

    suppressWarnings(didLoad <- tclRequire('Img'))
    # if (sysname == "Darwin" && identical(didLoad, FALSE)) {
    #     addTclPath("/System/Library/Tcl")
    #     suppressWarnings(didLoad <- tclRequire('Img'))
    #     .withCorrectTclImg <<- FALSE
    # }

    if(identical(didLoad, FALSE)) {
       # warning(
       #     paste("Can not load the tcl Img extension.",
       #           "Hence you can not use the\n'l_image_import_files'",
       #           "R function. Read the package vignette\non",
       #           "how to set up tcl/tk."))
    } else {
        .withTclImg <<- TRUE
    }

    if(FALSE && .withTclImg && !.withCorrectTclImg) {
        warning(
            paste("The tcl Img extension was loaded from",
                  "the system tcl installation.\nNote that the",
                  "tcl interpreter that is bundled with R is",
                  "not\ncompletely compatible with this system Img extension.",
                  "\nIn particular, exporting plots in the png, jpg, tiff, and",
                  "bmp formats\nis not possible.")
            )

    }



    .Tcl('package require loon')


#    if (Sys.info()['sysname'] != "Windows") {
        ## Use the version that comes with R package
        ## on windows I still experience issues with the compiled image_scale procedure
#        .Tcl('set ::loon::Options(image_scale) image_scale')
#    }

    if (Sys.info()['sysname'] == "Windows") {
        .Tcl(paste0('proc ::loon::loon_toplevel {} {',
                    .Tcl.callback(.ltoplevel),
                    '; return $::loon::last_toplevel}'))
    }

    .Tcl('set ::loon::Options(printInConfigurationWarningMsg) ".Tcl(\'set ::loon::Options(printInConfigurationWarning) FALSE\')"')

    # Will now turn off the message warning
    .Tcl('set ::loon::Options(printInConfigurationWarning) FALSE')

}

.ltoplevel <- function() {
    tt <- tktoplevel()
    tcl('set', '::loon::last_toplevel', tt$ID)
}
