l_linkingWarning <- function(widget, sync = "pull", args, modifiedLinkedStates = character(0L), l_className = NULL) {

    if(sync == "push") return(invisible())

    deprecatedLinkedVar <- l_getDeprecatedLinkedVar(widget, args, modifiedLinkedStates, l_className)

    if(length(deprecatedLinkedVar) > 0)
        warning("The aesthetics ",
                paste(deprecatedLinkedVar, collapse = ", "),
                " will not be set, since they are pulled from the existing linked plots.",
                call. = FALSE)
}
#
#
# Note that the following documentation is a quick attempt gleaned from a quick reading of the
# default code following it.
#
#
#' @title A helper function to determine which, if any, linked variables in a plot are now deprecated.
#'
#' @description Checks which linkable states are linked, whether their input state is the default one, valid,
#' and equal to the plot current states.  It is not intended for the general user.
#' @param widget the loon widget or a list of loon widgets
#' @param args named list of state values for linkable state variables named in the list.
#' @param modifiedLinkedStates vector of variable names having modified link states/
#' @param l_className class of the loon widget
#'
#' @return a named vector, or list of vectors, of logicals indicating whether these linkable states need be deprecated or not.
#' @export
#' @keywords internal
l_getDeprecatedLinkedVar <- function(widget, args, modifiedLinkedStates = character(0L), l_className = NULL) {
    UseMethod("l_getDeprecatedLinkedVar", widget)
}

#' @export
#' @keywords internal
l_getDeprecatedLinkedVar.default <- function(widget, args, modifiedLinkedStates = character(0L), l_className = NULL) {

    if(is.null(l_className)) l_className <- class(widget)[1L]

    linkableStatesVar <- hasDefaultLinkableStatesVar(l_className)

    deprecatedLinkedVar <- linkableStatesVar[vapply(linkableStatesVar,
                                                    function(var) {
                                                        state <- args[[var]]
                                                        if(is.null(state)) return(FALSE)

                                                        uniqueState <- unique(state)
                                                        # Why warnings? Both conditions must be "required"
                                                        #   1. the input linked states are not default
                                                        #   2. the input linked states are not equal to the plot current states

                                                        widgetVar <- widget[var]
                                                        con1IsViolated <- var %in% modifiedLinkedStates
                                                        # convert color as hex code

                                                        if(var == "color") {

                                                            # `color` is not a real color
                                                            con2IsViolated <- tryCatch(
                                                                expr = {
                                                                    any(hex12tohex6(widgetVar) != as_hex6color(state))
                                                                },
                                                                error = function(e) {

                                                                    levels1 <- levels(factor(widgetVar))
                                                                    levels2 <- levels(factor(state))

                                                                    # state is not a real color, instead it is a substitution
                                                                    ## match factors
                                                                    any(vapply(seq(length(widgetVar)),
                                                                           function(i) {
                                                                               which(levels1 %in% widgetVar[i]) !=
                                                                                   which(levels2 %in% state[i])
                                                                           }, logical(1L)))
                                                                }
                                                            )


                                                        } else {
                                                            con2IsViolated <- any(state != widgetVar)
                                                        }

                                                        con1IsViolated && con2IsViolated

                                                    }, logical(1L))]

    deprecatedLinkedVar
}


#' @export
#' @keywords internal
l_getDeprecatedLinkedVar.list <- function(widget, args, modifiedLinkedStates = character(0L), l_className = NULL) {

    deprecatedLinkedVars <- lapply(widget,
                                   function(w) {
                                       l_getDeprecatedLinkedVar.default(w, args, modifiedLinkedStates, l_className)
                                   })

    unique(unlist(deprecatedLinkedVars))
}
