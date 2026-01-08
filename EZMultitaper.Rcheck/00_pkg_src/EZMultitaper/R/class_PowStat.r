PowStat <- setClass(
    "PowStat",
    slots = list(
        qmatrix = "matrixOrNULL",
        groupMean  = "numericOrNULL",
        refMean ="numericOrNULL",
        groupSD    = "numericOrNULL",
        refSD   = "numericOrNULL",
        groupSEM   = "numericOrNULL",
        refSEM = "numericOrNULL"

    )
)



#' Getters and Setters for S4 object
#'
#' @param x S4 object
#' @param name Slot name
#' @param value Value to set
#' @return S4 object itself or slot value
#' @export
setMethod("$", "PowStat", function(x, name) {
    slot(x, name)
})

#' @rdname cash-PowStat-method
setMethod("$<-", "PowStat", function(x, name, value) {
    slot(x, name) <- value
    invisible(x)
})

#' Print the PowStat object
#' @param object A PowStat object
#' @rdname show-PowStat-method
#' @export
setMethod("show", "PowStat", function(object) {
    cat("\nPowStat object (Summary Statistics by Step)\n")
    printSlots(object)
    cat("Use '$attr' to access the data\n")
    invisible(object)
})
