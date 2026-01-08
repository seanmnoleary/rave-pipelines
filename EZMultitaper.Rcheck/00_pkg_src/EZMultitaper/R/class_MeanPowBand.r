.MeanPowBand<- setClass(
    "MeanPowBand",
    slots = list(
        pow = "matrix",
        startTimes = "numeric",
        electrodes = "character"
    )
)


MeanPowBand<- function(pow, startTimes, electrodes) {

  .MeanPowBand(
    pow = pow,
    startTimes = startTimes,
    electrodes = electrodes
  )
}


#' @rdname cash-PowStat-method
setMethod("$", "MeanPowBand", function(x, name) {
  slot(x, name)
})

#' @rdname cash-PowStat-method
setMethod("$<-", "MeanPowBand", function(x, name, value) {
  slot(x, name) <- value
  invisible(x)
})

#' Print the MeanPowBand object
#' @param object A MeanPowBand object
#' @rdname show-MeanPowBand-method
#' @export
setMethod("show", "MeanPowBand", function(object) {
  cat("\nMeanPowBand object\n")
    slots <- c("pow","startTimes","electrodes")
  printSlots(object, slots = slots)
  cat("Use '$attr' to access the data\n")
  invisible(object)
})


#' Get the number of rows or columns of a MeanPowBand object
#'
#' @param x A MeanPowBand object
#'
#' @rdname dim-MeanPowBand-method
setMethod("nrow", "MeanPowBand", function(x) {
  nrow(x@pow)
})

#' @rdname dim-MeanPowBand-method
setMethod("ncol", "MeanPowBand", function(x) {
  ncol(x@pow)
})


#' Subset a MeanPowBand object
#'
#' @param x A MeanPowBand object
#' @param i A logical vector or a numeric vector of indices to subset the electrodes
#' @param j A logical vector or a numeric vector of indices to subset the time windows
#' @param ... Additional arguments (not used)
#' @param drop Additional arguments (not used)
#' 
#' @rdname subset-MeanPowBand-method
setMethod("[", "MeanPowBand", function(x, i, j, ..., drop = FALSE) {
  
  if (!missing(i)){
    i <- checkIndex(i, x$electrodes)
  }else{
    i <- TRUE
  }
  if(missing(j)){
    j <- TRUE
  }
  
  pow_subset <- x@pow[i, j, drop = FALSE]
  startTimes_subset <- x@startTimes[j]
  electrodes_subset <- x@electrodes[i]
  .MeanPowBand(
    pow = pow_subset,
    startTimes = startTimes_subset,
    electrodes = electrodes_subset
  )
})
