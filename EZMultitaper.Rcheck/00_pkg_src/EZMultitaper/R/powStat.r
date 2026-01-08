# Utility functions

calcStat <- function(mat){
  if (nrow(mat) == 0 || ncol(mat) == 0) {
    return(list(mean = numeric(0), sd = numeric(0), sem = numeric(0)))
  }

  list(
    mean = colMeans(mat, na.rm = TRUE),
    sd = apply(mat, 2L, sd, na.rm = TRUE),
    sem = apply(mat, 2L, function(x) sd(x, na.rm = TRUE)/sqrt(length(x)))
  )
}


#' Compute quantiles, mean and standard deviation for two electrodes groups
#'
#' @param pow MeanPowBand object from \code{meanPowBaselineBand}
#' @param groupIndex Integer or string. A group of electrodes to mark
#' @param groupName Character. Name of the group of electrodes, default is "SOZ"
#'
#' @return list of 5 items with quantile matrix, mean and sdv from both electrodes groups
#'
#' @examples
#' data("pt01Frag")
#' data("pt01EcoG")
#' ## sozNames is the name of the electrodes we assume are in the SOZ
#' sozNames <- metaData(pt01EcoG)$sozNames
#' pt01fragstat <- fragStat(frag = pt01Frag, groupIndex = sozNames)
#' @export
powStat <- function(pow, groupIndex = NULL, groupName="SOZ") {

  stopifnot(is(pow, "MeanPowBand"))
  groupIndex <- checkIndex(groupIndex, pow$electrodes)

  powMat <- pow$pow
  stopifnot(is.matrix(powMat))

  steps <- ncol(powMat)
  refIndex <- setdiff(seq_len(nrow(powMat)), groupIndex)

  groupMat <- powMat[groupIndex, , drop = FALSE]
  refMat <- powMat[refIndex, , drop = FALSE]
  groupStat <- calcStat(groupMat)
  refStat <- calcStat(refMat)

  Q <- seq(.1, 1, by = .1)
  qmatrix <- rbind(
    apply(groupMat, 2, quantile, Q),
    apply(refMat, 2, quantile, Q)
  )

  rowPrefix <- rep(c(groupName, "REF"), each = 10)
  dimN <- dimnames(qmatrix)
  dimnames(qmatrix) <- list(
    Quantiles = paste0(rowPrefix, dimN[[1L]]),
    Step      = dimN[[2L]]
  )
  PowStat(
    qmatrix   = qmatrix,
    groupMean  = groupStat$mean,
    refMean = refStat$mean,
    groupSD    = groupStat$sd,
    refSD   = refStat$sd,
    groupSEM   = groupStat$sem,
    refSEM = refStat$sem
  )
}

