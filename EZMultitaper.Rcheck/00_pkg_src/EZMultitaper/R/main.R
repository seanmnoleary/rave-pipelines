standardizeIEEG <- function(data) {
  scaling <- 10^floor(log10(max(data)))
  plotData <- data / scaling
}

#' compute the mean power analysis over the frequency band using the multitaper method
#'
#' @param epoch Matrix or Epoch object. iEEG data matrix or Epoch object. If matrix, the row names are the electrode names and the column names are the time points
#' @param fs Numeric. frequency of signal iEEG acquisition
#' @param windowParams  Numeric. Window parameters of the multitaper method
#' @param rangeBand Numeric. Frequency band of the multitaper power analysis
#' @param powTimeWindow  Numeric. Time window around seizure onset of the power analysis
#' @param baseTimeWindow Numeric. Time window around seizure onset to subtract the mean baselien signal
#'
#' @return A power analysis matrix
#' @export
#'
#' @examples
#' data("pt01EcoG")
#' windowParams<-c(0.25,0.1)
#'betaBand<-c(13,30)
#'epoch <- Epoch(pt01EcoG)
#'betaBandPow<-meanPowBaselineBand( epoch=epoch, fs=1000, windowParams=windowParams, rangeBand=betaBand)
meanPowBaselineBand <- function(epoch, fs=1000, windowParams, rangeBand, powTimeWindow=c(0,20), baseTimeWindow=c(-30,-20)){


  timeSeries<-tblData(epoch)
  elecNames <- rownames(timeSeries)
  times <- as.numeric(colnames(timeSeries))
  elecNum <- length(elecNames)
  timeNum <- length(times)
  #TODO more option baselining line in mni
  nwt=floor((timeNum/fs-windowParams[1])/windowParams[2])+1
  #data   <- vector(mode="numeric", length=timeNum)
  data<-timeSeries[1,]
  # Compute the multitaper spectrogram
  results = multitaperSpectrogramR(data=data, fs=fs, windowParams = windowParams, frequencyRange=rangeBand)
  stimes = results[[2]]

  timesOnset<-results[[2]]+times[1]

  startBaseIndex<-which.min(abs(timesOnset - baseTimeWindow[1]))
  endBaseIndex<-which.min(abs(timesOnset - baseTimeWindow[2]))

  elecNum <- nrow(epoch)

  startEpochIndex<-which.min(abs(timesOnset - powTimeWindow[1]))
  endEpochIndex<-which.min(abs(timesOnset - powTimeWindow[2]))

  timesOnset<-timesOnset[startEpochIndex:endEpochIndex]
  powBandMaster=matrix(0,elecNum,length(timesOnset))


  for(ie in 1:elecNum){

    data<-timeSeries[ie,]
    # Compute the multitaper spectrogram
    results <-multitaperSpectrogramR(data=data, fs=fs, windowParams = windowParams, frequencyRange=rangeBand)
    spect<-results[[1]]

    powBandOneElec<-colSums(spect)

    mBasePow<-mean( powBandOneElec[startBaseIndex:endBaseIndex])
    powBandOneElec<- powBandOneElec-mBasePow

    powBandMaster[ie,1:length(timesOnset)]<-powBandOneElec[startEpochIndex:endEpochIndex]

  }

  MeanPowBand(
    pow = powBandMaster,
    startTimes = timesOnset,
    electrodes = elecNames
  )


}


#' Find Seizure Onset Zone
#'
#' The function estimates the seizure onset zone (SOZ). For each row, it calculates the maximum, minimum, or mean of row. The rows with the highest values are considered as the SOZ.
#'
#' @param x MeanPowBand object
#' @param method Character. The method to use to find the onset zone.
#' Must be one of 'max', 'min', or "mean"
#' @param proportion Numeric. The proportion of electrodes to consider as the onset zone.
#' The electrode number will be rounded to the nearest integer.
#' @param ... Additional arguments
#'
#' @return A vector of electrode names, or indices if the electrode names are NULL
#' @export
estimateSOZ <- function(x, method = c("mean", "median", "max", "min"), proportion = 0.1, ...) {
  method <- match.arg(method)

  stopifnot(is(x, "MeanPowBand"))

  powMat <- x$pow
  elCnt <- nrow(powMat)
  nSOZ <- ceiling(elCnt * proportion)
  stopifnot(nSOZ > 0 & nSOZ <= elCnt)

  if (method == "max") {
    stat <- apply(powMat, 1, max)
  } else if (method == "min") {
    stat <- apply(powMat, 1, min)
  } else if (method == "mean") {
    stat <- apply(powMat, 1, mean)
  } else if (method == "median") {
    stat <- apply(powMat, 1, median)
  } else {
    stop("Unknown method: ", method)
  }

  sozIndex <- order(stat, decreasing = TRUE)[seq_len(nSOZ)]
  if (!is.null(x$electrodes)) {
    sozIndex <- x$electrodes[sozIndex]
  }

  sozIndex
}
