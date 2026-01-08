# A plot function that takes a data frame and returns a heatmap plot
makeHeatMap <- function(df, xTicksNum = 10, maxLabels = Inf){
  xLabels <- colnames(df)
  yLabels <- rownames(df)

  if(is.null(xLabels)){
    xLabels <- seq_len(ncol(df))
  }
  if(is.null(yLabels)){
    yLabels <- seq_len(nrow(df))
  }

  df$y <- yLabels
  df_long <- reshape2::melt(df, id.vars = "y", variable.name = "x", value.name = "value")
  colnames(df_long) <- c("y", "x", "value")

  ## sort df_long by rownames
  df_long$x <- factor(df_long$x, levels = xLabels)
  df_long$y <- factor(df_long$y, levels = rev(yLabels))
  ## show 10 time points on x-axis at most
  if (length(xLabels) > xTicksNum){
    step <- ceiling(length(xLabels) / xTicksNum)
    breaksIdx <- seq(1, length(xLabels), by = step)
    #breaks <- as.character(round(as.numeric(xLabels[breaksIdx]),digits=1))
    breaks <- xLabels[breaksIdx]
  } else {
    breaks <- xLabels
  }

  ## limit the number of labels on y-axis
  yLabelsForDisplay <- rev(yLabels)  # Match the reversed factor levels
  if (length(yLabelsForDisplay) > maxLabels) {
    by_num <- ceiling(length(yLabelsForDisplay)/maxLabels)
    label_idx <- seq(length(yLabelsForDisplay), 1, by=-by_num)
    yLabelsForDisplay[-label_idx] <- ""
  }

  ggplot(df_long) +
    geom_tile(aes(x = .data$x, y = .data$y, fill = .data$value)) +
    scale_x_discrete(labels = breaks, breaks = breaks) +
    scale_y_discrete(labels = yLabelsForDisplay, breaks = rev(yLabels)) +
    theme(plot.title = element_markdown(hjust = 0.5)) +
    viridis::scale_fill_viridis(option = "turbo") +
    theme_minimal()
}



#' Visualization functions (raw signal, mean power band matrix)
#'
#' @description `plot`: plot mean power band with electrodes marked as soz colored
#'
#' @param x MeanPowBand object from \code{meanPowBaselineBand}
#' @param y Not used (for S4 method compatibility)
#' @param pow meanPowBaselineBand object from \code{meanPowBaselineBand}
#' @param x.lab.size Numeric. Size of x-axis labels. Default is 4.
#' @param y.lab.size Numeric. Size of y-axis labels. Default is 10
#' @inheritParams powStat
#' @param maxLabels Integer. Maximum number of labels to show on y-axis. Default is 50. The actual number of labels may be less than this value if there are too many electrodes.
#'
#' @return A ggplot object
#'
#' @examples
#'
#' data("pt01EcoG")
#' @rdname plotPow
#' @export
setMethod("plot", signature(x = "MeanPowBand", y = "missing"),
          function(x, y,
                   groupIndex = NULL,
                   maxLabels = 50,
                   ranked = FALSE,
                   x.lab.size = 10,
                   y.lab.size = 10) {
            powMat <- x$pow

            elecNum <- nrow(powMat)
            windowNum <- ncol(powMat)

            elecNames <- x$electrodes
            groupIndex <- checkIndex(groupIndex, elecNames)

            group1 <- groupIndex
            group2 <- setdiff(seq_len(elecNum), groupIndex)

            elecColor <- rep("blue", elecNum)
            elecColor[seq_along(group2)] <- "black"

            startTime <- x$startTimes
            if (is.null(startTime)) {
              xlabel <- "Time Index"
              stimes <- seq_len(windowNum)
            } else {
              xlabel <- "Time (s)"
              stimes <- round(startTime,digits=1)
            }

            rownames(powMat) <- x$electrodes
            colnames(powMat) <- stimes

            ## prepare the data.frame for visualization
            allIndex <- c(group1, group2)
            df <- as.data.frame(powMat[allIndex, ])

            makeHeatMap(df, maxLabels = maxLabels) +
              labs(x = xlabel, y = "Electrode") +
              theme(
                axis.text.x = element_text(size = x.lab.size),
                axis.text.y = element_markdown(size = y.lab.size, colour = elecColor), # Adjust depending on electrodes
              )
          }
)




#' @description `plotPowDistribution`: Plot power time distribution for two electrodes groups
#' @param bandType Character. The type of band to use, either "SEM" or "SD". Default is "SEM".
#' @param rollingWindow Integer. Window size for rolling average smoothing. Default is 1 (no smoothing).
#' @rdname plotPow
#' @examples
#' ## plot the mean band power distribution
#'
#' @export
plotPowDistribution <- function(
    pow, groupIndex = NULL,
    groupName="SOZ", bandType = c("SEM", "SD"),
    rollingWindow = 1, ranked=FALSE,
    x.lab.size = 10,
    y.lab.size = 10) {
  bandType <- match.arg(bandType)
  if (is.null(groupIndex)) {
    groupIndex <- estimateSOZ(pow)
  }
  groupIndex <- checkIndex(groupIndex, pow$electrodes)

  windowNum <- ncol(pow$pow)
  stat <- powStat(
    pow,
    groupIndex = groupIndex
  )

  groupMean <- stat$groupMean
  refMean <- stat$refMean
  if (bandType == "SEM") {
    groupWidth <- stat$groupSEM
    refWidth <- stat$refSEM
  } else if (bandType == "SD") {
    groupWidth <- stat$groupSD
    refWidth <- stat$refSD
  }

  ## Apply rolling average smoothing if requested
  if (rollingWindow > 1) {
    groupMean <- rolling_mean(groupMean, rollingWindow)
    refMean <- rolling_mean(refMean, rollingWindow)
    groupWidth <- rolling_mean(groupWidth, rollingWindow)
    refWidth <- rolling_mean(refWidth, rollingWindow)
  }

  groupUpperBound <- groupMean + groupWidth
  groupLowerBound <- groupMean - groupWidth
  refUpperBound <- refMean + refWidth
  refLowerBound <- refMean - refWidth

  startTimes <- pow$startTimes
  if (is.null(startTimes)) {
    xlabel <- "Time Index"
    timeTicks <- seq_len(windowNum)
  } else {
    xlabel <- "Time (s)"
    timeTicks <- startTimes
  }


  plotData <- data.frame(
    timeTicks = timeTicks,
    groupMean = groupMean,
    groupUpperBound = groupUpperBound,
    groupLowerBound = groupLowerBound,
    refMean = refMean,
    refUpperBound = refUpperBound,
    refLowerBound = refLowerBound
  )

  groupColor <- glue::glue("{groupName} +/- {bandType}")
  refColor <- glue::glue("REF +/- {bandType}")
  colors <- setNames(c("red", "black"), c(groupColor, refColor))

  ggplot(plotData, aes(x = .data$timeTicks)) +
    xlab(xlabel) +
    ylab("Mean Band Power") +
    geom_line(
      aes(y = .data$groupMean, color = groupColor)
    ) +
    geom_line(aes(y = .data$groupUpperBound), color = "red", linetype = "blank") +
    geom_line(aes(y = .data$groupLowerBound), color = "red", linetype = "blank") +
    geom_line(aes(y = .data$refMean, color = refColor)) +
    geom_line(aes(y = .data$refUpperBound), color = "black", linetype = "blank") +
    geom_line(aes(y = .data$refLowerBound), color = "black", linetype = "blank") +
    geom_ribbon(aes(ymin = .data$groupLowerBound, ymax = .data$groupUpperBound), fill = "red", alpha = 0.5) +
    geom_ribbon(aes(ymin = .data$refLowerBound, ymax = .data$refUpperBound), fill = "black", alpha = 0.5) +
    scale_color_manual(name = "Electrode groups", values = c(colors))+
    theme(
      axis.text.y = element_text(size = y.lab.size),
      axis.text.x = element_text(size = x.lab.size)
    )

}

#' @description `plotPowQuantile`: Plot mean power time quantiles for two electrodes groups
#'
#' @rdname plotPow
#' @examples
#' ## plot the mean power quantiles
#' plotPowQuantile(Pow = pt01Pow, groupIndex = sozNames)
#'
#' @export
plotPowQuantile <- function(pow, groupIndex = NULL, groupName = "SOZ",
                             x.lab.size = 10,
                             y.lab.size = 10) {
  if (is.null(groupIndex)) {
    groupIndex <- estimateSOZ(pow)
  }
  groupIndex <- checkIndex(groupIndex, pow$electrodes)
  windowNum <- ncol(pow)

  stat <- powStat(
    pow,
    groupIndex = groupIndex,
    groupName = groupName
  )
  qmatrix <- as.data.frame(stat$qmatrix)

  startTimes <- round(pow$startTimes,digits=1)
  if (is.null(startTimes)) {
    xlabel <- "Time Index"
    timeTicks <- seq_len(windowNum)
  } else {
    xlabel <- "Time (s)"
    timeTicks <- startTimes
  }

  colnames(qmatrix) <- timeTicks

  makeHeatMap(qmatrix)+
    labs(x = xlabel, y = "Quantiles") +
    theme(
      axis.text.y = element_text(size = y.lab.size),
      axis.text.x = element_text(size = x.lab.size)
    )
}
