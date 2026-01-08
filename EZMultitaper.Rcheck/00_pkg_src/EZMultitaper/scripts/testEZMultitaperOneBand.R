library(Epoch)
library(ggplot2)
library(ggtext)
library(gsignal)

# access package Data in OSF
dl<-EpochDownloader(progress = FALSE)
names(dl)

# test EZMultitaper with Epoch package
# data download one patient one seizure
pt01sz1 <- dl$FragilityData_subpt01_1
pt01sz1

#pt01sz1ieeg<-tblData(pt01sz1)

# crop
pt01sz1Cropped<-crop(pt01sz1, start=-30, end=20)
gainInput=0.5

# 120925 Load and build Epoch Package
# from https://github.com/KarasLabRAVE/Epoch/tree/devel-cecile
# Change gain
visualSOZ <- function(epoch, sozNames, gainInput) {
  p <- plot(epoch,gain=gainInput)

  elecColor <- rep("black", nrow(epoch))
  elecColor[rownames(epoch) %in% sozNames] <- 'red'
  elecColor <- rev(elecColor) # match the electrode order in the plot

  p +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1)+
    theme(axis.text.y = element_markdown(colour = elecColor))
}

#To visualize patient 01 seizure 1:
pt01sozName <- rownames(pt01sz1Cropped)[rowData(pt01sz1Cropped)$soz]
pt01Display <- c(pt01sozName, "MLT1", "MLT2", "MLT3", "MLT4")
pt01sz1Reordered <- pt01sz1Cropped[pt01Display, ]

visualSOZ(pt01sz1Reordered, pt01sozName,gainInput)

#Set spectrogram parameters
#define frequency bands
deltaBand<-c(0.5,4)
thetaBand<-c(4,8)
alphaBand<-c(8,13)
betaBand<-c(13,30)
gammaBand<-c(30,90)
highGammaBand<-c(90,150)

rangeBand<-betaBand
powTimeWindow<-c(0,20)
baseTimeWindow<-c(-30,-20)

meta<-metaData(pt01sz1Reordered)
fs<-metaData(pt01sz1Reordered)$samplingRate
windowParams<-c(0.25,0.1)


# compute the mean power analysis over the frequency band (rangeBand) over time window (powTimeWindow) and baselined time window (baseTimeWindow)
betaBandPow<-meanPowBaselineBand( epoch=pt01sz1Reordered, fs=fs, windowParams=windowParams, rangeBand=betaBand, powTimeWindow=powTimeWindow, baseTimeWindow=baseTimeWindow)

powMat <- betaBandPow$pow
colnames(powMat) <- round(betaBandPow$startTimes,digits=1)

# power heatmap with the same display options as the previous voltage plot.
# Looking at both plots allows to check correlation between soz patterns
powHeatmap <- function(pow, sozNames) {
  startTimes <- pow$startTimes

  indexsz <- which(abs(startTimes)<=0.01)
  elecColor <- rep("black", length(pow$electrodes))
  elecColor[pow$electrodes%in% sozNames] <- 'red'
  elecColor <- rev(elecColor)

  plot(pow)+
   # geom_vline(xintercept = indexsz, color = "black", linetype = "dashed", linewidth = 1) +
    theme(
      axis.text.y = element_markdown(colour = elecColor)
    )
}

powHeatmap(betaBandPow, pt01sozName)

## plot the mean band power distribution
plotPowDistribution(pow = betaBandPow, groupIndex = pt01sozName, bandType="SEM", rollingWindow = 1)

plotPowQuantile(pow = betaBandPow, groupIndex = pt01sozName)
