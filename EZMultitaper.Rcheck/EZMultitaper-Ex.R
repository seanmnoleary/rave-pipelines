pkgname <- "EZMultitaper"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "EZMultitaper-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('EZMultitaper')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("meanPowBaselineBand")
### * meanPowBaselineBand

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: meanPowBaselineBand
### Title: compute the mean power analysis over the frequency band using
###   the multitaper method
### Aliases: meanPowBaselineBand

### ** Examples

data("pt01EcoG")
windowParams<-c(0.25,0.1)
betaBand<-c(13,30)
epoch <- Epoch(pt01EcoG)
betaBandPow<-meanPowBaselineBand( epoch=epoch, fs=1000, windowParams=windowParams, rangeBand=betaBand)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("meanPowBaselineBand", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("multitaperSpectrogramR")
### * multitaperSpectrogramR

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: multitaperSpectrogramR
### Title: Multitaper Spectrogram functions
### Aliases: multitaperSpectrogramR

### ** Examples

data("pt01EcoG")

timeWindow <- c(-10, 20)
epoch <- Epoch(pt01EcoG)
fs=1000
timeNum <- ncol(epoch)
windowParams = c(1, 0.2) 
nwt=floor((timeNum/fs-windowParams[1])/windowParams[2])+1
data   <- vector(mode="numeric", length=timeNum)
data[1:timeNum]<-dataMat[sozIndex[1],1:timeNum]
# Compute the multitaper spectrogram
results = multitaperSpectrogramR(data, fs, frequencyRange, timeBandwidth, numTapers, windowParams, minNfft, weighting, detrendOpt, parallel, numWorkers,
                                 plotOn, verbose, xyflip)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("multitaperSpectrogramR", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plotPow")
### * plotPow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot,MeanPowBand,missing-method
### Title: Visualization functions (raw signal, mean power band matrix)
### Aliases: plot,MeanPowBand,missing-method plotPowDistribution
###   plotPowQuantile

### ** Examples


data("pt01EcoG")
## plot the mean band power distribution

## plot the mean power quantiles
plotPowQuantile(Pow = pt01Pow, groupIndex = sozNames)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plotPow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("powStat")
### * powStat

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: powStat
### Title: Compute quantiles, mean and standard deviation for two
###   electrodes groups
### Aliases: powStat

### ** Examples

data("pt01Frag")
data("pt01EcoG")
## sozNames is the name of the electrodes we assume are in the SOZ
sozNames <- metaData(pt01EcoG)$sozNames
pt01fragstat <- fragStat(frag = pt01Frag, groupIndex = sozNames)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("powStat", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("process_spectrogram_params")
### * process_spectrogram_params

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: process_spectrogram_params
### Title: Process spectrogram inputs. Helper function to create frequency
###   vector and window indices
### Aliases: process_spectrogram_params

### ** Examples

# Set up spectrogram parameters
res <- process_spectrogram_params(fs, nfft, frequencyRange, window_start, winsize_samples)
window_idxs <- res[[1]]
stimes <- res[[2]]
sfreqs <- res[[3]]
freq_inds <- res[[4]]



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("process_spectrogram_params", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
