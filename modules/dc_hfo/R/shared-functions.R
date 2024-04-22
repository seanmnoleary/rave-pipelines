# Put shared functions in `shared-*.R` so the pipeline is clean
get_default_cores <- function(round = TRUE) {
  re <- (raveio::raveio_getopt("max_worker") + 1) / 2
  if( round ) {
    re <- ceiling(re)
  }
  re
}

# Multitaper Spectrogram #
multitaper_spectrogram_R <- function(data, fs, frequency_range=NULL, time_bandwidth=5, num_tapers=NULL, window_params=c(5,1),
                                     min_nfft=0, weighting='unity', detrend_opt='linear', parallel=FALSE, num_workers=FALSE,
                                     plot_on=FALSE, verbose=FALSE, xyflip=FALSE){
  # Compute multitaper spectrogram of timeseries data
  #
  # Results tend to agree with Prerau Lab python implementation of multitaper spectrogram with precision on the order of at most
  # 10^-7 with SD of at most 10^-5
  #
  # params:
  #         data (numeric vector): time series data -- required
  #         fs (numeric): sampling frequency in Hz  -- required
  #         frequency_range (numeric vector): c(<min frequency>, <max frequency>) (default: NULL, adjusted to
  #                                           c(0, nyquist) later)
  #         time_bandwidth (numeric): time-half bandwidth product (window duration*half bandwidth of main lobe)
  #                                   (default: 5 Hz*s)
  #         num_tapers (numeric): number of DPSS tapers to use (default: NULL [will be computed
  #                                                               as floor(2*time_bandwidth - 1)])
  #         window_params (numeric vector): c(window size (seconds), step size (seconds)) (default: [5 1])
  #         min_nfft (numeric): minimum allowable NFFT size, adds zero padding for interpolation (closest 2^x) (default: 0)
  #         weighting (char): weighting of tapers ('unity' (default), 'eigen', 'adapt')
  #         detrend_opt (char): detrend data window ('linear' (default), 'constant', 'off')
  #         parallel (logical): use parallel processing to speed up calculation (default: FALSE). Note: speedup is faster on
  #                             unix-like machines (Mac, Linux) because they allow fork processes while Windows does not.
  #         num_workers (numeric): number of cpus/workers to dedicate to parallel processing (default: FALSE). Note: Will
  #                                be ignored if parallel is FALSE. If parallel is TRUE and num_workers is false (or if num_workers
  #                                exceeds available workers), will default to max number of workers available minus 1.
  #         plot_on (logical): plot results (default: TRUE)
  #         verbose (logical): display spectrogram properties (default: TRUE)
  #         xyflip (logical): return the transpose of mt_spectrogram
  #
  # returns:
  #         mt_spectrogram (matrix): spectral power matrix
  #         stimes (numeric vector): timepoints (s) in mt_spectrogram
  #         sfreqs (numeric vector): frequency values (Hz) in mt_spectrogram

  # Process user input
  res <- process_input(data, fs, frequency_range, time_bandwidth, num_tapers, window_params, min_nfft, weighting, detrend_opt,
                       plot_on, verbose)

  data <- res[[1]]
  fs <- res[[2]]
  frequency_range <- res[[3]]
  time_bandwidth <- res[[4]]
  num_tapers <- res[[5]]
  winsize_samples <- res[[6]]
  winstep_samples = res[[7]]
  window_start = res[[8]]
  num_windows <- res[[9]]
  nfft <- res[[10]]
  weighting = res[[11]]
  detrend_opt <- res[[12]]
  plot_on <- res[[13]]
  verbose <- res[[14]]

  # Set up spectrogram parameters
  res <- process_spectrogram_params(fs, nfft, frequency_range, window_start, winsize_samples)
  window_idxs <- res[[1]]
  stimes <- res[[2]]
  sfreqs <- res[[3]]
  freq_inds <- res[[4]]

  # Display spectrogram parameters if desired
  if(verbose){
    display_spectrogram_properties(fs, time_bandwidth, num_tapers, c(winsize_samples, winstep_samples), frequency_range,
                                   detrend_opt)
  }

  # Split data into window segments
  data_segments <- t(sapply(window_idxs, split_data_helper, data=data))

  # COMPUTE THE MULTITAPER SPECTROGRAM
  #     STEP 1: Compute DPSS tapers based on desired spectral properties
  #     STEP 2: Multiply the data segment by the DPSS Tapers
  #     STEP 3: Compute the spectrum for each tapered segment
  #     STEP 4: Take the mean of the tapered spectra

  tic <- proc.time() # start timer for multitaper

  # Compute DPSS tapers (STEP 1)
  dpss_tapers <- dpss(winsize_samples, num_tapers, time_bandwidth, returnEigenvalues=TRUE)
  dpss_eigen = dpss_tapers$eigen
  dpss_tapers = dpss_tapers$v

  # pre-compute weights
  if(weighting == 'eigen'){
    wt = dpss_eigen / num_tapers;
  }
  else if(weighting == 'unity'){
    wt = ones(num_tapers,1) / num_tapers;
  }
  else{
    wt = 0;
  }

  # Compute multitaper #
  if(parallel){ # Check for parallelization
    # workers_avail <- detectCores() - 1  # detect cores available and leave 1 for user
    # if(num_workers==FALSE | num_workers > workers_avail){
    #   num_workers = workers_avail
    #   warning(paste("Number of workers for parallelization either not specified or greater than workers available. Setting number
    #           of workers to number available minus 1 (", toString(num_workers), ")"))
    # }
    # registerDoParallel(cores=num_workers) # register workers with doParallel
    #
    # # Create cluster of workers differently depending on OS
    # if(.Platform$OS.type == "windows"){ # windows cannot use FORK argument
    #   cluster <- makeCluster(num_workers) # create cluster of workers without forking
    # }
    # else{
    #   cluster <- makeCluster(num_workers, type="FORK") # if not windows, use FORK because it's faster
    # }
    #
    # mt_spectrogram <- parApply(cluster, data_segments, 1, calc_mts_segment, dpss_tapers=dpss_tapers, nfft=nfft, freq_inds=freq_inds,
    #                            weighting=weighting, wt=wt, dpss_eigen=dpss_eigen, num_tapers=num_tapers, detrend_opt=detrend_opt)
    #
    # stopCluster(cluster) # stop cluster to give back resources
    # registerDoSEQ() # switch back to serial processing

    res <- raveio::lapply_async(seq_len(nrow(data_segments)), function(row_id) {
      calc_mts_segment(data_segments[row_id, ], dpss_tapers=dpss_tapers, nfft=nfft, freq_inds=freq_inds,
                       weighting=weighting, wt=wt, dpss_eigen=dpss_eigen, num_tapers=num_tapers, detrend_opt=detrend_opt)
    })
    # mt_spectrogram = do.call("cbind", res)
    mt_spectrogram = simplify2array(res)
  }
  else{ # if no parallelization, use normal apply
    mt_spectrogram = apply(data_segments, 1, calc_mts_segment, dpss_tapers=dpss_tapers, nfft=nfft, freq_inds=freq_inds,
                           weighting=weighting, wt=wt, dpss_eigen=dpss_eigen, num_tapers=num_tapers, detrend_opt=detrend_opt)
  }


  # Compute one-sided PSD spectrum
  DC_select = which(sfreqs==0)
  Nyquist_select = which(sfreqs==fs/2)
  select = setdiff(1:(length(sfreqs)), c(DC_select, Nyquist_select))
  mt_spectrogram = rbind(mt_spectrogram[DC_select,], 2*mt_spectrogram[select,], mt_spectrogram[Nyquist_select,]) / fs

  # mt_spectrogram= mt_spectrogram/fs

  # End timer and get elapsed time
  toc = proc.time()
  elapsed = toc-tic
  if(verbose){
    print(paste("Multitaper compute time: ", toString(round(elapsed[[3]], digits=5)), " seconds", sep=""))
  }


  if(all(as.vector(mt_spectrogram) == 0)){
    print("Spectrogram calculated as all zeros, no plot shown")
  }else if(plot_on){
    print("plotting...")

    # Saving to PNG, loading back in, and plotting as raster is actually faster than just plotting using image.plot
    png(filename=paste("spectrogram.png")) # save as png
    image.plot(x=stimes, y=sfreqs, nanpow2db(t(mt_spectrogram)), xlab="Time (s)",
               ylab='Frequency (Hz)')
    #image.plot(x=stimes, y=sfreqs, t(mt_spectrogram), xlab="Time (s)",
    #           ylab='Frequency (Hz)')
    dev.off()

    im <- readPNG("spectrogram.png") # load png
    file.remove("spectrogram.png") # remove png file
    plot.new()
    rasterImage(im,0,0,1,1, interpolate=FALSE) # plot as raster image
    print("done plotting")
  }

  if(xyflip){
    mt_spectrogram = t(mt_spectrogram)
  }

  return(list(mt_spectrogram, stimes, sfreqs))
}

split_data_helper <- function(indices, data){ # for sapply when splitting data into windows
  data_seg = data[indices]
  return(data_seg)
}

process_input <- function(data, fs, frequency_range=NULL, time_bandwidth=5, num_tapers=NULL,
                          window_params=c(5,1), min_nfft=0, weighting='unity', detrend_opt='linear', plot_on=TRUE,
                          verbose=TRUE){

  # Helper function to process multitaper_spectrogram arguments, mainly checking for validity
  #
  # Params:
  #        data (numeric vector): time series data -- required
  #        fs (numeric): sampling frequency in Hz -- required
  #        frequency range (numeric vector): c(<min frequency>, <max frequency>) (default: c(0 nyquist))
  #        time_bandwidth (numeric): time-half bandwidth product (window duration*half bandwidth of main lobe) (default: 5 Hz*s)
  #        num_tapers (numeric): number of DPSS tapers to use (default None [will be computed as floor(2*time_bandwidth - 1)])
  #        window_params (numeric vector): c(window size (seconds), step size (seconds)) default: c(5,1)
  #        detrend_opt (char): detrend data window ('linear' (default), 'constant', 'off')
  #        min_nfft: (numeric): minimum allowable NFFT size, adds zero padding for interpolation (default: 0)
  #        plot_on: (logical): plot results (default: TRUE)
  #        verbose (logical)L display spectrogram properties (default; TRUE)
  #
  # Returns:
  #         data (numeric vector) same as input
  #         fs (numeric): same as input
  #         frequency_range (numeric vector): same as input or calculated from fs if not given
  #         time_bandwidth (numeric): same as input or default if not given
  #         num_tapers (numeric): same as input or calculated from time bandwidth if not given
  #         winsize_samples (numeric): number of samples in a single time window
  #         winstep_samples (numeric): number of samples in a single window step
  #         window_start (numeric vector): matrix of timestamps representing the beginning time for each window
  #         num_windows (numeric): number of total windows
  #         nfft (numeric): length of signal to calculate fft on
  #         detrend_opt (char): same as input or default if not given
  #         plot_on (logical): same as input or default if not given
  #         verbose (logical): same as input or default if not given


  # Make sure data is 1D atomic vector
  if((is.atomic(data) == FALSE) | is.list(data)){
    stop("data must be a 1D atomic vector")
  }

  # Set frequency range if not provided
  if(is.null(frequency_range)){
    frequency_range <- c(0, fs/2)
  }

  # Set detrend method
  detrend_opt = tolower(detrend_opt)
  if(detrend_opt != 'linear'){
    if(detrend_opt == 'const' || detrend_opt == 'constant'){
      detrend_opt <- 'constant'
    } else if(detrend_opt == 'none' || detrend_opt == 'false' || detrend_opt == 'off'){
      detrend_opt <- 'off'
    }else{
      stop(paste("'", toString(detrend_opt), "' is not a valid detrend_opt argument. The",
                 " choices are: 'constant', 'linear', or 'off'.", sep=""))
    }
  }

  # Set taper weighting options
  weighting = tolower(weighting)
  if(weighting == 'adaptive' || weighting == 'adapt'){
    weighting = 'adapt'
  } else if(weighting == 'eig' || weighting == 'eigen'){
    weighting = 'eigen'
  } else if(weighting != 'unity'){
    stop(paste("'", toString(weighting), "' is not a valid weighing argument. Choices are: 'unity', 'eigen' or 'adapt'"))
  }


  # Check if frequency range is valid
  if(frequency_range[2] > fs/2){
    frequency_range[2] <- fs/2
    warning(paste("Upper frequency range greater than Nyquist, setting range to [",
                  toString(frequency_range[1]), ",", toString(frequency_range[2]), "].",
                  sep=""))
  }

  # Set number of tapers if none provided
  optimal_num_tapers = floor(2*time_bandwidth) - 1
  if(is.null(num_tapers)){
    num_tapers <- optimal_num_tapers
  }

  # Warn if number of tapers is suboptimal
  if(num_tapers != optimal_num_tapers){
    warning(paste("Suboptimal number of tapers being used. Number of tapers is optimal at floor(2*TW) - 1 which is ",
                  toString(optimal_num_tapers), " in this case.", sep=""))
  }


  # Check if window size is valid, fix if not
  if((window_params[1]*fs) %% 1 != 0){
    winsize_samples <- round(window_params[1]*fs)
    warning(paste("Window size is not divisible by sampling frequency. Adjusting window",
                  " size to ", toString(winsize_samples/fs), " seconds.", sep=""))
  } else{
    winsize_samples <- window_params[1]*fs
  }

  # Check if window step size is valid, fix if not
  if((window_params[2]*fs) %% 1 != 0){
    winstep_samples <- round(window_params[2]*fs)
    warning(paste("Window step size is not divisible by sampling frequency. Adjusting window",
                  " step size to ", toString(winstep_samples/fs), " seconds.", sep=""))
  } else{
    winstep_samples <- window_params[2]*fs
  }

  # Get total data length
  len_data = length(data)

  # Check if length of data is smaller than window (bad)
  if(len_data < winsize_samples){
    stop(paste("Data length (", toString(len_data), ") is shorter than the window size (",
               toString(winsize_samples), "). Either increase data length or decrease",
               " window size.", sep=""))
  }

  # Find window start indices and num of windows
  window_start = seq(1, len_data-winsize_samples+1, by=winstep_samples)
  num_windows = length(window_start)

  # Get num points in FFT
  nfft = max(max(2^ceiling(log2(abs(winsize_samples))), winsize_samples), 2^ceiling(log2(abs(min_nfft))))

  return(list(data, fs, frequency_range, time_bandwidth, num_tapers, winsize_samples, winstep_samples,
              window_start, num_windows, nfft, weighting, detrend_opt, plot_on, verbose))
}

# Process spectrogram inputs #
process_spectrogram_params <- function(fs, nfft, frequency_range, window_start, datawin_size){
  # Helper function to create frequency vector and window indices
  #
  # Params:
  #         fs (numeric): sampling frequency in Hz  -- required
  #         nfft (numeric): length of signal to calculate fft on -- required
  #         window_start (numeric vector): timestamps representing the beginning time for each window -- required
  #         datawin_size (numeric): seconds in one window -- required
  #
  # Returns:
  #         window_idxs (matrix): indices of timestamps for each window (nxm where n=number of windows and m=datawin_size)
  #         stimes (numeric vector): times for the centers of the spectral bins (1xt)
  #         sfreqs (numeric vector): frequency bins for spectrogram (1xf)
  #         freq_inds (logical vector): indicates which frequencies are being analyzed in an array of frequencies from 0 to fs
  #                    with steps of fs/nfft


  # Create frequency vector
  df <- fs/nfft
  sfreqs <- seq(0, fs, by=df)

  # Get frequencies for given frequency range
  freq_inds <- (sfreqs >= frequency_range[1]) & (sfreqs <= frequency_range[2])
  sfreqs <- sfreqs[freq_inds]

  # Compute times in middle of each spectrum
  window_middle_samples <- window_start + round(datawin_size/2)
  stimes <- (window_middle_samples-1) / fs  # stimes starts from 0

  # Get indices for each window
  window_idxs <- lapply(window_start, window_index_helper, datawin_size=datawin_size) # list of indices for n windows


  return(list(window_idxs, stimes, sfreqs, freq_inds))

}

window_index_helper <- function(start, datawin_size){
  res = seq(start, start+datawin_size-1, by=1)
  return(res)
}

# Display Spectrogram Properties #
display_spectrogram_properties <- function(fs, time_bandwidth, num_tapers, data_window_params, frequency_range, detrend_opt){
  # Prints spectrogram properties
  #
  # Params:
  #         fs (numeric): sampling frequency in Hz  -- required
  #         time_bandwidth (numeric): time-half bandwidth product (window duration*1/2*frequency_resolution) -- required
  #         num_tapers (numeric): number of DPSS tapers to use -- required
  #         data_window_params (numeric vector): c(window length(s), window step size(s) -- required
  #         frequency_range (numeric vector): c(<min frequency>, <max frequency>) -- required
  #         detrend_opt (char): detrend data window ('linear' (default), 'constant', 'off')
  #
  # Returns:
  #         This function does not return anythin

  data_window_params = data_window_params / fs

  # Print spectrogram properties
  print("Multitaper Spectrogram Properties: ")
  print(paste('     Spectral Resolution: ', toString(2 * time_bandwidth / data_window_params[1]), 'Hz', sep=""))
  print(paste('     Window Length: ', toString(data_window_params[1]), 's', sep=""))
  print(paste('     Window Step: ', toString(data_window_params[2]), 's', sep=""))
  print(paste('     Time Half-Bandwidth Product: ', toString(time_bandwidth), sep=""))
  print(paste('     Number of Tapers: ', toString(num_tapers), sep=""))
  print(paste('     Frequency Range: ', toString(frequency_range[1]), "-", toString(frequency_range[2]), 'Hz', sep=""))
  print(paste('     Detrend: ', detrend_opt, sep=""))

}

# Convert power to dB #
nanpow2db <- function(y){
  # Power to dB conversion, setting negatives and zeros to NaN
  #
  # params:
  #         y: power --required
  #
  # returns:
  #         ydB: dB (with 0s and negativs set to NaN)

  if(length(y)==1){
    if(y==0){
      return(NaN)
    } else(ydB <- 10*log10(y))
  }else{
    y[y==0] <- NaN
    ydB <- 10*log10(y)
  }
  return(ydB)
}

# Calculate multitpaer spectrum of single segment #
calc_mts_segment <- function(data_segment, dpss_tapers, nfft, freq_inds, weighting, wt, dpss_eigen, num_tapers, detrend_opt){
  # Calculate multitaper spectrum for a single segment of data
  #
  # params:
  #         data_segment (numeric vector): segment of the EEG data of length window size (s) * fs -- required
  #         dpss_tapers (numeric matrix): DPSS taper params to multiply signal by. Dims are (num_tapers, winsize_samples)
  #                                        -- required
  #         nfft (numeric): length of signal to calculate fft on -- required
  #         freq_inds (logical vector): boolean array indicating frequencies to use in an array of frequenices
  #                                    from 0 to fs with steps of fs/nfft --required
  #         weighting (char): weighting of tapers ('unity' (default), 'eigen', 'adapt') --required
  #         wt (numeric vector or numeric): precomputed taper weights --required
  #         dpss_eigen (numeric vector): --required
  #         num_tapers (numeric): number of dpss tapers being used --required
  #         detrend_opt (char): detrend data window ('linear' (default), 'constant', 'off') --required
  #
  # returns:
  #         mt_spectrum (numeric matrix): spectral power for single window

  library(pracma)

  # If segment has all zeros, return vector of zeros
  if(all(data_segment==0)){
    ret <- rep(0, sum(freq_inds))
    return(ret)
  }

  # Optionally detrend data to remove low freq DC component
  if(detrend_opt != 'off'){
    data_segment <- detrend(data_segment, tt=detrend_opt)
  }

  # Multiply data by dpss tapers (STEP 2)
  tapered_data <- sweep(dpss_tapers, 1, data_segment, '*')


  # Manually add nfft zero-padding (R's fft function does not support)
  tapered_padded_data <- rbind(tapered_data, matrix(0, nrow=nfft-nrow(tapered_data), ncol=ncol(tapered_data)))


  # Compute the FFT (STEP 3)
  fft_data <- apply(tapered_padded_data, 2, fft)
  # Compute the weighted mean spectral power across tapers (STEP 4)
  Spower = Im(fft_data)^2 + Re(fft_data)^2;
  if(weighting == 'adapt'){
    # daptive weights - for colored noise spectrum (Percival & Walden p368-p370)
    x = matrix(data_segment, nrow=1)
    Tpower <- x %*% (t(x)/length(x))
    Spower_iter <- rowMeans(Spower[,1:2])
    Spower_iter <- matrix(Spower_iter, ncol=1) # (nfft,1)
    a <- (1 - dpss_eigen) * as.vector(Tpower)
    a <- matrix(a, ncol=1) # (num_tapers, 1)
    dpss_eigen = matrix(dpss_eigen, nrow=1)
    for(i in 1:3){ # run 3 iterations
      # calculate the MSE weights
      b = (Spower_iter %*% ones(1,num_tapers)) / ( (Spower_iter %*% dpss_eigen) + repmat(t(a),nfft,1) )
      # calculate new spectral estimate
      wk = b^2 * (ones(nfft,1) %*% dpss_eigen)
      Spower_iter = matrix(colSums(t(wk) * t(Spower)), nrow=1) / rowSums(wk)
      Spower_iter = matrix(Spower_iter, ncol=1)
    }
    mt_spectrum = as.vector(Spower_iter)
  } else{
    # eigenvalue or uniform weights
    mt_spectrum = Spower %*% wt
    mt_spectrum = as.vector(mt_spectrum)
  }

  return(mt_spectrum[freq_inds])
}

plot_DC_shift <- function(repository,
                             load_electrodes,
                             subject,
                             condition,
                             time_windows,
                             reference,
                             soz_electrodes = NULL, resect_electrodes = NULL, ordered = FALSE,
                             name_type = c("name", "number"),
                             baseline_end = baseline_end,
                             condition_baseline = baseline) {
  # Extract necessary variables
  name_type <- match.arg(name_type)
  condition <- condition
  plot_electrodes <- dipsaus::parse_svec(load_electrodes)
  elecn <- dipsaus::parse_svec(load_electrodes)
  fs <- repository$sample_rate
  nel <- length(elecn)
  baseline_start <- 0

  # extract time values

  #Later adjust index we only need one plot so we will not need to subset
  index <- 1

  time_start <- as.numeric(time_window[1])
  time_end <- as.numeric(time_window[2])

  voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", plot_electrodes[1])]]
  condition <- gsub("\\s\\(\\d+\\)", "", condition)
  selector <- repository$epoch_table$Condition %in% c(condition)
  if(!any(selector)) {
    stop("Invalid condition selected.")
  }
  trial_list <- repository$epoch_table$Trial[selector]
  selected_trial_data <- subset(voltage_for_analysis, Trial ~ Trial %in% trial_list)
  collapsed_trial <- raveio::collapse2(selected_trial_data, keep = 1)

  collapsed_trials_matrix <- matrix(nrow = 0, ncol = length(collapsed_trial))

  # Loop through each 'e' in 'elecn'
  for(e in elecn) {
    voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", e)]]
    #collapse voltage for selected condition
    condition <- gsub("\\s\\(\\d+\\)", "", condition)
    selector <- repository$epoch_table$Condition %in% c(condition)
    if(!any(selector)) {
      stop("Invalid condition selected.")
    }
    trial_list <- repository$epoch_table$Trial[selector]
    selected_trial_data <- subset(voltage_for_analysis, Trial ~ Trial %in% trial_list)
    collapsed_trial <- raveio::collapse2(selected_trial_data, keep = 1)
    collapsed_trial <- t(matrix(collapsed_trial))
    collapsed_trials_matrix <- rbind(collapsed_trials_matrix, collapsed_trial)
  }

  electrode_names <- rev(repository$electrode_table$Electrode)
  collapsed_trials_matrix <- collapsed_trials_matrix[rev(1:nrow(collapsed_trials_matrix)), ]

  t1 <- as.numeric(time_window[1])
  t2 <- as.numeric(time_window[2])
  elect <- seq(t1, t2, length.out = ncol(collapsed_trials_matrix))

  subset_index <- (elect >= time_start) & (elect <= time_end)
  signal_matrix <- collapsed_trials_matrix[, subset_index]
  elect <- elect[subset_index]

  # Get signal data for baseline condition_baseline
  voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", elecn[1])]]
  condition_baseline <- gsub("\\s\\(\\d+\\)", "", condition_baseline)
  selector <- repository$epoch_table$Condition %in% c(condition_baseline)
  if(!any(selector)) {
    stop("Invalid condition_baseline selected.")
  }

  trial_list <- repository$epoch_table$Trial[selector]
  selected_trial_data <- subset(voltage_for_analysis, Trial ~ Trial %in% trial_list)
  collapsed_trial <- raveio::collapse2(selected_trial_data, keep = 1)

  collapsed_trials_matrix <- matrix(nrow = 0, ncol = length(collapsed_trial))

  # Loop through each 'e' in 'elecn'
  for(e in elecn) {
    voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", e)]]
    #collapse voltage for selected condition_baseline
    condition_baseline <- gsub("\\s\\(\\d+\\)", "", condition_baseline)
    selector <- repository$epoch_table$Condition %in% c(condition_baseline)
    if(!any(selector)) {
      stop("Invalid condition_baseline selected.")
    }
    trial_list <- repository$epoch_table$Trial[selector]
    selected_trial_data <- subset(voltage_for_analysis, Trial ~ Trial %in% trial_list)
    collapsed_trial <- raveio::collapse2(selected_trial_data, keep = 1)
    collapsed_trial <- t(matrix(collapsed_trial))
    collapsed_trials_matrix <- rbind(collapsed_trials_matrix, collapsed_trial)
  }

  collapsed_trials_matrix <- collapsed_trials_matrix[rev(1:nrow(collapsed_trials_matrix)), ]

  t1 <- as.numeric(time_window[1])
  t2 <- as.numeric(time_window[2])
  elect <- seq(t1, t2, length.out = ncol(collapsed_trials_matrix))
  subset_index <- (elect >= baseline_start) & (elect <= baseline_end)
  baseline_signal_matrix <- collapsed_trials_matrix[, subset_index]

  # Butterworth low-pass filter to isolate DC component
  fpass <- 1
  wpass <- fpass / (fs / 2)
  but <- butter(5, wpass, type = "low")
  lt2 <- 2 * fs

  # Threshold for DC detection
  maxelec <- max(signal_matrix)
  # thresholdn <- -maxelec / 10
  # thresholdp <- maxelec / 10
  thresholdn <- -maxelec * 0.5
  thresholdp <- maxelec * 0.5

  # Initialize vectors based on the number of signals (rows in signal_matrix)
  ntep <- ncol(signal_matrix)
  n_signals <- nrow(signal_matrix)
  sige <- vector("numeric", length = ntep)
  testdc <- vector("numeric", length = n_signals)
  lengthdc <- vector("numeric", length = n_signals)
  maxdc <- vector("numeric", length = n_signals)
  startdc <- vector("numeric", length = n_signals)  # Add this line

  resdc <- matrix(0,nel,4)

  for (i in 1:n_signals) {
    # Positive and Negative DC Shift Detection
    sige <- signal_matrix[i, ]
    sigedc <- filter(but, sige) - mean(baseline_signal_matrix)
    pos_shift_idx <- which(sigedc > thresholdp)
    neg_shift_idx <- which(sigedc < thresholdn)

    # Process positive shifts
    if (length(pos_shift_idx) > 0) {
      resultdc_pos <- rle(diff(pos_shift_idx))
      dclengthp <- max(resultdc_pos$lengths)
      if (dclengthp > lt2) {
        testdc[i] <- 1
        lengthdc[i] <- dclengthp
        maxdc[i] <- max(sigedc[pos_shift_idx])
        startdc[i] <- pos_shift_idx[1]
      }
    }

    # Process negative shifts
    if (length(neg_shift_idx) > 0) {
      resultdc_neg <- rle(diff(neg_shift_idx))
      dclengthn <- max(resultdc_neg$lengths)
      if (dclengthn > lt2) {
        testdc[i] <- 1
        if (dclengthn > lengthdc[i]) {
          lengthdc[i] <- dclengthn
          maxdc[i] <- max(abs(sigedc[neg_shift_idx]))
          startdc[i] <- neg_shift_idx[1]
        }
      }
    }
    resdc[i, 1] <- testdc[i]
    resdc[i, 2] <- lengthdc[i]
    resdc[i, 3] <- maxdc[i]
    resdc[i, 4] <- startdc[i]
  }


  colnames(resdc) <- c("DC", "Length", "Max", "Start")
  resdc <- data.frame(resdc)
  rownames(resdc) <- electrode_names

  top<- resdc[resdc$DC>0,]

  ## Edit plotting data ##

  collapsed_trials_matrix <- signal_matrix

  electrode_names <- repository$electrode_table$Electrode
  rownames(collapsed_trials_matrix) <- rev(electrode_names)

  t1 <- as.numeric(time_window[1])
  t2 <- as.numeric(time_window[2])
  elect <- seq(t1, t2, length.out = ncol(collapsed_trials_matrix))

  # Use analysis windows for this
  subset_index <- (elect >= t1) & (elect <= t2)
  collapsed_trials_matrix <- collapsed_trials_matrix[, subset_index]
  elect <- elect[subset_index]


  # Prepare data for ordering
  elec_names_plot <- as.character(plot_electrodes)
  rownames(collapsed_trials_matrix) <- plot_electrodes

  # Get SOZ electrodes for coloring/ordering
  electrode_table <- repository$electrode_table
  soz_electrodes <- dipsaus::parse_svec(soz_electrodes)
  resect_electrodes <- dipsaus::parse_svec(resect_electrodes)
  is_soz <- electrode_table$Electrode %in% soz_electrodes
  is_resect <- electrode_table$Electrode %in% resect_electrodes

  # determine the y-axis labels
  if( name_type == "name" ) {
    y_labels <- electrode_table$Label
  } else {
    y_labels <- electrode_table$Electrode
  }

  if (ordered == TRUE & (length(soz_electrodes) > 0 | length(resect_electrodes) > 0) ) {
    all_electrodes <- unique(c(soz_electrodes, resect_electrodes))
    selected_columns <- match(rev(all_electrodes), rev(electrode_table$Electrode))
    selected_columns <- na.omit(selected_columns)
    collapsed_trials_matrix_ordered <- collapsed_trials_matrix[selected_columns, ]
    remaining_columns <- collapsed_trials_matrix[-selected_columns, ]
    collapsed_trials_matrix <- rbind(remaining_columns, collapsed_trials_matrix_ordered)
    selected_columns <- match(all_electrodes, electrode_table$Electrode)
    selected_columns <- na.omit(selected_columns)
    elec_order_temp_ordered <- elec_names_plot[selected_columns]
    elec_names_plot <- c(elec_order_temp_ordered, elec_names_plot[-selected_columns])
    y_labels_ordered <- y_labels[selected_columns]
    y_labels <- c(y_labels_ordered, y_labels[-selected_columns])
    is_soz <- elec_names_plot %in% soz_electrodes
    is_resect <- elec_names_plot %in% resect_electrodes
  }

  # Define the data and gaps
  plotData <- data.frame(t(collapsed_trials_matrix))
  colnames(plotData) <- plot_electrodes

  gaps <- 1

  # Loop through each electrode
  for (i in seq_along(plotData)) {
    # Adjust the data for each electrode
    plotData[, i] <- (plotData[, i] - mean(plotData[, i])) +
      (ncol(plotData) - i) * gaps
  }

  ############## save group of electrodes
  scalingdata <- (max(plotData) / 2)
  plotData <- plotData / scalingdata

  gaps <- 2

  for(i in seq_along(plotData)){
    plotData[, i] <- (plotData[, i]- mean(plotData[, i]))+
      (ncol(plotData)-i)*gaps
  }

  # Reverse the order of electrode names
  elec_names_plot <- rev(elec_names_plot)
  y_labels <- rev(y_labels)


  # Set up ticks and positions for the y-axis
  num_ticks <- 7
  tick_positions <- seq(1, nrow(plotData), length.out = num_ticks)

  # Plot the data
  plot(plotData[, 1], type = "l", cex = 0.1,
       ylim = range(plotData), yaxt = "n", xaxt = "n", xlab = paste("Time"),
       ylab = paste("Electrode"))
  for(i in 2:ncol(plotData)){
    lines(plotData[, i])
  }

  # Set up ticks for the x-axis
  axis(side = 1, at = tick_positions, labels = elect[tick_positions])

  # Set up colors based on conditions
  y_colors <- rep("black", length(y_labels))
  y_colors[is_soz & !is_resect] <- "#00bfff"  # Blue for is_soz
  y_colors[is_resect & !is_soz] <- "#bf00ff"  # Purple for is_resect
  y_colors[is_soz & is_resect] <- "green"     # Green for both is_soz and is_resect

  is_soz <- rev(is_soz)
  is_resect <- rev(is_resect)
  # Set up labels for the y-axis in reverse order
  y_labels_tmp <- y_labels
  y_labels_tmp[is_soz | is_resect] <- ""
  graphics::axis(side = 2, at = rev(seq_along(y_labels) - 1) * gaps, labels = y_labels_tmp, las = 1)

  y_labels_tmp <- y_labels
  y_labels_tmp[!is_resect & !is_soz] <- ""
  graphics::axis(side = 2, at = rev(seq_along(y_labels) - 1) * gaps, labels = y_labels_tmp, las = 1, col.axis = "green")

  y_labels_tmp <- y_labels
  y_labels_tmp[!is_soz] <- ""
  graphics::axis(side = 2, at = rev(seq_along(y_labels) - 1) * gaps, labels = y_labels_tmp, las = 1, col.axis = "#00bfff")

  y_labels_tmp <- y_labels
  y_labels_tmp[!is_resect] <- ""
  graphics::axis(side = 2, at = rev(seq_along(y_labels) - 1) * gaps, labels = y_labels_tmp, las = 1, col.axis = "#bf00ff")

  top_indices <- as.numeric(rownames(top))
  matching_indices <- which(as.numeric(y_labels) %in% top_indices)

  # Highlight top 3 rows in red
  for (i in matching_indices) {
    lines(plotData[, i], col = "red")
  }

  return(resdc)
}

# Code for generating heatmap plot for a specific freq heatmap
HFO_matrix <- function(load_electrodes, SOZ_elec, resect_elec, repository, condition, time_window, label = "numeric", threshold) {

  # Set spectrogram params
  frequency_range <- c(80, 250)
  time_bandwidth <- 3
  num_tapers <- 5
  window_params <- c(2.5, 0.5)
  weighting <- 'unity'
  detrend_opt <- 'linear'
  parallel <- TRUE
  min_nfft <- 0
  num_workers <- 3

  # Sample freq
  sample_rate <- repository$sample_rate

  elecn <- dipsaus::parse_svec(load_electrodes)
  nel <- length(elecn)

  voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", elecn[1])]]
  fs <- repository$sample_rate
  time_dimnames <- dimnames(voltage_for_analysis)$Time
  nt <- length(time_dimnames)
  nwt <- floor((nt/fs-as.numeric(window_params[1]))/as.numeric(window_params[2]))+1
  t1 <- as.numeric(time_window[1])
  t2 <- as.numeric(time_window[2])
  elect <- seq(t1, t2, length.out = nwt)

  conditions <- repository$epoch_table$Condition
  epoch_table <- repository$epoch_table
  condition <- gsub("\\s*\\([^\\)]+\\)", "", sprintf("%s (%s)", condition, epoch_table$Trial))

  # Calculate freq_start and freq_end
  freq_temp <- frequency_range
  freq_start <- as.numeric(freq_temp[1])
  freq_end <- as.numeric(freq_temp[2])

  time_dimnames <- dimnames(voltage_for_analysis)$Time
  nt <- length(time_dimnames)
  nwt <- floor((nt/fs-as.numeric(window_params[1]))/as.numeric(window_params[2]))+1

  # Generate multitaper spectrogram and compute beta power matrix
  spectrogram_list <- vector("list", length(elecn))
  heatmapbeta <- matrix(0, nrow = length(elecn), ncol = nwt)

  for (i in 1:length(elecn)) {
    e <- elecn[i]
    voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", e)]]
    condition <- gsub("\\s\\(\\d+\\)", "", condition)
    selector <- repository$epoch_table$Condition %in% c(condition)
    if(!any(selector)) {
      stop("Invalid condition selected.")
    }
    trial_list <- repository$epoch_table$Trial[selector]
    selected_trial_data <- subset(voltage_for_analysis, Trial ~ Trial %in% trial_list)
    collapsed_trial <- raveio::collapse2(selected_trial_data, keep = 1)
    spectrogram_result <- multitaper_spectrogram_R(collapsed_trial, sample_rate, frequency_range, time_bandwidth, num_tapers, window_params,
                                                   min_nfft, weighting, detrend_opt, parallel, num_workers,
                                                   plot_on = FALSE, verbose = FALSE, xyflip = FALSE)
    specR <- spectrogram_result[[1]]
    specF <- spectrogram_result[[3]]
    freq_start_index <- which.min(abs(specF - freq_start))
    freq_end_index <- which.min(abs(specF - freq_end))
    spectb <- specR[freq_start_index:freq_end_index,]
    betaie <- colMeans(spectb)
    heatmapbeta[i, 1:nwt] <- betaie[1:nwt]
  }


  # Normalize heatmapbeta
  maxheatBeta=max(heatmapbeta)
  heatmapbetcorresponding_labelsan=heatmapbeta/maxheatBeta

  maxcolbetap=apply(heatmapbeta,2,max)

  heatmapbetacol=zeros(nel,nwt)

  for(it in 1:nwt){
    for(ie in 1:nel){
      heatmapbetacol[ie,it]=heatmapbeta[ie,it]/maxcolbetap[it]
    }
  }

  heatmapbetacol <- t(heatmapbetacol)
  heatmapbetacol <- cbind(elect, heatmapbetacol)
  colnames(heatmapbetacol)[1] <- "stimes"

  #remove rows in stimes with values less than time_start and greater than time_end
  HMap <- heatmapbetacol[heatmapbetacol[, "stimes"] >=  time_window[1] & heatmapbetacol[, "stimes"] <= time_window[2], ]
  HMap <- HMap[,-1]


  electrode_table <- repository$electrode_table
  soz_elec <- dipsaus::parse_svec(SOZ_elec)
  resect_elec <- dipsaus::parse_svec(resect_elec)
  soz_elec <- soz_elec[soz_elec %in% electrode_table$Electrode]
  resect_elec <- resect_elec[resect_elec %in% electrode_table$Electrode]

  # Identify HFO electrodes
  maxlineelec <- apply(HMap, 2, max)
  hfoelecp <- which(maxlineelec > threshold)
  reshfo <- matrix(0, ncol(HMap), 1)
  reshfo[hfoelecp, 1] <- 1
  rownames(reshfo) <- elecn

  #Plot heatmap
  HMap_plot <- as.matrix(HMap)

  heatmap_data <- expand.grid(Time = elect, Electrode = elecn)
  heatmap_data$Value <- c(HMap_plot)

  heatmap_data$High_HFO <- rep(reshfo[,1], each = length(elect))
  heatmap_data$Electrode <- factor(heatmap_data$Electrode, levels = unique(heatmap_data$Electrode))

  plot <- ggplot(heatmap_data, aes(x = Time, y = Electrode, fill = Value)) +
    geom_tile() +
    labs(x = "Time (s)", y = "") +  # Removing y-axis label since we'll add custom text labels
    scale_fill_viridis(option = "turbo") +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),  # Hide default y-axis text labels
      axis.ticks.y = element_blank(), # Hide y-axis ticks to clean up the plot
      plot.margin = margin(5, 5, 5, 5, "mm")  # Adjust plot margins if needed
    ) +
    scale_y_discrete(limits = heatmap_data$Electrode)

  # Add custom y-axis labels with conditional coloring
  plot <- plot + geom_text(data = heatmap_data, aes(y = Electrode, label = Electrode),
                           x = min(heatmap_data$Time) - abs(diff(range(heatmap_data$Time))) * 0.05,
                           hjust = 1,
                           color = ifelse(heatmap_data$High_HFO == 1, "red", "black"),
                           size = 1.5, inherit.aes = FALSE)

  # Print the plot
  print(plot)

  return(reshfo)
}

