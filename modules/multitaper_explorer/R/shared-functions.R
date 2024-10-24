# Put shared functions in `shared-*.R` so the pipeline is clean
get_default_cores <- function(round = TRUE) {
  re <- (raveio::raveio_getopt("max_worker") + 1) / 2
  if( round ) {
    re <- ceiling(re)
  }
  re
}

# Generate multitaper for every condition
generate_multitaper <- function (repository, load_electrodes, frequency_range,
                                 time_bandwidth, num_tapers, window_params, min_nfft,
                                 weighting, detrend_opt, parallel) {
  fs <- repository$sample_rate
  results <- parse_electrodes(load_electrodes)
  nel <- results$nel
  elecn <-results$elecn

  voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", elecn[1])]]

  # Generate data format for storing epochs and corresponding voltage data
  conditions <- repository$epoch_table$Condition
  df <- data.frame(Conditions = conditions)

  pCNT <- 1

  for(condition in conditions) {

    spectrogram_list <- vector("list", length(elecn))
    cnt <- 1


    for(e in elecn) {
      cat(paste0(repository$subject$subject_code, " Progress: ", floor(pCNT / (length(elecn) * length(conditions)) * 100), "%\n"))

      pCNT <- pCNT + 1
      #Get voltage data
      voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", e)]]

      #collapse voltage for selected condition
      selector <- repository$epoch_table$Condition %in% c(condition)
      if(!any(selector)) {
        stop("Invalid condition selected.")
      }
      trial_list <- repository$epoch_table$Trial[selector]
      selected_trial_data <- subset(voltage_for_analysis, Trial ~ Trial %in% trial_list)
      collapsed_trial <- raveio::collapse2(selected_trial_data, keep = 1)

      # Compute the multitaper spectrogram
      results = multitaper_spectrogram_R(collapsed_trial, fs, frequency_range, time_bandwidth, num_tapers, window_params,
                                         min_nfft, weighting, detrend_opt, parallel, num_workers,
                                         plot_on=FALSE, verbose=FALSE, xyflip=FALSE)

      spect <- results
      spectrogram_list[[cnt]] <- spect
      cnt <- cnt + 1
    }

    #add condition to output
    row_index <- which(df$Conditions == condition)
    df$MultitaperData[row_index] <- list(spectrogram_list)
  }

  epoch_table <- repository$epoch_table
  df$Conditions <- sprintf("%s (%s)", df$Conditions, epoch_table$Trial)
  return(df)
}

## generate all frequency plots for a specific condition
## Code for computing beta power matrix
generate_heatmap <- function(repository, multitaper_result, time_window,
                             analysis_windows, load_electrodes,
                             window_params, condition, label, normalize) {

  # create frequency and time list
  freq_list <- list()
  for (i in seq_along(analysis_windows)) {
    freq_list_name <- paste0("f", i)
    freq_list[[freq_list_name]] <- analysis_windows[[i]]$frequency_range
  }

  fs <- repository$sample_rate
  results <- parse_electrodes(load_electrodes)
  nel <- results$nel
  elecn <-results$elecn

  row_index <- which(multitaper_result$Conditions == condition)
  spectrogram_list <- multitaper_result$MultitaperData[row_index]

  voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", elecn[1])]]
  fs <- repository$sample_rate
  time_dimnames <- dimnames(voltage_for_analysis)$Time
  nt <- length(time_dimnames)
  nwt <- floor((nt/fs-as.numeric(window_params[1]))/as.numeric(window_params[2]))+1
  t1 <- as.numeric(time_window[1])
  t2 <- as.numeric(time_window[2])
  elect <- seq(t1, t2, length.out = nwt)

  heatmap_freq_list <- vector("list", length(freq_list))


  for (i in seq_along(freq_list)) {

    # extract frequency values
    freq_temp <- freq_list[[i]]
    freq_start <- as.numeric(freq_temp[1])
    freq_end <- as.numeric(freq_temp[2])

    heatmapbeta=zeros(nel,nwt)

    cnt <- 1

    for(e in elecn) {
      spec <- spectrogram_list[[1]][[cnt]]
      specR <- spec[[1]]
      specF <- spec[[3]]
      freq_start_index <- which.min(abs(specF - freq_start))
      freq_end_index  <- which.min(abs(specF - freq_end))
      spectb = specR[freq_start_index:freq_end_index,]
      betaie=colMeans(spectb)
      heatmapbeta[cnt,1:nwt]=betaie[1:nwt]
      cnt <- cnt + 1
    }

    heatmapbetacol=zeros(nel,nwt)

    # Normalization
    if (normalize == TRUE) {
      maxheatBeta <- max(heatmapbeta)
      heatmapbetcorresponding_labelsan <- heatmapbeta / maxheatBeta

      maxcolbetap <- apply(heatmapbeta, 2, max)

      for (it in 1:nwt) {
        for (ie in 1:nel) {
          heatmapbetacol[ie, it] <- heatmapbeta[ie, it] / maxcolbetap[it]
        }
      }
    } else {
      heatmapbetacol <- heatmapbeta
    }

    #Prepare Data ----
    heatmapbetacol <- cbind(elecn, heatmapbetacol)
    heatmapbetacol <- t(heatmapbetacol)
    # Extract the first row as column names
    if (label == "numeric") {
      colnames(heatmapbetacol) <- heatmapbetacol[1, ]
    } else if (label == "names") {
      colnames(heatmapbetacol) <- repository$electrode_table$Label[which(heatmapbetacol[1, ]==repository$electrode_table$Electrode)]
    }

    # Remove the first row
    heatmapbetacol <- heatmapbetacol[-1, ]
    # Add the times column
    heatmapbetacol <- cbind(elect, heatmapbetacol)
    colnames(heatmapbetacol)[1] <- "stimes"

    heatmap_freq_list[[i]] <- heatmapbetacol
  }

  return(heatmap_freq_list)

}

# Code for generating heatmap plot for a specific freq heatmap
plot_heatmap <- function(heatmapbetacol, SOZ_elec, plot_SOZ_elec, resect_elec, plot_resect_elec, name_type,
                         organize_top, repository, subject_code, analysis_windows, index, text_size) {

  time_list <- list()
  for (i in seq_along(analysis_windows)) {
    time_list_name <- paste0("t", i)
    time_list[[time_list_name]] <- analysis_windows[[i]]$time_range
  }

  # extract time values
  time_temp <- time_list[[index]]
  time_start <-time_temp[[1]]
  time_end <- time_temp[[2]]

  #remove rows in stimes with values less than time_start and greater than time_end
  heatmapbetacol <- heatmapbetacol[heatmapbetacol[, "stimes"] >= time_start & heatmapbetacol[, "stimes"] <= time_end, ]

  # Generate list of SOZ electrodes
  results_soz <- parse_electrodes(SOZ_elec)
  soz_elec <- results_soz$elecn

  # Generate list of Resect electrodes
  results_resect <- parse_electrodes(resect_elec)
  resect_elec <- results_resect$elecn

  electrode_table<- repository$electrode_table

  #Validate only correct SOZ_elec and resect_elec were input
  soz_elec <- soz_elec[soz_elec %in% electrode_table$Electrode]
  resect_elec <- resect_elec[resect_elec %in% electrode_table$Electrode]

  if (name_type == "names") {
    #change soz_elec
    indices <- which(electrode_table$Electrode %in% soz_elec)
    corresponding_labels <- electrode_table$Label[indices]
    soz_elec <- corresponding_labels
    #change resect_elec
    indices <- which(electrode_table$Electrode %in% resect_elec)
    corresponding_labels <- electrode_table$Label[indices]
    resect_elec <- corresponding_labels
  }

  # Plot ----
  data <- as.data.frame(heatmapbetacol)
  stimes <- data$stimes
  data <- data[ ,-1]
  elecnum <- colnames(data)

  # Reorder electrodes if organize_top is TRUE
  if (organize_top) {
    # Intersecting electrodes between soz_elec and resect_elec
    intersect_electrodes <- intersect(soz_elec, resect_elec)

    # Electrodes in soz_elec not in resect_elec
    unique_soz_elec <- setdiff(soz_elec, resect_elec)

    # Electrodes in resect_elec not in soz_elec
    unique_resect_elec <- setdiff(resect_elec, soz_elec)

    # Combine the lists while ensuring no duplicates
    ordered_electrodes <- unique(c(unique_resect_elec, intersect_electrodes, unique_soz_elec))

    # Add remaining electrodes that are not in soz_elec or resect_elec
    all_electrodes <- setdiff(elecnum, ordered_electrodes)
    ordered_electrodes <- unique(c(ordered_electrodes, all_electrodes))

    # Reorder the rows of the data matrix based on the order of electrodes
    reordered_indices <- match(ordered_electrodes, elecnum)
    heatmapbetacol <- as.matrix(data[, reordered_indices, drop = FALSE])
  } else {
    ordered_electrodes <- elecnum
    heatmapbetacol <- as.matrix(data)
  }


  heatmap_data <- expand.grid(Time = stimes, Electrode = factor(ordered_electrodes, levels = ordered_electrodes))
  heatmap_data$Value <- c(heatmapbetacol)

  # Define a custom function to change color of SOZ electrodes
  color_electrodes <- function(electrode) {
    if ((electrode %in% resect_elec) & (electrode %in% soz_elec) & plot_SOZ_elec == TRUE & plot_resect_elec == TRUE) {
      return("purple")
    } else if ((electrode %in% resect_elec) & plot_resect_elec == TRUE) {
      return("blue")
    } else if ((electrode %in% soz_elec) & plot_SOZ_elec == TRUE) {
      return("red")
    } else {
      return("black")
    }
  }

  plot <- ggplot(heatmap_data, aes(x = Time, y = Electrode, fill = Value)) +
    geom_tile() +
    labs(x = "Time (s)", y = "Electrode", title = subject_code) +
    scale_fill_viridis(option = "turbo") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = text_size),
      axis.text.y = element_text(size = text_size, color = sapply(levels(heatmap_data$Electrode), color_electrodes)),
      plot.title = element_text(hjust = 0.5)
    )


  return(plot)
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

parse_electrodes <- function(load_electrodes){
  # Split the input by comma to handle multiple ranges
  ranges <- strsplit(load_electrodes, ",")[[1]]

  # Initialize an empty vector to hold all electrode numbers
  elecn <- integer(0)

  # Process each range
  for (range_str in ranges) {
    # Split the range into start and end, if '-' is present, otherwise it's a single number
    if (grepl("-", range_str)) {
      range_parts <- as.integer(strsplit(range_str, "-")[[1]])
      # Generate the sequence of numbers and add to the vector
      elecn <- c(elecn, seq(range_parts[1], range_parts[2]))
    } else {
      # If it's a single number, just add it to the vector
      elecn <- c(elecn, as.integer(range_str))
    }
  }

  # Calculate nel as the total number of electrodes
  nel <- length(elecn)

  # Return both nel and the sequence of electrodes
  return(list(nel = nel, elecn = elecn))
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

# Save for 3D brain display
electrode_powertime <- function(heatmapbetacol, subject_code, analysis_windows, SOZ_elec, resect_elec, load_electrodes) {

  # extract frequency list
  freq_list <- list()
  for (i in seq_along(analysis_windows)) {
    freq_list_name <- paste0("f", i)
    freq_list[[freq_list_name]] <- analysis_windows[[i]]$frequency_range
  }

  # Generate list of included SOZ electrodes
  SOZ_results <- parse_electrodes(SOZ_elec)
  SOZ_elec <- SOZ_results$elecn
  # Generate list of Resect electrodes
  results_resect <- parse_electrodes(resect_elec)
  resect_elec <- results_resect$elecn
  # Generate list of included electrodes
  elec_results <- parse_electrodes(load_electrodes)
  elecn <- elec_results$elecn

  powertime_freq_list <- vector("list", length(freq_list))

  for (i in seq_along(freq_list)) {
    data <- as.data.frame(heatmapbetacol[[i]])
    stimes <- data$stimes
    data <- data[ ,-1]
    elecnum <- colnames(data)

    Hmap <- as.matrix(data)

    heatmap_data <- expand.grid(Time = stimes, Electrode_label = elecnum)
    heatmap_data$Electrode_numeric <- rep(elecn, each = length(stimes))

    #Add a column for SOZ electrodes
    SOZ_vector <- as.numeric(heatmap_data$Electrode_numeric %in% SOZ_elec)
    #Add a column for Resect electrodes
    Resect_vector <- as.numeric(heatmap_data$Electrode_numeric %in% resect_elec)
    #Add a column for Resect and SOZ electrodes
    SOZResect_vector <- ifelse(heatmap_data$Electrode_numeric %in% SOZ_elec & heatmap_data$Electrode_numeric %in% resect_elec, 0.5,
                                     ifelse(heatmap_data$Electrode_numeric %in% SOZ_elec, 1,
                                     ifelse(heatmap_data$Electrode_numeric %in% resect_elec, 0, NA)))


    heatmap_data$Value <- c(Hmap)

    #Normalize value between 0 and 1
    heatmap_data$Value <- (heatmap_data$Value - min(heatmap_data$Value)) / (max(heatmap_data$Value) - min(heatmap_data$Value))

    #Create data sheet for YAEL ----
    subject_name <- subject_code

    # Create a new data frame with the desired structure
    YAEL_data <- data.frame(
      Subject = rep(subject_name, nrow(heatmap_data)),
      Electrode = heatmap_data$Electrode_label,
      Time = heatmap_data$Time,
      HeatmapValue = heatmap_data$Value,
      SOZ = SOZ_vector,
      Resect = Resect_vector,
      SOZ_Resect = SOZResect_vector
    )

    # save to list
    powertime_freq_list[[i]] <- YAEL_data
  }

  return(powertime_freq_list)
}

# Analyze Data
analyze_score <- function (heatmap, repository, window_params, time_stat_start, time_stat_end, threshold_type, threshold_level) {

  # Cut heatmap based on start and end time
  heatmap <- heatmap[heatmap[, "stimes"] >= time_stat_start & heatmap[, "stimes"] <= time_stat_end, ]

  # Prepare data
  data <- as.data.frame(heatmap)
  stimes <- data$stimes
  data <- data[ ,-1]
  elecnum <- as.character(repository$electrode_list)
  data <- as.matrix(data)

  lengthzl<-vector(mode="numeric", length=length(elecnum))
  startzl<-vector(mode="numeric", length=length(elecnum))

  if (threshold_type == "zscore") {

    # Computer Z-Score
    heatzscore <-(data-mean(data))/sd(data)

    for(ie in 1:length(elecnum)){
      zl = heatzscore[, ie]
      seqzl = which(zl > threshold_level)

      if(length(seqzl) > 0){
        # Compute differences of the positions
        resultzl = rle(diff(seqzl))

        # Find the start of the first sequence
        first_sequence_start = seqzl[1]
        startzl[ie] = first_sequence_start

        # Identify the lengths of continuous sequences
        continuous_sequences = resultzl$lengths[resultzl$values == 1] + 1

        # Store the length of the first sequence
        lengthzl[ie] = if(length(continuous_sequences) > 0) continuous_sequences[1] else 0

        #correct for step size
        startzl[ie] <- (startzl[ie] * window_params[2])
        lengthzl[ie] <- (lengthzl[ie] * window_params[2])
      } else {
        startzl[ie] = NA  # No sequence found
        lengthzl[ie] = 0
      }
    }

  } else if (threshold_type == "raw") {

    for(ie in 1:length(elecnum)){
      zl = data[, ie]
      seqzl = which(zl > threshold_level)

      if(length(seqzl) > 0){
        # Compute differences of the positions
        resultzl = rle(diff(seqzl))

        # Find the start of the first sequence
        first_sequence_start = seqzl[1]
        startzl[ie] = first_sequence_start

        # Identify the lengths of continuous sequences
        continuous_sequences = resultzl$lengths[resultzl$values == 1] + 1

        # Store the length of the first sequence
        lengthzl[ie] = if(length(continuous_sequences) > 0) continuous_sequences[1] else 0

        #correct for step size
        startzl[ie] <- (startzl[ie] * window_params[2])
        lengthzl[ie] <- (lengthzl[ie] * window_params[2])
      } else {
        startzl[ie] = NA  # No sequence found
        lengthzl[ie] = 0
      }
    }

  }

  results <- list(
    Length = c(lengthzl),
    Start = c(startzl),
    Electrodes = c(elecnum)
  )

}
