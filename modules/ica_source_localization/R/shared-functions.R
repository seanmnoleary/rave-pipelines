

# plot components function
plot_component <- function() {

}

# Process Data
compute_ICA <- function(load_electrodes, repository, time_window, condition) {
  electrodes<- dipsaus::parse_svec(load_electrodes)

  # Get initial matrix structure
  voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", electrodes[1])]]
  repository$epoch_table$Condition
  voltage_trace <- voltage_for_analysis[, 1, 1]
  selector <- repository$epoch_table$Condition %in% condition
  trial_list <- repository$epoch_table$Trial[selector]
  selected_trial_data <- subset(voltage_for_analysis, Trial ~ Trial %in% trial_list)
  collapsed_trial <- raveio::collapse2(selected_trial_data, keep = 1)
  electrodes_by_time <- matrix(nrow = length(electrodes), ncol = length(collapsed_trial))

  # Loop through each electrode
  for (i in 1:length(electrodes)) {
    electrode <- electrodes[i]

    voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", electrode)]]

    repository$epoch_table$Condition

    voltage_trace <- voltage_for_analysis[, 1, 1]

    selector <- repository$epoch_table$Condition %in% condition
    trial_list <- repository$epoch_table$Trial[selector]
    selected_trial_data <- subset(voltage_for_analysis, Trial ~ Trial %in% trial_list)
    collapsed_trial <- raveio::collapse2(selected_trial_data, keep = 1)

    electrodes_by_time[i, ] <- collapsed_trial

  }

  nt <- ncol(electrodes_by_time)
  nel <- length(electrodes)
  fs <- repository$sample_rate
  dt <- as.numeric(1/fs)
  ts <- 1:nt
  ts <- ts*dt

  time <- seq(time_window[1], time_window[2], length = ncol(electrodes_by_time))

  # set matrix names based on input variables
  rownames(electrodes_by_time) <- electrodes
  colnames(electrodes_by_time) <- time

  # set the number of components
  # ncomp <- nel/2
  ncomp <- 40
  # Replace NA/Inf values with 0
  if (any(is.na(electrodes_by_time)) || any(is.infinite(electrodes_by_time))) {
    electrodes_by_time[is.na(electrodes_by_time) | is.infinite(electrodes_by_time)] <- 0
  }

  ica_result <- icaimax(t(electrodes_by_time), nc = ncomp)

  Sr <- ica_result$S

  # Add EI to ICA results
  EI <- compute_EI(Sr, nc = ncomp, nt, nel, fs, dt, ts)
  EI_vector <- data.frame(EI)
  component_weights <- t(ica_result$M)
  component_matrix <- cbind(EI_vector, component_weights)
  colnames(component_matrix)[3:ncol(component_matrix)] <- electrodes

  filtered_indices <- rownames(EI)[EI$EI > 0]
  filtered_indices <- as.numeric(filtered_indices)

  if (!all(is.na(filtered_indices))) {
    # Calculate h2 for components with high EI only
    # Define a function to compute h2 between two signals using non-linear regression
    compute_h2 <- function(X, Y, max_lag = 50) {
      h2_values <- numeric(max_lag)

      # Iterate over time lags
      for (tau in 1:max_lag) {
        # Compute the conditional variance of Y given X shifted by tau
        conditional_var <- var(Y[(tau + 1):length(Y)] - mean(Y[(tau + 1):length(Y)]))
        total_var <- var(Y)

        # Estimate h2 for the given time lag
        h2_values[tau] <- 1 - (conditional_var / total_var)
      }

      # Return the maximum h2 value and the corresponding lag
      max_h2 <- max(h2_values)
      optimal_tau <- which.max(h2_values)
      list(h2 = max_h2, tau = optimal_tau)
    }

    # Calculate h2 matrix for selected components
    h2_matrix <- matrix(0, nrow = length(filtered_indices), ncol = length(filtered_indices))
    tau_matrix <- matrix(0, nrow = length(filtered_indices), ncol = length(filtered_indices))

    for (i in seq_along(filtered_indices)) {
      for (j in seq_along(filtered_indices)) {
        if (i != j) {
          # Get h2 and tau for the non-linear relationship between components i and j
          result_ij <- compute_h2(ica_result$S[, filtered_indices[i]], ica_result$S[, filtered_indices[j]])
          result_ji <- compute_h2(ica_result$S[, filtered_indices[j]], ica_result$S[, filtered_indices[i]])

          # Store h2 and tau values
          h2_matrix[i, j] <- result_ij$h2 - result_ji$h2 # Compute Δh2 (asymmetry)
          tau_matrix[i, j] <- result_ij$tau - result_ji$tau # Compute Δτ
        }
      }
    }

    # Calculate the direction index D_XY
    direction_index <- matrix(0, nrow = length(filtered_indices), ncol = length(filtered_indices))

    for (i in seq_along(filtered_indices)) {
      for (j in seq_along(filtered_indices)) {
        if (i != j) {
          direction_index[i, j] <- 0.5 * (sign(h2_matrix[i, j]) + sign(tau_matrix[i, j]))
        }
      }
    }

    # Identify the strongest directional couplings
    strongest_couplings <- which(abs(direction_index) == 1, arr.ind = TRUE)

    # Perform connectivity analysis for selected components
    out_strength <- rowSums(h2_matrix)
  }
  # add outstrenth to component matrix
  filtered_indices_numeric <- as.numeric(filtered_indices)
  component_matrix$h2 <- NA
  if (!all(is.na(filtered_indices))) {
    component_matrix$h2[filtered_indices_numeric] <- out_strength
  }
  component_matrix <- component_matrix[, c(ncol(component_matrix), 1:(ncol(component_matrix) - 1))]



  electrode_matrix <- t(component_matrix)
  electrode_matrix <- data.frame(electrode_matrix)
  electrode_label <- c(NA, NA, NA, electrodes)
  electrode_matrix$Electrode <- electrode_label
  electrode_matrix <- electrode_matrix[, c(ncol(electrode_matrix), 1:(ncol(electrode_matrix) - 1))]
  component_names <- as.vector(seq(1:ncomp))
  component_names <- c(NA, component_names)
  colnames(electrode_matrix) <- component_names

  electrode_matrix <- as.matrix(electrode_matrix)
  return(electrode_matrix)
}

# Compute EI
compute_EI <- function(Sr, nc, nt, nel, fs, dt, ts) {
  # Parameters
  time_bandwidth = 3  # Set time-half bandwidth
  num_tapers = 5  # Set number of tapers (optimal is time_bandwidth*2 - 1)
  window_params = c(1, 0.2)  # Window size is 1s with step size of 0.2s
  min_nfft = 0  # No minimum nfft
  weighting = 'unity'  # weight each taper at 1
  detrend_opt = 'off'  # detrend each window by subtracting the average
  parallel = TRUE  # use multiprocessing
  num_workers = 3  # use 3 cores in multiprocessing
  plot_on = FALSE  # plot spectrogram
  verbose = FALSE  # print extra info
  xyflip = FALSE  # do not transpose spect output matrix

  # Set spectrogram params
  frequency_range = c(0.5, 250)  # Frequency Band
  thetaband<-c(3.5,7.4)
  alphaband<-c(7.4,12.4)
  betaband<-c(12.4,24) #[13-30]Hz
  gammaband<-c(24,140) #[30-90]Hz

  # This number sort of serves as a built-in "time cost". High-frequency
  # activity needs to be robust enough to surpass this v number and still
  # overcome the threshold.
  # It's arbitrary but I think 0.5 is what Bartolomei used.
  v=0.5

  # Lambda also serves as a somewhat arbitrary threshold value.
  lambda=15

  data   <- vector(mode="numeric", length=nt)

  nwt=floor((nt/fs-window_params[1])/window_params[2])+1

  data[1:nt]<-Sr[1:nt,1]

  results = multitaper_spectrogram_R(data, fs, frequency_range, time_bandwidth, num_tapers, window_params, min_nfft, weighting, detrend_opt, parallel, num_workers,
                                     plot_on, verbose, xyflip)

  stimes = results[[2]]
  nwt=length(stimes)
  ermaster=matrix(0,nc,nwt)
  unmaster=matrix(0,nc,nwt)

  maxspec=0

  for(ie in 1:nc){
    #ie=40
    #print(ie)
    data[1:nt]<-Sr[1:nt,ie]
    # Compute the multitaper spectrogram
    results = multitaper_spectrogram_R(data, fs, frequency_range, time_bandwidth, num_tapers, window_params, min_nfft, weighting, detrend_opt, parallel, num_workers,
                                       plot_on, verbose, xyflip)
    spec=results[[1]]
    sfreq=results[[3]]
    # spec=spec[-c(57,67),]
    # sfreq=sfreq[-c(57:67)]

    freq_start=thetaband[1]
    freq_end=thetaband[2]
    freq_start_index <- which.min(abs(sfreq - freq_start))
    freq_end_index  <- which.min(abs(sfreq - freq_end))
    specttheta = results[[1]][freq_start_index:freq_end_index,]
    ertheta=colSums(specttheta)

    freq_start=alphaband[1]
    freq_end=alphaband[2]
    freq_start_index <- which.min(abs(sfreq - freq_start))
    freq_end_index  <- which.min(abs(sfreq - freq_end))
    spectalpha = results[[1]][freq_start_index:freq_end_index,]
    eralpha=colSums(spectalpha)

    freq_start=betaband[1]
    freq_end=betaband[2]
    freq_start_index <- which.min(abs(sfreq - freq_start))
    freq_end_index  <- which.min(abs(sfreq - freq_end))
    spectbeta = results[[1]][freq_start_index:freq_end_index,]
    erbeta=colSums(spectbeta)

    freq_start=gammaband[1]
    freq_end=gammaband[2]
    freq_start_index <- which.min(abs(sfreq - freq_start))
    freq_end_index  <- which.min(abs(sfreq - freq_end))
    spectgamma = results[[1]][freq_start_index:freq_end_index,]
    ergamma=colSums(spectgamma)

    er=(erbeta+ergamma)/(eralpha+ertheta)
    ern=cumsum(er)/c(1:nwt)
    un=er-ern-v
    un=cumsum(un)

    ermaster[ie,]=er
    unmaster[ie,]=un

  }

  energy_ratio <- apply(ermaster, 1, function(row) trapz(seq_along(row), row))

  erdf<-data.frame(ermaster)
  colnames(erdf)<-stimes
  rownames(erdf)<-1:nc
  elecnum=1:nc
  ERmap_data <- expand.grid(Time = stimes, Electrode = elecnum)
  ERmap_data$Value <- c(t(ermaster))
  colorelec=elecnum
  colorelec[1:nc]="black"
  titlepng="ER ICA"

  # ggplot(ERmap_data, aes(x = Time, y = Electrode, fill = Value)) +
  #   geom_tile() +
  #   ggtitle(titlepng) +
  #   labs(x = "Time (s)", y = "Electrode") +
  #   scale_fill_viridis(option = "turbo") +
  #   theme_minimal() +
  #   theme(
  #     axis.text.y = element_text(size = 10, colour = colorelec)  # Adjust depending on electrodes
  #   ) +
  #   scale_y_continuous(breaks = unique(ERmap_data$Electrode))  # Show all y-axis ticks
  #

  Nd   <- vector(mode="numeric", length=nc)
  Na   <- vector(mode="numeric", length=nc)

  Nd[1:nc]=10*nwt
  Na[1:nc]=nwt

  # plot(stimes,ermaster[31,],type='lines')
  # plot(stimes,unmaster[31,],type='lines')

  for(it in 2:nwt){

    unt=unmaster[,1:it]
    un=apply(unt,1,FUN=min)
    ind=apply(unt,1,which.min)

    undiff=unmaster[,it]-un
    pastThreshold=undiff>lambda
    pastThreshold[Nd!=10*nwt]=FALSE

    ie=which(pastThreshold==TRUE)
    Nd[ie]=ind[ie]
    Na[ie]=it

  }


  tau=1
  H=5

  hspan=which.min(abs(stimes-H))

  Nd[is.na(Nd)]=nwt-hspan
  Nd[Nd>nwt-hspan]=nwt-hspan
  #
  EI<- vector(mode="numeric", length=nc)
  N0=min(Nd)
  t0=stimes[N0]

  for(ie in 1:nc){
    Ndlim=Nd[ie]+hspan
    if(Ndlim<nwt){
      EI[ie]=mean(ermaster[ie,Nd[ie]:Nd[ie]+hspan])/(stimes[Nd[ie]]-t0+tau)
    }
  }

  t0=t0+time_window[1]

  maxei=max(EI)
  EI=EI/maxei

  EI <- data.frame(EI)

  # Normalize each EI time window between 0 and 1
  EI_colnames <- colnames(EI)
  colnames(EI) <- EI_colnames

  result <- cbind(energy_ratio, EI)

  return(result)
}

# Multitaper Spectrogram #
multitaper_spectrogram_R <- function(data, fs, frequency_range=NULL, time_bandwidth=5, num_tapers=NULL, window_params=c(5,1),
                                     min_nfft=0, weighting='unity', detrend_opt='linear', parallel=FALSE, num_workers=FALSE,
                                     plot_on=TRUE, verbose=TRUE, xyflip=FALSE){
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
    workers_avail <- detectCores() - 1  # detect cores available and leave 1 for user
    if(num_workers==FALSE | num_workers > workers_avail){
      num_workers = workers_avail
      warning(paste("Number of workers for parallelization either not specified or greater than workers available. Setting number
              of workers to number available minus 1 (", toString(num_workers), ")"))
    }
    registerDoParallel(cores=num_workers) # register workers with doParallel

    # Create cluster of workers differently depending on OS
    if(.Platform$OS.type == "windows"){ # windows cannot use FORK argument
      cluster <- makeCluster(num_workers) # create cluster of workers without forking
    }
    else{
      cluster <- makeCluster(num_workers, type="FORK") # if not windows, use FORK because it's faster
    }

    mt_spectrogram <- parApply(cluster, data_segments, 1, calc_mts_segment, dpss_tapers=dpss_tapers, nfft=nfft, freq_inds=freq_inds,
                               weighting=weighting, wt=wt, dpss_eigen=dpss_eigen, num_tapers=num_tapers, detrend_opt=detrend_opt)

    stopCluster(cluster) # stop cluster to give back resources
    registerDoSEQ() # switch back to serial processing
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

# Process user input #
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




