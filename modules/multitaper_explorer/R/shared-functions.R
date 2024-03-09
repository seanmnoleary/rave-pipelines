# Put shared functions in `shared-*.R` so the pipeline is clean

`%within%` <- rutabaga::`%within%`


COLOR_PALETTES <- list(
  "BlueGrayRed" = rev(c("#67001f", "#b2182b", "#d6604d",
                        "#f4a582", "#b4b4b4", "#92c5de", "#4393c3", "#2166ac",
                        "#053061")),
  "WhiteRed" = c("#f9e5e5", "#ffaaaa", "#d46a6a", "#aa3939", "#550000", "#190909"),
  "YellowRed" = c("#ffff00", "#ffbf00", "#ff8000", "#ff4000", "#ff0000", "#990000"),
  "WhitePink" = c("#FFFFFF", "#FFE0E0", "#FFC0CB", "#FFB6C1", "#FF69B4", "#FF1493"),
  "WhiteBlack" = c("#FFFFFF", "#E6E6E6", "#CCCCCC", "#999999", "#666666", "#333333"),
  "Turbo" = c("#30123BFF", "#3E9BFEFF", "#46F884FF", "#E1DD37FF", "#F05B12FF", "#7A0403FF")
)

plot_preferences <- pipeline$load_preferences(
  name = "graphics",
  # default options
  # heatmap_palette = c("#ffffff", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
  heatmap_palette = COLOR_PALETTES$Turbo,
  # heatmap_palette_name = "BlueGrayRed",
  .overwrite = FALSE
)

use_color_map <- function(name) {
  if(!name %in% names(COLOR_PALETTES)) {
    name <- "Default"
  }
  plot_preferences$set("heatmap_palette_name", name)
  plot_preferences$set("heatmap_palette", COLOR_PALETTES[[name]])
  invisible()
}

# debug, REMOVE it when publishing the code
# assign("use_color_map", use_color_map, envir = globalenv())

get_default_cores <- function(round = TRUE) {
  re <- (raveio::raveio_getopt("max_worker") + 1) / 2
  if( round ) {
    re <- ceiling(re)
  }
  re
}


# For debug use; see `dipsaus::rs_show_shortcut(1)`
# DIPSAUS DEBUG START
# raveio::pipeline_setup_rmd("multitaper_explorer")
# subject <- raveio::RAVESubject$new(project_name = project_name, subject_code = subject_code)
# repository <- raveio::prepare_subject_voltage_with_epoch(
#   subject = subject,
#   epoch_name = epoch_file_name,
#   electrodes = load_electrodes,
#   time_windows = time_window,
#   reference = reference_name
# )
# multitaper_result <- generate_multitaper(
#   repository, load_electrodes, frequency_range,
#   time_bandwidth, num_tapers, window_params, min_nfft,
#   weighting, detrend_opt, parallel = FALSE)
# power_over_time_data <- generate_power_over_time_data(multitaper_result, analysis_time_frequencies)

# Generate multitaper for every condition
generate_multitaper <- function (
    repository, load_electrodes, frequency_range,
    time_bandwidth = 5, num_tapers = NULL, window_params = c(5,1), min_nfft = 0,
    weighting = "unity", detrend_opt = "linear", parallel = TRUE,
    num_workers = raveio::raveio_getopt("max_worker"),
    verbose = TRUE
) {
  fs <- repository$sample_rate
  results <- parse_electrodes(load_electrodes)
  nel <- results$nel
  elecn <-results$elecn
  nfft <- min_nfft
  if(!isTRUE(nfft >= 1)) { nfft <- NA }
  data_length <- length(repository$voltage$dimnames$Time)


  # generate multitaper with cache
  repository_signature <- repository$signature
  electrode_parse <- results

  filebase <- file.path(pipeline$extdata_path, "karaslab_multitaper_explorer")
  dnames <- NULL
  epoch_table <- repository$epoch_table
  epoch_table$Condition2 <- sprintf("%s (%s)", epoch_table$Condition, epoch_table$Trial)

  # save meta to the array headers
  meta <- list()
  meta$epoch_table <- epoch_table
  meta$electrode_table <- repository$electrode_table
  meta$electrodes <- electrode_parse
  meta$project_name <- repository$subject$project_name
  meta$subject_code <- repository$subject$subject_code

  time_freq_data <- raveio::cache_to_filearray(
    filebase = filebase,
    globals = c("repository_signature", "data_length", "nfft", "electrode_parse", "frequency_range",
                "time_bandwidth", "num_tapers", "window_params", "weighting", "detrend_opt"),
    fun = function(){
      # voltage_for_analysis <- repository$voltage$data_list[[sprintf("e_%s", elecn[1])]]
      #
      # # Generate data format for storing epochs and corresponding voltage data
      # conditions <- epoch_table$Condition
      #
      # multitaper_config <- ravetools::multitaper_config(
      #   data_length = data_length,
      #   fs = fs,
      #   frequency_range = frequency_range,
      #   time_bandwidth = time_bandwidth,
      #   num_tapers = num_tapers,
      #   window_params = window_params,
      #   nfft = nfft,
      #   detrend_opt = detrend_opt
      # )
      # res <- ravetools:::multitaper_process_input(
      #   data_length,
      #   fs,
      #   frequency_range,
      #   time_bandwidth,
      #   num_tapers,
      #   window_params,
      #   nfft,
      #   detrend_opt
      # )
      #
      # time <- (res$window_start - 1 + res$winsize_samples / 2) / res$fs

      # We want to run multitaper on all of the trials (sz case)
      if(parallel && num_workers > 1) {
        lapply2 <- function(X, FUN, callback) {
          raveio::lapply_async(X, FUN, ncores = num_workers, callback = callback)
        }
      } else {
        lapply2 <- function(X, FUN, callback) {
          progress <- shidashi::shiny_progress(title = "Applying multitaper", max = length(X),
                                               shiny_auto_close = TRUE)
          lapply(X, function(x) {
            s <- c(rev(strsplit(callback(x), split = "\\|")[[1]]), "Applying multitaper")[c(1,2)]
            progress$inc(detail = s[[1]], message = s[[2]])
            FUN(x)
          })
        }
      }

      time_freq_data <- lapply2(repository$voltage$data_list, function(voltage_for_analysis) {

        time_start <- min(as.numeric(dimnames(voltage_for_analysis)$Time))

        # load data
        time_freq_per_chann_data <- filearray::apply(voltage_for_analysis, 2L, function(voltage) {
          res <- multitaper_spectrogram_R(
            voltage, fs, frequency_range, time_bandwidth, num_tapers,
            window_params, min_nfft, weighting, detrend_opt, parallel = FALSE,
            num_workers = FALSE, plot_on = FALSE, verbose = FALSE, xyflip = FALSE)
          re <- t(res[[1]])
          # time <- (res$window_start - 1 + res$winsize_samples / 2) / res$fs
          dimnames(re) <- list(
            Time = res[[2]] + time_start,
            Frequency = res[[3]]
          )
          re
        }, simplify = FALSE)
        dnames <- dimnames(time_freq_per_chann_data[[1]])

        # Time x Frequency x Trial
        time_freq_per_chann_data <- simplify2array(time_freq_per_chann_data)
        dimnames(time_freq_per_chann_data) <- c(
          dnames,
          list(Trial = epoch_table$Trial)
        )

        time_freq_per_chann_data
      }, callback = function(el) {
        sprintf("Applying multitaper|Channel %s",
                paste(el$dimnames()$Electrode, collapse = ""))
      })

      dnames <- dimnames(time_freq_data[[1]])
      dnames <- list(
        Time = as.numeric(dnames$Time),
        Frequency = as.numeric(dnames$Frequency),
        Trial = as.numeric(dnames$Trial),
        Electrode = repository$electrode_list
      )
      time_freq_data <- simplify2array(time_freq_data)
      dimnames(time_freq_data) <- dnames
      meta$dnames <- dnames

      # set "extra" headers, this information will be saved
      attr(time_freq_data, "extra") <- meta
      time_freq_data
    },
    verbose = verbose
  )


  time_freq_data
}

## generate all frequency plots for a specific condition
## Code for computing beta power matrix
generate_power_over_time_data <- function(
    multitaper_result,
    analysis_time_frequencies,
    baselined,
    baseline,
    start_time_baseline,
    end_time_baseline
) {

  # get header information
  meta <- multitaper_result$get_header("extra")
  dnames <- meta$dnames
  epoch_table <- meta$epoch_table
  electrode_table <- meta$electrode_table
  electrode_parse <- meta$electrodes
  project_name <- meta$project_name
  subject_code <- meta$subject_code

  nel <- electrode_parse$nel
  elecn <- electrode_parse$elecn

  value_range <- c(0, 0)

  # collapse over frequency for each time point and condition
  group_data <- lapply(seq_along(analysis_time_frequencies), function(ii) {
    # ii <- 1
    # analysis_item <- list(frequency_range = c(1L, 200L), time_range = c(-1L, 2L))
    analysis_item <- analysis_time_frequencies[[ ii ]]
    analysis_item$frequency_range <- range(unlist(analysis_item$frequency_range))
    analysis_item$time_range <- range(unlist(analysis_item$time_range))

    frequency_selection <- dnames$Frequency %within% analysis_item$frequency_range

    if(!any(frequency_selection)) {
      # invalid frequency range is selected, return NULL so the visualization
      # can skip
      return(NULL)
    }

    time_range_for_analysis <- range(unlist(analysis_item$time_range))
    time <- dnames$Time
    actual_ranges <- range(time)
    if(anyNA(time_range_for_analysis)) {
      time_range_for_analysis <- actual_ranges
    } else {
      if(time_range_for_analysis[[1]] < actual_ranges[[1]]){
        time_range_for_analysis[[1]] = actual_ranges[[1]]
      }
      if(time_range_for_analysis[[2]] > actual_ranges[[2]]){
        time_range_for_analysis[[2]] = actual_ranges[[2]]
      }
    }

    # Time x Frequency x Trial x Electrode,
    # drop=FALSE will keep the dimensions when there is one frequency selected, otherwise R drops frequency margin
    sub_array <- multitaper_result[, frequency_selection, , , drop = FALSE, dimnames = NULL]

    # Time (keep) x Frequency (collapse) x Trial (keep) x Electrode (keep)
    data_over_time_trial_per_elec <- ravetools::collapse(sub_array, keep = c(1, 3, 4), average = TRUE)

    #Perform baselining here if needed
    if (baselined) {

      if(length(baseline)) {
        if(is.numeric(baseline)) {
          baseline_sel <- which(epoch_table$Trial %in% baseline)
        } else {
          baseline_sel <- which(epoch_table$Condition2 %in% baseline)
        }
      } else {
        baseline_sel <- NULL
        print("Please set baseline condition")
      }

      # subset the baselining by the time range for analysis

      start_index <- which(time >= start_time_baseline)[1]
      end_index <- which(time <= end_time_baseline)[length(which(time <= end_time_baseline))]
      baseline_matrix <- data_over_time_trial_per_elec[start_index:end_index,baseline_sel,]

      dims <- dim(data_over_time_trial_per_elec)

      # run through the conditions
      for (j in 1:dims[2]) {
        #data_over_time_trial_per_elec[,j,] <- ((data_over_time_trial_per_elec[,j,] - mean(baselined_temp)) / sd(baselined_temp))

        m <- mean(baseline_matrix)
        baseline_sd <- sd(baseline_matrix)

        # Apply baseline correction (Z-score transformation)
        data_over_time_trial_per_elec[, j, ] <- (data_over_time_trial_per_elec[, j, ] - m) / baseline_sd

      }
    }

    value_range <<- range(c(range(data_over_time_trial_per_elec, na.rm = TRUE), value_range))

    list(
      group_id = ii,

      data_over_time_trial_per_elec = data_over_time_trial_per_elec,

      # time information
      time_range_for_analysis = time_range_for_analysis,

      # frequency collapsed
      frequency = dnames$Frequency[frequency_selection]

    )

  })


  list(
    project_name = project_name,
    subject_code = subject_code,

    group_data = group_data,

    value_range = value_range,

    time = dnames$Time,

    baselined = baselined,

    # epoch
    epoch_table = epoch_table,

    # channel information
    electrode_table = electrode_table[electrode_table$Electrode %in% dnames$Electrode, ]
  )

}

plot_power_over_time_data <- function(
    power_over_time_data, trial = NULL, soz_electrodes = NULL, resect_electrodes = NULL,
    name_type = c("name", "number"), value_range = NULL,
    scale = c("None", "Min_Max_Normalized_Column"),
    palette = plot_preferences$get('heatmap_palette'), ordered = FALSE, save_path = NULL) {
  # users can and only can select from given choices, i.e. one of c("name", "number")
  name_type <- match.arg(name_type)
  scale <- match.arg(scale)

  if(length(palette) < 101) {
    palette <- colorRampPalette(palette)(101)
  }

  # copy variables
  time <- power_over_time_data$time
  epoch_table <- power_over_time_data$epoch_table
  electrode_table <- power_over_time_data$electrode_table
  actual_range <- power_over_time_data$value_range
  project_name <- power_over_time_data$project_name
  subject_code <- power_over_time_data$subject_code

  if(length(value_range) > 0 && !anyNA(value_range)) {
    value_range <- range(value_range, na.rm = TRUE)
    if( value_range[[2]] == value_range[[1]] ) {
      if( scale == "Min_Max_Normalized_Column") {
        value_range <- c(0,1)
      } else {
        value_range <- actual_range
      }
    }
  } else {
    if( scale == "Min_Max_Normalized_Column") {
      value_range <- c(0,1)
    } else {
      value_range <- actual_range
    }
  }

  # determine the y-axis labels
  if( name_type == "name" ) {
    y_labels <- electrode_table$Label
  } else {
    y_labels <- electrode_table$Electrode
  }

  # dipsaus::parse_svec is the builtin function to parse text to integer channels
  soz_electrodes <- dipsaus::parse_svec(soz_electrodes)
  resect_electrodes <- dipsaus::parse_svec(resect_electrodes)
  is_soz <- electrode_table$Electrode %in% soz_electrodes
  is_resect <- electrode_table$Electrode %in% resect_electrodes

  # plot <- ggplot(heatmap_data, aes(x = Time, y = Electrode, fill = Value)) +
  #   geom_tile() +
  #   labs(x = "Time (s)", y = "Electrode") +
  #   scale_fill_viridis(option = "turbo") +
  #   theme_minimal() +
  #   theme(
  #     axis.text.y = element_text(size = 5, color = sapply(levels(heatmap_data$Electrode), color_electrodes))
  #   )

  if(length(trial)) {
    if(is.numeric(trial)) {
      trial_sel <- which(epoch_table$Trial %in% trial)
    } else {
      trial_sel <- which(epoch_table$Condition2 %in% trial)
    }
  } else {
    trial_sel <- NULL
  }

  group_data_is_valid <- !sapply(power_over_time_data$group_data, is.null)

  if(!any(group_data_is_valid)) { stop("No valid data; please check analysis frequency and time range.") }

  layout_heat_maps(sum(group_data_is_valid), max_col = 2, layout_color_bar = TRUE)
  par("mar" = c(3.5, 4.3, 3, 0.1), cex = 1.2)

  # loop 1: calculate value ranges & data for figures
  plot_data <- lapply(power_over_time_data$group_data[group_data_is_valid], function(group_item) {
    # No data is selected
    if(is.null(group_item)) { return(NULL) }

    data <- group_item$data_over_time_trial_per_elec
    if(length(trial_sel)) {
      data <- data[, trial_sel ,, drop = FALSE]
    }
    ntrials <- dim(data)[[2]]
    nchanns <- dim(data)[[3]]

    # Time x Trial (collapse) x Electrode
    data_over_time_per_elec <- ravetools::collapse(data, keep = c(1, 3), average = TRUE)

    elec_order_temp <- electrode_table$Electrode

    if (ordered == TRUE & (length(soz_electrodes) > 0 | length(resect_electrodes) > 0) ) {
      all_electrodes <- unique(c(soz_electrodes, resect_electrodes))
      selected_columns <- match(all_electrodes, electrode_table$Electrode)
      selected_columns <- na.omit(selected_columns)
      data_over_time_per_elec_ordered <- data_over_time_per_elec[, selected_columns]
      remaining_columns <- data_over_time_per_elec[, -selected_columns]
      data_over_time_per_elec <- cbind(data_over_time_per_elec_ordered, remaining_columns)
      data_over_time_per_elec <- as.matrix(data_over_time_per_elec)
      y_labels_ordered <- y_labels[selected_columns]
      y_labels <- c(y_labels_ordered, y_labels[-selected_columns])
      elec_order_temp_ordered <- elec_order_temp[selected_columns]
      elec_order_temp <- c(elec_order_temp_ordered, elec_order_temp[-selected_columns])
      is_soz <- elec_order_temp %in% soz_electrodes
      is_resect <- elec_order_temp %in% resect_electrodes
    }

    # calculate the value range
    time_range <- group_item$time_range_for_analysis
    time_range <- time_range[!is.na(time_range)]
    if(length(time_range) < 2) {
      time_range <- range(time)
    } else {
      time_range <- range(time_range)
    }
    # which time points are within the analysis time range
    time_selection <- time >= time_range[[1]] & time <= time_range[[2]]

    # overall data range
    data_range_all <- range(data_over_time_per_elec, na.rm = TRUE)

    # data range within the analysis window
    data_range_analysis <- range(data_over_time_per_elec[time_selection, ], na.rm = TRUE)

    list(
      # Time x Trial (collapse) x Electrode
      data_over_time_per_elec = data_over_time_per_elec,
      # bind two data ranges in one vector
      data_ranges_max = c(max(abs(data_range_all)), max(abs(data_range_analysis))),
      data_ranges_min = c(min(data_range_all), min(data_range_analysis)),

      # Tiem related
      time = time,
      time_range_for_analysis = group_item$time_range_for_analysis,

      # label-related
      labels = y_labels,
      is_soz = is_soz,
      is_resect = is_resect,

      # sub-title
      freq_range = range(group_item$frequency),
      nchanns = nchanns,
      ntrials = ntrials,
      group_id = group_item$group_id,
      epoch = trial_sel
    )
  })

  # get ranges
  plot_data <- dipsaus::drop_nulls(plot_data)
  data_ranges_max <- sapply(plot_data, "[[", "data_ranges_max")
  data_ranges_min <- sapply(plot_data, "[[", "data_ranges_min")
  data_max <- max(data_ranges_max[2, ])
  data_min <- min(data_ranges_min[2, ])
  baselined <- power_over_time_data$baselined

  lapply(plot_data, function(group_item) {

    data_over_time_per_elec <- group_item$data_over_time_per_elec
    time <- group_item$time
    y_labels <- group_item$labels
    is_soz <- group_item$is_soz
    is_resect <- group_item$is_resect
    freq_range <- group_item$freq_range
    ntrials <- group_item$ntrials
    nchanns <- group_item$nchanns
    group_id <- group_item$group_id
    epoch <- group_item$epoch

    if( scale == "Min_Max_Normalized_Column") {
      # use group_item$data_ranges_max[2] as the value cap
      max_value <- group_item$data_ranges_max[2]
      data_over_time_per_elec <- t(data_over_time_per_elec)

      maxcol <- apply(data_over_time_per_elec, 2, max)
      data_over_time_per_elec_temp <- data_over_time_per_elec

      mincol <- apply(data_over_time_per_elec, 2, min)
      maxcol <- apply(data_over_time_per_elec, 2, max)

      data_over_time_per_elec_temp <- data_over_time_per_elec

      for (col in 1:ncol(data_over_time_per_elec_temp)) {
        for (row in 1:nrow(data_over_time_per_elec_temp)) {
          # Avoid division by zero if max equals min
          if(maxcol[col] != mincol[col]) {
            data_over_time_per_elec_temp[row, col] <- (data_over_time_per_elec[row, col] - mincol[col]) / (maxcol[col] - mincol[col])
          } else {
            # Handle case where max equals min, potentially setting to 0, 0.5, or another appropriate value
            data_over_time_per_elec_temp[row, col] <- 0
          }
        }
      }

      data_over_time_per_elec <- t(data_over_time_per_elec_temp)

      if( baselined ) {
        data_over_time_per_elec[data_over_time_per_elec < -1] <- -1
        value_range <- c(-1, 1)
      } else {
        data_over_time_per_elec[data_over_time_per_elec < 0] <- 0
        value_range <- c(0, 1)
      }

    } else {
      data_over_time_per_elec[data_over_time_per_elec > data_max] <- data_max
      if( baselined ) {
        data_over_time_per_elec[data_over_time_per_elec < data_min] <- data_min
        value_range <- c(data_min, data_max)
      } else {
        data_over_time_per_elec[data_over_time_per_elec < data_min] <- data_min
        value_range <- c(data_min, data_max)
      }
    }

    graphics::image(data_over_time_per_elec,
                    x = time, y = seq_along(y_labels),
                    xlim = group_item$time_range_for_analysis, zlim = value_range,
                    axes = FALSE, xlab = "", ylab = "", col = palette)
    graphics::axis(side = 1, at = pretty(time))
    graphics::mtext(text = "Time (s)", side = 1, line = 2.2, cex = par("cex"))

    y_labels_tmp <- y_labels
    y_labels_tmp[is_soz | is_resect] <- ""
    graphics::axis(side = 2, at = seq_along(y_labels), labels = y_labels_tmp, las = 1)
    y_labels_tmp <- y_labels
    y_labels_tmp[!is_soz] <- ""
    graphics::axis(side = 2, at = seq_along(y_labels), labels = y_labels_tmp, las = 1, col.axis = "#00bfff")
    y_labels_tmp <- y_labels
    y_labels_tmp[!is_resect] <- ""
    graphics::axis(side = 2, at = seq_along(y_labels), labels = y_labels_tmp, las = 1, col.axis = "#bf00ff")
    y_labels_tmp <- y_labels
    y_labels_tmp[!is_resect | !is_soz] <- ""
    graphics::axis(side = 2, at = seq_along(y_labels), labels = y_labels_tmp, las = 1, col.axis = "green")

    if( name_type == "number" ) {
      graphics::mtext(text = "Electrode Channel", side = 2, line = 2.7, cex = par("cex"))
    }

    graphics::title(sprintf("# Channel=%s, # Epoch=%d, Freq=%.0f~%.0f Hz, Unit=%s", nchanns, ntrials, freq_range[[1]], freq_range[[2]], scale), adj = 0, line = 1.5)
    # sub-title
    graphics::title(sprintf("%s/%s - Analysis Group %d", project_name, subject_code, group_item$group_id), adj = 0, line = 0.5, cex.main = 0.8)

    if(!is.null(save_path)) {
      data_over_time_per_elec <- t(data_over_time_per_elec)
      colnames(data_over_time_per_elec) <- time
      rownames(data_over_time_per_elec) <- seq_along(y_labels)
      filename <- paste0(subject_code, "_", trial, ".csv")
      filename <- paste0(save_path, filename)
      write.csv(data_over_time_per_elec, filename, row.names = TRUE)
    }

    return(TRUE)
  })

  if( scale == "Min_Max_Normalized_Column" ) {
    if( baselined ) {
      legend_range <- c(-1, 1)
    } else {
      legend_range <- c(0, 1)
    }

  } else {
    # if( baselined ) {
    #   legend_range <- c(data_min, data_max)
    # } else {
    #   legend_range <- c(0, data_max)
    # }
    legend_range <- c(data_min, data_max)
  }
  par("mar" = c(3.1, 0.5, 3, 3.1))
  pal_val <- seq(legend_range[[1]], legend_range[[2]], length.out = 101)
  graphics::image(matrix(pal_val, nrow = 1), x = 0, y = pal_val, axes = FALSE, xlab = "", ylab = "", col = palette)
  legend_at <- unique(c(legend_range, 0))
  graphics::axis(side = 4, at = legend_at, labels = sprintf("%.1f", legend_at), las = 1)

  if(scale == "Min_Max_Normalized_Column") {
    graphics::title("Max Normalized", line = 0.6, adj = 0, cex.main = 0.8)
  } else {
    data_max_text <- sprintf("%.1f", data_max)
    data_min_text <- sprintf("%.1f", data_min)
    if( baselined ) {
      actual_range_text <- paste0("-", data_min_text, " ~ ", data_max_text)
    } else {
      actual_range_text <- paste(c("0", data_max_text), collapse = " ~ ")
    }

    graphics::title(sprintf("[%s]", actual_range_text), line = 0.6, adj = 0, cex.main = 0.8)
  }

}

plot_power_over_time_data_line <- function(
    power_over_time_data, trial = NULL, soz_electrodes = NULL, resect_electrodes = NULL,
    name_type = c("name", "number"), value_range = NULL,
    scale = c("None", "Min_Max_Normalized_Column"),
    palette = plot_preferences$get('heatmap_palette'), save_path = NULL) {
  # users can and only can select from given choices, i.e. one of c("name", "number")
  name_type <- match.arg(name_type)
  scale <- match.arg(scale)

  if(length(palette) < 101) {
    palette <- colorRampPalette(palette)(101)
  }

  # copy variables
  time <- power_over_time_data$time
  epoch_table <- power_over_time_data$epoch_table
  electrode_table <- power_over_time_data$electrode_table
  actual_range <- power_over_time_data$value_range
  project_name <- power_over_time_data$project_name
  subject_code <- power_over_time_data$subject_code

  if(length(value_range) > 0 && !anyNA(value_range)) {
    value_range <- range(value_range, na.rm = TRUE)
    if( value_range[[2]] == value_range[[1]] ) {
      if( scale == "Min_Max_Normalized_Column") {
        value_range <- c(0,1)
      } else {
        value_range <- actual_range
      }
    }
  } else {
    if( scale == "Min_Max_Normalized_Column") {
      value_range <- c(0,1)
    } else {
      value_range <- actual_range
    }
  }

  # determine the y-axis labels
  if( name_type == "name" ) {
    y_labels <- electrode_table$Label
  } else {
    y_labels <- electrode_table$Electrode
  }

  # dipsaus::parse_svec is the builtin function to parse text to integer channels
  soz_electrodes <- dipsaus::parse_svec(soz_electrodes)
  resect_electrodes <- dipsaus::parse_svec(resect_electrodes)
  is_soz <- electrode_table$Electrode %in% soz_electrodes
  is_resect <- electrode_table$Electrode %in% resect_electrodes

  # plot <- ggplot(heatmap_data, aes(x = Time, y = Electrode, fill = Value)) +
  #   geom_tile() +
  #   labs(x = "Time (s)", y = "Electrode") +
  #   scale_fill_viridis(option = "turbo") +
  #   theme_minimal() +
  #   theme(
  #     axis.text.y = element_text(size = 5, color = sapply(levels(heatmap_data$Electrode), color_electrodes))
  #   )

  if(length(trial)) {
    if(is.numeric(trial)) {
      trial_sel <- which(epoch_table$Trial %in% trial)
    } else {
      trial_sel <- which(epoch_table$Condition2 %in% trial)
    }
  } else {
    trial_sel <- NULL
  }

  group_data_is_valid <- !sapply(power_over_time_data$group_data, is.null)

  if(!any(group_data_is_valid)) { stop("No valid data; please check analysis frequency and time range.") }

  layout_heat_maps(sum(group_data_is_valid), max_col = 2, layout_color_bar = TRUE)
  par("mar" = c(3.5, 4.3, 3, 0.1), cex = 1.2)

  # loop 1: calculate value ranges & data for figures
  plot_data <- lapply(power_over_time_data$group_data[group_data_is_valid], function(group_item) {
    # No data is selected
    if(is.null(group_item)) { return(NULL) }

    data <- group_item$data_over_time_trial_per_elec
    if(length(trial_sel)) {
      data <- data[, trial_sel ,, drop = FALSE]
    }
    ntrials <- dim(data)[[2]]
    nchanns <- dim(data)[[3]]

    # Time x Trial (collapse) x Electrode
    data_over_time_per_elec <- ravetools::collapse(data, keep = c(1, 3), average = TRUE)

    elec_order_temp <- electrode_table$Electrode

    # calculate the value range
    time_range <- group_item$time_range_for_analysis
    time_range <- time_range[!is.na(time_range)]
    if(length(time_range) < 2) {
      time_range <- range(time)
    } else {
      time_range <- range(time_range)
    }
    # which time points are within the analysis time range
    time_selection <- time >= time_range[[1]] & time <= time_range[[2]]

    # overall data range
    data_range_all <- range(data_over_time_per_elec, na.rm = TRUE)

    # data range within the analysis window
    data_over_time_per_elec_find_border <- data_over_time_per_elec[time_selection, ]
    data_range_analysis <- range(data_over_time_per_elec[time_selection, ], na.rm = TRUE)

    if (any(is_soz) & !any(is_resect)) {
      soz_columns <- which(is_soz)
      soz_data <- data_over_time_per_elec_find_border[, soz_columns]
      if (is.null(nrow(soz_data))) {
        soz_data <- as.matrix(cbind(soz_data, soz_data))
        dimnames(soz_data) <- NULL
      }
      average_power_soz <- apply(soz_data, 1, mean)
      std_err_soz <- apply(soz_data, 1, function(x) sd(x) / sqrt(length(x)))

      non_soz_columns <- which(!is_soz)
      non_soz_data <- data_over_time_per_elec_find_border[, non_soz_columns]
      if (is.null(nrow(non_soz_data))) {
        non_soz_data <- as.matrix(cbind(non_soz_data, non_soz_data))
        dimnames(non_soz_data) <- NULL
      }
      average_power_not_soz <- apply(non_soz_data, 1, mean)
      std_err_not_soz <- apply(non_soz_data, 1, function(x) sd(x) / sqrt(length(x)))


      shade_upper_soz <- average_power_soz + std_err_soz
      shade_lower_soz <- average_power_soz - std_err_soz
      shade_upper_not_soz <- average_power_not_soz + std_err_not_soz
      shade_lower_not_soz <- average_power_not_soz - std_err_not_soz

      lowerrange <- min(shade_lower_soz, shade_lower_not_soz)
      highrange <- max(shade_upper_soz, shade_lower_not_soz)

      data_range_analysis <- c(lowerrange, highrange)

    } else if (!any(is_soz) & any(is_resect)) {
      resect_columns <- which(is_resect)
      resect_data <- data_over_time_per_elec_find_border[, resect_columns]
      if (is.null(nrow(resect_data))) {
        resect_data <- as.matrix(cbind(resect_data, resect_data))
        dimnames(resect_data) <- NULL
      }
      average_power_resect <- apply(resect_data, 1, mean)
      std_err_resect <- apply(resect_data, 1, function(x) sd(x) / sqrt(length(x)))

      non_resect_columns <- which(!is_resect)
      non_resect_data <- data_over_time_per_elec_find_border[, non_resect_columns]
      if (is.null(nrow(non_resect_data))) {
        non_resect_data <- as.matrix(cbind(non_resect_data, non_resect_data))
        dimnames(non_resect_data) <- NULL
      }
      average_power_not_resect <- apply(non_resect_data, 1, mean)
      std_err_not_resect <- apply(non_resect_data, 1, function(x) sd(x) / sqrt(length(x)))

      shade_upper_resect <- average_power_resect + std_err_resect
      shade_lower_resect <- average_power_resect - std_err_resect
      shade_upper_not_resect <- average_power_not_resect + std_err_not_resect
      shade_lower_not_resect <- average_power_not_resect - std_err_not_resect

      lowerrange <- min(shade_lower_resect, shade_lower_not_resect)
      highrange <- max(shade_upper_resect, shade_upper_not_resect)

      data_range_analysis <- c(lowerrange, highrange)

    } else if (any(is_soz) & any(is_resect)) {
      soz_columns <- which(is_soz)
      soz_data <- data_over_time_per_elec_find_border[, soz_columns]
      if (is.null(nrow(soz_data))) {
        soz_data <- as.matrix(cbind(soz_data, soz_data))
        dimnames(soz_data) <- NULL
      }
      average_power_soz <- apply(soz_data, 1, mean)
      std_err_soz <- apply(soz_data, 1, function(x) sd(x) / sqrt(length(x)))

      resect_columns <- which(is_resect)
      resect_data <- data_over_time_per_elec_find_border[, resect_columns]
      if (is.null(nrow(resect_data))) {
        resect_data <- as.matrix(cbind(resect_data, resect_data))
        dimnames(resect_data) <- NULL
      }
      average_power_resect <- apply(resect_data, 1, mean)
      std_err_resect <- apply(resect_data, 1, function(x) sd(x) / sqrt(length(x)))

      non_soz_non_resect_columns <- which(!is_soz & !is_resect)
      non_soz_non_resect_data <- data_over_time_per_elec_find_border[, non_soz_non_resect_columns]
      if (is.null(nrow(non_soz_non_resect_data))) {
        non_soz_non_resect_data <- as.matrix(cbind(non_soz_non_resect_data, non_soz_non_resect_data))
        dimnames(non_soz_non_resect_data) <- NULL
      }
      average_power_not_soz_not_resect <- apply(non_soz_non_resect_data, 1, mean)
      std_err_not_soz_not_resect <- apply(non_soz_non_resect_data, 1, function(x) sd(x) / sqrt(length(x)))

      shade_upper_resect <- average_power_resect + std_err_resect
      shade_lower_resect <- average_power_resect - std_err_resect

      shade_upper_soz <- average_power_soz + std_err_soz
      shade_lower_soz <- average_power_soz - std_err_soz

      shade_upper_not_resect <- average_power_not_soz_not_resect + std_err_not_soz_not_resect
      shade_lower_not_resect <- average_power_not_soz_not_resect - std_err_not_soz_not_resect

      lowerrange <- min(shade_lower_resect, shade_lower_soz, shade_lower_not_resect)
      highrange <- max(shade_upper_resect, shade_upper_soz, shade_upper_not_resect)

      data_range_analysis <- c(lowerrange, highrange)

    } else {
      if (is.null(nrow(data_over_time_per_elec))) {
        data_over_time_per_elec <- as.matrix(cbind(data_over_time_per_elec, data_over_time_per_elec))
        dimnames(data_over_time_per_elec) <- NULL
      }
      average_power <- apply(data_over_time_per_elec, 1, mean)
      std_err <- apply(data_over_time_per_elec, 1, function(x) sd(x) / sqrt(length(x)))

      shade_upper <- average_power + std_err
      shade_lower <- average_power - std_err

      lowerrange <- min(shade_lower)
      highrange <- max(shade_upper)

      data_range_analysis <- c(lowerrange, highrange)

    }

    list(
      # Time x Trial (collapse) x Electrode
      data_over_time_per_elec = data_over_time_per_elec,
      # bind two data ranges in one vector
      data_ranges_max = c(max(abs(data_range_all)), max(abs(data_range_analysis))),
      data_ranges_min = c(min(data_range_all), min(data_range_analysis)),

      # Tiem related
      time = time,
      time_range_for_analysis = group_item$time_range_for_analysis,

      # label-related
      labels = y_labels,
      is_soz = is_soz,
      is_resect = is_resect,

      # sub-title
      freq_range = range(group_item$frequency),
      nchanns = nchanns,
      ntrials = ntrials,
      group_id = group_item$group_id
    )
  })

  # get ranges
  plot_data <- dipsaus::drop_nulls(plot_data)
  data_ranges_max <- sapply(plot_data, "[[", "data_ranges_max")
  data_ranges_min <- sapply(plot_data, "[[", "data_ranges_min")
  data_max <- max(data_ranges_max[2, ])
  data_min <- min(data_ranges_min[2, ])
  baselined <- power_over_time_data$baselined

  lapply(plot_data, function(group_item) {

    data_over_time_per_elec <- group_item$data_over_time_per_elec
    time <- group_item$time
    y_labels <- group_item$labels
    is_soz <- group_item$is_soz
    is_resect <- group_item$is_resect
    freq_range <- group_item$freq_range
    ntrials <- group_item$ntrials
    nchanns <- group_item$nchanns
    group_id <- group_item$group_id
    epoch <- group_item$epoch

    if( scale == "Min_Max_Normalized_Column") {
      # use group_item$data_ranges_max[2] as the value cap
      max_value <- group_item$data_ranges_max[2]
      data_over_time_per_elec <- t(data_over_time_per_elec)

      maxcol <- apply(data_over_time_per_elec, 2, max)
      data_over_time_per_elec_temp <- data_over_time_per_elec

      mincol <- apply(data_over_time_per_elec, 2, min)
      maxcol <- apply(data_over_time_per_elec, 2, max)

      data_over_time_per_elec_temp <- data_over_time_per_elec

      for (col in 1:ncol(data_over_time_per_elec_temp)) {
        for (row in 1:nrow(data_over_time_per_elec_temp)) {
          # Avoid division by zero if max equals min
          if(maxcol[col] != mincol[col]) {
            data_over_time_per_elec_temp[row, col] <- (data_over_time_per_elec[row, col] - mincol[col]) / (maxcol[col] - mincol[col])
          } else {
            # Handle case where max equals min, potentially setting to 0, 0.5, or another appropriate value
            data_over_time_per_elec_temp[row, col] <- 0
          }
        }
      }

      data_over_time_per_elec <- t(data_over_time_per_elec_temp)

      if( baselined ) {
        value_range <- c(-1, 1)
      } else {
        value_range <- c(0, 1)
      }

    } else {
      if( baselined ) {
        value_range <- c(min(group_item$data_ranges_min[2]), max(group_item$data_ranges_max[2]))
      } else {
        value_range <- c(0, data_max)
      }
    }

    if (any(is_soz) & !any(is_resect)) {
      soz_columns <- which(is_soz)
      soz_data <- data_over_time_per_elec[, soz_columns]
      if (is.null(nrow(soz_data))) {
        soz_data <- as.matrix(cbind(soz_data, soz_data))
        dimnames(soz_data) <- NULL
      }
      average_power_soz <- apply(soz_data, 1, mean)
      std_err_soz <- apply(soz_data, 1, function(x) sd(x) / sqrt(length(x)))

      non_soz_columns <- which(!is_soz)
      non_soz_data <- data_over_time_per_elec[, non_soz_columns]
      if (is.null(nrow(non_soz_data))) {
        non_soz_data <- as.matrix(cbind(non_soz_data, non_soz_data))
        dimnames(non_soz_data) <- NULL
      }
      average_power_not_soz <- apply(non_soz_data, 1, mean)
      std_err_not_soz <- apply(non_soz_data, 1, function(x) sd(x) / sqrt(length(x)))

      # Plot line plot of average power for SOZ
      graphics::plot(time, average_power_soz, type = "l", xlim = group_item$time_range_for_analysis, ylim = value_range,
                     col = "#00bfff", lwd = 5,
                     xlab = "Time (s)", ylab = "Average Heatmap Value")

      # Add shaded area for standard error for SOZ
      shade_upper_soz <- average_power_soz + std_err_soz
      shade_lower_soz <- average_power_soz - std_err_soz
      graphics::polygon(c(time, rev(time)), c(shade_upper_soz, rev(shade_lower_soz)), col = rgb(0, 0, 1, alpha = 0.3), border = NA)

      # Plot line plot of average power for non-SOZ
      graphics::lines(time, average_power_not_soz, col = "black", lwd = 5)

      # Add shaded area for standard error for non-SOZ
      shade_upper_not_soz <- average_power_not_soz + std_err_not_soz
      shade_lower_not_soz <- average_power_not_soz - std_err_not_soz
      graphics::polygon(c(time, rev(time)), c(shade_upper_not_soz, rev(shade_lower_not_soz)), col = rgb(0.7, 0.7, 0.7, alpha = 0.5), border = NA)

      # Add legend
      legend("topright", legend=c("SOZ", "Non-SOZ"), col=c("#00bfff", "black"), lwd=5)

      if(!(is.null(save_path))) {
        save_data <- data.frame(
          soz_std_err = std_err_soz,
          average_power_soz = average_power_soz,
          average_power_other = average_power_not_soz,
          other_std_err = std_err_not_soz,
          time = time
        )
        filename <- paste0(subject_code, "_epoch_line_plot_", trial, ".csv")
        filename <- paste0(save_path, filename)
        write.csv(save_data, filename)
      }

    } else if (!any(is_soz) & any(is_resect)) {

      resect_columns <- which(is_resect)
      resect_data <- data_over_time_per_elec[, resect_columns]
      if (is.null(nrow(resect_data))) {
        resect_data <- as.matrix(cbind(resect_data, resect_data))
        dimnames(resect_data) <- NULL
      }
      average_power_resect <- apply(resect_data, 1, mean)
      std_err_resect <- apply(resect_data, 1, function(x) sd(x) / sqrt(length(x)))

      non_resect_columns <- which(!is_resect)
      non_resect_data <- data_over_time_per_elec[, non_resect_columns]
      if (is.null(nrow(non_resect_data))) {
        non_resect_data <- as.matrix(cbind(non_resect_data, non_resect_data))
        dimnames(non_resect_data) <- NULL
      }
      average_power_not_resect <- apply(non_resect_data, 1, mean)
      std_err_not_resect <- apply(non_resect_data, 1, function(x) sd(x) / sqrt(length(x)))

      # Plot line plot of average power for SOZ
      graphics::plot(time, average_power_resect, type = "l", xlim = group_item$time_range_for_analysis, ylim = value_range,
                     col = "#bf00ff", lwd = 5,
                     xlab = "Time (s)", ylab = "Average Heatmap Value")

      # Add shaded area for standard error for SOZ
      shade_upper_resect <- average_power_resect + std_err_resect
      shade_lower_resect <- average_power_resect - std_err_resect
      graphics::polygon(c(time, rev(time)), c(shade_upper_resect, rev(shade_lower_resect)),col = rgb(0.75, 0, 1, alpha = 0.3), border = NA)

      # Plot line plot of average power for non-SOZ
      graphics::lines(time, average_power_not_resect, col = "black", lwd = 5)

      # Add shaded area for standard error for non-SOZ
      shade_upper_not_resect <- average_power_not_resect + std_err_not_resect
      shade_lower_not_resect <- average_power_not_resect - std_err_not_resect
      graphics::polygon(c(time, rev(time)), c(shade_upper_not_resect, rev(shade_lower_not_resect)), col = rgb(0.7, 0.7, 0.7, alpha = 0.5), border = NA)

      # Add legend
      legend("topright", legend=c("Resect", "Non-Resect"), col=c("#bf00ff", "black"), lwd=5)

      if(!(is.null(save_path))) {
        save_data <- data.frame(
          std_err_resect = std_err_resect,
          average_power_resect = average_power_resect,
          average_power_other = average_power_not_resect,
          other_std_err = std_err_not_resect,
          time = time
        )
        filename <- paste0(subject_code, "_epoch_line_plot_", trial, ".csv")
        filename <- paste0(save_path, filename)
        write.csv(save_data, filename)
      }

    } else if (any(is_soz) & any(is_resect)) {
      # Compute average power and standard error for SOZ
      soz_columns <- which(is_soz)
      soz_data <- data_over_time_per_elec[, soz_columns]
      if (is.null(nrow(soz_data))) {
        soz_data <- as.matrix(cbind(soz_data, soz_data))
        dimnames(soz_data) <- NULL
      }
      average_power_soz <- apply(soz_data, 1, mean)
      std_err_soz <- apply(soz_data, 1, function(x) sd(x) / sqrt(length(x)))

      # Compute average power and standard error for resect
      resect_columns <- which(is_resect)
      resect_data <- data_over_time_per_elec[, resect_columns]
      if (is.null(nrow(resect_data))) {
        resect_data <- as.matrix(cbind(resect_data, resect_data))
        dimnames(resect_data) <- NULL
      }
      average_power_resect <- apply(resect_data, 1, mean)
      std_err_resect <- apply(resect_data, 1, function(x) sd(x) / sqrt(length(x)))

      # Compute average power and standard error for non-SOZ and non-resect
      non_soz_non_resect_columns <- which(!is_soz & !is_resect)
      non_soz_non_resect_data <- data_over_time_per_elec[, non_soz_non_resect_columns]
      if (is.null(nrow(non_soz_non_resect_data))) {
        non_soz_non_resect_data <- as.matrix(cbind(non_soz_non_resect_data, non_soz_non_resect_data))
        dimnames(non_soz_non_resect_data) <- NULL
      }
      average_power_not_soz_not_resect <- apply(non_soz_non_resect_data, 1, mean)
      std_err_not_soz_not_resect <- apply(non_soz_non_resect_data, 1, function(x) sd(x) / sqrt(length(x)))

      # Plot line plot of average power for SOZ
      graphics::plot(time, average_power_soz, type = "l", xlim = group_item$time_range_for_analysis, ylim = value_range,
                     col = "#00bfff", lwd = 5,
                     xlab = "Time (s)", ylab = "Average Heatmap Value")

      # Add shaded area for standard error for SOZ
      shade_upper_soz <- average_power_soz + std_err_soz
      shade_lower_soz <- average_power_soz - std_err_soz
      graphics::polygon(c(time, rev(time)), c(shade_upper_soz, rev(shade_lower_soz)), col = rgb(0, 0, 1, alpha = 0.3), border = NA)

      # Plot line plot of average power for resect
      graphics::lines(time, average_power_resect, col = "#bf00ff", lwd = 5)

      # Add shaded area for standard error for resect
      shade_upper_resect <- average_power_resect + std_err_resect
      shade_lower_resect <- average_power_resect - std_err_resect
      graphics::polygon(c(time, rev(time)), c(shade_upper_resect, rev(shade_lower_resect)), col = rgb(0.75, 0, 1, alpha = 0.3), border = NA)

      # Plot line plot of average power for non-SOZ and non-resect
      graphics::lines(time, average_power_not_soz_not_resect, col = "black", lwd = 5)

      # Add shaded area for standard error for non-SOZ and non-resect
      shade_upper_not_soz_not_resect <- average_power_not_soz_not_resect + std_err_not_soz_not_resect
      shade_lower_not_soz_not_resect <- average_power_not_soz_not_resect - std_err_not_soz_not_resect
      graphics::polygon(c(time, rev(time)), c(shade_upper_not_soz_not_resect, rev(shade_lower_not_soz_not_resect)), col = rgb(0.7, 0.7, 0.7, alpha = 0.5), border = NA)

      # Add legend
      legend("topright", legend=c("SOZ", "Resect", "Non-SOZ & Non-Resect"), col=c("#00bfff", "#bf00ff", "black"), lwd=5)

      if(!(is.null(save_path))) {
        save_data <- data.frame(
          average_power_resect = average_power_resect,
          std_err_resect = std_err_resect,
          average_power_soz = average_power_soz,
          std_err_soz = std_err_soz,
          average_power_other = average_power_not_soz_not_resect,
          other_std_err = std_err_not_soz_not_resect,
          time = time
        )
        filename <- paste0(subject_code, "_epoch_line_plot_", trial, ".csv")
        filename <- paste0(save_path, filename)
        write.csv(save_data, filename)
      }

    } else {
      # Compute average power across time for each electrode
      if (is.null(nrow(data_over_time_per_elec))) {
        data_over_time_per_elec <- as.matrix(cbind(data_over_time_per_elec, data_over_time_per_elec))
        dimnames(data_over_time_per_elec) <- NULL
      }
      average_power <- apply(data_over_time_per_elec, 1, mean)
      std_err <- apply(data_over_time_per_elec, 1, function(x) sd(x) / sqrt(length(x)))

      # Plot line plot of average power
      graphics::plot(time, average_power, type = "l", xlim = group_item$time_range_for_analysis, ylim = value_range,
                     col = "black", lwd = 5,
                     xlab = "Time (s)", ylab = "Average Heatmap Value")

      # Add shaded area for standard error
      shade_upper <- average_power + std_err
      shade_lower <- average_power - std_err
      graphics::polygon(c(time, rev(time)), c(shade_upper, rev(shade_lower)), col = rgb(0.7, 0.7, 0.7, alpha = 0.5), border = NA)

      # Add legend
      legend("topright", legend="Overall", col="black", lwd=5)

      if(!(is.null(save_path))) {
        save_data <- data.frame(
          average_power_all = average_power,
          std_err_all = std_err
        )
        filename <- paste0(subject_code, "_epoch_line_plot_", trial, ".csv")
        filename <- paste0(save_path, filename)
        write.csv(save_data, filename)
      }

    }


    # Add axes
    graphics::axis(side = 1, at = pretty(time))
    graphics::axis(side = 2)


    graphics::title(sprintf("# Channel=%s, # Epoch=%d, Freq=%.0f~%.0f Hz, Unit=%s", nchanns, ntrials, freq_range[[1]], freq_range[[2]], scale), adj = 0, line = 1.5)
    # sub-title
    graphics::title(sprintf("%s/%s - Analysis Group %d", project_name, subject_code, group_item$group_id), adj = 0, line = 0.5, cex.main = 0.8)


    return(TRUE)
  })


}

# plot_quantile_plot <- function(
#     power_over_time_data, trial = NULL, soz_electrodes = NULL, resect_electrodes = NULL,
#     name_type = c("name", "number"), value_range = NULL,
#     scale = c("None", "Min_Max_Normalized_Column"),
#     palette = plot_preferences$get('heatmap_palette'), save_path = NULL) {
#   # users can and only can select from given choices, i.e. one of c("name", "number")
#   name_type <- match.arg(name_type)
#   scale <- match.arg(scale)
#
#   if(length(palette) < 101) {
#     palette <- colorRampPalette(palette)(101)
#   }
#
#   # copy variables
#   time <- power_over_time_data$time
#   epoch_table <- power_over_time_data$epoch_table
#   electrode_table <- power_over_time_data$electrode_table
#   actual_range <- power_over_time_data$value_range
#   project_name <- power_over_time_data$project_name
#   subject_code <- power_over_time_data$subject_code
#
#   if(length(value_range) > 0 && !anyNA(value_range)) {
#     value_range <- range(value_range, na.rm = TRUE)
#     if( value_range[[2]] == value_range[[1]] ) {
#       if( scale == "Min_Max_Normalized_Column") {
#         value_range <- c(0,1)
#       } else {
#         value_range <- actual_range
#       }
#     }
#   } else {
#     if( scale == "Min_Max_Normalized_Column") {
#       value_range <- c(0,1)
#     } else {
#       value_range <- actual_range
#     }
#   }
#
#   # determine the y-axis labels
#   if( name_type == "name" ) {
#     y_labels <- electrode_table$Label
#   } else {
#     y_labels <- electrode_table$Electrode
#   }
#
#   # dipsaus::parse_svec is the builtin function to parse text to integer channels
#   soz_electrodes <- dipsaus::parse_svec(soz_electrodes)
#   resect_electrodes <- dipsaus::parse_svec(resect_electrodes)
#   is_soz <- electrode_table$Electrode %in% soz_electrodes
#   is_resect <- electrode_table$Electrode %in% resect_electrodes
#
#   # plot <- ggplot(heatmap_data, aes(x = Time, y = Electrode, fill = Value)) +
#   #   geom_tile() +
#   #   labs(x = "Time (s)", y = "Electrode") +
#   #   scale_fill_viridis(option = "turbo") +
#   #   theme_minimal() +
#   #   theme(
#   #     axis.text.y = element_text(size = 5, color = sapply(levels(heatmap_data$Electrode), color_electrodes))
#   #   )
#
#   if(length(trial)) {
#     if(is.numeric(trial)) {
#       trial_sel <- which(epoch_table$Trial %in% trial)
#     } else {
#       trial_sel <- which(epoch_table$Condition2 %in% trial)
#     }
#   } else {
#     trial_sel <- NULL
#   }
#
#   group_data_is_valid <- !sapply(power_over_time_data$group_data, is.null)
#
#   if(!any(group_data_is_valid)) { stop("No valid data; please check analysis frequency and time range.") }
#
#   layout_heat_maps(sum(group_data_is_valid), max_col = 2, layout_color_bar = TRUE)
#   par("mar" = c(4.5, 5, 3, 0.2), cex = 1.2)
#
#
#   sapply(power_over_time_data$group_data[group_data_is_valid], function(group_item) {
#     # No data is selected
#     if(is.null(group_item)) { return(FALSE) }
#
#     data <- group_item$data_over_time_trial_per_elec
#     if(length(trial_sel)) {
#       data <- data[, trial_sel ,, drop = FALSE]
#     }
#     ntrials <- dim(data)[[2]]
#     nchanns <- dim(data)[[3]]
#
#     # Time x Trial (collapse) x Electrode
#     data_over_time_per_elec <- ravetools::collapse(data, keep = c(1, 3), average = TRUE)
#
#     if( scale == "Min_Max_Normalized_Column") {
#       qval <- value_range[[2]]
#       if( qval <= 0 || qval > 1) {
#         qval <- 1
#       }
#
#       data_over_time_per_elec <- t(data_over_time_per_elec)
#
#       mincol <- apply(data_over_time_per_elec, 2, min)
#       maxcol <- apply(data_over_time_per_elec, 2, max)
#
#       data_over_time_per_elec_temp <- data_over_time_per_elec
#
#       for (col in 1:ncol(data_over_time_per_elec_temp)) {
#         for (row in 1:nrow(data_over_time_per_elec_temp)) {
#           # Avoid division by zero if max equals min
#           if(maxcol[col] != mincol[col]) {
#             data_over_time_per_elec_temp[row, col] <- (data_over_time_per_elec[row, col] - mincol[col]) / (maxcol[col] - mincol[col])
#           } else {
#             # Handle case where max equals min, potentially setting to 0, 0.5, or another appropriate value
#             data_over_time_per_elec_temp[row, col] <- 0
#           }
#         }
#       }
#
#       data_over_time_per_elec <- t(data_over_time_per_elec_temp)
#
#       value_range <- c(0, qval)
#     }
#
#     # Compute average power across time for each electrode
#     if (any(is_soz) & !any(is_resect)) {
#
#       soz_columns <- which(is_soz)
#       soz_data <- data_over_time_per_elec[, soz_columns]
#
#       non_soz_columns <- which(!is_soz)
#       non_soz_data <- data_over_time_per_elec[, non_soz_columns]
#
#       target_electrodes <- t(soz_data)
#       other_electrodes <- t(non_soz_data)
#
#       quantilematrixsozsozc=matrix(0,20,nrow(data_over_time_per_elec))
#
#
#       for(i in 1:nrow(data_over_time_per_elec)){
#
#         colsoz=target_electrodes[,i]
#         colsozc=other_electrodes[,i]
#
#         f10colsoz<-quantile(colsoz,probs=c(0.1))
#         f20colsoz<-quantile(colsoz,probs=c(0.2))
#         f30colsoz<-quantile(colsoz,probs=c(0.3))
#         f40colsoz<-quantile(colsoz,probs=c(0.4))
#         f50colsoz<-quantile(colsoz,probs=c(0.5))
#         f60colsoz<-quantile(colsoz,probs=c(0.6))
#         f70colsoz<-quantile(colsoz,probs=c(0.7))
#         f80colsoz<-quantile(colsoz,probs=c(0.8))
#         f90colsoz<-quantile(colsoz,probs=c(0.9))
#         f100colsoz<-quantile(colsoz,probs=c(1.0))
#
#         f10colsozc<-quantile(colsozc,probs=c(0.1))
#         f20colsozc<-quantile(colsozc,probs=c(0.2))
#         f30colsozc<-quantile(colsozc,probs=c(0.3))
#         f40colsozc<-quantile(colsozc,probs=c(0.4))
#         f50colsozc<-quantile(colsozc,probs=c(0.5))
#         f60colsozc<-quantile(colsozc,probs=c(0.6))
#         f70colsozc<-quantile(colsozc,probs=c(0.7))
#         f80colsozc<-quantile(colsozc,probs=c(0.8))
#         f90colsozc<-quantile(colsozc,probs=c(0.9))
#         f100colsozc<-quantile(colsoz,probs=c(1.0))
#
#         quantilematrixsozsozc[1,i]=f10colsoz
#         quantilematrixsozsozc[2,i]=f20colsoz
#         quantilematrixsozsozc[3,i]=f30colsoz
#         quantilematrixsozsozc[4,i]=f40colsoz
#         quantilematrixsozsozc[5,i]=f50colsoz
#         quantilematrixsozsozc[6,i]=f60colsoz
#         quantilematrixsozsozc[7,i]=f70colsoz
#         quantilematrixsozsozc[8,i]=f80colsoz
#         quantilematrixsozsozc[9,i]=f90colsoz
#         quantilematrixsozsozc[10,i]=f100colsoz
#         quantilematrixsozsozc[11,i]=f10colsozc
#         quantilematrixsozsozc[12,i]=f20colsozc
#         quantilematrixsozsozc[13,i]=f30colsozc
#         quantilematrixsozsozc[14,i]=f40colsozc
#         quantilematrixsozsozc[15,i]=f50colsozc
#         quantilematrixsozsozc[16,i]=f60colsozc
#         quantilematrixsozsozc[17,i]=f70colsozc
#         quantilematrixsozsozc[18,i]=f80colsozc
#         quantilematrixsozsozc[19,i]=f90colsozc
#         quantilematrixsozsozc[20,i]=f100colsozc
#
#       }
#
#       quantilesname<-c('SOZ(10th)','SOZ(20th)','SOZ(30th)','SOZ(40th)','SOZ(50th)',
#                        'SOZ(60th)','SOZ(70th)','SOZ(80th)','SOZ(90th)','SOZ(100th)',
#                        'OTHER(10th)','OTHER(20th)','OTHER(30th)','OTHER(40th)','OTHER(50th)',
#                        'OTHER(60th)','OTHER(70th)','OTHER(80th)','OTHER(90th)','OTHER(100th)')
#
#       time_vector <- seq(from = group_item$time_range_for_analysis[1],
#                          to = group_item$time_range_for_analysis[2],
#                          length.out = nrow(data_over_time_per_elec))
#
#
#       quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
#       quantileplot$Value <- c(t(quantilematrixsozsozc))
#
#       all_stats_labels <- levels(quantileplot$Stats)
#
#       # Plot the heatmap using image()
#       graphics::image(1:length(unique(quantileplot$Time)),
#                       1:length(all_stats_labels),
#                       t(matrix(quantileplot$Value,
#                                ncol = length(unique(quantileplot$Time)),
#                                byrow = TRUE)),
#                       col = palette,
#                       xlab = "Time (s)",
#                       ylab = "",
#                       axes = FALSE)
#       mtext("SOZ", side = 4, at = max(1:length(unique(quantileplot$Time))) + 3, col = "#00bfff", cex = 1.2)
#       mtext("OTHER", side = 4, at = max(1:length(unique(quantileplot$Time))) + 13, col = "black", cex = 1.2)
#
#       if(!is.null(save_path)) {
#         save_data <- matrix(quantileplot$Value,
#                               ncol = length(unique(quantileplot$Time)),
#                               byrow = TRUE)
#         colnames(save_data) <- unique(quantileplot$Time)
#         rownames(save_data) <- all_stats_labels
#         filename <- paste0(subject_code, "_epoch_statistical_plot_", trial, ".csv")
#         filename <- paste0(save_path, filename)
#         write.csv(save_data, filename, row.names = TRUE)
#       }
#
#       graphics::abline(h = 10 + 0.5, col = "red", lwd = 10, lty = "dashed")
#
#       min_time <- min(unique(quantileplot$Time))
#       max_time <- max(unique(quantileplot$Time))
#
#       graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
#                      labels = unique(quantileplot$Time), cex.axis = 1)
#
#       graphics::axis(2, at = 1:length(all_stats_labels),
#                      labels = all_stats_labels,
#                      las = 2, cex.axis = 0.6)
#
#     } else if (!any(is_soz) & any(is_resect)) {
#
#       resect_columns <- which(is_resect)
#       resect_data <- data_over_time_per_elec[, resect_columns]
#
#       non_resect_columns <- which(!is_resect)
#       non_resect_data <- data_over_time_per_elec[, non_resect_columns]
#
#       target_electrodes <- t(resect_data)
#       other_electrodes <- t(non_resect_data)
#
#       quantilematrixresectc=matrix(0,20,nrow(data_over_time_per_elec))
#
#
#       for(i in 1:nrow(data_over_time_per_elec)){
#
#         colresect=target_electrodes[,i]
#         colresectc=other_electrodes[,i]
#
#         f10colresect<-quantile(colresect,probs=c(0.1))
#         f20colresect<-quantile(colresect,probs=c(0.2))
#         f30colresect<-quantile(colresect,probs=c(0.3))
#         f40colresect<-quantile(colresect,probs=c(0.4))
#         f50colresect<-quantile(colresect,probs=c(0.5))
#         f60colresect<-quantile(colresect,probs=c(0.6))
#         f70colresect<-quantile(colresect,probs=c(0.7))
#         f80colresect<-quantile(colresect,probs=c(0.8))
#         f90colresect<-quantile(colresect,probs=c(0.9))
#         f100colresect<-quantile(colresect,probs=c(1.0))
#
#         f10colresectc<-quantile(colresectc,probs=c(0.1))
#         f20colresectc<-quantile(colresectc,probs=c(0.2))
#         f30colresectc<-quantile(colresectc,probs=c(0.3))
#         f40colresectc<-quantile(colresectc,probs=c(0.4))
#         f50colresectc<-quantile(colresectc,probs=c(0.5))
#         f60colresectc<-quantile(colresectc,probs=c(0.6))
#         f70colresectc<-quantile(colresectc,probs=c(0.7))
#         f80colresectc<-quantile(colresectc,probs=c(0.8))
#         f90colresectc<-quantile(colresectc,probs=c(0.9))
#         f100colresectc<-quantile(colresect,probs=c(1.0))
#
#         quantilematrixresectc[1,i]=f10colresect
#         quantilematrixresectc[2,i]=f20colresect
#         quantilematrixresectc[3,i]=f30colresect
#         quantilematrixresectc[4,i]=f40colresect
#         quantilematrixresectc[5,i]=f50colresect
#         quantilematrixresectc[6,i]=f60colresect
#         quantilematrixresectc[7,i]=f70colresect
#         quantilematrixresectc[8,i]=f80colresect
#         quantilematrixresectc[9,i]=f90colresect
#         quantilematrixresectc[10,i]=f100colresect
#         quantilematrixresectc[11,i]=f10colresectc
#         quantilematrixresectc[12,i]=f20colresectc
#         quantilematrixresectc[13,i]=f30colresectc
#         quantilematrixresectc[14,i]=f40colresectc
#         quantilematrixresectc[15,i]=f50colresectc
#         quantilematrixresectc[16,i]=f60colresectc
#         quantilematrixresectc[17,i]=f70colresectc
#         quantilematrixresectc[18,i]=f80colresectc
#         quantilematrixresectc[19,i]=f90colresectc
#         quantilematrixresectc[20,i]=f100colresectc
#
#       }
#
#       quantilesname<-c('RESECT(10th)','RESECT(20th)','RESECT(30th)','RESECT(40th)','RESECT(50th)',
#                        'RESECT(60th)','RESECT(70th)','RESECT(80th)','RESECT(90th)','RESECT(100th)',
#                        'OTHER(10th)','OTHER(20th)','OTHER(30th)','OTHER(40th)','OTHER(50th)',
#                        'OTHER(60th)','OTHER(70th)','OTHER(80th)','OTHER(90th)','OTHER(100th)')
#
#       time_vector <- seq(from = group_item$time_range_for_analysis[1],
#                          to = group_item$time_range_for_analysis[2],
#                          length.out = nrow(data_over_time_per_elec))
#
#       quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
#       quantileplot$Value <- c(t(quantilematrixresectc))
#
#       all_stats_labels <- levels(quantileplot$Stats)
#
#       # Plot the heatmap using image()
#       graphics::image(1:length(unique(quantileplot$Time)),
#                       1:length(all_stats_labels),
#                       t(matrix(quantileplot$Value,
#                                ncol = length(unique(quantileplot$Time)),
#                                byrow = TRUE)),
#                       col = palette,
#                       xlab = "Time (s)",
#                       ylab = "",
#                       axes = FALSE)
#
#
#       mtext("RESECT", side = 4, at = max(1:length(unique(quantileplot$Time))) + 3.5, col = "#bf00ff", cex = 1.2)
#       mtext("OTHER", side = 4, at = max(1:length(unique(quantileplot$Time))) + 13, col = "black", cex = 1.2)
#
#       graphics::abline(h = 10 + 0.5, col = "red", lwd = 10, lty = "dashed")
#
#       # Add axis labels
#
#       min_time <- min(unique(quantileplot$Time))
#       max_time <- max(unique(quantileplot$Time))
#
#       graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
#                      labels = unique(quantileplot$Time), cex.axis = 1)
#
#       graphics::axis(2, at = 1:length(all_stats_labels),
#                      labels = all_stats_labels,
#                      las = 2, cex.axis = 0.6)
#
#     } else if (any(is_soz) & any(is_resect)) {
#       resect_columns <- which(is_resect)
#       resect_data <- data_over_time_per_elec[, resect_columns]
#       resect_data <- t(resect_data)
#       soz_columns <- which(is_soz)
#       soz_data <- data_over_time_per_elec[, soz_columns]
#       soz_data <- t(soz_data)
#
#       non_resect_and_soz_columns <- which(!is_resect & !is_soz)
#       non_resect_and_soz_data <- data_over_time_per_elec[, non_resect_and_soz_columns]
#       non_resect_and_soz_data <- t(non_resect_and_soz_data)
#
#       quantilematrixresectsozc=matrix(0,30,nrow(data_over_time_per_elec))
#
#
#       for(i in 1:nrow(data_over_time_per_elec)){
#
#         colresect=resect_data[,i]
#         colsoz=soz_data[,i]
#         colother=non_resect_and_soz_data[,i]
#
#         f10colsoz<-quantile(colsoz,probs=c(0.1))
#         f20colsoz<-quantile(colsoz,probs=c(0.2))
#         f30colsoz<-quantile(colsoz,probs=c(0.3))
#         f40colsoz<-quantile(colsoz,probs=c(0.4))
#         f50colsoz<-quantile(colsoz,probs=c(0.5))
#         f60colsoz<-quantile(colsoz,probs=c(0.6))
#         f70colsoz<-quantile(colsoz,probs=c(0.7))
#         f80colsoz<-quantile(colsoz,probs=c(0.8))
#         f90colsoz<-quantile(colsoz,probs=c(0.9))
#         f100colsoz<-quantile(colsoz,probs=c(1.0))
#
#         f10colresect<-quantile(colresect,probs=c(0.1))
#         f20colresect<-quantile(colresect,probs=c(0.2))
#         f30colresect<-quantile(colresect,probs=c(0.3))
#         f40colresect<-quantile(colresect,probs=c(0.4))
#         f50colresect<-quantile(colresect,probs=c(0.5))
#         f60colresect<-quantile(colresect,probs=c(0.6))
#         f70colresect<-quantile(colresect,probs=c(0.7))
#         f80colresect<-quantile(colresect,probs=c(0.8))
#         f90colresect<-quantile(colresect,probs=c(0.9))
#         f100colresect<-quantile(colresect,probs=c(1.0))
#
#         f10colresectsozc<-quantile(colother,probs=c(0.1))
#         f20colresectsozc<-quantile(colother,probs=c(0.2))
#         f30colresectsozc<-quantile(colother,probs=c(0.3))
#         f40colresectsozc<-quantile(colother,probs=c(0.4))
#         f50colresectsozc<-quantile(colother,probs=c(0.5))
#         f60colresectsozc<-quantile(colother,probs=c(0.6))
#         f70colresectsozc<-quantile(colother,probs=c(0.7))
#         f80colresectsozc<-quantile(colother,probs=c(0.8))
#         f90colresectsozc<-quantile(colother,probs=c(0.9))
#         f100colresectsozc<-quantile(colother,probs=c(1.0))
#
#         quantilematrixresectsozc[1,i]=f10colsoz
#         quantilematrixresectsozc[2,i]=f20colsoz
#         quantilematrixresectsozc[3,i]=f30colsoz
#         quantilematrixresectsozc[4,i]=f40colsoz
#         quantilematrixresectsozc[5,i]=f50colsoz
#         quantilematrixresectsozc[6,i]=f60colsoz
#         quantilematrixresectsozc[7,i]=f70colsoz
#         quantilematrixresectsozc[8,i]=f80colsoz
#         quantilematrixresectsozc[9,i]=f90colsoz
#         quantilematrixresectsozc[10,i]=f100colsoz
#         quantilematrixresectsozc[11,i]=f10colresect
#         quantilematrixresectsozc[12,i]=f20colresect
#         quantilematrixresectsozc[13,i]=f30colresect
#         quantilematrixresectsozc[14,i]=f40colresect
#         quantilematrixresectsozc[15,i]=f50colresect
#         quantilematrixresectsozc[16,i]=f60colresect
#         quantilematrixresectsozc[17,i]=f70colresect
#         quantilematrixresectsozc[18,i]=f80colresect
#         quantilematrixresectsozc[19,i]=f90colresect
#         quantilematrixresectsozc[20,i]=f100colresect
#         quantilematrixresectsozc[21,i]=f10colresectsozc
#         quantilematrixresectsozc[22,i]=f20colresectsozc
#         quantilematrixresectsozc[23,i]=f30colresectsozc
#         quantilematrixresectsozc[24,i]=f40colresectsozc
#         quantilematrixresectsozc[25,i]=f50colresectsozc
#         quantilematrixresectsozc[26,i]=f60colresectsozc
#         quantilematrixresectsozc[27,i]=f70colresectsozc
#         quantilematrixresectsozc[28,i]=f80colresectsozc
#         quantilematrixresectsozc[29,i]=f90colresectsozc
#         quantilematrixresectsozc[30,i]=f100colresectsozc
#
#       }
#
#       quantilesname<-c('SOZ(10th)','SOZ(20th)','SOZ(30th)','SOZ(40th)','SOZ(50th)',
#                        'SOZ(60th)','SOZ(70th)','SOZ(80th)','SOZ(90th)','SOZ(100th)',
#                        'RESECT(10th)','RESECT(20th)','RESECT(30th)','RESECT(40th)','RESECT(50th)',
#                        'RESECT(60th)','RESECT(70th)','RESECT(80th)','RESECT(90th)','RESECT(100th)',
#                        'OTHERc(10th)','OTHERc(20th)','OTHERc(30th)','OTHERc(40th)','OTHERc(50th)',
#                        'OTHERc(60th)','OTHERc(70th)','OTHERc(80th)','OTHERc(90th)','OTHERc(100th)')
#
#       time_vector <- seq(from = group_item$time_range_for_analysis[1],
#                          to = group_item$time_range_for_analysis[2],
#                          length.out = nrow(data_over_time_per_elec))
#
#
#
#       quantileplot<- expand.grid(Time = time_vector, Stats=quantilesname)
#       quantileplot$Value <- c(t(quantilematrixresectsozc))
#
#       all_stats_labels <- levels(quantileplot$Stats)
#
#
#
#       # Plot the heatmap using image()
#       graphics::image(1:length(unique(quantileplot$Time)),
#                       1:length(all_stats_labels),
#                       t(matrix(quantileplot$Value,
#                                ncol = length(unique(quantileplot$Time)),
#                                byrow = TRUE)),
#                       col = palette,
#                       xlab = "Time (s)",
#                       ylab = "",
#                       axes = FALSE)
#
#       # Add axis labels
#       mtext("SOZ", side = 4, at = max(1:length(unique(quantileplot$Time))) + 3.5, col = "#00bfff", cex = 1.2)
#       mtext("RESECT", side = 4, at = max(1:length(unique(quantileplot$Time))) + 13, col = "#bf00ff", cex = 1.2)
#       mtext("OTHER", side = 4, at = max(1:length(unique(quantileplot$Time))) + 22.5, col = "black", cex = 1.2)
#
#       graphics::abline(h = 10 + 0.5, col = "red", lwd = 10, lty = "dashed")
#       graphics::abline(h = 20 + 0.5, col = "red", lwd = 10, lty = "dashed")
#
#
#       min_time <- min(unique(quantileplot$Time))
#       max_time <- max(unique(quantileplot$Time))
#
#       graphics::axis(1, at = 1:length(unique(quantileplot$Time)),
#                      labels = unique(quantileplot$Time), cex.axis = 1)
#
#       graphics::axis(2, at = 1:length(all_stats_labels),
#                      labels = all_stats_labels,
#                      las = 2, cex.axis = 0.6)
#
#     }
#
#     if (any(is_soz) | any(is_resect)) {
#       # Add subtitle
#       freq_range <- range(group_item$frequency)
#       graphics::title(sprintf("# Channel=%s, # Epoch=%d, Freq=%.0f~%.0f Hz, Unit=%s", nchanns, ntrials, freq_range[[1]], freq_range[[2]], scale), adj = 0, line = 1.5)
#       # sub-title
#       graphics::title(sprintf("%s/%s - Analysis Group %d", project_name, subject_code, group_item$group_id), adj = 0, line = 0.5, cex.main = 0.8)
#
#       par("mar" = c(3.1, 1, 3, 3.1))
#       pal_val <- seq(value_range[[1]], value_range[[2]], length.out = 101)
#       graphics::image(matrix(pal_val, nrow = 1), x = 0, y = pal_val, axes = FALSE, xlab = "", ylab = "", col = palette, xlim = c(0, 0.1))
#       graphics::axis(side = 4, at = c(value_range[[2]], 0), labels = c(sprintf("%.1f", value_range[[2]]), "0"), las = 1)
#
#       if(scale == "Min_Max_Normalized_Column") {
#         graphics::title("Max\nNormalized", line = 0.6, adj = 0, cex.main = 0.8)
#       } else {
#         actual_range_text <- paste(sprintf("%.1f", actual_range), collapse = " ~ ")
#         graphics::title(sprintf("[%s]", actual_range_text), line = 0.6, adj = 0, cex.main = 0.8)
#       }
#     }
#
#     return(TRUE)
#   }, USE.NAMES = FALSE)
# }


generate_3dviewer_data <- function(power_over_time_data, trial = NULL) {

  # copy variables
  time <- power_over_time_data$time
  epoch_table <- power_over_time_data$epoch_table
  electrode_table <- power_over_time_data$electrode_table
  actual_range <- power_over_time_data$value_range
  subject_code <- power_over_time_data$subject_code
  if(length(trial)) {
    if(is.numeric(trial)) {
      trial_sel <- which(epoch_table$Trial %in% trial)
    } else {
      trial_sel <- which(epoch_table$Condition2 %in% trial)
    }
  } else {
    trial_sel <- NULL
  }
  viewer_data <- lapply(power_over_time_data$group_data, function(group_item) {
    # group_item <- power_over_time_data$group_data[[1]]
    # No data is selected
    if(is.null(group_item)) { return(FALSE) }

    time_sel <- time %within% group_item$time_range_for_analysis

    if(!any(time_sel)) { return(NULL)}

    time_subset <- time[time_sel]

    data <- group_item$data_over_time_trial_per_elec
    if(length(trial_sel)) {
      data <- data[time_sel, trial_sel ,, drop = FALSE]
    } else {
      data <- data[time_sel,,, drop = FALSE]
    }
    # Time x Trial (collapse) x Electrode -> Time x Electrode
    data_over_time_per_elec <- ravetools::collapse(data, keep = c(1, 3), average = TRUE)
    nchanns <- ncol(data_over_time_per_elec)
    data_over_time_per_elec <- t(data_over_time_per_elec)
    ntimepts <- sum(time_sel)

    re <- data.frame(
      Subject = subject_code,
      Electrode = rep(electrode_table$Electrode, ntimepts),
      Time = rep(time_subset, each = nchanns)
    )
    re[[sprintf("Analysis%dPower", group_item$group_id)]] <- as.vector(data_over_time_per_elec)
    re
  })
  viewer_data <- dipsaus::drop_nulls(viewer_data)
  # is a list of tables, each table represents a valid group
  viewer_data
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
  names(data) <- NULL
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
  dpss_tapers <- multitaper::dpss(winsize_samples, num_tapers, time_bandwidth, returnEigenvalues=TRUE)
  dpss_eigen = dpss_tapers$eigen
  dpss_tapers = dpss_tapers$v

  # pre-compute weights
  if(weighting == 'eigen'){
    wt = dpss_eigen / num_tapers;
  } else if(weighting == 'unity'){
    wt = pracma::ones(num_tapers,1) / num_tapers;
  } else{
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

parse_electrodes <- function(load_electrodes) {
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
  if(anyNA( data_segment )) {
    data_segment[is.na(data_segment)] <- 0
  }
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
electrode_powertime <- function(heatmapbetacol, subject_code, freq_list) {

  powertime_freq_list <- vector("list", length(freq_list))

  for (i in seq_along(freq_list)) {
    data <- as.data.frame(heatmapbetacol[[i]])
    stimes <- data$stimes
    data <- data[ ,-1]
    elecnum <- colnames(data)

    Hmap <- as.matrix(data)

    heatmap_data <- expand.grid(Time = stimes, Electrode = elecnum)
    heatmap_data$Value <- c(Hmap)

    #Normalize value between 0 and 1
    heatmap_data$Value <- (heatmap_data$Value - min(heatmap_data$Value)) / (max(heatmap_data$Value) - min(heatmap_data$Value))



    #Create data sheet for YAEL ----
    subject_name <- subject_code

    # Create a new data frame with the desired structure
    YAEL_data <- data.frame(
      Subject = rep(subject_name, nrow(heatmap_data)),
      Electrode = heatmap_data$Electrode,
      Time = heatmap_data$Time,
      HeatmapValue = heatmap_data$Value
    )

    # save to list
    powertime_freq_list[[i]] <- YAEL_data
  }
  return(powertime_freq_list)
}

