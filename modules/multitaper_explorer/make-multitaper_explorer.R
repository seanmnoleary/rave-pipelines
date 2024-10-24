library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
._._env_._. <- environment()
lapply(sort(list.files(
  "R/", ignore.case = TRUE,
  pattern = "^shared-.*\\.R", 
  full.names = TRUE
)), function(f) {
  source(f, local = ._._env_._., chdir = TRUE)
})
targets::tar_option_set(envir = ._._env_._.)
rm(._._env_._.)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_time_stat_start = targets::tar_target_raw("time_stat_start", 
        quote({
            settings[["time_stat_start"]]
        }), deps = "settings"), input_condition = targets::tar_target_raw("condition", 
        quote({
            settings[["condition"]]
        }), deps = "settings"), input_save_pipeline = targets::tar_target_raw("save_pipeline", 
        quote({
            settings[["save_pipeline"]]
        }), deps = "settings"), input_reference = targets::tar_target_raw("reference", 
        quote({
            settings[["reference"]]
        }), deps = "settings"), input_intervals = targets::tar_target_raw("intervals", 
        quote({
            settings[["intervals"]]
        }), deps = "settings"), input_type = targets::tar_target_raw("type", 
        quote({
            settings[["type"]]
        }), deps = "settings"), input_window_params = targets::tar_target_raw("window_params", 
        quote({
            settings[["window_params"]]
        }), deps = "settings"), input_frequency_range = targets::tar_target_raw("frequency_range", 
        quote({
            settings[["frequency_range"]]
        }), deps = "settings"), input_num_tapers = targets::tar_target_raw("num_tapers", 
        quote({
            settings[["num_tapers"]]
        }), deps = "settings"), input_min_nfft = targets::tar_target_raw("min_nfft", 
        quote({
            settings[["min_nfft"]]
        }), deps = "settings"), input_weighting = targets::tar_target_raw("weighting", 
        quote({
            settings[["weighting"]]
        }), deps = "settings"), input_detrend_opt = targets::tar_target_raw("detrend_opt", 
        quote({
            settings[["detrend_opt"]]
        }), deps = "settings"), input_parallel = targets::tar_target_raw("parallel", 
        quote({
            settings[["parallel"]]
        }), deps = "settings"), input_num_workers = targets::tar_target_raw("num_workers", 
        quote({
            settings[["num_workers"]]
        }), deps = "settings"), input_plot_on = targets::tar_target_raw("plot_on", 
        quote({
            settings[["plot_on"]]
        }), deps = "settings"), input_verbose = targets::tar_target_raw("verbose", 
        quote({
            settings[["verbose"]]
        }), deps = "settings"), input_xyflip = targets::tar_target_raw("xyflip", 
        quote({
            settings[["xyflip"]]
        }), deps = "settings"), input_time_bandwidth = targets::tar_target_raw("time_bandwidth", 
        quote({
            settings[["time_bandwidth"]]
        }), deps = "settings"), input_time_window = targets::tar_target_raw("time_window", 
        quote({
            settings[["time_window"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name", 
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_load_electrodes = targets::tar_target_raw("load_electrodes", 
        quote({
            settings[["load_electrodes"]]
        }), deps = "settings"), input_SOZ_elec = targets::tar_target_raw("SOZ_elec", 
        quote({
            settings[["SOZ_elec"]]
        }), deps = "settings"), input_plot_SOZ_elec = targets::tar_target_raw("plot_SOZ_elec", 
        quote({
            settings[["plot_SOZ_elec"]]
        }), deps = "settings"), input_epoch_file_name = targets::tar_target_raw("epoch_file_name", 
        quote({
            settings[["epoch_file_name"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_label = targets::tar_target_raw("label", 
        quote({
            settings[["label"]]
        }), deps = "settings"), input_organize_top = targets::tar_target_raw("organize_top", 
        quote({
            settings[["organize_top"]]
        }), deps = "settings"), input_analysis_windows = targets::tar_target_raw("analysis_windows", 
        quote({
            settings[["analysis_windows"]]
        }), deps = "settings"), input_text_size = targets::tar_target_raw("text_size", 
        quote({
            settings[["text_size"]]
        }), deps = "settings"), input_resect_elec = targets::tar_target_raw("resect_elec", 
        quote({
            settings[["resect_elec"]]
        }), deps = "settings"), input_plot_resect_elec = targets::tar_target_raw("plot_resect_elec", 
        quote({
            settings[["plot_resect_elec"]]
        }), deps = "settings"), input_normalize = targets::tar_target_raw("normalize", 
        quote({
            settings[["normalize"]]
        }), deps = "settings"), input_threshold_level = targets::tar_target_raw("threshold_level", 
        quote({
            settings[["threshold_level"]]
        }), deps = "settings"), input_threshold_type = targets::tar_target_raw("threshold_type", 
        quote({
            settings[["threshold_type"]]
        }), deps = "settings"), input_time_stat_end = targets::tar_target_raw("time_stat_end", 
        quote({
            settings[["time_stat_end"]]
        }), deps = "settings"), load_subject = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- raveio::RAVESubject$new(project_name = project_name, 
                  subject_code = subject_code)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(subject)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "subject", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave-subject", 
            target_export = "subject", target_expr = quote({
                {
                  subject <- raveio::RAVESubject$new(project_name = project_name, 
                    subject_code = subject_code)
                }
                subject
            }), target_depends = c("project_name", "subject_code"
            )), deps = c("project_name", "subject_code"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), load_voltage = targets::tar_target_raw(name = "repository", 
        command = quote({
            .__target_expr__. <- quote({
                repository <- raveio::prepare_subject_voltage_with_epoch(subject = subject, 
                  epoch_name = epoch_file_name, electrodes = load_electrodes, 
                  time_windows = time_window, reference = reference_name)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(repository)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "repository", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave_prepare_subject_voltage_with_epoch", 
            target_export = "repository", target_expr = quote({
                {
                  repository <- raveio::prepare_subject_voltage_with_epoch(subject = subject, 
                    epoch_name = epoch_file_name, electrodes = load_electrodes, 
                    time_windows = time_window, reference = reference_name)
                }
                repository
            }), target_depends = c("subject", "epoch_file_name", 
            "load_electrodes", "time_window", "reference_name"
            )), deps = c("subject", "epoch_file_name", "load_electrodes", 
        "time_window", "reference_name"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), find_multitaper = targets::tar_target_raw(name = "multitaper_result", 
        command = quote({
            .__target_expr__. <- quote({
                multitaper_result <- generate_multitaper(repository, 
                  load_electrodes, frequency_range, time_bandwidth, 
                  num_tapers, window_params, min_nfft, weighting, 
                  detrend_opt, parallel)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(multitaper_result)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "multitaper_result", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "multitaper_result", target_expr = quote({
                {
                  multitaper_result <- generate_multitaper(repository, 
                    load_electrodes, frequency_range, time_bandwidth, 
                    num_tapers, window_params, min_nfft, weighting, 
                    detrend_opt, parallel)
                }
                multitaper_result
            }), target_depends = c("repository", "load_electrodes", 
            "frequency_range", "time_bandwidth", "num_tapers", 
            "window_params", "min_nfft", "weighting", "detrend_opt", 
            "parallel")), deps = c("repository", "load_electrodes", 
        "frequency_range", "time_bandwidth", "num_tapers", "window_params", 
        "min_nfft", "weighting", "detrend_opt", "parallel"), 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    find_heatmap = targets::tar_target_raw(name = "heatmap_result", 
        command = quote({
            .__target_expr__. <- quote({
                heatmap_result <- generate_heatmap(repository, 
                  multitaper_result, time_window, analysis_windows, 
                  load_electrodes, window_params, condition, 
                  label, normalize)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(heatmap_result)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "heatmap_result", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "heatmap_result", target_expr = quote({
                {
                  heatmap_result <- generate_heatmap(repository, 
                    multitaper_result, time_window, analysis_windows, 
                    load_electrodes, window_params, condition, 
                    label, normalize)
                }
                heatmap_result
            }), target_depends = c("repository", "multitaper_result", 
            "time_window", "analysis_windows", "load_electrodes", 
            "window_params", "condition", "label", "normalize"
            )), deps = c("repository", "multitaper_result", "time_window", 
        "analysis_windows", "load_electrodes", "window_params", 
        "condition", "label", "normalize"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), electrode_powertime = targets::tar_target_raw(name = "YAEL_data", 
        command = quote({
            .__target_expr__. <- quote({
                YAEL_data <- electrode_powertime(heatmap_result, 
                  subject_code = subject_code, analysis_windows = analysis_windows, 
                  SOZ_elec = SOZ_elec, resect_elec = resect_elec, 
                  load_electrodes = load_electrodes)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(YAEL_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "YAEL_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "YAEL_data", target_expr = quote({
                {
                  YAEL_data <- electrode_powertime(heatmap_result, 
                    subject_code = subject_code, analysis_windows = analysis_windows, 
                    SOZ_elec = SOZ_elec, resect_elec = resect_elec, 
                    load_electrodes = load_electrodes)
                }
                YAEL_data
            }), target_depends = c("heatmap_result", "subject_code", 
            "analysis_windows", "SOZ_elec", "resect_elec", "load_electrodes"
            )), deps = c("heatmap_result", "subject_code", "analysis_windows", 
        "SOZ_elec", "resect_elec", "load_electrodes"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), analyze_score = targets::tar_target_raw(name = "analysis_data", 
        command = quote({
            .__target_expr__. <- quote({
                for (i in 1:length(heatmap_result)) {
                  analysis_data <- analyze_score(heatmap_result[[i]], 
                    repository, window_params, time_stat_start, 
                    time_stat_end, threshold_type = threshold_type, 
                    threshold_level = threshold_level)
                  cat("Frequency: ", analysis_windows[[i]]$frequency_range[1], 
                    " - ", analysis_windows[[i]]$frequency_range[2], 
                    "\n")
                  for (j in 1:length(analysis_data$Start)) {
                    if (!is.na(analysis_data$Start[j])) {
                      cat("Electrode:", analysis_data$Electrodes[j], 
                        "Start:", analysis_data$Start[j], "Length:", 
                        analysis_data$Length[j], "\n")
                    }
                  }
                }
            })
            tryCatch({
                eval(.__target_expr__.)
                return(analysis_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "analysis_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "analysis_data", target_expr = quote({
                {
                  for (i in 1:length(heatmap_result)) {
                    analysis_data <- analyze_score(heatmap_result[[i]], 
                      repository, window_params, time_stat_start, 
                      time_stat_end, threshold_type = threshold_type, 
                      threshold_level = threshold_level)
                    cat("Frequency: ", analysis_windows[[i]]$frequency_range[1], 
                      " - ", analysis_windows[[i]]$frequency_range[2], 
                      "\n")
                    for (j in 1:length(analysis_data$Start)) {
                      if (!is.na(analysis_data$Start[j])) {
                        cat("Electrode:", analysis_data$Electrodes[j], 
                          "Start:", analysis_data$Start[j], "Length:", 
                          analysis_data$Length[j], "\n")
                      }
                    }
                  }
                }
                analysis_data
            }), target_depends = c("heatmap_result", "repository", 
            "window_params", "time_stat_start", "time_stat_end", 
            "threshold_type", "threshold_level", "analysis_windows"
            )), deps = c("heatmap_result", "repository", "window_params", 
        "time_stat_start", "time_stat_end", "threshold_type", 
        "threshold_level", "analysis_windows"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
