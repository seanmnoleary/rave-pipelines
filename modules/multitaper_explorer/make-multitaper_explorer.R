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
    input_condition = targets::tar_target_raw("condition", quote({
        settings[["condition"]]
    }), deps = "settings"), input_freq_list = targets::tar_target_raw("freq_list", 
        quote({
            settings[["freq_list"]]
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
        }), deps = "settings"), input_baseline = targets::tar_target_raw("baseline", 
        quote({
            settings[["baseline"]]
        }), deps = "settings"), input_baseline_method = targets::tar_target_raw("baseline_method", 
        quote({
            settings[["baseline_method"]]
        }), deps = "settings"), input_window_params = targets::tar_target_raw("window_params", 
        quote({
            settings[["window_params"]]
        }), deps = "settings"), input_sampling_frequency = targets::tar_target_raw("sampling_frequency", 
        quote({
            settings[["sampling_frequency"]]
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
        }), deps = "settings"), input_epoch = targets::tar_target_raw("epoch", 
        quote({
            settings[["epoch"]]
        }), deps = "settings"), input_time_window = targets::tar_target_raw("time_window", 
        quote({
            settings[["time_window"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name", 
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_load_electrodes = targets::tar_target_raw("load_electrodes", 
        quote({
            settings[["load_electrodes"]]
        }), deps = "settings"), input_epoch_file_name = targets::tar_target_raw("epoch_file_name", 
        quote({
            settings[["epoch_file_name"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
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
                  multitaper_result, time_window, freq_list, 
                  load_electrodes, window_params, condition)
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
                    multitaper_result, time_window, freq_list, 
                    load_electrodes, window_params, condition)
                }
                heatmap_result
            }), target_depends = c("repository", "multitaper_result", 
            "time_window", "freq_list", "load_electrodes", "window_params", 
            "condition")), deps = c("repository", "multitaper_result", 
        "time_window", "freq_list", "load_electrodes", "window_params", 
        "condition"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), electrode_powertime = targets::tar_target_raw(name = "YAEL_data", 
        command = quote({
            .__target_expr__. <- quote({
                YAEL_data <- electrode_powertime(heatmap_result, 
                  subject_code = subject_code, freq_list)
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
                    subject_code = subject_code, freq_list)
                }
                YAEL_data
            }), target_depends = c("heatmap_result", "subject_code", 
            "freq_list")), deps = c("heatmap_result", "subject_code", 
        "freq_list"), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"))
