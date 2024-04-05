library(targets)
library(raveio)
source("common.R", local = TRUE, chdir = TRUE)
._._env_._. <- environment()
._._env_._.$pipeline <- pipeline_from_path(".")
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
    input_time_windows = targets::tar_target_raw("time_windows", 
        quote({
            settings[["time_windows"]]
        }), deps = "settings"), input_electrodes = targets::tar_target_raw("electrodes", 
        quote({
            settings[["electrodes"]]
        }), deps = "settings"), input_epoch_name = targets::tar_target_raw("epoch_name", 
        quote({
            settings[["epoch_name"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_ordered = targets::tar_target_raw("ordered", 
        quote({
            settings[["ordered"]]
        }), deps = "settings"), input_heatmap_name_type = targets::tar_target_raw("heatmap_name_type", 
        quote({
            settings[["heatmap_name_type"]]
        }), deps = "settings"), input_resect_electrodes = targets::tar_target_raw("resect_electrodes", 
        quote({
            settings[["resect_electrodes"]]
        }), deps = "settings"), input_soz_electrodes = targets::tar_target_raw("soz_electrodes", 
        quote({
            settings[["soz_electrodes"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_epoch_file_name = targets::tar_target_raw("epoch_file_name", 
        quote({
            settings[["epoch_file_name"]]
        }), deps = "settings"), input_selected_electrode = targets::tar_target_raw("selected_electrode", 
        quote({
            settings[["selected_electrode"]]
        }), deps = "settings"), input_load_electrodes = targets::tar_target_raw("load_electrodes", 
        quote({
            settings[["load_electrodes"]]
        }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name", 
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_time_window = targets::tar_target_raw("time_window", 
        quote({
            settings[["time_window"]]
        }), deps = "settings"), input_time_bandwidth = targets::tar_target_raw("time_bandwidth", 
        quote({
            settings[["time_bandwidth"]]
        }), deps = "settings"), input_xyflip = targets::tar_target_raw("xyflip", 
        quote({
            settings[["xyflip"]]
        }), deps = "settings"), input_verbose = targets::tar_target_raw("verbose", 
        quote({
            settings[["verbose"]]
        }), deps = "settings"), input_plot_on = targets::tar_target_raw("plot_on", 
        quote({
            settings[["plot_on"]]
        }), deps = "settings"), input_num_workers = targets::tar_target_raw("num_workers", 
        quote({
            settings[["num_workers"]]
        }), deps = "settings"), input_parallel = targets::tar_target_raw("parallel", 
        quote({
            settings[["parallel"]]
        }), deps = "settings"), input_detrend_opt = targets::tar_target_raw("detrend_opt", 
        quote({
            settings[["detrend_opt"]]
        }), deps = "settings"), input_weighting = targets::tar_target_raw("weighting", 
        quote({
            settings[["weighting"]]
        }), deps = "settings"), input_min_nfft = targets::tar_target_raw("min_nfft", 
        quote({
            settings[["min_nfft"]]
        }), deps = "settings"), input_num_tapers = targets::tar_target_raw("num_tapers", 
        quote({
            settings[["num_tapers"]]
        }), deps = "settings"), input_frequency_range = targets::tar_target_raw("frequency_range", 
        quote({
            settings[["frequency_range"]]
        }), deps = "settings"), input_window_params = targets::tar_target_raw("window_params", 
        quote({
            settings[["window_params"]]
        }), deps = "settings"), input_reference = targets::tar_target_raw("reference", 
        quote({
            settings[["reference"]]
        }), deps = "settings"), input_condition = targets::tar_target_raw("condition", 
        quote({
            settings[["condition"]]
        }), deps = "settings"), input_analysis_time_frequencies = targets::tar_target_raw("analysis_time_frequencies", 
        quote({
            settings[["analysis_time_frequencies"]]
        }), deps = "settings"), input_plot_resect_elec = targets::tar_target_raw("plot_resect_elec", 
        quote({
            settings[["plot_resect_elec"]]
        }), deps = "settings"), input_resect_elec = targets::tar_target_raw("resect_elec", 
        quote({
            settings[["resect_elec"]]
        }), deps = "settings"), input_SOZ_elec = targets::tar_target_raw("SOZ_elec", 
        quote({
            settings[["SOZ_elec"]]
        }), deps = "settings"), input_text_size = targets::tar_target_raw("text_size", 
        quote({
            settings[["text_size"]]
        }), deps = "settings"), input_organize_top = targets::tar_target_raw("organize_top", 
        quote({
            settings[["organize_top"]]
        }), deps = "settings"), input_label = targets::tar_target_raw("label", 
        quote({
            settings[["label"]]
        }), deps = "settings"), input_plot_SOZ_elec = targets::tar_target_raw("plot_SOZ_elec", 
        quote({
            settings[["plot_SOZ_elec"]]
        }), deps = "settings"), input_analysis_windows = targets::tar_target_raw("analysis_windows", 
        quote({
            settings[["analysis_windows"]]
        }), deps = "settings"), input_time_stat_start = targets::tar_target_raw("time_stat_start", 
        quote({
            settings[["time_stat_start"]]
        }), deps = "settings"), input_time_stat_end = targets::tar_target_raw("time_stat_end", 
        quote({
            settings[["time_stat_end"]]
        }), deps = "settings"), input_end_time_baseline = targets::tar_target_raw("end_time_baseline", 
        quote({
            settings[["end_time_baseline"]]
        }), deps = "settings"), input_start_time_baseline = targets::tar_target_raw("start_time_baseline", 
        quote({
            settings[["start_time_baseline"]]
        }), deps = "settings"), input_baselined = targets::tar_target_raw("baselined", 
        quote({
            settings[["baselined"]]
        }), deps = "settings"), input_scale = targets::tar_target_raw("scale", 
        quote({
            settings[["scale"]]
        }), deps = "settings"), input_baseline = targets::tar_target_raw("baseline", 
        quote({
            settings[["baseline"]]
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
        pattern = NULL, iteration = "list"), load_power = targets::tar_target_raw(name = "repository_power", 
        command = quote({
            .__target_expr__. <- quote({
                repository_power <- raveio::prepare_subject_power(subject = subject, 
                  epoch_name = epoch_file_name, electrodes = load_electrodes, 
                  time_windows = time_window, reference = reference_name)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(repository_power)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "repository_power", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "rave_prepare_subject_power", 
            target_export = "repository_power", target_expr = quote({
                {
                  repository_power <- raveio::prepare_subject_power(subject = subject, 
                    epoch_name = epoch_file_name, electrodes = load_electrodes, 
                    time_windows = time_window, reference = reference_name)
                }
                repository_power
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
                heatmap_result <- generate_power_over_time_data(multitaper_result, 
                  analysis_time_frequencies, baselined = baselined, 
                  baseline = baseline, start_time_baseline = start_time_baseline, 
                  end_time_baseline = end_time_baseline)
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
                  heatmap_result <- generate_power_over_time_data(multitaper_result, 
                    analysis_time_frequencies, baselined = baselined, 
                    baseline = baseline, start_time_baseline = start_time_baseline, 
                    end_time_baseline = end_time_baseline)
                }
                heatmap_result
            }), target_depends = c("multitaper_result", "analysis_time_frequencies", 
            "baselined", "baseline", "start_time_baseline", "end_time_baseline"
            )), deps = c("multitaper_result", "analysis_time_frequencies", 
        "baselined", "baseline", "start_time_baseline", "end_time_baseline"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list"), generate_signal_plot = targets::tar_target_raw(name = "plot_signal", 
        command = quote({
            .__target_expr__. <- quote({
                plot_signal <- plot_signal_data(repository, load_electrodes = load_electrodes, 
                  subject = subject, condition = condition, time_windows = time_window, 
                  reference = reference_name, analysis_time_frequencies = analysis_time_frequencies)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plot_signal)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plot_signal", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "plot_signal", target_expr = quote({
                {
                  plot_signal <- plot_signal_data(repository, 
                    load_electrodes = load_electrodes, subject = subject, 
                    condition = condition, time_windows = time_window, 
                    reference = reference_name, analysis_time_frequencies = analysis_time_frequencies)
                }
                plot_signal
            }), target_depends = c("repository", "load_electrodes", 
            "subject", "condition", "time_window", "reference_name", 
            "analysis_time_frequencies")), deps = c("repository", 
        "load_electrodes", "subject", "condition", "time_window", 
        "reference_name", "analysis_time_frequencies"), cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list"), generate_data_for_heatmap = targets::tar_target_raw(name = "plot_heatmap", 
        command = quote({
            .__target_expr__. <- quote({
                plot_heatmap <- plot_power_over_time_data(heatmap_result, 
                  soz_electrodes = soz_electrodes, resect_electrodes = resect_electrodes, 
                  name_type = heatmap_name_type, trial = condition, 
                  ordered = ordered)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plot_heatmap)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plot_heatmap", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "plot_heatmap", target_expr = quote({
                {
                  plot_heatmap <- plot_power_over_time_data(heatmap_result, 
                    soz_electrodes = soz_electrodes, resect_electrodes = resect_electrodes, 
                    name_type = heatmap_name_type, trial = condition, 
                    ordered = ordered)
                }
                plot_heatmap
            }), target_depends = c("heatmap_result", "soz_electrodes", 
            "resect_electrodes", "heatmap_name_type", "condition", 
            "ordered")), deps = c("heatmap_result", "soz_electrodes", 
        "resect_electrodes", "heatmap_name_type", "condition", 
        "ordered"), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), generate_data_for_lineplot = targets::tar_target_raw(name = "plot_lineplot", 
        command = quote({
            .__target_expr__. <- quote({
                plot_lineplot <- plot_power_over_time_data_line(heatmap_result, 
                  soz_electrodes = soz_electrodes, resect_electrodes = resect_electrodes, 
                  name_type = heatmap_name_type, trial = condition)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plot_lineplot)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plot_lineplot", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "plot_lineplot", target_expr = quote({
                {
                  plot_lineplot <- plot_power_over_time_data_line(heatmap_result, 
                    soz_electrodes = soz_electrodes, resect_electrodes = resect_electrodes, 
                    name_type = heatmap_name_type, trial = condition)
                }
                plot_lineplot
            }), target_depends = c("heatmap_result", "soz_electrodes", 
            "resect_electrodes", "heatmap_name_type", "condition"
            )), deps = c("heatmap_result", "soz_electrodes", 
        "resect_electrodes", "heatmap_name_type", "condition"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), generate_data_for_time_frequency_plot = targets::tar_target_raw(name = "plot_time_frequency", 
        command = quote({
            .__target_expr__. <- quote({
                plot_time_frequency <- plot_time_frequency(selected_electrode, 
                  load_electrodes, repository_power)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(plot_time_frequency)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "plot_time_frequency", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "plot_time_frequency", target_expr = quote({
                {
                  plot_time_frequency <- plot_time_frequency(selected_electrode, 
                    load_electrodes, repository_power)
                }
                plot_time_frequency
            }), target_depends = c("plot_time_frequency", "selected_electrode", 
            "load_electrodes", "repository_power")), deps = c("plot_time_frequency", 
        "selected_electrode", "load_electrodes", "repository_power"
        ), cue = targets::tar_cue("always"), pattern = NULL, 
        iteration = "list"), generate_data_for_3d_viewer = targets::tar_target_raw(name = "viewer3d_data", 
        command = quote({
            .__target_expr__. <- quote({
                viewer3d_data <- generate_3dviewer_data(heatmap_result, 
                  trial = condition)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(viewer3d_data)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "viewer3d_data", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "viewer3d_data", target_expr = quote({
                {
                  viewer3d_data <- generate_3dviewer_data(heatmap_result, 
                    trial = condition)
                }
                viewer3d_data
            }), target_depends = c("heatmap_result", "condition"
            )), deps = c("heatmap_result", "condition"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
