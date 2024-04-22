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
    input_SOZ_elec = targets::tar_target_raw("SOZ_elec", quote({
        settings[["SOZ_elec"]]
    }), deps = "settings"), input_resect_elec = targets::tar_target_raw("resect_elec", 
        quote({
            settings[["resect_elec"]]
        }), deps = "settings"), input_plot_resect_elec = targets::tar_target_raw("plot_resect_elec", 
        quote({
            settings[["plot_resect_elec"]]
        }), deps = "settings"), input_analysis_time_frequencies = targets::tar_target_raw("analysis_time_frequencies", 
        quote({
            settings[["analysis_time_frequencies"]]
        }), deps = "settings"), input_condition = targets::tar_target_raw("condition", 
        quote({
            settings[["condition"]]
        }), deps = "settings"), input_reference = targets::tar_target_raw("reference", 
        quote({
            settings[["reference"]]
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
        }), deps = "settings"), input_selected_electrode = targets::tar_target_raw("selected_electrode", 
        quote({
            settings[["selected_electrode"]]
        }), deps = "settings"), input_epoch_file_name = targets::tar_target_raw("epoch_file_name", 
        quote({
            settings[["epoch_file_name"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
        }), deps = "settings"), input_soz_electrodes = targets::tar_target_raw("soz_electrodes", 
        quote({
            settings[["soz_electrodes"]]
        }), deps = "settings"), input_resect_electrodes = targets::tar_target_raw("resect_electrodes", 
        quote({
            settings[["resect_electrodes"]]
        }), deps = "settings"), input_heatmap_name_type = targets::tar_target_raw("heatmap_name_type", 
        quote({
            settings[["heatmap_name_type"]]
        }), deps = "settings"), input_ordered = targets::tar_target_raw("ordered", 
        quote({
            settings[["ordered"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_epoch_name = targets::tar_target_raw("epoch_name", 
        quote({
            settings[["epoch_name"]]
        }), deps = "settings"), input_electrodes = targets::tar_target_raw("electrodes", 
        quote({
            settings[["electrodes"]]
        }), deps = "settings"), input_time_windows = targets::tar_target_raw("time_windows", 
        quote({
            settings[["time_windows"]]
        }), deps = "settings"), input_time_stat_end = targets::tar_target_raw("time_stat_end", 
        quote({
            settings[["time_stat_end"]]
        }), deps = "settings"), input_time_stat_start = targets::tar_target_raw("time_stat_start", 
        quote({
            settings[["time_stat_start"]]
        }), deps = "settings"), input_analysis_windows = targets::tar_target_raw("analysis_windows", 
        quote({
            settings[["analysis_windows"]]
        }), deps = "settings"), input_plot_SOZ_elec = targets::tar_target_raw("plot_SOZ_elec", 
        quote({
            settings[["plot_SOZ_elec"]]
        }), deps = "settings"), input_label = targets::tar_target_raw("label", 
        quote({
            settings[["label"]]
        }), deps = "settings"), input_organize_top = targets::tar_target_raw("organize_top", 
        quote({
            settings[["organize_top"]]
        }), deps = "settings"), input_text_size = targets::tar_target_raw("text_size", 
        quote({
            settings[["text_size"]]
        }), deps = "settings"), input_decibal = targets::tar_target_raw("decibal", 
        quote({
            settings[["decibal"]]
        }), deps = "settings"), input_baseline = targets::tar_target_raw("baseline", 
        quote({
            settings[["baseline"]]
        }), deps = "settings"), input_scale = targets::tar_target_raw("scale", 
        quote({
            settings[["scale"]]
        }), deps = "settings"), input_baselined = targets::tar_target_raw("baselined", 
        quote({
            settings[["baselined"]]
        }), deps = "settings"), input_start_time_baseline = targets::tar_target_raw("start_time_baseline", 
        quote({
            settings[["start_time_baseline"]]
        }), deps = "settings"), input_end_time_baseline = targets::tar_target_raw("end_time_baseline", 
        quote({
            settings[["end_time_baseline"]]
        }), deps = "settings"), input_threshold = targets::tar_target_raw("threshold", 
        quote({
            settings[["threshold"]]
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
        pattern = NULL, iteration = "list"), generate_signal_plot = targets::tar_target_raw(name = "DC_matrix", 
        command = quote({
            .__target_expr__. <- quote({
                DC_matrix <- plot_DC_shift(repository, load_electrodes = load_electrodes, 
                  subject = subject, condition = condition, time_windows = time_window, 
                  reference = reference_name, soz_electrodes = soz_electrodes, 
                  resect_electrodes = resect_electrodes, ordered = ordered, 
                  name_type = heatmap_name_type, baseline_end = end_time_baseline, 
                  condition_baseline = baseline)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(DC_matrix)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "DC_matrix", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "DC_matrix", target_expr = quote({
                {
                  DC_matrix <- plot_DC_shift(repository, load_electrodes = load_electrodes, 
                    subject = subject, condition = condition, 
                    time_windows = time_window, reference = reference_name, 
                    soz_electrodes = soz_electrodes, resect_electrodes = resect_electrodes, 
                    ordered = ordered, name_type = heatmap_name_type, 
                    baseline_end = end_time_baseline, condition_baseline = baseline)
                }
                DC_matrix
            }), target_depends = c("repository", "load_electrodes", 
            "subject", "condition", "time_window", "reference_name", 
            "soz_electrodes", "resect_electrodes", "ordered", 
            "heatmap_name_type", "end_time_baseline", "baseline"
            )), deps = c("repository", "load_electrodes", "subject", 
        "condition", "time_window", "reference_name", "soz_electrodes", 
        "resect_electrodes", "ordered", "heatmap_name_type", 
        "end_time_baseline", "baseline"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"), get_HFO = targets::tar_target_raw(name = "HFO_matrix", 
        command = quote({
            .__target_expr__. <- quote({
                HFO_matrix <- HFO_matrix(load_electrodes, SOZ_elec, 
                  resect_elec, repository, condition, time_window, 
                  label, threshold)
                print(HFO_matrix)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(HFO_matrix)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "HFO_matrix", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "HFO_matrix", target_expr = quote({
                {
                  HFO_matrix <- HFO_matrix(load_electrodes, SOZ_elec, 
                    resect_elec, repository, condition, time_window, 
                    label, threshold)
                  print(HFO_matrix)
                }
                HFO_matrix
            }), target_depends = c("HFO_matrix", "load_electrodes", 
            "SOZ_elec", "resect_elec", "repository", "condition", 
            "time_window", "label", "threshold")), deps = c("HFO_matrix", 
        "load_electrodes", "SOZ_elec", "resect_elec", "repository", 
        "condition", "time_window", "label", "threshold"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
