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
    input_condition = targets::tar_target_raw("condition", quote({
        settings[["condition"]]
    }), deps = "settings"), input_reference_name = targets::tar_target_raw("reference_name", 
        quote({
            settings[["reference_name"]]
        }), deps = "settings"), input_time_window = targets::tar_target_raw("time_window", 
        quote({
            settings[["time_window"]]
        }), deps = "settings"), input_load_electrodes = targets::tar_target_raw("load_electrodes", 
        quote({
            settings[["load_electrodes"]]
        }), deps = "settings"), input_epoch_file_name = targets::tar_target_raw("epoch_file_name", 
        quote({
            settings[["epoch_file_name"]]
        }), deps = "settings"), input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_project_name = targets::tar_target_raw("project_name", 
        quote({
            settings[["project_name"]]
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
        pattern = NULL, iteration = "list"), find_ICA = targets::tar_target_raw(name = "ica_matrix", 
        command = quote({
            .__target_expr__. <- quote({
                ica_matrix <- compute_ICA(load_electrodes, repository, 
                  time_window, condition)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(ica_matrix)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "ica_matrix", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = "filearray", 
            target_export = "ica_matrix", target_expr = quote({
                {
                  ica_matrix <- compute_ICA(load_electrodes, 
                    repository, time_window, condition)
                }
                ica_matrix
            }), target_depends = c("load_electrodes", "repository", 
            "time_window", "condition")), deps = c("load_electrodes", 
        "repository", "time_window", "condition"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list"))
