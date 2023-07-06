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
if(dir.exists("py/")) {
  try({
    library(rpymat)
    rpymat::ensure_rpymat(verbose = FALSE)
    lapply(sort(
      list.files("py/", ignore.case = TRUE, 
                 pattern = "^shared-.*\\.py", 
                 full.names = TRUE)), 
      function(f) {
        f <- normalizePath(f, mustWork = TRUE)
        rpymat::run_script(f, work_dir = basename(f), local = FALSE, convert = FALSE)
      })
  })
}
targets::tar_option_set(envir = ._._env_._.)
rm(._._env_._.)
...targets <- list(`__Check_settings_file` = targets::tar_target_raw("settings_path", 
    "settings.yaml", format = "file"), `__Load_settings` = targets::tar_target_raw("settings", 
    quote({
        yaml::read_yaml(settings_path)
    }), deps = "settings_path", cue = targets::tar_cue("always")), 
    input_subject_code = targets::tar_target_raw("subject_code", 
        quote({
            settings[["subject_code"]]
        }), deps = "settings"), input_image_path = targets::tar_target_raw("image_path", 
        quote({
            settings[["image_path"]]
        }), deps = "settings"), input_resample = targets::tar_target_raw("resample", 
        quote({
            settings[["resample"]]
        }), deps = "settings"), input_calculate_cortical_thickness = targets::tar_target_raw("calculate_cortical_thickness", 
        quote({
            settings[["calculate_cortical_thickness"]]
        }), deps = "settings"), get_subject_instance = targets::tar_target_raw(name = "subject", 
        command = quote({
            .__target_expr__. <- quote({
                subject <- raveio::as_rave_subject(subject_id = sprintf("demo1/%s", 
                  subject_code), strict = FALSE)
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
                  subject <- raveio::as_rave_subject(subject_id = sprintf("demo1/%s", 
                    subject_code), strict = FALSE)
                }
                subject
            }), target_depends = "subject_code"), deps = "subject_code", 
        cue = targets::tar_cue("thorough"), pattern = NULL, iteration = "list"), 
    collect_paths = targets::tar_target_raw(name = "image_path_normalized", 
        command = quote({
            .__target_expr__. <- quote({
                image_path_normalized <- normalizePath(image_path, 
                  mustWork = TRUE)
            })
            tryCatch({
                eval(.__target_expr__.)
                return(image_path_normalized)
            }, error = function(e) {
                asNamespace("raveio")$resolve_pipeline_error(name = "image_path_normalized", 
                  condition = e, expr = .__target_expr__.)
            })
        }), format = asNamespace("raveio")$target_format_dynamic(name = NULL, 
            target_export = "image_path_normalized", target_expr = quote({
                {
                  image_path_normalized <- normalizePath(image_path, 
                    mustWork = TRUE)
                }
                image_path_normalized
            }), target_depends = "image_path"), deps = "image_path", 
        cue = targets::tar_cue("always"), pattern = NULL, iteration = "list"), 
    load_original_image = targets::tar_target_raw(name = "image_original", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "image_original.py")
            writeLines(text = paste("image_original = ants.image_read(image_path_normalized)", 
                collapse = "\n"), con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in "image_path_normalized") {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "image_original"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = "image_path_normalized", cue = targets::tar_cue("always"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "image_original")), resample_image = targets::tar_target_raw(name = "image_resampled", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "image_resampled.py")
            writeLines(text = paste(c("image_resampled = image_original", 
            "if resample == True:", "  image_resampled = ants.resample_image(", 
            "    image = image_original, ", "    resample_params = (256,256,256), ", 
            "    use_voxels = True, ", "    interp_type = 4", 
            "  )"), collapse = "\n"), con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in c("resample", "image_original")) {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "image_resampled"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = c("resample", "image_original"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "image_resampled")), register_to_template = targets::tar_target_raw(name = "transforms", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "transforms.py")
            writeLines(text = paste(c("transforms = antspynet.preprocess_brain_image(", 
            "  image_resampled,", "  truncate_intensity = (0.01, 0.99),", 
            "  brain_extraction_modality = \"t1\",", "  template = \"croppedMni152\",", 
            "  template_transform_type = \"antsRegistrationSyNRepro[a]\",", 
            "  do_bias_correction = True,", "  do_denoising = True", 
            ")", "skull_strip_template = transforms['preprocessed_image'].clone()", 
            "skull_strip_template[transforms['brain_mask'] == 0] = 0", 
            "transforms[\"skull_strip\"] = skull_strip_template"
            ), collapse = "\n"), con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in "image_resampled") {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "transforms"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = "image_resampled", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "transforms")), normalize_brain = targets::tar_target_raw(name = "normalized", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "normalized.py")
            writeLines(text = paste(c("normalized = ants.apply_transforms(", 
            "  fixed = image_resampled,", "  moving = transforms['preprocessed_image'],", 
            "  transformlist = transforms['template_transforms']['invtransforms'],", 
            "  whichtoinvert = [True], interpolator=\"linear\", verbose = True)"
            ), collapse = "\n"), con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in c("image_resampled", "transforms")) {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "normalized"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = c("image_resampled", "transforms"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "normalized")), generate_brain_mask = targets::tar_target_raw(name = "brain_mask", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "brain_mask.py")
            writeLines(text = paste(c("brain_mask = ants.apply_transforms(", 
            "  fixed = image_resampled,", "  moving = transforms['brain_mask'],", 
            "  transformlist=transforms['template_transforms']['invtransforms'],", 
            "  whichtoinvert = [True], interpolator=\"linear\", verbose = True)"
            ), collapse = "\n"), con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in c("image_resampled", "transforms")) {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "brain_mask"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = c("image_resampled", "transforms"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "brain_mask")), strip_skull = targets::tar_target_raw(name = "skull_strip", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "skull_strip.py")
            writeLines(text = paste(c("skull_strip = normalized.clone()", 
            "skull_strip[brain_mask == 0] = 0"), collapse = "\n"), 
                con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in c("normalized", "brain_mask")) {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "skull_strip"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = c("normalized", "brain_mask"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "skull_strip")), segmentation_using_Atropos = targets::tar_target_raw(name = "atropos_template", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "atropos_template.py")
            writeLines(text = paste(c("atropos_template = antspynet.deep_atropos(", 
            "  t1 = transforms[\"skull_strip\"],", "  do_preprocessing = False, ", 
            "  use_spatial_priors = True, ", "  verbose = True)"
            ), collapse = "\n"), con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in "transforms") {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "atropos_template"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = "transforms", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "atropos_template")), obtain_Atropos_on_native_brain = targets::tar_target_raw(name = "atropos_native", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "atropos_native.py")
            writeLines(text = paste(c("transform_list = transforms['template_transforms']['invtransforms']", 
            "atropos_native = {}", "atropos_native['segmentation_image'] = ants.apply_transforms(", 
            "  fixed = image_resampled,", "  moving = atropos_template['segmentation_image'],", 
            "  transformlist = transform_list,", "  whichtoinvert = [True], interpolator=\"nearestNeighbor\", verbose = False)", 
            "", "probability_images = []", "atropos_native['probability_images'] = probability_images", 
            "for img in atropos_template['probability_images']:", 
            "  probability_images.append(ants.apply_transforms(", 
            "    fixed = image_resampled,", "    moving = img,", 
            "    transformlist = transform_list,", "    whichtoinvert = [True], interpolator=\"linear\", verbose = False", 
            "  ))"), collapse = "\n"), con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in c("image_resampled", "atropos_template", 
            "transforms")) {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "atropos_native"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = c("image_resampled", "atropos_template", "transforms"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "atropos_native")), `segmentation_using_Desikan-Killiang-Tourville_labeling` = targets::tar_target_raw(name = "DKTatlas_template", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "DKTatlas_template.py")
            writeLines(text = paste(c("DKTatlas_template = antspynet.desikan_killiany_tourville_labeling(", 
            "  t1 = transforms['skull_strip'],", "  do_preprocessing = False, ", 
            "  return_probability_images = False,", "  do_lobar_parcellation = False, ", 
            "  verbose = True ", ")"), collapse = "\n"), con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in "transforms") {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "DKTatlas_template"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = "transforms", cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "DKTatlas_template")), obtain_DKT_on_native_brain = targets::tar_target_raw(name = "DKTatlas_native", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "DKTatlas_native.py")
            writeLines(text = paste(c("DKTatlas_native = ants.apply_transforms(", 
            "  fixed = image_resampled,", "  moving = DKTatlas_template,", 
            "  transformlist=transforms['template_transforms']['invtransforms'],", 
            "  whichtoinvert = [True], interpolator=\"nearestNeighbor\", verbose = True)"
            ), collapse = "\n"), con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in c("transforms", "DKTatlas_template", "image_resampled"
            )) {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "DKTatlas_native"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = c("transforms", "DKTatlas_template", "image_resampled"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "DKTatlas_native")), cortical_thickness_using_Kelly_Kapowski = targets::tar_target_raw(name = "cortical_thickness", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "cortical_thickness.py")
            writeLines(text = paste(c("cortical_thickness = None", 
            "", "if calculate_cortical_thickness:", "  # https://www.medrxiv.org/content/10.1101/2020.10.19.20215392v1.full", 
            "  kk_segmentation = ants.image_clone(atropos_native['segmentation_image'])", 
            "  kk_segmentation[kk_segmentation == 4] = 3", "  gray_matter = atropos_native['probability_images'][2]", 
            "  white_matter = (", "    atropos_native['probability_images'][3] + ", 
            "    atropos_native['probability_images'][4]", "  )", 
            "  cortical_thickness = ants.kelly_kapowski(", "    s=kk_segmentation, g=gray_matter, w=white_matter,", 
            "    its=45, r=0.025, m=1.5, x=0, verbose=1)"), collapse = "\n"), 
                con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in c("atropos_native", "calculate_cortical_thickness"
            )) {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "cortical_thickness"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = c("atropos_native", "calculate_cortical_thickness"
        ), cue = targets::tar_cue("thorough"), pattern = NULL, 
        iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "cortical_thickness")), DKT_propagate_through_cortex = targets::tar_target_raw(name = "DKT_propagated", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "DKT_propagated.py")
            writeLines(text = paste(c("DKT_propagated = None", 
            "", "if calculate_cortical_thickness:", "  dtk_cortical_mask = ants.threshold_image(", 
            "    image=DKTatlas_native, low_thresh=1000, ", "    high_thresh=3000, inval=1, outval=0)", 
            "  ", "  dtk = dtk_cortical_mask * DKTatlas_native", 
            "  ", "  kk_mask = ants.threshold_image(", "    image=cortical_thickness, low_thresh=0,", 
            "    high_thresh = 0, inval = 0, outval = 1)", "  ", 
            "  DKT_propagated = ants.iMath(kk_mask, \"PropagateLabelsThroughMask\", kk_mask * dtk)"
            ), collapse = "\n"), con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in c("DKTatlas_native", "cortical_thickness", 
            "calculate_cortical_thickness")) {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "DKT_propagated"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = c("DKTatlas_native", "cortical_thickness", 
        "calculate_cortical_thickness"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "DKT_propagated")), get_average_regional_thickness_values = targets::tar_target_raw(name = "cortical_thickness_regional_stats", 
        command = quote({
            script_dir <- file.path(targets::tar_config_get("store"), 
                "_pyscripts")
            if (!dir.exists(script_dir)) {
                script_dir <- dir_create2(script_dir)
            }
            script_path <- file.path(script_dir, "cortical_thickness_regional_stats.py")
            writeLines(text = paste(c("cortical_thickness_regional_stats = None", 
            "", "if calculate_cortical_thickness:", "  cortical_thickness_regional_stats = ants.label_stats(", 
            "    cortical_thickness, DKT_propagated)"), collapse = "\n"), 
                con = script_path)
            rpymat::ensure_rpymat(verbose = FALSE)
            py <- reticulate::py
            for (nm in c("calculate_cortical_thickness", "DKT_propagated", 
            "cortical_thickness")) {
                py[[nm]] <- get(nm)
            }
            re <- rpymat::run_script(script_path, local = TRUE, 
                convert = FALSE)
            target_name <- "cortical_thickness_regional_stats"
            if (!target_name %in% names(re)) {
                stop(sprintf("Cannot find target name [%s] in Python object", 
                  target_name))
            }
            return(re[[target_name]])
        }), deps = c("calculate_cortical_thickness", "DKT_propagated", 
        "cortical_thickness"), cue = targets::tar_cue("thorough"), 
        pattern = NULL, iteration = "list", format = asNamespace("raveio")$target_format_dynamic("user-defined-python", 
            target_export = "cortical_thickness_regional_stats")))
