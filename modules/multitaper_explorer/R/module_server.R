
module_server <- function(input, output, session, ...){

  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)

  # Run analysis once the following input IDs are changed
  # This is used by auto-recalculation feature
  server_tools$run_analysis_onchange("condition")

  run_multitaper <- function() {
    # SOZ_string: 12-13,17-21,25-29,31,33-37,39,42-43,51-52
    # num_windows: 100
    # epoch: sz2
    # time_window:
    #   - -60

    num_workers <- input$mt_max_workers
    if( num_workers <= 1 || !isTRUE(input$mt_parallel) ) {
      num_workers <- 1
      parallel <- FALSE
    } else {
      num_workers <- min(raveio::raveio_getopt("max_worker"), input$mt_max_workers)
      parallel <- TRUE
    }

    num_tapers <- ceiling(input$mt_num_tapers)
    if(!isTRUE(num_tapers > 0)) {
      num_tapers <- NULL
    }


    pipeline$set_settings(
      window_params = c(input$mt_window_size, input$mt_step_size),
      frequency_range = c(input$mt_frequency_lower_bound, input$mt_frequency_upper_bound),
      num_tapers = num_tapers,
      min_nfft = input$mt_nfft,
      weighting = input$mt_weighting,
      detrend_opt = input$mt_detrend_opt,
      time_bandwidth = input$mt_time_bandwidth,

      parallel = parallel,
      num_workers = num_workers
    )

    local_data$results <- NULL

    ravedash::logger("Scheduled: ", pipeline$pipeline_name,
                     level = 'debug', reset_timer = TRUE)

    dipsaus::shiny_alert2(
      title = "Calculating in progress",
      text = ravedash::be_patient_text(),
      icon = "info",
      danger_mode = FALSE,
      auto_close = FALSE,
      buttons = FALSE,
      session = session
    )

    pipeline$run(
      as_promise = FALSE,
      scheduler = "none",
      type = "callr",
      callr_function = NULL,
      # shortcut = TRUE,
      names = c("multitaper_result")
    )

    # local_data$results <- pipeline$read(c("multitaper_result", "frequency_range"))
    frequency_range <- range(unlist(pipeline$get_settings('frequency_range')))
    analysis_windows <- pipeline$get_settings('analysis_windows')

    # update inputs depending on multitaper
    dipsaus::updateCompoundInput2(
      session = session,
      inputId = "analysis_settings",
      initialization = list(
        "frequency_range" = list(
          min = frequency_range[[1]],
          max = frequency_range[[2]]
        )
      ),
      value = analysis_windows,
      ncomp = length(analysis_windows)
    )

    Sys.sleep(0.5)
    ravedash::logger("Fulfilled: ", pipeline$pipeline_name,
                     level = 'debug')
    shidashi::clear_notifications(class = "pipeline-error")
    dipsaus::close_alert2()

    shidashi::card_operate(
      title = "Multitaper Parameters",
      method = "collapse"
    )
    return()
  }


  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      # set inputs first, and then decide whether to run multitaper
      label_input <- "numeric"
      if (input$hm_label == TRUE) {
        label_input <- "names"
      } else {
        label_input <- "numeric"
      }
      pipeline$set_settings(
        condition = input$condition,
        analysis_windows = input$analysis_settings,
        plot_SOZ_elec = input$hm_showSOZ,
        SOZ_elec = input$input_SOZ_electrodes,
        label = label_input,
        organize_top = input$hm_group,
        text_size = input$electrode_text_size,
        plot_resect_elec = input$hm_showResection,
        resect_elec = input$input_Resection_electrodes
      )

      local_data$analysis_settings <- input$analysis_settings
      local_data$SOZ_elec <- input$input_SOZ_electrodes
      local_data$plot_SOZ_elec <- input$hm_showSOZ
      local_data$group_SOZResect <- input$hm_group
      local_data$label <- label_input
      local_data$text_size <- input$electrode_text_size
      local_data$plot_resect_elec <- input$hm_showResection
      local_data$resect_elec <- input$input_Resection_electrodes

      output_flags <- local_reactives$update_outputs
      if(!length(output_flags) || isFALSE(output_flags) || !is.list(local_data$results)) {
        run_multitaper()
      }

      # run the rest

      local_data$results <- pipeline$run(c("YAEL_data", "heatmap_result"))
      local_reactives$update_outputs <- Sys.time()

    }, error_wrapper = "alert"),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      run_multitaper()

    }, error_wrapper = "alert"),
    input$run_multitaper,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # (Optional) check whether the loaded data is valid
  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }
      new_repository <- pipeline$read("repository")
      if(!inherits(new_repository, "rave_prepare_subject_voltage_with_epoch")){
        ravedash::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_subject_voltage_with_epoch`. Abort initialization", level = "warning")
        return()
      }
      ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

      # check if the repository has the same subject as current one
      old_repository <- component_container$data$repository
      if(inherits(old_repository, "rave_prepare_subject_voltage_with_epoch")){

        if( !attr(loaded_flag, "force") &&
            identical(old_repository$signature, new_repository$signature) ){
          ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
          return()
        }
      }

      # TODO: reset UIs to default

      # Reset preset UI & data
      component_container$reset_data()
      component_container$data$repository <- new_repository

      epoch_table <- new_repository$epoch_table
      epoch_table$Condition2 <- sprintf("%s (%s)", epoch_table$Condition, epoch_table$Trial)
      component_container$data$epoch_table <- epoch_table
      component_container$initialize_with_new_data()
      local_data$results <- NULL

      # Initialize inputs
      time_window_range <- range(unlist(new_repository$time_windows))
      nyquist_floor <- floor(new_repository$sample_rate / 2)
      frequency_range <- pipeline$get_settings('frequency_range')
      analysis_windows <- pipeline$get_settings('analysis_windows')

      shiny::updateNumericInput(
        session = session,
        inputId = 'mt_frequency_lower_bound',
        max = floor(new_repository$sample_rate / 2),
        value = min(nyquist_floor, input$mt_frequency_lower_bound)
      )
      shiny::updateNumericInput(
        session = session,
        inputId = 'mt_frequency_upper_bound',
        max = floor(new_repository$sample_rate / 2),
        value = min(nyquist_floor, input$mt_frequency_upper_bound)
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "condition",
        choices = epoch_table$Condition2,
        selected = input$condition %OF% epoch_table$Condition2
      )

      # "analysis_settings"
      dipsaus::updateCompoundInput2(
        session = session,
        inputId = "analysis_settings",
        initialization = list(
          "time_range" = list(
            min = time_window_range[[1]],
            max = time_window_range[[2]]
          ),
          "frequency_range" = list(
            min = frequency_range[[1]],
            max = frequency_range[[2]]
          )
        ),
        value = analysis_windows,
        ncomp = length(analysis_windows)
      )
      shidashi::card_operate(
        title = "Multitaper Parameters",
        method = "expand"
      )

      # Reset outputs
      local_reactives$update_outputs <- FALSE

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  # ---- Events : BEGIN ------------------------------------------
  shiny::bindEvent(
    ravedash::safe_observe({

      lower_bound <- input$mt_frequency_lower_bound
      upper_bound <- input$mt_frequency_upper_bound

      if( !length(lower_bound) || is.na(lower_bound) || lower_bound < 0.1 ) {
        shiny::updateNumericInput(
          session = session,
          input = "mt_frequency_lower_bound",
          value = 0.1
        )
        return()
      }

      repository <- component_container$data$repository
      if(is.null(repository)) { return() }
      nyquist_floor <- floor(repository$sample_rate / 2)

      if( lower_bound > nyquist_floor - 1 ) {
        shiny::updateNumericInput(
          session = session,
          input = "mt_frequency_lower_bound",
          value = nyquist_floor - 1
        )
        return()
      }
      if( upper_bound < lower_bound + 1 ) {
        shiny::updateNumericInput(
          session = session,
          input = "mt_frequency_upper_bound",
          value = min(nyquist_floor, lower_bound + 1)
        )
      }

    }),
    input$mt_frequency_lower_bound,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      repository <- component_container$data$repository
      if(is.null(repository)) { return() }
      nyquist_floor <- floor(repository$sample_rate / 2)

      lower_bound <- input$mt_frequency_lower_bound
      upper_bound <- input$mt_frequency_upper_bound

      if(!length(upper_bound) || is.na(upper_bound)) {
        shiny::updateNumericInput(
          session = session,
          input = "mt_frequency_upper_bound",
          value = nyquist_floor
        )
        return()
      }

      if( upper_bound < 0.1 ) {
        shiny::updateNumericInput(
          session = session,
          input = "mt_frequency_upper_bound",
          value = 0.1
        )
        return()
      }

      if( upper_bound > nyquist_floor ) {
        shiny::updateNumericInput(
          session = session,
          input = "mt_frequency_upper_bound",
          value = nyquist_floor
        )
        return()
      }
      if( upper_bound < lower_bound + 1 ) {
        shiny::updateNumericInput(
          session = session,
          input = "mt_frequency_lower_bound",
          value = max(0.1, upper_bound - 1)
        )
      }

    }),
    input$mt_frequency_upper_bound,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      epoch_table <- component_container$data$epoch_table
      current_condition <- input$condition
      current_idx <- which(epoch_table$Condition2 == current_condition)
      if(!length(current_idx)) {
        current_idx <- 1
      } else {
        current_idx <- current_idx - 1
        if( current_idx <= 0 ) {
          current_idx <- nrow(epoch_table)
        }
      }
      new_condition <- epoch_table$Condition2[[ current_idx ]]
      shiny::updateSelectInput(
        session = session,
        inputId = "condition",
        selected = new_condition
      )
      ravedash::fire_rave_event()
    }),
    input$condition_prev,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      epoch_table <- component_container$data$epoch_table
      current_condition <- input$condition
      current_idx <- which(epoch_table$Condition2 == current_condition)
      if(!length(current_idx)) {
        current_idx <- 1
      } else {
        current_idx <- current_idx + 1
        if( current_idx > nrow(epoch_table) ) {
          current_idx <- 1
        }
      }
      new_condition <- epoch_table$Condition2[[ current_idx ]]
      shiny::updateSelectInput(
        session = session,
        inputId = "condition",
        selected = new_condition
      )
    }),
    input$condition_next,
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  # ---- Events : END ------------------------------------------



  # ---- Output renderers: BEGIN ------------------------------------------

  ravedash::register_output(
    outputId = "sz_power_plot",
    output_type = "image",
    title = "Download power over time plot",
    shiny::renderPlot({
      shiny::validate(
        shiny::need(
          length(local_reactives$update_outputs) &&
            !isFALSE(local_reactives$update_outputs),
          message = "Please run the module first"
        )
      )

      shiny::validate(
        shiny::need(
          isTRUE(is.list(local_data$results)),
          message = "One or more errors while executing pipeline. Please check the notification."
        )
      )

      heatmap_result <- local_data$results$heatmap_result
      SOZ_elec <-  local_data$SOZ_elec
      plot_SOZ_elec <- local_data$plot_SOZ_elec
      label_type <- local_data$label
      group_SOZResect <- local_data$group_SOZResect
      analysis_settings <- local_data$analysis_settings
      text_size <- local_data$text_size
      resect_elec <- local_data$resect_elec
      plot_resect_elec <- local_data$plot_resect_elec

      repository_plot <- pipeline$read("repository")
      subject_code <- pipeline$read("subject_code")
      analysis_windows <- pipeline$read("subject_code")

      p <- plot_heatmap(heatmap_result[[1]], SOZ_elec, plot_SOZ_elec, resect_elec, plot_resect_elec,  label_type,
                        group_SOZResect, repository_plot, subject_code, analysis_settings, index = 1, text_size)


      print(p)
    })
  )

  ravedash::register_output(
    outputId = "sz_power_on_brain",
    output_type = "threeBrain",
    title = "Share the viewer",
    threeBrain::renderBrain({

      force(local_reactives$update_outputs)

      repository <- component_container$data$repository
      if(is.null(repository)) { return() }
      subject <- repository$subject
      if(is.null(subject)) { return() }
      brain <- raveio::rave_brain(subject)

      has_plot_data <- FALSE
      plot_data <- NULL
      if(is.list(local_data$results)) {
        plot_data <- local_data$results$YAEL_data[[1]]
        if(is.data.frame(plot_data)) {
          has_plot_data <- TRUE
        }
      }

      if(is.null(brain)) {
        brain <- threeBrain::merge_brain()
        electrode_table <- subject$get_electrode_table()
        electrode_table$Subject <- brain$template_subject
        brain$set_electrodes(electrode_table)

        if( has_plot_data ) {
          # plot onto template brain
          plot_data$SubjectCode <- plot_data$Subject
          plot_data$Subject <- brain$template_subject
        }
      }

      #Define a color palette to use for blue to purple to red
      BPR_palette <- colorRampPalette(c("blue", "purple", "red"))(10)

      if( has_plot_data ) {
        brain$set_electrode_values(plot_data)

        brain$plot(
          side_display = FALSE,
          val_ranges = list(
            "HeatmapValue" = c(0, 1),
            "SOZ" = c(0, 1),
            "Resect" = c(0, 1),
            "SOZ_Resect" = c(0, 1)
          ),
          palettes = list(
            "HeatmapValue" = viridis::turbo(128),
            "SOZ_Resect" = BPR_palette
          )
        )

      } else {
        brain$plot(
          side_display = FALSE
        )
      }
    })
  )

  # ---- Output renderers: END ------------------------------------------




}
