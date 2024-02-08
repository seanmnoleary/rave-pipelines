
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
  # server_tools$run_analysis_onchange()

  # This function only cares about running multitaper
  # Do not put plotting or analysis configurations here
  run_multitaper <- function(target_names = c("multitaper_result"),
                             with_notification = TRUE, ...) {
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

    # label_input <- "numeric"
    # if (input$hm_label == TRUE) {
    #   label_input <- "names"
    # } else {
    #   label_input <- "numeric"
    # }

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

      # analysis_time_frequencies = input$analysis_settings

    )

    # stores the multitaper results
    local_data$results <- NULL

    # local_data$SOZ_elec <- input$input_SOZ_electrodes
    # local_data$plot_SOZ_elec <- input$hm_showSOZ
    # local_data$label_type <- label_input

    ravedash::logger("Scheduled: ", pipeline$pipeline_name, level = 'debug', reset_timer = TRUE)

    if( with_notification ) {
      dipsaus::shiny_alert2(
        title = "Calculating in progress",
        text = ravedash::be_patient_text(),
        icon = "info",
        danger_mode = FALSE,
        auto_close = FALSE,
        buttons = FALSE,
        session = session
      )
    }


    res <- tryCatch(
      {

        shidashi::clear_notifications(class = "pipeline-error")

        results <- pipeline$run(
          as_promise = FALSE,
          scheduler = "none",
          type = "callr",
          # shortcut = TRUE,
          names = target_names,
          ...
        )

        multitaper_result <- pipeline$read("multitaper_result")

        dname <- multitaper_result$get_header("extra", list(dname = dimnames(multitaper_result)))$dname
        frequency_range <- range(dname$Frequency)

        local_data$results <- list(
          multitaper_result = multitaper_result
        )

        # need to update `analysis_settings` input
        dipsaus::updateCompoundInput2(
          session = session,
          inputId = "analysis_settings",
          initialization = list(
            "frequency_range" = list(
              min = floor(frequency_range[[1]]),
              max = ceiling(frequency_range[[2]])
            )
          )
        )

        shidashi::card_operate(
          title = "Multitaper Parameters",
          method = "collapse"
        )

        if( with_notification ) {
          ravedash::logger("Fulfilled: ", pipeline$pipeline_name,
                           level = 'debug')
        }
        local_reactives$update_outputs <- Sys.time()
        TRUE
      },
      error = function(e) {
        msg <- paste(e$message, collapse = "\n")
        if(inherits(e, "error")){
          ravedash::logger(msg, level = 'error')
          ravedash::logger(traceback(e), level = 'error', .sep = "\n")
          shidashi::show_notification(
            message = msg,
            title = "Error while running pipeline", type = "danger",
            autohide = FALSE, close = TRUE, class = "pipeline-error"
          )
          shidashi::card_operate(
            title = "Multitaper Parameters",
            method = "expand"
          )
        }
        e
      }
    )

    if( with_notification ) {
      Sys.sleep(0.5)
      dipsaus::close_alert2()
    }
    return( res )
  }

  run_analysis <- function() {
    output_flags <- local_reactives$update_outputs
    if(!length(output_flags) || isFALSE(output_flags) || !is.list(local_data$results)) {
      run_multitaper()
    }
    pipeline$set_settings(
      analysis_time_frequencies = input$analysis_settings
      # soz_electrodes = soz_electrodes,
      # heatmap_name_type = heatmap_name_type,
      # condition = condition
    )

    # DIPSAUS DEBUG START (run this first for debugging)
    # pipeline$run("multitaper_result")

    # This is what's needed before visualization
    res <- pipeline$eval(c("heatmap_result"), shortcut = TRUE)

    # run the rest
    local_data$results$heatmap_result <- res$heatmap_result

    local_reactives$update_outputs <- Sys.time()
  }


  # Register event: main pipeline need to run
  shiny::bindEvent(
    ravedash::safe_observe({

      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      run_analysis()

    }, error_wrapper = "alert"),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # Listening to "run multitaper" button
  shiny::bindEvent(
    ravedash::safe_observe({

      run_multitaper()

    }, error_wrapper = "alert"),
    input$run_multitaper,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  # check whether the loaded data is valid
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
      frequency_range <- pipeline$read("frequency_range")
      if(length(frequency_range) != 2 || !is.numeric(frequency_range)) {
        frequency_range <- c(1, nyquist_floor)
      }
      analysis_time_frequencies <- lapply(pipeline$get_settings("analysis_time_frequencies"), function(item) {
        list(
          frequency_range = unlist(item$frequency_range),
          time_range = unlist(item$time_range)
        )
      })
      if(!length(analysis_time_frequencies)) { analysis_time_frequencies <- NULL }

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
      ncomp <- length(analysis_time_frequencies)
      if( ncomp < 1 ) {
        ncomp <- length(input$analysis_settings)
      }
      dipsaus::updateCompoundInput2(
        session = session,
        inputId = "analysis_settings",
        initialization = list(
          "frequency_range" = list(
            min = frequency_range[[1]],
            max = frequency_range[[2]]
          ),
          "time_range" = list(
            min = time_window_range[[1]],
            max = time_window_range[[2]]
          )
        ),
        value = analysis_time_frequencies,
        ncomp = ncomp
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

  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #     pipeline$set_settings(
  #       condition = input$condition
  #     )
  #
  #     dipsaus::shiny_alert2(
  #       title = "Updating...",
  #       text = ravedash::be_patient_text(),
  #       icon = "info",
  #       danger_mode = FALSE,
  #       auto_close = FALSE,
  #       buttons = FALSE,
  #       session = session
  #     )
  #
  #     results <- pipeline$run(
  #       as_promise = TRUE,
  #       scheduler = "none",
  #       type = "callr",
  #       callr_function = NULL,
  #       # shortcut = TRUE,
  #       names = c("heatmap_result", "YAEL_data")
  #     )
  #
  #     results$promise$then(
  #       onFulfilled = function(...){
  #
  #         local_data$results <- pipeline$read(c("heatmap_result", "YAEL_data"))
  #
  #
  #         Sys.sleep(0.5)
  #         ravedash::logger("Fulfilled: ", pipeline$pipeline_name,
  #                          level = 'debug')
  #         shidashi::clear_notifications(class = "pipeline-error")
  #         dipsaus::close_alert2()
  #
  #         local_reactives$update_outputs <- Sys.time()
  #         return(TRUE)
  #       },
  #       onRejected = function(e, ...){
  #         msg <- paste(e$message, collapse = "\n")
  #         if(inherits(e, "error")){
  #           ravedash::logger(msg, level = 'error')
  #           ravedash::logger(traceback(e), level = 'error', .sep = "\n")
  #           shidashi::show_notification(
  #             message = msg,
  #             title = "Error while running pipeline", type = "danger",
  #             autohide = FALSE, close = TRUE, class = "pipeline-error"
  #           )
  #         }
  #         Sys.sleep(0.5)
  #         dipsaus::close_alert2()
  #         return(msg)
  #       }
  #     )
  #     return()
  #
  #   }),
  #   input$condition,
  #   ignoreNULL = TRUE,
  #   ignoreInit = TRUE
  # )

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

  get_soz_electrodes <- shiny::debounce(
    shiny::reactive({
      if(!isTRUE(input$hm_showSOZ)) { return(integer()) }
      return(as.integer(dipsaus::parse_svec(input$input_SOZ_electrodes)))
    }),
    1000
  )

  get_resect_electrodes <- shiny::debounce(
    shiny::reactive({
      if(!isTRUE(input$hm_showResect)) { return(integer()) }
      return(as.integer(dipsaus::parse_svec(input$input_resect_electrodes)))
    }),
    1000
  )

  ordered_electrodes <- shiny::debounce(
    shiny::reactive({
      if(!isTRUE(input$hm_ordered)) { return(FALSE) }
      return(TRUE)
    }),
    1
  )

  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #
  #     label_input <- "numeric"
  #     if (input$hm_label == TRUE) {
  #       label_input <- "names"
  #     } else {
  #       label_input <- "numeric"
  #     }
  #
  #     local_data$label <- label_input
  #     local_data$SOZ_elec <- input$input_SOZ_electrodes
  #
  #     pipeline$set_settings(
  #       label = label_input,
  #       SOZ_elec = input$input_SOZ_electrodes
  #     )
  #
  #     dipsaus::shiny_alert2(
  #       title = "Updating...",
  #       text = ravedash::be_patient_text(),
  #       icon = "info",
  #       danger_mode = FALSE,
  #       auto_close = FALSE,
  #       buttons = FALSE,
  #       session = session
  #     )
  #
  #     results <- pipeline$run(
  #       as_promise = TRUE,
  #       scheduler = "none",
  #       type = "callr",
  #       callr_function = NULL,
  #       # shortcut = TRUE,
  #       names = c("heatmap_result", "YAEL_data")
  #     )
  #
  #     results$promise$then(
  #       onFulfilled = function(...){
  #
  #         local_data$results <- pipeline$read(c("heatmap_result", "YAEL_data"))
  #
  #         Sys.sleep(0.5)
  #         ravedash::logger("Fulfilled: ", pipeline$pipeline_name,
  #                          level = 'debug')
  #         shidashi::clear_notifications(class = "pipeline-error")
  #         dipsaus::close_alert2()
  #
  #         local_reactives$update_outputs <- Sys.time()
  #         return(TRUE)
  #       },
  #       onRejected = function(e, ...){
  #         msg <- paste(e$message, collapse = "\n")
  #         if(inherits(e, "error")){
  #           ravedash::logger(msg, level = 'error')
  #           ravedash::logger(traceback(e), level = 'error', .sep = "\n")
  #           shidashi::show_notification(
  #             message = msg,
  #             title = "Error while running pipeline", type = "danger",
  #             autohide = FALSE, close = TRUE, class = "pipeline-error"
  #           )
  #         }
  #         Sys.sleep(0.5)
  #         dipsaus::close_alert2()
  #         return(msg)
  #       }
  #     )
  #     return()
  #
  #   }),
  #   input$hm_label,
  #   ignoreNULL = TRUE,
  #   ignoreInit = TRUE
  # )
  #
  # shiny::bindEvent(
  #   ravedash::safe_observe({
  #
  #     local_data$plot_SOZ_elec <- input$hm_showSOZ
  #     local_data$SOZ_elec <- input$input_SOZ_electrodes
  #
  #     pipeline$set_settings(
  #       plot_SOZ_elec = input$hm_showSOZ,
  #       SOZ_elec = input$input_SOZ_electrodes
  #     )
  #
  #     dipsaus::shiny_alert2(
  #       title = "Updating...",
  #       text = ravedash::be_patient_text(),
  #       icon = "info",
  #       danger_mode = FALSE,
  #       auto_close = FALSE,
  #       buttons = FALSE,
  #       session = session
  #     )
  #
  #     results <- pipeline$run(
  #       as_promise = TRUE,
  #       scheduler = "none",
  #       type = "callr",
  #       callr_function = NULL,
  #       # shortcut = TRUE,
  #       names = c("heatmap_result", "YAEL_data")
  #     )
  #
  #     results$promise$then(
  #       onFulfilled = function(...){
  #
  #         local_data$results <- pipeline$read(c("heatmap_result", "YAEL_data"))
  #
  #
  #         Sys.sleep(0.5)
  #         ravedash::logger("Fulfilled: ", pipeline$pipeline_name,
  #                          level = 'debug')
  #         shidashi::clear_notifications(class = "pipeline-error")
  #         dipsaus::close_alert2()
  #
  #         local_reactives$update_outputs <- Sys.time()
  #         return(TRUE)
  #       },
  #       onRejected = function(e, ...){
  #         msg <- paste(e$message, collapse = "\n")
  #         if(inherits(e, "error")){
  #           ravedash::logger(msg, level = 'error')
  #           ravedash::logger(traceback(e), level = 'error', .sep = "\n")
  #           shidashi::show_notification(
  #             message = msg,
  #             title = "Error while running pipeline", type = "danger",
  #             autohide = FALSE, close = TRUE, class = "pipeline-error"
  #           )
  #         }
  #         Sys.sleep(0.5)
  #         dipsaus::close_alert2()
  #         return(msg)
  #       }
  #     )
  #     return()
  #
  #   }),
  #   input$hm_showSOZ,
  #   ignoreNULL = TRUE,
  #   ignoreInit = TRUE
  # )

  shiny::bindEvent(
    ravedash::safe_observe({
      use_color_map(input$color_palette)
      local_reactives$update_outputs <- Sys.time()
    }),
    input$color_palette,
    ignoreNULL = TRUE, ignoreInit = TRUE
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
          message = "Please run the module first."
        )
      )

      shiny::validate(
        shiny::need(
          isTRUE(is.list(local_data$results)) &&
            isTRUE(is.list(local_data$results$heatmap_result)),
          message = "No heatmap data. Please check analysis options."
        )
      )

      heatmap_result <- local_data$results$heatmap_result
      heatmap_name_type <- ifelse(isTRUE(input$hm_label), "name", "number")
      soz_electrodes <- get_soz_electrodes()
      resect_electrodes <- get_resect_electrodes()
      ordered <- ordered_electrodes()
      condition <- input$condition
      scale <- ifelse(isTRUE(input$hm_normalize), "0-1", "normal")

      plot_power_over_time_data(
        heatmap_result,
        soz_electrodes = soz_electrodes,
        resect_electrodes = resect_electrodes,
        name_type = heatmap_name_type,
        trial = condition,
        scale = scale,
        ordered = ordered
      )
    })
  )

  ravedash::register_output(
    outputId = "sz_line_plot",
    output_type = "image",
    title = "Download average power over time line plot",
    shiny::renderPlot({
      shiny::validate(
        shiny::need(
          length(local_reactives$update_outputs) &&
            !isFALSE(local_reactives$update_outputs),
          message = "Please run the module first."
        )
      )

      shiny::validate(
        shiny::need(
          isTRUE(is.list(local_data$results)) &&
            isTRUE(is.list(local_data$results$heatmap_result)),
          message = "No heatmap data. Please check analysis options."
        )
      )

      heatmap_result <- local_data$results$heatmap_result
      heatmap_name_type <- ifelse(isTRUE(input$hm_label), "name", "number")
      soz_electrodes <- get_soz_electrodes()
      resect_electrodes <- get_resect_electrodes()
      condition <- input$condition
      scale <- ifelse(isTRUE(input$hm_normalize), "0-1", "normal")

      plot_power_over_time_data_line(
        heatmap_result,
        soz_electrodes = soz_electrodes,
        resect_electrodes = resect_electrodes,
        name_type = heatmap_name_type,
        trial = condition,
        scale = scale,
      )
    })
  )

  ravedash::register_output(
    outputId = "sz_statistic_plot",
    output_type = "image",
    title = "Download statistic over time line plot",
    shiny::renderPlot({
      shiny::validate(
        shiny::need(
          length(local_reactives$update_outputs) &&
            !isFALSE(local_reactives$update_outputs),
          message = "Please run the module first."
        )
      )

      shiny::validate(
        shiny::need(
          isTRUE(is.list(local_data$results)) &&
            isTRUE(is.list(local_data$results$heatmap_result)),
          message = "No heatmap data. Please check analysis options."
        )
      )

      heatmap_result <- local_data$results$heatmap_result
      heatmap_name_type <- ifelse(isTRUE(input$hm_label), "name", "number")
      soz_electrodes <- get_soz_electrodes()
      resect_electrodes <- get_resect_electrodes()
      condition <- input$condition
      scale <- ifelse(isTRUE(input$hm_normalize), "0-1", "normal")

      plot_quantile_plot(
        heatmap_result,
        soz_electrodes = soz_electrodes,
        resect_electrodes = resect_electrodes,
        name_type = heatmap_name_type,
        trial = condition,
        scale = scale,
      )
    })
  )

  ravedash::register_output(
    outputId = "sz_power_on_brain",
    output_type = "threeBrain",
    title = "Share the viewer",
    threeBrain::renderBrain({

      force(local_reactives$update_outputs)

      soz_electrodes <- get_soz_electrodes()
      resect_electrodes <- get_resect_electrodes()

      repository <- component_container$data$repository


      if(is.null(repository)) { return() }
      subject <- repository$subject
      if(is.null(subject)) { return() }
      brain <- raveio::rave_brain(subject)

      has_plot_data <- FALSE
      plot_data <- NULL
      if(is.list(local_data$results)) {
        heatmap_result <- local_data$results$heatmap_result
        if(is.list(heatmap_result)) {
          plot_data <- generate_3dviewer_data(
            heatmap_result,
            trial = input$condition
          )
          if(length(plot_data)) {
            has_plot_data <- TRUE
          }
        }
      }

      use_template_brain <- FALSE
      soz_electrodes <- dipsaus::parse_svec(soz_electrodes)
      resect_electrodes <- dipsaus::parse_svec(resect_electrodes)
      resect_or_soz_electrodes <- intersect(soz_electrodes, resect_electrodes)

      if(is.null(brain)) {
        brain <- threeBrain::merge_brain()
        electrode_table <- subject$get_electrode_table()
        electrode_table$Subject <- brain$template_subject
        brain$set_electrodes(electrode_table)
        use_template_brain <- TRUE
        lapply(soz_electrodes, function(elec) {
          tryCatch({

            brain$template_object$electrodes$fix_electrode_color(number = elec, color = "#00bfff", inclusive = FALSE)
          }, error = function(e) {

          })
        })
        lapply(resect_electrodes, function(elec) {
          tryCatch({

            brain$template_object$electrodes$fix_electrode_color(number = elec, color = "#bf00ff", inclusive = FALSE)
          }, error = function(e) {

          })
        })
        lapply(resect_or_soz_electrodes, function(elec) {
          tryCatch({

            brain$template_object$electrodes$fix_electrode_color(number = elec, color = "green", inclusive = FALSE)
          }, error = function(e) {

          })
        })

      } else {
        lapply(soz_electrodes, function(elec) {
          tryCatch({
            brain$electrodes$fix_electrode_color(number = elec, color = "#00bfff", inclusive = FALSE)
          }, error = function(e) {

          })
        })
        lapply(resect_electrodes, function(elec) {
          tryCatch({
            brain$electrodes$fix_electrode_color(number = elec, color = "#bf00ff", inclusive = FALSE)
          }, error = function(e) {

          })
        })
        lapply(resect_or_soz_electrodes, function(elec) {
          tryCatch({

            brain$template_object$electrodes$fix_electrode_color(number = elec, color = "green", inclusive = FALSE)
          }, error = function(e) {

          })
        })
      }

      if( has_plot_data ) {
        cols <- plot_preferences$get('heatmap_palette')
        palettes <- list()
        val_ranges <- list()
        for(value_table in plot_data) {
          if( use_template_brain ) {
            # plot onto template brain
            value_table$SubjectCode <- value_table$Subject
            value_table$Subject <- brain$template_subject
            nms <- names(value_table)
            nms <- unique(c(nms[nms %in% c("Subject", "Electrode", "Time", "SubjectCode")], nms))
            value_table <- value_table[, nms]
          }

          brain$set_electrode_values(value_table)
          nms <- names(value_table)
          nms <- nms[!nms %in% c("Subject", "Electrode", "Time", "SubjectCode")]
          for(nm in nms) {
            v <- value_table[[nm]]
            v <- v[!is.na(v)]

            palettes[[nm]] <- cols
            if(length(v)) {
              val_ranges[[nm]] <- c(0, max(v))
            }
          }
        }


        if( use_template_brain ) {
          brain$template_object$render(
            side_display = FALSE,
            val_ranges = val_ranges,
            palettes = palettes,
            outputId = "sz_power_on_brain", session = session
          )
        } else {

          brain$render(
            side_display = FALSE,
            val_ranges = val_ranges,
            palettes = palettes,
            outputId = "sz_power_on_brain", session = session
          )
        }

      } else {
        brain$plot(
          side_display = FALSE
        )
      }
    })
  )

  # ---- Output renderers: END ------------------------------------------




}
