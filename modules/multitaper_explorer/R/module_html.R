

module_html <- function(){

  current_inputs <- pipeline$get_settings()
  frequency_range <- unlist(current_inputs$frequency_range)
  if(length(frequency_range) != 2) {
    frequency_range <- c(1, 200)
  }
  window_params <- current_inputs$window_params
  if(length(window_params) != 2) {
    window_params <- c(2, 0.5)
  }
  time_bandwidth <- current_inputs$time_bandwidth
  if(length(time_bandwidth) != 1 || !is.numeric(time_bandwidth)) {
    time_bandwidth <- 3
  }

  shiny::fluidPage(
    shiny::fluidRow(

      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll",
          shiny::column(
            width = 12L,

            ravedash::input_card(
              title = "Multitaper Parameters",

              ravedash::flex_group_box(
                title = "Basic",

                shidashi::flex_container(
                  shidashi::flex_item(
                    shiny::numericInput(
                      inputId = ns("mt_frequency_lower_bound"),
                      label = "Frequency from",
                      min = 0.1, step = 0.1, value = frequency_range[[1]], width = "100%"
                    )
                  ),
                  shidashi::flex_item(
                    shiny::numericInput(
                      inputId = ns("mt_frequency_upper_bound"),
                      label = "to (Hz)",
                      min = 0.1, step = 0.1, value = frequency_range[[2]], width = "100%"
                    )
                  )
                ),

                shidashi::flex_container(
                  shidashi::flex_item(
                    shiny::numericInput(
                      inputId = ns("mt_window_size"),
                      label = "Window size (s)",
                      min = 0.1, step = 0.1, value = window_params[[1]], width = "100%"
                    )
                  ),
                  shidashi::flex_item(
                    shiny::numericInput(
                      inputId = ns("mt_step_size"),
                      label = "Step size (s)",
                      min = 0.1, step = 0.1, value = window_params[[2]], width = "100%"
                    )
                  )
                ),

                shiny::numericInput(
                  inputId = ns("mt_time_bandwidth"),
                  label = "Time-half bandwidth product (window duration x half bandwidth of main lobe)",
                  value = time_bandwidth,
                  min = 0.1, width = "100%"
                )
              ),


              shiny::p(
                shiny::a(
                  href = "#",
                  class = "toggle-advance-options",
                  "Toggle advanced options"
                )
              ),

              shiny::div(
                class = "rave-optional soft-hidden",
                ravedash::flex_group_box(
                  title = "Advanced",

                  shiny::numericInput(
                    inputId = ns("mt_num_tapers"),
                    label = "Number of DPSS tapers (leave 0 or blank for default)",
                    value = 0, min = 0, max = 100, step = 1, width = "100%"
                  ),
                  shiny::numericInput(
                    inputId = ns("mt_nfft"),
                    label = "Minimum allowable NFFT size",
                    value = 0,
                    min = 0, step = 1, width = "100%"
                  ),
                  shiny::selectInput(
                    inputId = ns("mt_weighting"),
                    label = "Taper weights",
                    choices = c("unity", "eigen", "adapt"),
                    selected = "unity", multiple = FALSE, width = "100%"
                  ),
                  shiny::selectInput(
                    inputId = ns("mt_detrend_opt"),
                    label = "Detrend option",
                    choices = c("linear", "constant", "off"),
                    selected = "linear", multiple = FALSE, width = "100%"
                  ),
                  shiny::checkboxInput(
                    inputId = ns("mt_parallel"),
                    label = "Parallel computing",
                    value = TRUE
                  ),
                  shiny::numericInput(
                    inputId = ns("mt_max_workers"),
                    label = "Parallel CPU cores",
                    min = 0, max = raveio::raveio_getopt("max_worker"),
                    value = raveio::raveio_getopt("max_worker"), step = 1,
                    width = "100%"
                  )
                )
              ),

              dipsaus::actionButtonStyled(
                inputId = ns("run_multitaper"),
                label = "Run multitaper", width = "100%"
              )
            ),

            ravedash::input_card(
              title = "Condition Selector",

              shiny::fluidRow(

                shiny::column(
                  width = 12,
                  shiny::selectInput(
                    inputId = ns("condition"),
                    label = "Select a condition",
                    choices = character()
                  )
                ),

                shiny::column(
                  width = 6,
                  shiny::actionButton(
                    inputId = ns("condition_prev"),
                    label = "Prev"
                  )
                ),
                shiny::column(
                  width = 6,
                  shiny::actionButton(
                    inputId = ns("condition_next"),
                    label = "Next"
                  )
                )

              )
            ),

            ravedash::input_card(
              title = "Analysis Options",

              shiny::fluidRow(

                shiny::column(
                  width = 12,

                  dipsaus::compoundInput2(
                    inputId = ns("analysis_settings"),
                    label = "Group",
                    initial_ncomp = 1, min_ncomp = 1, max_ncomp = 4,
                    components = shiny::div(

                      shiny::sliderInput(
                        inputId = "frequency_range",
                        label = "Frequency range (Hz)",
                        min = 0,
                        max = 2000,
                        value = c(0, 2000),
                        step = 0.1,
                        post = " Hz",
                        width = "100%",
                        ticks = FALSE
                      ),

                      shiny::sliderInput(
                        inputId = "time_range",
                        label = 'Time range (s)',
                        min = -1000,
                        max = 1000,
                        value = c(-1000, 1000),
                        step = 0.1,
                        round = TRUE
                      )

                    )
                  )
                ),

                shiny::column(
                  width = 12,
                  ravedash::run_analysis_button("Update analysis", width = "100%", class = "margin-top-5")
                )
              )
            ),

            ravedash::input_card(
              title = "Plot Options",
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  shiny::checkboxInput(
                    inputId = ns("hm_normalize"),
                    label = "Normalize Power to 0 ~ 1",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = ns("hm_label"),
                    label = "Show Electrode Labels",
                    value = TRUE
                  ),
                  shiny::checkboxInput(
                    inputId = ns("hm_showSOZ"),
                    label = "Show SOZ",
                    value = FALSE
                  ),
                  shiny::textInput(
                    inputId = ns("input_SOZ_electrodes"),
                    label = "SOZ electrodes (numeric input)",
                    value = "0"
                  )
                )
              )
            )
          )
        )
      ),

      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll output-wrapper",
          shiny::column(
            width = 12L,

            ravedash::output_card(
              'Power over time per electrode',
              class_body = "no-padding fill-width height-550 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                ravedash::output_gadget_container(
                  shiny::plotOutput(ns("sz_power_plot"), width = '100%', height = "100%")
                )
              )
            ),


            ravedash::output_card(
              '3D visualization',
              class_body = "no-padding fill-width height-550 min-height-450 resize-vertical",
              shiny::div(
                class = 'position-relative fill',
                ravedash::output_gadget_container(
                  threeBrain::threejsBrainOutput(ns("sz_power_on_brain"), width = '100%', height = "100%")
                )
              )

              # footer = shiny::fluidRow(
              #   shiny::column(
              #     width = 4,
              #     shiny::sliderInput(
              #       inputId = ns('asdad'),
              #       label = 'slider',
              #       min = -60,
              #       max = 60,
              #       value = c(-60, 60), step = 0.1, round = TRUE
              #     )
              #   )
              # )

            )

          )
        )
      )

    )
  )
}
