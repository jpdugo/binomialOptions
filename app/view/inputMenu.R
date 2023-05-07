box::use(
  shiny[
    NS, tagList, moduleServer, eventReactive, is.reactive, selectInput,
    observeEvent, req, renderText, textOutput, reactiveVal, tags, br, observe
  ],
  shiny.fluent[...],
  glue[glue],
  shinyjs
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    CommandBar(
      items = list(
        list(
          text = Toggle.shinyInput(
            inputId = ns("plt_activate"),
            label   = "Activate Plot",
            value   = TRUE
          )
        ),
        list(
          text = Slider.shinyInput(
            inputId = ns("plt_height"),
            label   = "Plot Height (px)",
            min     = 200,
            max     = 1000,
            step    = 20,
            value   = 500
          )
        )
      ),
      farItems = list(
        list(
          id        = ns("icon_settings"),
          key       = "settings",
          text      = "Settings",
          ariaLabel = "Settings",
          iconOnly  = TRUE,
          iconProps = list(iconName = "Settings")
        )
      )
    ),
    Panel(
      id = ns("main_panel"),
      headerText = "Configuration Panel",
      isOpen = TRUE,
      onDismiss = JS(
        glue(
          "function closePanel() {
             const panelElement = document.getElementById('≤input_id≥');
             panelElement.classList.add('shinyjs-hide');
           }",
          input_id = ns("main_panel"),
          .open = "≤",
          .close = "≥"
        )
      ),
      list(
        Slider.shinyInput(
          inputId = ns("opt_underlying"),
          label   = "Underlying Asset Value",
          min     = 1,
          max     = 500,
          step    = 1,
          value   = 100
        ),
        Slider.shinyInput(
          inputId = ns("opt_strike"),
          label   = "Strike Price",
          min     = 1,
          max     = 500,
          step    = 1,
          value   = 100
        ),
        Slider.shinyInput(
          inputId = ns("opt_sigma"),
          label   = "Annual Volatility",
          min     = 0.01,
          max     = 1.5,
          step    = 0.01,
          value   = 0.15
        ),
        Slider.shinyInput(
          inputId = ns("opt_risk_free"),
          label   = "Risk Free Rate",
          min     = 0.01,
          max     = 0.5,
          step    = 0.01,
          value   = 0.03
        ),
        Slider.shinyInput(
          inputId = ns("opt_time"),
          label   = "T",
          min     = 0.1,
          max     = 5,
          step    = 0.1,
          value   = 0.6
        ),
        Slider.shinyInput(
          inputId = ns("opt_steps"),
          label   = "Number of Steps",
          min     = 1,
          max     = 25,
          step    = 1,
          value   = 5
        ),
        br(),
        PrimaryButton.shinyInput(
          inputId = ns("run"),
          text    = "Calculate"
        )
      )
    ),
    reactOutput(ns("teaching_bubble"))
  )
}

#' @export
server <- function(id, reactive_values) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      bubble_activate <- reactiveVal(TRUE)

      observe({
        shinyjs$runjs(
          glue(
            "App.addClick('{ns('icon_settings')}', '{element_id}');
             // avoid overflow: hidden !important; from calling Panel function
             const bodyElement = document.getElementsByClassName('ms-Fabric');
             bodyElement[0].className = 'ms-Fabric ms-Fabric--isFocusHidden';",
            element_id = ns("show_panel")
          )
        )
        shinyjs$hide("main_panel")
      })

      output$teaching_bubble <- renderReact({
        if (bubble_activate()) {
          TeachingBubble(
            target   = glue("#{ns('icon_settings')}"),
            headline = "Press To Customize Parameters!"
          )
        }
      })

      observeEvent(input$show_panel, {
        bubble_activate(FALSE)
      })

      observeEvent(input$show_panel, {
        shinyjs$show("main_panel")
      })

      observeEvent(input$plt_height, {
        reactive_values$height <- input$plt_height
      })

      observeEvent(input$plt_activate, {
        reactive_values$activate <- input$plt_activate
      })

      eventReactive(input$run,
        {
          list(
            class      = input$opt_class,
            underlying = input$opt_underlying,
            strike     = input$opt_strike,
            sigma      = input$opt_sigma,
            risk_free  = input$opt_risk_free,
            t          = input$opt_time,
            steps      = input$opt_steps
          )
        },
        ignoreNULL = FALSE
      )
    }
  )
}
