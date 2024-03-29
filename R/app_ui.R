#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    fluidPage(
      # Navbar structure for UI
      
      navbarPage("Batson App - Proof of Concept", theme = shinythemes::shinytheme("paper"),
                 
                 tabPanel("Prototype", fluid = TRUE,
                          sidebarPanel(width = 4,
                            
                            "Current Strike Tally",
                            rhandsontable::rHandsontableOutput("hot"),

                            #"Prior Strike History by Attorney",
                            selectInput("atty_d", "Strike History - Defense:", 
                                        choices=atty_levels_d,
                                        selected = "None"),
                            selectInput("atty_p", "Strike History - Prosecution:", 
                                        choices=atty_levels_p,
                                        selected = "None"),
                            
                            radioButtons("weight", "Strike History Weight:", inline=TRUE, selected = 1,
                                         choiceValues = c(1, 0.5, 0.2),
                                         choiceNames = c("Equal","Half","Minimal") 
                            ),
                            #hr(),
                            
                          ),
                          mainPanel(
                            #actionButton("updateButton", "Update",
                            #             icon = icon("refresh")),
                            plotOutput("plot"),
                            selectInput("cog_c", 
                                        "Cognizable Class:",
                                        choices=cog_c_levels,
                                        width = "25%"),
                            tags$button(
                              id = 'close',
                              type = "button",
                              class = "btn action-button",
                              onclick = "setTimeout(function(){window.close();},500);",  # close browser
                              "Close App"
                            ),
                          
                            )
                 )
      ))
    )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'batson.app.pkg'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

