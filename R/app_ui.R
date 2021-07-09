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
                          sidebarPanel(
                            
                            "Prior Strike History by Attorney",
                            selectInput("atty_d", "Defense:", 
                                        choices=atty_levels_d,
                                        selected = "None"),
                            
                            selectInput("atty_p", "Prosecution:", 
                                        choices=atty_levels_p,
                                        selected = "None"),
                            
                            selectInput("cog_c", "Cognizable Class:", choices=cog_c_levels),
                            
                            radioButtons("weight", "Prior Strike History Weight:", inline=TRUE, selected = 1,
                                         choiceValues = c(1, 0.5, 0.2),
                                         choiceNames = c("Equal","Half","Minimal") 
                            ),
                            hr(),
                            
                            "Current Strike Tally",
                            rhandsontable::rHandsontableOutput("hot"),
                            
                          ),
                          mainPanel(
                            #actionButton("updateButton", "Update",
                            #             icon = icon("refresh")),
                            plotOutput("plot"))
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

