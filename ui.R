
shinyUI( fluidPage(
    
    titlePanel("NBA Shot chart"),
    p("This ShinyApp creates a shot chart for a selected NBA player in 2015-2016 season. " ),
    p("Please select a team and a player from that team. Execution starts when you press the \"Go!\" button.  "),
    inputPanel(
        uiOutput("teamSelect"),
        uiOutput("playerSelect"),
        actionButton("go", "Go!")
    ),
    fluidRow(
        column(width = 6,
               p("All shots are plotted and the shooting percentage for each region is calculated."),
               p("An error appears if a player didn't take any shots this season. "),
               plotOutput("shotChart", click = "plot_click")
        ),
        column(width = 6,
               h4("10 Closest points to the mouse click"),
               p("Click on the chart to display some details about the shot. 10 
                 closest shots to the click will be printed."),
               verbatimTextOutput("click_info")
        )
    )
))
