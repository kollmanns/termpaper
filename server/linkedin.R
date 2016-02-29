## linkedin.R ##
linkedinUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("linkedInHeader")),
    fluidRow(
      box(title = "Education",
          width = 12,
          plotOutput(ns("linkedInEducation"), height = 280),
          collapsible = TRUE
      ),
      box(title = "Positions",
          width = 12,
          plotOutput(ns("linkedInPositions"), height = 230),
          collapsible = TRUE
      )
    )
  )
  
}

linkedin <- function(input, output, session, linkedIn_General,
                     linkedIn_Education, linkedIn_Positions) {
  ns <- session$ns
  
  output$linkedInHeader <- renderUI({
    currentPosition <- linkedIn_Positions %>% filter(PositionIsCurrent == TRUE)
    name <- paste0(linkedIn_General$Firstname, " ", linkedIn_General$Lastname)
    
    div(
      h2(currentPosition$Position, " @ ", currentPosition$Company),
      box(
        width = "100%",
        column(width = 2,
               img(src=linkedIn_General$Picture, width = "100%", class = "img-rounded")
        ),
        column(width = 10,
               tags$ul(
                 style = "list-style-type: none;",
                 tags$li("Current Position since ", currentPosition$StartYearMonth),
                 tags$li(linkedIn_General$Connections, " Connections"),
                 tags$li(a(href=linkedIn_General$ProfileURL, name, "- LinkedIn Profile"))
               )
        )
      )
      
    )
    
  })
  
  output$linkedInEducation <- renderPlot({
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    ggplot(linkedIn_Education, aes(colour=SchoolName)) + 
      geom_segment(aes(x=DegreeStartDate, xend=DegreeEndDate, y=DegreeField, yend=DegreeField), size=3) +
      scale_x_continuous(breaks = min(linkedIn_Education$DegreeStartDate):max(linkedIn_Education$DegreeEndDate)) +
      scale_y_discrete(expand = c(0, 0.9)) + 
      geom_text(data = linkedIn_Education,
                aes(x = DegreeEndDate, y = DegreeField,
                    label = paste0(DegreeField, "\n", SchoolName), vjust=1.2, hjust=1)
      ) + ylab("") +  xlab("") +
      theme(
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        panel.background = element_rect(fill = "white")
      )
  })
  
  output$linkedInPositions <- renderPlot({
    par(mar = c(0.1, 0.1, 0.1, 0.1))
    ggplot(linkedIn_Positions, aes(colour=Company)) + 
      geom_segment(aes(x=StartYearMonth, xend=EndYearMonth, y=Position, yend=Position), size=3) +
      scale_x_date(breaks = date_breaks("6 months")) +
      scale_y_discrete(expand = c(0, 0.9)) + 
      geom_text(data = linkedIn_Positions,
                aes(x = EndYearMonth, y = Position,
                    label = paste0(Position, "\n", Company), vjust=1.2, hjust=1)
      ) + ylab("") +  xlab("") +
      theme(
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey"),
        panel.background = element_rect(fill = "white")
      )
  })
  
}