## transactions.R ##
transactionsUI <- function(id) {
  ns <- NS(id)
  
  DT::dataTableOutput(ns("transactionDataTable"))
  
}

transactions <- function(input, output, session, transactionsDf) {

  # https://rstudio.github.io/DT/shiny.html
  output$transactionDataTable <- DT::renderDataTable(
    DT::datatable(transactionsDf, filter = 'bottom',
                  options = list(scrollX = TRUE,
                                 autoWidth = TRUE,
                                 lengthMenu = c(10, 25, 50, 100, 500
                                 )
                  )
    )
  )
  
}