# Matchathon app

library(matchathon)

# Define UI ---- 
ui <- fluidPage(
    # Panel title ----
    titlePanel("Student-faculty matching & meeting schedule generator"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            # Input: Select a file ----
            fileInput("faculty", "Upload faculty spreadsheet", multiple = F),
            # Input: Select a file ----
            fileInput("students", "Upload student spreadsheet", multiple = F),
            # Optional input: Select a file ----
            #fileInput("already_met","Upload spreadsheet of faculty students have already met with (optional)", multiple = F),
            # Optional input: Select a file ----
            fileInput("f_unavail","Upload spreadsheet of faculty unavailability times (optional)", multiple = F),
            # Choose number of meeting slots
            numericInput('slots', 'Number of meeting slots:', value=12, min=1),
            # Minimum number of faculty meetings
            numericInput('fslots', 'Minumum number of faculty meetings (note: this outcome partially depends on the number of faculty and students being matched):', value=6, min=0),
            # Action button to run analysis
            uiOutput('go'),
            # Horizontal line ----
            tags$hr(),
            # Download buttons ----
            uiOutput("download_ranks"),
            tags$div(tags$br()),
            uiOutput("download_rank_vals"),
            tags$div(tags$br()),
            uiOutput("download_sschedule"),
            tags$div(tags$br()),
            uiOutput("download_fschedule")
            ),# end sidebar panel
        # Main panel for displaying outputs ----
        mainPanel(
            uiOutput("tabers")
        )
    ) # end sidebarLayout
)

# Define server logic
server <- function(input, output) {
    
    output$go = renderUI({
        req(input$faculty, input$students)
        actionButton("go01", "Get matches & schedules")
    })
    
    dat <- eventReactive(input$go01, {
        req(input$faculty)
        req(input$students)
        dfs = read_in_data(input$faculty$datapath,input$students$datapath,input$f_unavail$datapath,input$already_met$datapath)
        results = matchathon(dfs$faculty_df,dfs$student_df,
                             meeting_slots = input$slots, min_fslots = input$fslots, f_unavail = dfs$f_unavail)
        return(results)
    })
    
    output$ranked_faculty <- renderTable({dat()$ranked_faculty},rownames=TRUE)
    output$ranked_faculty_score <- renderTable({dat()$ranked_faculty_score},rownames=TRUE)
    output$s_schedule <- renderTable({dat()$s_schedule},rownames=TRUE)
    output$f_schedule <- renderTable({dat()$f_schedule},rownames=TRUE)
    
    # Output: Tabset w/ plot, summary, and table ----
    output$tabers<-renderUI({
        req(input$faculty, input$students, dat())
        tabsetPanel(type = "tabs",
                    tabPanel("Ranked matches", tableOutput("ranked_faculty")),
                    tabPanel("Student schedule", tableOutput("s_schedule")),
                    tabPanel("Faculty schedule", tableOutput("f_schedule")))
    })
    
    # Downloadable csv of selected dataset ----   
    output$download_ranks = renderUI({
        req(input$faculty, input$students, dat())
        downloadButton("download_ranks01",'Download matches')
    })
    output$download_ranks01 <- downloadHandler(
        filename = 'ranked_faculty_matches.csv',
        content = function(file) {
            write.csv(dat()$ranked_faculty, file, row.names = TRUE)
        })
    output$download_rank_vals = renderUI({
        req(input$faculty, input$students, dat())
        downloadButton("download_rank_vals01",'Download rank values')
    })
    output$download_rank_vals01 <- downloadHandler(
        filename = 'ranked_faculty_values.csv',
        content = function(file) {
            write.csv(dat()$ranked_faculty_score, file, row.names = TRUE)
        })
    output$download_sschedule = renderUI({
        req(input$faculty, input$students, dat())
        downloadButton("download_sschedule01",'Download student schedule')
    })
    output$download_sschedule01 <- downloadHandler(
        filename = 'student_schedule.csv',
        content = function(file) {
            write.csv(dat()$s_schedule, file, row.names = TRUE)
        })
    output$download_fschedule = renderUI({
        req(input$faculty, input$students, dat())
        downloadButton("download_fschedule01",'Download faculty schedule')
    })
    output$download_fschedule01 <- downloadHandler(
        filename = 'faculty_schedule.csv',
        content = function(file) {
            write.csv(dat()$f_schedule, file, row.names = TRUE)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
