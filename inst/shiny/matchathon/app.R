# Matchathon app

#library(matchathon)

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
            fileInput("already_met","Upload spreadsheet of faculty students have already met with (optional)", multiple = F),
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
          p("To help students find faculty members with mutual research interests, we use this algorithm to match first-year PhD students and faculty who are in search of trainees based on mutual research interests, and generate a schedule for a matchathon event. During the event,  students and faculty meet individually with each other for a short period of time (5-7 minutes). To learn more about the app, you can go to our", tags$a(href='https://github.com/zenalapp/matchathon',"GitHub"),'page.'),
            p("This app generates the faculty-student matches and schedule for a matchathon event."),
          h3("What do I input into the app?"),
          tags$ol(
            tags$li(tags$b("Required:"), 'csv (comma-separated values file) of faculty research interests (1=interest, 0=no interest) where the rows are the faculty and the columns are the interests.'), 
            tags$li(tags$b("Required:"), 'csv (comma-separated values file) of student research interests (1=interest, 0=no interest) where the rows are the students and the columns are the interests.'), 
            tags$li(tags$em("Optional:"), "csv (comma-separated values file) of faculty that students have already met with two named columns: Student and Faculty."),
            tags$li(tags$em("Optional:"), "csv (comma-separated values file) of faculty unavailability times with two named columns: Faculty and Slot."),
            tags$li(tags$em("Optional:"), "Number of meeting slots for the event (default: 12)."),
            tags$li(tags$em("Optional:"), "Minimum number of faculty meetings (default: 6).")
          ),
          p("Notes:"),
          tags$ul(
            tags$li("The names of the research interest columns should be identical between the student and faculty files."), 
            tags$li(tags$a(href="https://www.computerhope.com/issues/ch001356.htm","This website"), "explains how you can create a csv by hand or from a Microsoft Excel spreadsheet."), 
            tags$li(tags$a(href="https://github.com/zenalapp/matchathon/tree/master/data-raw","Here"),"are example input files.")
          ),
          h3("How long does it take to run?"),
          p("Longer than we'd like. If you have a lot of meeting slots, it might take a while. We're going to try to make it faster!"),
          h3("What is the output?"),
          tags$ol(
            tags$li("Top faculty matches for students (top_matches.csv)."),
            tags$li("Rank values of matches (ranked_faculty_valules.csv; note: these are somewhat difficult to interpret)."),
            tags$li("Student schedule for the event (student_schedule.csv)."),
            tags$li("Faculty schedule for the event (faculty_schedule.csv).")
          ),
          p("Scroll down to see the output of the results on the page!"),
          h3("Help! Things aren't working!"),
          p("Please open an issue on our", tags$a(href='https://github.com/zenalapp/matchathon/issues',"GitHub"), "page!"),
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
        results = matchathon(dfs$faculty,dfs$student,
                             meeting_slots = input$slots, min_fslots = input$fslots, f_unavail_df = dfs$f_unavail, already_met_df = dfs$already_met)
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
