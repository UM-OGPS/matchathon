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
        
        validate( 
          need( tolower( tools::file_ext( input$faculty$datapath ) ) == 'csv', 'Error reading in faculty data - did you input a csv?' ),
          need( tolower( tools::file_ext( input$students$datapath ) ) == 'csv', 'Error reading in student data - did you input a csv?' )
        )
        
        if ( ! is.null( input$f_unavail$datapath ) ){
          
          validate( 
            need( tolower( tools::file_ext( input$f_unavail$datapath ) ) == 'csv', 'Error reading in faculty unavailable data - did you input a csv?' )
          )
          
        }
        
        if ( ! is.null( input$already_met$datapath ) ){
          
          validate( 
            need( tolower( tools::file_ext( input$already_met$datapath ) ) == 'csv', 'Error reading in faculty unavailable data - did you input a csv?' )
          )
          
        }
        
        dfs = read_in_data(input$faculty$datapath,input$students$datapath,input$f_unavail$datapath,input$already_met$datapath)
        
        #print( dfs )
        
        #lets validate this input!
        #first collect the class for each column in the various inputs
        fac_dtype = unlist( lapply( dfs$faculty, class ) )
        stud_dtype = unlist( lapply( dfs$student, class ) )
        unavail_dtype = unlist( lapply( dfs$f_unavail, class ) )
        met_dtype = unlist( lapply( dfs$already_met, class ) )
        
        validate(
          need( fac_dtype[1] == 'character' , 'The first column of the faculty data must be names!' ),
          need( unique( fac_dtype[ 2: length( fac_dtype ) ] ) == 'numeric' , 'The faculty interest columns must be numeric!' ),
          
          need( stud_dtype[1] == 'character' , 'The first column of the student data must be names!' ),
          need( unique( stud_dtype[ 2: length( stud_dtype ) ] ) == 'numeric' , 'The student interest columns must be numeric!' )
        )
        
        if ( ! is.null( input$f_unavail$datapath ) ){
          
          validate( 
            #requiring the faculty unavailble data to have two columns may be too strict but it will help validate
            need( length( unavail_dtype ) == 2, 'Faculty unavailble has unknown columns - should have two: student names and then faculty names' ),
            need( unique( unavail_dtype ) == 'character' , 'Both faculty unavailable columns should have names!' )
          )
          
        }
        
        if ( ! is.null( input$already_met$datapath ) ){
          
          validate( 
            #requiring the faculty unavailble data to have two columns may be too strict but it will help validate
            need( length( met_dtype ) == 2, 'Already met data has unknown columns - should have two: student names and then faculty names' ),
            need( unique( met_dtype ) == 'character' , 'Both already met columns should have names!' )
          )
          
        }
        
        fac_col = colnames( dfs$faculty )
        stud_col = colnames( dfs$student )
        unavail_col = colnames( dfs$f_unavail )
        met_col = colnames( dfs$already_met )
        
        #check that the input file has column headers
        #do this by checking the second column name is not numeric
        #hack-y but should work since those columns were previously required to be numeric
        validate( 
          need( is.na( as.numeric( fac_col[2] ) ), 'Faculty column headers are numeric - please make sure your input file includes character column headers' ),
          need( is.na( as.numeric( stud_col[2] ) ), 'Student column headers are numeric - please make sure your input file includes character column headers' )
        )
        
        #is there a good way to validate the already met and unavailble column headers? Not sure right now....
        
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
