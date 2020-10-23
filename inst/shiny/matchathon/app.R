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
            uiOutput("download_output")
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


    output$download_output = renderUI({
        req(input$faculty, input$students, dat())

    # output_list <- list(
    #   student_schedule=dat()$s_schedule,
    #   faculty_schedule=dat()$f_schedule,
    #   ranked_faculty_matches=dat()$ranked_faculty,
    #   ranked_faculty_score=dat()$ranked_faculty_score)
downloadButton("download01",'Download schedules')
    })

  output$download01 <- downloadHandler(
    filename='matchathon_schedules.xlsx',
    content=function(file) {
      write_xlsx(dat())
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
