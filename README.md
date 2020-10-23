# Matchathon
R Shiny app to match two sets of people based on mutual interests (e.g. students and faculty based on research interest)

Created for the Matchathon event for the Program in Biomedical Sciences (PIBS) incoming first-year student Matchathon event.

To help students find faculty members with mutual research interests, we use this algorithm to match first-year PhD students and faculty who are in search of trainees based on mutual research interests. Then these students and faculty meet during a two hour Matchathon event. At the event students meet individually with 12 different faculty members for 5 minutes each. 

**Now you can use this app from the web without needing to download R or RStudio! \
Just go to https://zenalapp.shinyapps.io/matchathon/**

## Input & Output

Input: 
1. csv (comma-separated values file) of faculty research interests (1=interest, 0=no interest) where the rows are the faculty and the columns are the interests.
1. csv (comma-separated values file) of student research interests (1=interest, 0=no interest) where the rows are the students and the columns are the interests.

The names of the research interest columns should be identical between the student and faculty files.

[This website](https://www.computerhope.com/issues/ch001356.htm) explains how you can create a csv by hand or from a Microsoft Excel spreadsheet. 

Output that you can download:
1. Top faculty matches for students (`top_matches.csv`)
1. Rank values of matches (`ranked_faculty_valules.csv`; note: these are somewhat difficult to interpret)
1. Student schedule for the event (`student_schedule.csv`)
1. Faculty schedule for the event (`faculty_schedule.csv`)


## Example

Currently, you need to have RStudio downloaded on your computer to run this app. 

To run the app on the web:
1. Go to: https://zenalapp.shinyapps.io/matchathon/.
1. Input your own faculty and student data, or the data in `testdata`.
1. Click the 'Get matches & schedules' button, and download the results if you would like. 

To run the app on your computer:
1. Download [R](https://www.r-project.org/) and [RStudio](https://rstudio.com/products/rstudio/download/) to your computer. 
1. Open RStudio and install the shiny and tidyverse packages: `install.packages(c("shiny", "tidyverse"))`
1. Download   `matchathon.R` and the test data (in the `testdata` directory).
1. Open `matchathon.R` in RStudio.
1. Click the 'Run' button. 
1. Input `testdata/faculty.csv` and `testdata/students.csv`.
1. Click the 'Get matches & schedules' button, and download the results if you would like. 

