# Matchathon
R Shiny app to match two sets of people based on mutual interests (e.g. students and faculty based on research interest)

Created for the Matchathon event for the Program in Biomedical Sciences (PIBS) incoming first-year student Matchathon event.

To help students find faculty members with mutual research interests, we use this algorithm to match first-year PhD students and faculty who are in search of trainees based on mutual research interests. Then these students and faculty meet during a two hour Matchathon event. At the event students meet individually with 12 different faculty members for 5 minutes each. 

## Input & Output

Input: 
1. csv (comma-separated values file) of faculty research interests (1=interest, 0=no interest) where the rows are the faculty and the columns are the interests.
1. csv (comma-separated values file) of student research interests (1=interest, 0=no interest) where the rows are the students and the columns are the interests.

The names of the research interest columns should be identical between the student and faculty files.

[This website](https://www.computerhope.com/issues/ch001356.htm) explains how you can create a csv by hand or from a Microsoft Excel spreadsheet. 

Output:
1. Top faculty matches for students (`top_matches.csv`)
1. Student schedule for the event (`student_schedule.csv`)
1. Faculty schedule for the event (`faculty_schedule.csv`)

## Example

Currently, you need to have RStudio downloaded on your computer to run this app. In the future, we will also host the app on a web server.

To run the app:
1. Download   `matchathon.R` and the test data (in the `testdata` directory).
1. Open `matchathon.R` in RStudio.
1. Click the 'Run' button. 
1. Input `testdata/faculty.csv` and `testdata/students.csv`.
1. Wait for it to run, and download the results if you would like. 

