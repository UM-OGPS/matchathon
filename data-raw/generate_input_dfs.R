# code to prepare example data

dfs <- read_in_data('data-raw/faculty.csv','data-raw/students.csv','data-raw/unavailable_faculty_times.csv','data-raw/already_met.csv')
faculty_interests <- dfs$faculty
student_interests <- dfs$student
faculty_unavailable <- dfs$f_unavail
already_met <- dfs$already_met

usethis::use_data(faculty_interests, overwrite = TRUE)
usethis::use_data(student_interests, overwrite = TRUE)
usethis::use_data(faculty_unavailable, overwrite = TRUE)
usethis::use_data(already_met, overwrite = TRUE)
