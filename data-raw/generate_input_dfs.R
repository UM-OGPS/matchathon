# code to prepare example data

all_data <- read_in_data('data-raw/faculty.csv','data-raw/students.csv','data-raw/unavailable_faculty_times.csv','data-raw/already_met.csv')
faculty_interests <- all_data$faculty
student_interests <- all_data$student
faculty_unavailable <- all_data$f_unavail
already_met <- all_data$already_met

usethis::use_data(faculty_interests, overwrite = TRUE)
usethis::use_data(student_interests, overwrite = TRUE)
usethis::use_data(faculty_unavailable, overwrite = TRUE)
usethis::use_data(already_met, overwrite = TRUE)
usethis::use_data(all_data, overwrite = TRUE)
