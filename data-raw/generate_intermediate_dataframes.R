library(tidyverse)
library(here)

source(here('R/matchathon.R'))

# student interests
students = read_csv(here('data-raw/students.csv'))
# faculty interests
faculty = read_csv(here('data-raw/faculty.csv'))

meeting_slots = 12
min_fslots = 6

# optional input data
f_unavail = read_csv('data-raw/unavailable_faculty_times.csv')

# match scores
match_scores = get_match_scores(students,faculty)
# ranked faculty for each student
ranked_faculty = rank_faculty(match_scores)
# student schedule (any number of faculty meetings)
s_schedule_orig = make_student_schedule(ranked_faculty$ranked_faculty,slots=meeting_slots,f_unavail=f_unavail)
# faculty schedule(any number of faculty meetings)
f_schedule_orig = make_faculty_schedule(s_schedule_orig)
# student schedule (minimum number of faculty meetings)
s_schedule_min = add_min_fac_meetings(s_schedule_orig,ranked_faculty$ranked_faculty,min_fac_mtg=min_fslots,f_unavail=f_unavail)
# faculty schedule
f_schedule = make_faculty_schedule(s_schedule_min)

# smaller ones for testing
faculty_small <- faculty[1:8,1:4]
students_small <- dplyr::tibble(`student name`=c('Najera, Omar','al-Abdul, Hameeda','al-Allee, Ridwaan','Darity, Samantha'), Aging=c(0,1,1,1), Bacteriology=c(0,1,0,0),Behavior=c(1,0,1,0))
faculty_small <- dplyr::tibble(`faculty name`=c('Nelson, Julia ','al-Majeed, Haaroon','Myers, Austin','Carter, Christopher'), Aging=c(0,1,1,0), Bacteriology=c(0,1,0,1),Behavior=c(1,0,1,1))

# match scores
match_scores = get_match_scores(students_small,faculty_small)
# ranked faculty for each student
ranked_faculty = rank_faculty(match_scores)
# student schedule (any number of faculty meetings)
s_schedule_orig = make_student_schedule(ranked_faculty$ranked_faculty,slots=3,f_unavail=f_unavail)
# faculty schedule(any number of faculty meetings)
f_schedule_orig = make_faculty_schedule(s_schedule_orig,f_unavail = f_unavail)
# student schedule (minimum number of faculty meetings)
s_schedule_min = add_min_fac_meetings(s_schedule_orig,ranked_faculty$ranked_faculty,min_fac_mtg=min_fslots,f_unavail=f_unavail)
# faculty schedule
f_schedule = make_faculty_schedule(s_schedule_min)