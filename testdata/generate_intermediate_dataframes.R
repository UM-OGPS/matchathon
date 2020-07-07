library(tidyverse)
library(here)

source(here('R/match.R'))

# student interests
students = read_csv(here('testdata/students.csv'))
# faculty interests
faculty = read_csv(here('testdata/faculty.csv'))

meeting_slots = 12
min_fslots = 6

# match scores
match_scores = get_match_scores(students,faculty)
# ranked faculty for each student
ranked_faculty = rank_faculty(match_scores)
# student schedule (any number of faculty meetings)
s_schedule_orig = make_student_schedule(ranked_faculty$ranked_faculty,slots=meeting_slots)
# student schedule (minimum number of faculty meetings)
s_schedule_min = add_min_fac_meetings(s_schedule_orig,ranked_faculty$ranked_faculty,min_fac_mtg=min_fslots)
# faculty schedule
f_schedule = make_faculty_schedule(s_schedule_min)


