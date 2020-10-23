# student interests
students_small <- dplyr::tibble(`student name`=c('Najera, Omar','al-Abdul, Hameeda','al-Allee, Ridwaan','Darity, Samantha'), Aging=c(0,1,1,1), Bacteriology=c(0,1,0,0),Behavior=c(1,0,1,0))
# faculty interests
faculty_small <- dplyr::tibble(`faculty name`=c('Nelson, Julia ','al-Majeed, Haaroon','Myers, Austin','Carter, Christopher'), Aging=c(0,1,1,0), Bacteriology=c(0,1,0,1),Behavior=c(1,0,1,1))

# match scores
match_scores = structure(list(`Nelson, Julia ` = c(10, 0, 5, 0), `al-Majeed, Haaroon` = c(0,5, 2.5, 5), `Myers, Austin` = c(5, 2.5, 5, 5), `Carter, Christopher` = c(5,2.5, 2.5, 0)), class = "data.frame", row.names = c("Najera, Omar", "al-Abdul, Hameeda", "al-Allee, Ridwaan", "Darity, Samantha")) #get_match_scores(students_small,faculty_small)
# ranked faculty for each student
ranked_faculty = list(ranked_faculty = structure(c("Nelson, Julia ", "Myers, Austin","Carter, Christopher", "al-Majeed, Haaroon", "al-Majeed, Haaroon","Myers, Austin", "Carter, Christopher", "Nelson, Julia ", "Nelson, Julia ","Myers, Austin", "al-Majeed, Haaroon", "Carter, Christopher", "al-Majeed, Haaroon", "Myers, Austin", "Nelson, Julia ", "Carter, Christopher"), .Dim = c(4L, 4L), .Dimnames = list(NULL, c("Najera, Omar","al-Abdul, Hameeda", "al-Allee, Ridwaan", "Darity, Samantha"))), 
ranked_faculty_score = structure(c(10, 5, 5, 0, 5, 2.5, 2.5, 
                                   0, 5, 5, 2.5, 2.5, 5, 5, 0, 0), .Dim = c(4L, 4L), .Dimnames = list(
                                     NULL, c("Najera, Omar", "al-Abdul, Hameeda", "al-Allee, Ridwaan", 
                                             "Darity, Samantha")))) #rank_faculty(match_scores)

# already met
am <- dplyr::tibble(
  Student = c('al-Abdul, Hameeda','al-Abdul, Hameeda','al-Allee, Ridwaan'),
  Faculty = c('Dunn, Savannah','Young, Thomas','Myers, Austin'))

am_wrong <- dplyr::tibble(
  Student = c('al-Abdul, Hameeda','al-Abdul, Hameeda',''),
  Faculty = c('Dunn, Savannah','Young, Thomas','Myers, Austin'))

test_that('already_met works',{
  expect_true(already_met(am, ranked_faculty, faculty_small)$ranked_faculty[4,'al-Allee, Ridwaan'] == "Myers, Austin")
  expect_true(already_met(am, ranked_faculty, faculty_small)$ranked_faculty_score[4,'al-Allee, Ridwaan'] == 5.0)
  expect_error(already_met(am_wrong, ranked_faculty, faculty_small), 'The student names in already met and ranked faculty don\'t match')
})
