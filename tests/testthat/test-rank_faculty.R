# student interests
students_small <- dplyr::tibble(`student name`=c('Najera, Omar','al-Abdul, Hameeda','al-Allee, Ridwaan','Darity, Samantha'), Aging=c(0,1,1,1), Bacteriology=c(0,1,0,0),Behavior=c(1,0,1,0))
# faculty interests
faculty_small <- dplyr::tibble(`faculty name`=c('Nelson, Julia ','al-Majeed, Haaroon','Myers, Austin','Carter, Christopher'), Aging=c(0,1,1,0), Bacteriology=c(0,1,0,1),Behavior=c(1,0,1,1))

# match scores
match_scores = structure(list(`Nelson, Julia ` = c(10, 0, 5, 0), `al-Majeed, Haaroon` = c(0,5, 2.5, 5), `Myers, Austin` = c(5, 2.5, 5, 5), `Carter, Christopher` = c(5,2.5, 2.5, 0)), class = "data.frame", row.names = c("Najera, Omar", "al-Abdul, Hameeda", "al-Allee, Ridwaan", "Darity, Samantha")) #get_match_scores(students_small,faculty_small)
# ranked faculty for each student
ranked_faculty = list(ranked_faculty = structure(c("Nelson, Julia ", "Myers, Austin","Carter, Christopher", "al-Majeed, Haaroon", "al-Majeed, Haaroon","Myers, Austin", "Carter, Christopher", "Nelson, Julia ", "Nelson, Julia ","Myers, Austin", "al-Majeed, Haaroon", "Carter, Christopher", "al-Majeed, Haaroon", "Myers, Austin", "Nelson, Julia ", "Carter, Christopher"), .Dim = c(4L, 4L), .Dimnames = list(NULL, c("Najera, Omar","al-Abdul, Hameeda", "al-Allee, Ridwaan", "Darity, Samantha"))), ranked_faculty_score = structure(c(10, 5, 5, 0, 5, 2.5, 2.5,0, 5, 5, 2.5, 2.5, 5, 5, 0, 0), .Dim = c(4L, 4L), .Dimnames = list(NULL, c("Najera, Omar", "al-Abdul, Hameeda", "al-Allee, Ridwaan","Darity, Samantha")))) #rank_faculty(match_scores)

test_that('rank_faculty works',{
  expect_equal(rank_faculty(match_scores), ranked_faculty)
})