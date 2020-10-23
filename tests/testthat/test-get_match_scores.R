# student interests
students_small <- dplyr::tibble(`student name`=c('Najera, Omar','al-Abdul, Hameeda','al-Allee, Ridwaan','Darity, Samantha'), Aging=c(0,1,1,1), Bacteriology=c(0,1,0,0),Behavior=c(1,0,1,0))
# faculty interests
faculty_small <- dplyr::tibble(`faculty name`=c('Nelson, Julia ','al-Majeed, Haaroon','Myers, Austin','Carter, Christopher'), Aging=c(0,1,1,0), Bacteriology=c(0,1,0,1),Behavior=c(1,0,1,1))

test_that('get_match_scores works',{
  expect_equal(get_match_scores(students_small, faculty_small), {
    structure(list(`Nelson, Julia ` = c(10, 0, 5, 0), `al-Majeed, Haaroon` = c(0,5, 2.5, 5), `Myers, Austin` = c(5, 2.5, 5, 5), `Carter, Christopher` = c(5,2.5, 2.5, 0)), class = "data.frame", row.names = c("Najera, Omar","al-Abdul, Hameeda", "al-Allee, Ridwaan", "Darity, Samantha"))
  })
})