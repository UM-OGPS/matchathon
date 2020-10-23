
# student interests
students_small <- dplyr::tibble(`student name`=c('Najera, Omar','al-Abdul, Hameeda','al-Allee, Ridwaan','Darity, Samantha'), Aging=c(0,1,1,1), Bacteriology=c(0,1,0,0),Behavior=c(1,0,1,0))
# faculty interests
faculty_small <- dplyr::tibble(`faculty name`=c('Nelson, Julia ','al-Majeed, Haaroon','Myers, Austin','Carter, Christopher'), Aging=c(0,1,1,0), Bacteriology=c(0,1,0,1),Behavior=c(1,0,1,1))

am <- dplyr::tibble(
  Student = c('al-Abdul, Hameeda','al-Abdul, Hameeda','al-Allee, Ridwaan'),
  Faculty = c('Dunn, Savannah','Young, Thomas','Myers, Austin'))

f_unavail <- structure(list(Faculty = c("Dunn, Savannah", "Dunn, Savannah", "Young, Thomas", "al-Majeed, Haaroon"), Slot = c(3, 4, 1, 1)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))


test_that("matchathon works",{
  fn_output <- matchathon(faculty_small, students_small, 2, 1, already_met_df = am, f_unavail_df = f_unavail)
  fn_output$s_schedule <- fn_output$s_schedule[,sort(names(fn_output$s_schedule))]
  fn_output$f_schedule <- fn_output$f_schedule[,sort(names(fn_output$f_schedule))]
  expected_output <- list(ranked_faculty = structure(c("Nelson, Julia ", "Myers, Austin", "Carter, Christopher", "al-Majeed, Haaroon", "al-Majeed, Haaroon",  "Myers, Austin", "Carter, Christopher", "Nelson, Julia ", "Nelson, Julia ", "Myers, Austin", "al-Majeed, Haaroon", "Carter, Christopher", "al-Majeed, Haaroon", "Myers, Austin", "Nelson, Julia ", "Carter, Christopher"), .Dim = c(4L, 4L), .Dimnames = list(NULL, c("Najera, Omar", "al-Abdul, Hameeda", "al-Allee, Ridwaan", "Darity, Samantha"))),ranked_faculty_score = structure(c(10, 5, 5, 0, 5, 2.5, 2.5, 0, 5, 5, 2.5, 2.5, 5, 5, 0, 0), .Dim = c(4L, 4L), .Dimnames = list(NULL, c("Najera, Omar", "al-Abdul, Hameeda", "al-Allee, Ridwaan", "Darity, Samantha"))), s_schedule = structure(list(`al-Abdul, Hameeda` = c("Myers, Austin", "al-Majeed, Haaroon"), `al-Allee, Ridwaan` = c("Carter, Christopher", "Nelson, Julia "), `Darity, Samantha` = c(NA, "Carter, Christopher"), `Najera, Omar` = c("Nelson, Julia ", "Myers, Austin")), class = "data.frame", row.names = c(NA,-2L)), f_schedule = structure(list(`al-Majeed, Haaroon` = c("Unavailable","al-Abdul, Hameeda"), `Carter, Christopher` = c("al-Allee, Ridwaan", "Darity, Samantha"), `Myers, Austin` = c("al-Abdul, Hameeda","Najera, Omar"), `Nelson, Julia ` = c("Najera, Omar", "al-Allee, Ridwaan")), class = "data.frame", row.names = c(NA, -2L)))
  expected_output$s_schedule <- expected_output$s_schedule[,sort(names(expected_output$s_schedule))]
  expected_output$f_schedule <- expected_output$f_schedule[,sort(names(expected_output$f_schedule))]
  expect_equal(fn_output, expected_output)
  
  fn_output <- matchathon(faculty_small, students_small, 2, NULL, already_met_df = am, f_unavail_df = f_unavail)
  fn_output$s_schedule <- fn_output$s_schedule[,sort(names(fn_output$s_schedule))]
  fn_output$f_schedule <- fn_output$f_schedule[,sort(names(fn_output$f_schedule))]
  expect_equal(fn_output,expected_output)
  
  fn_output <- matchathon(faculty_small, students_small, 2, 3, already_met_df = am, f_unavail_df = f_unavail)
  fn_output$s_schedule <- fn_output$s_schedule[,sort(names(fn_output$s_schedule))]
  fn_output$f_schedule <- fn_output$f_schedule[,sort(names(fn_output$f_schedule))]
  expect_equal(fn_output,expected_output)
})