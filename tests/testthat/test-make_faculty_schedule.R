f_unavail <- structure(list(Faculty = c("Dunn, Savannah", "Dunn, Savannah", "Young, Thomas", "al-Majeed, Haaroon"), Slot = c(3, 4, 1, 1)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))

stu_sched_all_avail <- structure(list(`al-Abdul, Hameeda` = c("al-Majeed, Haaroon","Carter, Christopher"), `al-Allee, Ridwaan` = c("Carter, Christopher", "Nelson, Julia "), `Darity, Samantha` = c("Myers, Austin", "al-Majeed, Haaroon"), `Najera, Omar` = c("Nelson, Julia ", "Myers, Austin")), class = "data.frame", row.names = c(NA,-2L))

stu_sched_unavail <- structure(list(`al-Abdul, Hameeda` = c("Myers, Austin", "al-Majeed, Haaroon"), `Najera, Omar` = c("Nelson, Julia ", "Myers, Austin")), class = "data.frame", row.names = c(NA,-2L))

test_that('make_faculty_schedule works',{
  
  fn_result <- make_faculty_schedule(stu_sched_all_avail, f_unavail = NULL)
  expected_output <- structure(list(`al-Majeed, Haaroon` = c("al-Abdul, Hameeda", "Darity, Samantha"), `Carter, Christopher` = c("al-Allee, Ridwaan", "al-Abdul, Hameeda"), `Myers, Austin` = c("Darity, Samantha","Najera, Omar"), `Nelson, Julia ` = c("Najera, Omar", "al-Allee, Ridwaan")), class = "data.frame", row.names = c(NA, -2L))
  fn_result <- fn_result[,sort(names(fn_result))]
  expected_output <- expected_output[,sort(names(expected_output))]
  expect_equal(fn_result,expected_output)
  
  fn_result <- make_faculty_schedule(stu_sched_unavail, f_unavail = f_unavail)
  expected_output <- structure(list(`al-Majeed, Haaroon` = c("Unavailable", "al-Abdul, Hameeda"), `Myers, Austin` = c("al-Abdul, Hameeda", "Najera, Omar"), `Nelson, Julia ` = c("Najera, Omar", NA)), class = "data.frame", row.names = c(NA,-2L))
  fn_result <- fn_result[,sort(names(fn_result))]
  expected_output <- expected_output[,sort(names(expected_output))]
  expect_equal(fn_result,expected_output)
})