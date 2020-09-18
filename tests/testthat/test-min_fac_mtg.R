stu_sched_all_avail <- structure(list(`al-Abdul, Hameeda` = c("al-Majeed, Haaroon","Carter, Christopher"), `al-Allee, Ridwaan` = c("Carter, Christopher", "Nelson, Julia "), `Darity, Samantha` = c("Myers, Austin", "al-Majeed, Haaroon"), `Najera, Omar` = c("Nelson, Julia ", "Myers, Austin")), class = "data.frame", row.names = c(NA,-2L))[,1:2]

stu_sched_unavail <- structure(list(`al-Abdul, Hameeda` = c("Myers, Austin", "al-Majeed, Haaroon"), `Najera, Omar` = c("Nelson, Julia ", "Myers, Austin")), class = "data.frame", row.names = c(NA,-2L))

ranked_faculty = list(ranked_faculty = structure(c("Nelson, Julia ", "Myers, Austin","Carter, Christopher", "al-Majeed, Haaroon", "al-Majeed, Haaroon","Myers, Austin", "Carter, Christopher", "Nelson, Julia ", "Nelson, Julia ","Myers, Austin", "al-Majeed, Haaroon", "Carter, Christopher", "al-Majeed, Haaroon", "Myers, Austin", "Nelson, Julia ", "Carter, Christopher"), .Dim = c(4L, 4L), .Dimnames = list(NULL, c("Najera, Omar","al-Abdul, Hameeda", "al-Allee, Ridwaan", "Darity, Samantha"))), ranked_faculty_score = structure(c(10, 5, 5, 0, 5, 2.5, 2.5,0, 5, 5, 2.5, 2.5, 5, 5, 0, 0), .Dim = c(4L, 4L), .Dimnames = list(NULL, c("Najera, Omar", "al-Abdul, Hameeda", "al-Allee, Ridwaan","Darity, Samantha"))))

f_unavail <- structure(list(Faculty = c("Dunn, Savannah", "Dunn, Savannah", "Young, Thomas", "al-Majeed, Haaroon"), Slot = c(3, 4, 1, 1)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))

test_that("add_min_fac_meetings works", {
  expect_equal(add_min_fac_meetings(stu_sched_all_avail, ranked_faculty$ranked_faculty),
               structure(list(`al-Abdul, Hameeda` = c("al-Majeed, Haaroon", "Myers, Austin"), `al-Allee, Ridwaan` = c("Carter, Christopher", "Nelson, Julia ")), row.names = c(NA, -2L), class = "data.frame"))
  expect_equal(add_min_fac_meetings(stu_sched_unavail, ranked_faculty$ranked_faculty, f_unavail = f_unavail), structure(list(`al-Abdul, Hameeda` = c("Myers, Austin", "al-Majeed, Haaroon"), `Najera, Omar` = c("Nelson, Julia ", "Carter, Christopher")), row.names = c(NA, -2L), class = "data.frame")) 
})