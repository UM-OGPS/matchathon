# faculty interests
faculty_small <- dplyr::tibble(`faculty name`=c('Nelson, Julia ','al-Majeed, Haaroon','Myers, Austin','Carter, Christopher'), Aging=c(0,1,1,0), Bacteriology=c(0,1,0,1),Behavior=c(1,0,1,1))

f_unavail <- structure(list(Faculty = c("Dunn, Savannah", "Dunn, Savannah", "Young, Thomas", "al-Majeed, Haaroon"), Slot = c(3, 4, 1, 10)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))

f_sched <- structure(list(`al-Majeed, Haaroon` = c("al-Abdul, Hameeda", "Darity, Samantha", "al-Allee, Ridwaan"), `Carter, Christopher` = c("al-Allee, Ridwaan", "al-Abdul, Hameeda", "Najera, Omar"), `Dunn, Savannah` = c(NA, NA, "Unavailable"), `Myers, Austin` = c("Darity, Samantha", "Najera, Omar", "al-Abdul, Hameeda"), `Nelson, Julia ` = c("Najera, Omar", "al-Allee, Ridwaan","Darity, Samantha"), `Young, Thomas` = c("Unavailable", NA, NA)), class = "data.frame", row.names = c(NA, -3L))

test_that('fac_unavail works',{
  expect_equal(add_fac_unavail(f_unavail,f_sched), structure(list(`al-Majeed, Haaroon` = c("al-Abdul, Hameeda", "Darity, Samantha", "al-Allee, Ridwaan"), `Carter, Christopher` = c("al-Allee, Ridwaan","al-Abdul, Hameeda", "Najera, Omar"), `Dunn, Savannah` = c(NA, NA, "Unavailable"), `Myers, Austin` = c("Darity, Samantha", "Najera, Omar","al-Abdul, Hameeda"), `Nelson, Julia ` = c("Najera, Omar", "al-Allee, Ridwaan", "Darity, Samantha"), `Young, Thomas` = c("Unavailable", NA, NA)), row.names = c(NA, -3L), class = "data.frame"))
})
