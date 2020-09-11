f_unavail <- structure(list(Faculty = c("Dunn, Savannah", "Dunn, Savannah", "Young, Thomas", "al-Majeed, Haaroon"), Slot = c(3, 4, 1, 10)), row.names = c(NA, -4L), class = c("tbl_df", "tbl", "data.frame"))

test_that('check_fac_avail works',{
  expect_true(check_fac_avail(NULL))
  expect_true(check_fac_avail(f_unavail, "Dunn, Savannah", 1))
  expect_false(check_fac_avail(f_unavail, "Dunn, Savannah", 3))
})
