#' Read in csvs
#'
#' @param faculty_csv csv of faculty interests
#' @param student_csv csv of student interests
#' @param f_unavail_csv csv of faculty unavailability
#' @param already_met_csv csv of faculty students have already met with
#'
#' @return list with dataframes for each csv
#' @export
read_in_data <- function(faculty_csv,student_csv,f_unavail_csv=NULL,already_met_csv=NULL){
  # read in faculty data
  f <- readr::read_csv(faculty_csv)
  # read in student data
  s <- readr::read_csv(student_csv)
  # read in faculty availability
  f_unavail <- NULL
  if(!is.null(f_unavail_csv)){
    f_unavail <- readr::read_csv(f_unavail_csv)
    f_unavail
  }
  already_met <- NULL
  if(!is.null(already_met_csv)){
    already_met <- readr::read_csv(already_met_csv)
  }
  return(list(faculty=f,student=s,f_unavail=f_unavail,already_met=already_met))
}