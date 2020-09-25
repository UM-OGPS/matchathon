# functions to perform matching

#' Get match scores
#'
#' @param students dataframe where each row is a student and each column is a research interest. First column is student names.
#' @param faculty dataframe where each row is a faculty member and each column is a research interest. First column is faculty names.
#'
#' @return dataframe where rows are students and columns are faculty and elements are a score of how well the student and faculty research interests align. Higher scores indicate more mutual research interests.
#' @export
get_match_scores = function(students, faculty){
  # keep only names and columns in both 
  fnames = unlist(faculty[,1])
  f_keep = colnames(faculty)[colnames(faculty) %in% colnames(students)[2:ncol(students)]]
  snames = unlist(students[,1])
  s_keep = colnames(students)[colnames(students) %in% colnames(faculty)[2:ncol(faculty)]]
  faculty = faculty %>% dplyr::select(f_keep)
  students = students %>% dplyr::select(s_keep)
  # matrix of student by faculty to populate with fraction overlap
  match_score = matrix(NA,nrow=nrow(students),ncol=nrow(faculty))
  # fraction of overlap between faculty and students
  for(i in 1:nrow(students)){
    stu = as.numeric(students[i,])
    for(j in 1:nrow(faculty)){
      fac = as.numeric(faculty[j,])
      # number of overlapping research interests normalized by total interests of students and faculty
      score = sum(stu+fac == 2)/(sum(stu)*sum(fac))
      match_score[i,j] = score
    }
  }
  match_score = data.frame(match_score)
  rownames(match_score) = snames
  colnames(match_score) = fnames
  return(match_score*10)
}
