#' Make faculty schedule
#'
#' @param student_schedule student schedule (generated with make_student_schedule)
#' @param f_unavail dataframe of faculty unavailability (name, time slot)
#'
#' @return faculty schedule
#' @export
make_faculty_schedule = function(student_schedule, f_unavail){
  # initialize schedule
  faculty = unique(unname(unlist(student_schedule)))
  schedule = data.frame(matrix(NA,nrow=nrow(student_schedule),
                               ncol=length(faculty)))
  colnames(schedule) = faculty
  for(f in faculty){
    for(s in colnames(student_schedule)){
      stu_fac_match = student_schedule[,s] == f
      if(sum(stu_fac_match) > 0){
        schedule[which(stu_fac_match),f] = s
      }
    }
  }
  schedule = add_fac_unavail(f_unavail,schedule)
  return(schedule[,order(colnames(schedule))])
}