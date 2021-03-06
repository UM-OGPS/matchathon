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
  faculty = faculty[!is.na(faculty)]
  schedule = data.frame(matrix(NA,nrow=nrow(student_schedule),
                               ncol=length(faculty)))
  colnames(schedule) = faculty
  for(f in faculty){
    for(s in colnames(student_schedule)){
      stu_fac_match = student_schedule[,s] == f
      if(sum(stu_fac_match,na.rm=TRUE) > 0){
        schedule[which(stu_fac_match),f] = s
      }
    }
  }
  if(!is.null(f_unavail)){
    f_unavail <- f_unavail[unlist(c(f_unavail[,1])) %in% faculty,]
    if(nrow(f_unavail) != 0){
      schedule = add_fac_unavail(f_unavail,schedule)
    } 
  }
  return(schedule[,order(colnames(schedule))])
}