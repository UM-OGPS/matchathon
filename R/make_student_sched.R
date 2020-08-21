#' Make student schedule
#'
#' @param ranked_faculty dataframe returned from rank_faculty
#' @param slots number of slots in schedule
#' @param f_unavail data frame of whether faculty are available to meet (columns: Faculty, Slot)
#'
#' @return student schedule
#' @export
make_student_schedule = function(ranked_faculty,slots=12,f_unavail=NULL){
  # set seed so results are same each time
  set.seed(0)
  # initialize schedule
  schedule = data.frame(matrix(NA,nrow=slots,ncol=ncol(ranked_faculty)))
  colnames(schedule) = colnames(ranked_faculty)
  # loop over ordered matches based on student
  for(r in 1:nrow(ranked_faculty)){
    # randomly order the students so there's no bias towards e.g. students with names at the beginning of the alphabet
    stus = sample(colnames(ranked_faculty))
    # loop over students
    for(s in stus){
      # get faculty match for that student
      f = ranked_faculty[r,s]
      # get schedule for that student
      s_scheduled = schedule[,s]
      # identify empty spots for student
      s_empty = which(is.na(s_scheduled))
      if(length(s_empty) == 0) next 
      for(i in s_empty){
        # see if faculty is available at that time
        f_avail = check_fac_avail(f_unavail, f, i)
        # if faculty is unavailable, go to next time slot
        if(!f_avail) next
        # see if faculty already has meeting during that time
        f_scheduled = f %in% schedule[i,]
        # if faculty doesn't already have meeting, schedule faculty meeting with student
        if(!f_scheduled){
          schedule[i,s] = f
          break
        }
      }
    }
  }
  return(schedule[,order(colnames(schedule))])
}