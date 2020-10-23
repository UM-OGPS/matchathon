#' Matchathon master function
#'
#' @param faculty_df csv of faculty interests
#' @param student_df csv of student interests
#' @param meeting_slots number of meeting slots 
#' @param min_fslots minimum number of faculty meeting slots not empty
#' @param f_unavail_df dataframe of faculty unavailability (name, time slot)
#' @param already_met_df dataframe of faculty students have already met with (student name, faculty name)
#'
#' @return faculty and student schedules and ranked faculty lists for students
#' @export
matchathon = function(faculty_df,student_df,meeting_slots=12,min_fslots=NULL,f_unavail_df=NULL,already_met_df=NULL){
  n = 2
  #shiny::withProgress(message = '', value = 0, {
  # get minimum number of faculty meetings
  if(is.null(min_fslots)) min_fslots = meeting_slots/2
  if(min_fslots > meeting_slots) min_fslots = meeting_slots
  
  f = faculty_df
  s = student_df
  
  # keep only names and columns in both 
  f_keep = c(names(f)[1],names(f)[names(f) %in% names(s)])
  f = f %>% dplyr::select(f_keep)
  s_keep = c(names(s)[1],names(s)[names(s) %in% names(f)])
  s = s %>% dplyr::select(s_keep)
  # get match scores
  #incProgress(0, detail = 'Getting match scores (1st of 3 tasks)')
  match_scores = get_match_scores(s,f)
  ranked_faculty = rank_faculty(match_scores)
  ranked_faculty_orig <- ranked_faculty
  if(!is.null(already_met_df)){
    ranked_faculty <- already_met(already_met_df, ranked_faculty, f)
  }
  #incProgress(1/n, detail = 'Creating student schedule (2nd of 3 tasks)')
  s_schedule = make_student_schedule(ranked_faculty$ranked_faculty,slots=meeting_slots,f_unavail=f_unavail_df)
  s_schedule = add_min_fac_meetings(s_schedule,ranked_faculty$ranked_faculty,min_fac_mtg=min_fslots,f_unavail=f_unavail_df)
  #incProgress(1/n, detail = 'Creating faculty schedule (3rd of 3 tasks)')
  f_schedule = make_faculty_schedule(s_schedule,f_unavail=f_unavail_df)
  #})
  return(list(ranked_faculty=ranked_faculty_orig$ranked_faculty,
              ranked_faculty_score=ranked_faculty_orig$ranked_faculty_score,
              s_schedule=s_schedule,
              f_schedule=f_schedule))
}