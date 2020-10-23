#' Add faculty unavailability to schedule
#'
#' @param f_unavail dataframe of faculty unavailability (columns: Faculty, Slot)
#' @param f_schedule faculty schedule (generated with make_faculty_schedule)
#'
#' @return faculty schedule with unavailability documented
#' @export
add_fac_unavail = function(f_unavail,f_schedule){
  f_unavail <- f_unavail[f_unavail$Slot <= nrow(f_schedule),]
  if(!is.null(f_unavail)){
    for(row in 1:nrow(f_unavail)){
      fac = as.character(f_unavail[row,'Faculty'])
      slot = as.numeric(f_unavail[row,'Slot'])
      f_schedule[slot,fac] = 'Unavailable'
    }
  }
  return(f_schedule)
}