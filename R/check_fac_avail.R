#' Check faculty availability
#'
#' @param f_unavail dataframe of faculty unavailability (columns: Faculty, Slots)
#' @param f_name name of faculty to check
#' @param slot time slot to check
#'
#' @return
#' @export
check_fac_avail = function(f_unavail, f_name, slot){
  if(is.null(f_unavail)){
    f_avail = TRUE
  }else{
    f_un = f_unavail %>% dplyr::filter(Faculty == f_name) %>% dplyr::filter(Slot == slot)
    f_avail = nrow(f_un) == 0
  }
  return(f_avail)
}