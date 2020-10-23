#' Add minimum nuber of faculty meetings
#'
#' @param s_schedule student schedule 
#' @param ranked_faculty data frame of ranked faculty
#' @param min_fac_mtg minimum number of faculty meetings
#' @param f_unavail dataframe of faculty unavailability (name, time slot)
#'
#' @return updated student schedule
#' @export
add_min_fac_meetings = function(s_schedule,ranked_faculty,min_fac_mtg=nrow(s_schedule)/2,f_unavail=NULL){
  # # count number of meetings each faculty member has
  # mtg_ct = table(unlist(s_schedule))
  # # order so the ones with the most come first
  # mtg_ct = sort(mtg_ct,decreasing = T)
  # # find faculty with no empty slots
  # too_many = names(mtg_ct)[mtg_ct == nrow(s_schedule)]
  # # find faculty members with too few meetings
  # too_few = names(mtg_ct)[mtg_ct < min_fac_mtg]
  fac <- unique(unlist(c(ranked_faculty)))
  s_sched_modified = s_schedule
  ct = 0
  # count number of meetings each faculty member has
  mtg_ct = table(factor(unlist(s_sched_modified),levels=fac))
  check <- min(table(factor(unlist(s_sched_modified),levels=fac))) < min_fac_mtg
  while(check){
    # order so the ones with the most come first
    mtg_ct = sort(table(factor(unlist(s_sched_modified),levels=fac)),decreasing = T)
    # find faculty members with too few meetings
    too_few = names(mtg_ct)[mtg_ct < min_fac_mtg]
    for(f in too_few){
      # get which indices in array are given faculty member
      m_inds = which(ranked_faculty == f)
      # first column is array index, second column is column number
      rc_inds = arrayInd(m_inds,dim(ranked_faculty))
      # order by array index (i.e. by students that match best with faculty member)
      rc_inds = rc_inds[order(rc_inds[,1]),]
      # loop over column indices (students)
      for(j in rc_inds[,2]){
        # get student name
        s = colnames(ranked_faculty)[j]
        if(!s %in% names(s_sched_modified)) next
        # check if student already meeting with faculty
        if(f %in% s_sched_modified[,s]) next
        # find worst match for student 
        current_ranks = which(ranked_faculty[,s] %in% s_schedule[,s])
        if(length(current_ranks)-ct < 1) break 
        worst_match = current_ranks[length(current_ranks)-ct]
        worst_match = ranked_faculty[,s][worst_match]
        # check if worst match already has too few meetings
        if(table(unlist(s_sched_modified))[worst_match] <= min_fac_mtg) next
        wm_ind = which(s_schedule[,s] == worst_match)
        # see if faculty is available at that time
        f_avail = check_fac_avail(f_unavail, f, wm_ind)
        # if faculty is unavailable, go to next time slot
        if(!f_avail) next
        # check to see if facutly is already scheduled for that time slot
        f_scheduled = f %in% s_sched_modified[wm_ind,]
        # if faculty not scheduled for that time slot, replace student meeting with that faculty
        if(!f_scheduled){
          s_sched_modified[wm_ind,s] = f
        }
        # stop when faculty has enough meetings
        if(sum(s_sched_modified == f) >= min_fac_mtg) break
      }
    }
    ct = ct + 1
    tab <- table(factor(unlist(s_sched_modified),levels=fac))
    # for now, break after 100 iterations so it's not an infinite loop (might mean some faculty don't have enough meetings, so maybe good to change this eventually)
    if(ct > 100) break
  }
  
  return(s_sched_modified)
}
