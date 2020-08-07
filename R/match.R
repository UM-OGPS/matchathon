# functions to perform matching

#' Get match scores
#'
#' @param students dataframe where each row is a student and each column is a research interest. First column is student names.
#' @param faculty dataframe where each row is a faculty member and each column is a research interest. First column is faculty names.
#' @param already_met named list of students with faculty the students have met as elements in the list.
#'
#' @return dataframe where rows are students and columns are faculty and elements are a score of how well the student and faculty research interests align. Higher scores indicate more mutual research interests.
#' @export
#'
#' @examples
get_match_scores = function(students, faculty, already_met=NULL){
  # keep only names and columns in both 
  fnames = unlist(faculty[,1])
  f_keep = colnames(faculty)[colnames(faculty) %in% colnames(students)[2:ncol(students)]]
  snames = unlist(students[,1])
  s_keep = colnames(students)[colnames(students) %in% colnames(faculty)[2:ncol(faculty)]]
  faculty = faculty %>% select(f_keep)
  students = students %>% select(s_keep)
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

#' Rank faculty for each student 
#'
#' @param match_score dataframe returned by get_match_scores
#'
#' @return For each student (column), faculty with mutual interests ordered by match score.
#' @export
#'
#' @examples
rank_faculty = function(match_score){
  ranked_faculty = apply(match_score,1,function(x){
    colnames(match_score)[order(x,decreasing = T)]
  })
  ranked_faculty_score = apply(match_score,1,function(x){
    x[order(x,decreasing = T)]
  })
  # ranked_faculty = apply(match_score,1,function(x){
  #   x[order(x,decreasing = T)]
  # })
  return(list(ranked_faculty=ranked_faculty,
              ranked_faculty_score=ranked_faculty_score))
}

#' Remove faculty the student already met with
#'
#' @param am dataframe with student names as the first column and faculty names as the second column. Student names can be repeated.
#' @param ranked_faculty dataframe with student names as column names
#' @param faculty dataframe where each row is a faculty member and each column is a research interest. First column is faculty names. 
#'
#' @return
#' @export
#'
#' @examples
already_met = function( am, rf, faculty ){
  
  #first lets convert faculty names to lowercase to remove case sensitivity
  #shouldn't need to do this for student names ( right? ) since they're from the same form (ie the students don't have a chance to type diff names )
  faculty_names = faculty %>% pull( 1 )
  faculty_names_l = tolower( faculty_names )
  am_fac_l = tolower( am %>% pull( 2 ) )
  
  #copy already met so we don't screw around with original
  #am_c = am
  
  #extract student name headers from ranked faculty mat
  student_names_col = dimnames( rf$ranked_faculty )[[ 2 ]]
  
  #get a vector indicating columns of ranked_faculty corresponding to student names in already met
  am = am %>% add_column( rf_col_num = match( ( am %>% pull( 1 ) ), student_names_col ) )
  
  #if the student names don't match this is a bust - throw an error
  #maybe this functionality could be better and throw a warning instead but for now stop the process
  if( any( is.na( am %>% pull( rf_col_num ) ) ) ){
    stop( "The student names in already met and ranked faculty don't match" )
  }
  
  num_faculty = dim( rf$ranked_faculty )[ 1 ]
  for( i in 1:length( am_fac_l ) ) {
    
    if ( am_fac_l[i] %in% faculty_names_l ){
      rf_col = am[ i, 3 ][[ 1 ]]
      fname = faculty_names[ which( faculty_names_l == am_fac_l[i] ) ]
      frow_in_rf = which( rf$ranked_faculty[ , rf_col ] == fname )
      rf_resorted_col = c( rf$ranked_faculty[ 1:( frow_in_rf - 1 ), rf_col ], 
                        rf$ranked_faculty[ ( frow_in_rf + 1 ):num_faculty, rf_col ], 
                        rf$ranked_faculty[ frow_in_rf, rf_col ] )
      rf$ranked_faculty[ , rf_col ] = rf_resorted_col
      rfs_resorted_col = c( rf$ranked_faculty_score[ 1:( frow_in_rf - 1 ), rf_col ], 
                           rf$ranked_faculty_score[ ( frow_in_rf + 1 ):num_faculty, rf_col ], 
                           rf$ranked_faculty_score[ frow_in_rf, rf_col ] )
      rf$ranked_faculty_score[ , rf_col ] = rfs_resorted_col
    }else{
      print( 'Faculty name not found' )
    }
    
  }
  
  return( rf )
}

#' Check faculty availability
#'
#' @param f_unavail dataframe of faculty unavailability (columns: Faculty, Slots)
#' @param f_name name of faculty to check
#' @param slot time slot to check
#'
#' @return
#' @export
#'
#' @examples
check_fac_avail = function(f_unavail, f_name, slot){
  if(is.null(f_unavail)){
    f_avail = TRUE
  }else{
    f_un = f_unavail %>% dplyr::filter(Faculty == f_name) %>% filter(Slot == slot)
    f_avail = nrow(f_un) == 0
  }
  return(f_avail)
}

#' Make student schedule
#'
#' @param ranked_faculty dataframe returned from rank_faculty
#' @param slots number of slots in schedule
#' @param f_unavail data frame of whether faculty are available to meet (columns: Faculty, Slot)
#'
#' @return student schedule
#' @export
#'
#' @examples
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

#' Ad minimum nuber of faculty meetings
#'
#' @param s_schedule student schedule 
#' @param ranked_faculty data frame of ranked faculty
#' @param min_fac_mtg minimum number of faculty meetings
#'
#' @return updated student schedule
#' @export
#'
#' @examples
add_min_fac_meetings = function(s_schedule,ranked_faculty,min_fac_mtg=nrow(s_schedule)/2,f_unavail=NULL){
  # count number of meetings each faculty member has
  mtg_ct = table(unlist(s_schedule))
  # order so the ones with the most come first
  mtg_ct = sort(mtg_ct,decreasing = T)
  # find faculty members with too few meetings
  too_few = names(mtg_ct)[mtg_ct < min_fac_mtg]
  s_sched_modified = s_schedule
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
      # check if student already meeting with faculty
      if(f %in% s_schedule[,s]) next
      # find worst match for student 
      current_ranks = which(ranked_faculty[,s] %in% s_schedule[,s])
      worst_match = current_ranks[length(current_ranks)]
      worst_match = ranked_faculty[,s][worst_match]
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
  return(s_sched_modified)
}

#' Add vaculty unavailability to schedule
#'
#' @param f_unavail dataframe of faculty unavailability (columns: Faculty, Slot)
#' @param f_schedule faculty schedule (generated with make_faculty_schedule)
#'
#' @return faculty schedule with unavailability documented
#' @export
#'
#' @examples
add_fac_unavail = function(f_unavail,f_schedule){
  if(!is.null(f_unavail)){
    for(row in 1:nrow(f_unavail)){
      fac = as.character(f_unavail[row,'Faculty'])
      slot = as.numeric(f_unavail[row,'Slot'])
      f_schedule[slot,fac] = 'Unavailable'
    }
  }
  return(f_schedule)
}

#' Make faculty schedule
#'
#' @param student_schedule 
#'
#' @return faculty schedule
#' @export
#'
#' @examples
make_faculty_schedule = function(student_schedule){
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

#' Matchathon master function
#'
#' @param faculty_csv csv of faculty interests
#' @param students_csv csv of student interests
#'
#' @return faculty and student schedules and ranked faculty lists for students
#' @export
#'
#' @examples
matchathon = function(faculty_csv,students_csv,meeting_slots=12,min_fslots=NULL,f_unavail=NULL){
  n = 2
  withProgress(message = '', value = 0, {
  # get minimum number of faculty meetings
  if(is.null(min_fslots)) min_fslots = meeting_slots/2
  if(min_fslots > meeting_slots) min_fslots = meeting_slots
  # read in faculty data
  f <- read_csv(faculty_csv)
  # read in student data
  s <- read_csv(students_csv)
  # read in faculty availability
  if(!is.null(f_unavail)){
    f_unavail <- read_csv(f_unavail)
  }
  # keep only names and columns in both 
  f_keep = c(names(f)[1],names(f)[names(f) %in% names(s)])
  f = f %>% select(f_keep)
  s_keep = c(names(s)[1],names(s)[names(s) %in% names(f)])
  s = s %>% select(s_keep)
  # get match scores
  incProgress(0, detail = 'Getting match scores (1st of 3 tasks)')
  match_scores = get_match_scores(s,f)
  ranked_faculty = rank_faculty(match_scores)
  incProgress(1/n, detail = 'Creating student schedule (2nd of 3 tasks)')
  s_schedule = make_student_schedule(ranked_faculty$ranked_faculty,slots=meeting_slots,f_unavail=f_unavail)
  s_schedule = add_min_fac_meetings(s_schedule,ranked_faculty$ranked_faculty,min_fac_mtg=min_fslots,f_unavail=f_unavail)
  incProgress(1/n, detail = 'Creating faculty schedule (3rd of 3 tasks)')
  f_schedule = make_faculty_schedule(s_schedule)
  })
  return(list(ranked_faculty=ranked_faculty$ranked_faculty,
              ranked_faculty_score=ranked_faculty$ranked_faculty_score,
              s_schedule=s_schedule,
              f_schedule=f_schedule))
}
