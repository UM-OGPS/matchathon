#' Remove faculty the student already met with
#'
#' @param am dataframe with student names as the first column and faculty names as the second column. Student names can be repeated.
#' @param rf ranked faculty dataframe with student names as column names
#' @param faculty dataframe where each row is a faculty member and each column is a research interest. First column is faculty names. 
#'
#' @return
#' @export
already_met = function( am, rf, faculty ){
  
  #first lets convert faculty names to lowercase to remove case sensitivity
  #shouldn't need to do this for student names ( right? ) since they're from the same form (ie the students don't have a chance to type diff names )
  faculty_names = faculty %>% dplyr::pull( 1 )
  faculty_names_l = tolower( faculty_names )
  am_fac_l = tolower( am %>% dplyr::pull( 2 ) )
  
  #copy already met so we don't screw around with original
  #am_c = am
  
  #extract student name headers from ranked faculty mat
  student_names_col = dimnames( rf$ranked_faculty )[[ 2 ]]
  
  #get a vector indicating columns of ranked_faculty corresponding to student names in already met
  am = am %>% tibble::add_column( rf_col_num = match( ( am %>% dplyr::pull( 1 ) ), student_names_col ) )
  
  #if the student names don't match this is a bust - throw an error
  #maybe this functionality could be better and throw a warning instead but for now stop the process
  if( any( is.na( am %>% dplyr::pull( rf_col_num ) ) ) ){
    stop( "The student names in already met and ranked faculty don't match" )
  }
  
  num_faculty = dim( rf$ranked_faculty )[ 1 ]
  for( i in 1:length( am_fac_l ) ) {
    
    if ( am_fac_l[i] %in% faculty_names_l ){
      rf_col = am[ i, 3 ][[ 1 ]]
      fname = faculty_names[ which( faculty_names_l == am_fac_l[i] ) ]
      frow_in_rf = which( rf$ranked_faculty[ , rf_col ] == fname )
      f_vec = rf$ranked_faculty[, rf_col]
      score_vec = rf$ranked_faculty_score[, rf_col]
      rf_resorted_col = c(f_vec[f_vec != fname],f_vec[frow_in_rf])
      #rf_resorted_col = c( rf$ranked_faculty[ 1:( frow_in_rf - 1 ), rf_col ], 
      #                  rf$ranked_faculty[ ( frow_in_rf + 1 ):num_faculty, rf_col ], 
      #                  rf$ranked_faculty[ frow_in_rf, rf_col ] )
      rf$ranked_faculty[ , rf_col ] = rf_resorted_col
      rf$ranked_faculty_score[ , rf_col ] = c(score_vec[f_vec != fname],score_vec[frow_in_rf])
    }else{
      #print( paste('Faculty name not found:', am_fac_l[i]))
    }
    
  }
  
  return( rf )
}