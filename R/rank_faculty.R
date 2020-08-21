#' Rank faculty for each student 
#'
#' @param match_score dataframe returned by get_match_scores
#'
#' @return For each student (column), faculty with mutual interests ordered by match score.
#' @export
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