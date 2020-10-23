#' Read in csvs
#'
#' @param faculty_csv csv of Google Form output from  faculty 
#' @param student_csv csv of Google Form output from students 
#' @param f_unavail_csv csv of faculty unavailability
#' @param already_met_csv csv of faculty students have already met with
#'
#' @return two binary matrices (one for faculty and one for students) of research interests, 1 = listed interest, 0 = not an interest.
#' @export

library(dplyr)
library(stringr)

#incoporate saving already met

read_in_data<-function(faculty_csv,student_csv,f_unavail_csv=NULL,already_met_csv=NULL){
  # Read in faculty data
  fac<-readr::read.csv(faculty_csv)
  fac<-as.data.frame(fac)
  
  # Extracting personal identifier, research interests, and model systems 
  faculty<-cbind.data.frame(fac$Name..First.and.Last.,fac$Research.interest,fac$model.systems)
  colnames(faculty)<-c('Name','Research_interest','Model_systems')
  faculty<-data.frame(lapply(faculty, as.character), stringsAsFactors = FALSE)
  
  # Read in student data
  student<-readr::read.csv(student_csv)
  student<-as.data.frame(student)
  
  # Extracting personal identifier, research interests, and model systems
  students<-cbind.data.frame(student$Name..First.and.Last.,student$Research.interest,student$model.systems)
  colnames(students)<-c('Name','Research_interest','Model_systems')
  students<-data.frame(lapply(students, as.character), stringsAsFactors = FALSE)
  
  # Create lists of all model systems and research interests from faculty and students
  
  list_stu<- strsplit(as.character(students$Research_interest),';')
  models_stu<-strsplit(as.character(students$Model_systems), ';')
  
  list_fac<- strsplit(as.character(faculty$Research_interest),';')
  models_fac<-strsplit(as.character(faculty$Model_systems), ';')
  
  # Combine both sets of interests to create a reference 
  
  full_interests<-c(list_fac,list_stu)
  full_models<-c(models_fac,models_stu)
  
  lvls_interests<-unique(unlist(full_interests))
  lvls_models<-unique(unlist(full_models))
  
  # Build a dataframe that counts the instance of each interest for each student and faculty. Compares to reference built above
  faculty[,lvls_interests]<-sapply(lvls_interests, function(y){str_count(faculty$Research_interest,y)})
  faculty[,lvls_models]<-sapply(lvls_models, function(y){str_count(faculty$Model_system,y)})
  
  students[,lvls_interests]<-sapply(lvls_interests, function(y){str_count(students$Research_interest,y)})
  
  students[,lvls_models]<-sapply(lvls_models, function(y){str_count(students$Model_system,y)})
  
  # Remove unecessary columns to be ready for algorithm processing 
  
  drops<-c('Research_interest','Model_systems')
  
  students<-students[, !(names(students) %in% drops)]
  faculty<-faculty[, !(names(faculty) %in% drops)]
  
  # read in facutly availability
  f_unavail <-NULL
  if(!is.null(f_unavail_csv)){
    f_unavail <- readr::read_csv(f_unavail_csv)
    f_unavail
  }

  # read in already met 
  already_met <- NULL
  if(!is.null(already_met_csv)){
    already_met <- readr::read_csv(already_met_csv)
  }

  return(list(faculty=faculty, students=students, f_unavail=f_unavail,already_met=already_met))
}

