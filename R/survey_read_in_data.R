library(dplyr)
library(stringr)

#incoporate saving already met

read_in_data<-function(faculty_csv,student_csv){
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
  
  return(list(faculty=faculty, students=students))
}

