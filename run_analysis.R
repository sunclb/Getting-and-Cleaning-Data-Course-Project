#How to use the scrip?
#Step 1: unzip getdata_projectfiles_UCI HAR Dataset.zip to 
#getdata_projectfiles_UCI HAR Dataset folder in your working directory
#Step 2: run function run_analysis() in R
#####################################
#

run_analysis<-function(){
  #load library
  library(dplyr)
  #set main folder url
  Mfolderurl<-"./getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset"
  
  ##########################################
  ##Load data
  #load feature table,use " " as seperator in featuretable
  featuretable<-read.table(paste(Mfolderurl,"/features.txt",sep=""),sep=" ")
  #load activity table, use " " as seperator in activitytable
  activitytable<-read.table(paste(Mfolderurl,"/activity_labels.txt",sep=""),sep=" ")
  #load test subject table in testsubtab
  testsubtab<-read.table(paste(Mfolderurl,"/test/subject_test.txt",sep=""))
  #load test activity class table in testclasstab
  testclasstab<-read.table(paste(Mfolderurl,"/test/y_test.txt",sep=""))
  #load test data table in testtable
  testtable<-read.table(paste(Mfolderurl,"/test/X_test.txt",sep=""))
  #load train subject table in trainsubtab
  trainsubtab<-read.table(paste(Mfolderurl,"/train/subject_train.txt",sep=""))
  #load train activity class table in trainclasstab
  trainclasstab<-read.table(paste(Mfolderurl,"/train/y_train.txt",sep=""))
  #load train data table in traintable
  traintable<-read.table(paste(Mfolderurl,"/train/X_train.txt",sep=""))
  
  ############################
  #find mean and std column index/name from featuretable in variableindex
  variableindex<-grep("mean\\(|std\\(",featuretable[,2])
  variablename<-grep("mean\\(|std\\(",featuretable[,2],value = TRUE)
  variablename<-sub("\\(\\)","",variablename)
  
  ##############################
  #Extracts only the measurements on the mean and standard deviation for each measurement
  testtable<-testtable[,variableindex]
  traintable<-traintable[,variableindex]
  
  ###############################
  #rename variables
  names(testtable)<-variablename
  names(traintable)<-variablename
  
  ###############################
  #add activity description to classtables
  testclasstab[,2]<-factor(testclasstab[,1],levels=activitytable[,1],labels=activitytable[,2])
  trainclasstab[,2]<-factor(trainclasstab[,1],levels=activitytable[,1],labels=activitytable[,2])
  
  ###############################
  #add activity description to data table
  testtable<-mutate(testtable,activity=testclasstab[,2])
  traintable<-mutate(traintable,activity=trainclasstab[,2])
  
  ###############################
  #add subject to data table
  testtable<-mutate(testtable,subject=testsubtab[,1])
  traintable<-mutate(traintable,subject=trainsubtab[,1])
  
  ##################################
  #combine two data table
  alltable<-rbind(testtable,traintable)
  
  #####################################
  #calculate mean for all per each activity and each subject
  allmean<-alltable%>%group_by(activity,subject)%>%summarise_all(mean)
  
  #########################################
  #output files result.txt
  write.table(allmean,file = paste0(Mfolderurl,"/result.txt"),row.names = FALSE)
}