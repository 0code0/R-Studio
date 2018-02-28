
Outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
----------------------------------------------------------------------------------------------------------------------------------------------------------

  #Question 1
  hist(as.numeric(Outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))

--------------------------------------------------------------------------------------------------------------------------------
  #Question 2
  best<-function(state,outcomes){
  Outcome$Lower.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Heart.Attack
    if (outcomes == 'heart attack') {
      
      heartattach<-subset(Outcome,Outcome$State == state)
      heartattachmins<-min(heartattach$Lower.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Heart.Attack)
      
      heartattachminsvalue<-subset(heartattach,heartattach$Lower.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Heart.Attack ==heartattachmins)
      
      heartattachminsvalue$Hospital.Name
      
      }else if (outcomes == 'heart failure'){
        
        heartFailure<-subset(Outcome,Outcome$State == state)
        
        heartFailuremins<-min(heartFailure$Lower.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Heart.Failure)
        heartFailureminsalue<-subset(heartFailure,heartFailure$Lower.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Heart.Failure ==heartFailuremins)
        heartFailureminsalue$Hospital.Name
        
        }else if(outcomes == 'pneumonia'){
          
          pneumonia<-subset(Outcome,Outcome$State == state)
          pneumoniamins<-min(pneumonia$Lower.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Pneumonia)
          
          pneumoniaminsvalue<-subset(pneumonia,pneumonia$Lower.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Pneumonia == pneumoniamins)
          
          pneumoniaminsvalue$Hospital.Name
          
        }else{
              
          "Error Check your function attribute Three Attribute are allow -> heart attack,heart failure,pneumonia"
            }
              
    }

best('TX','pneumonia')
------------------------------------------------------------------------------------------------------------------------------
  #Question 3
hospitalRank<-data.frame(Outcome$State,Outcome$Hospital.Name,Outcome$Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,stringsAsFactors = FALSE)
View(hospitalRank)
newhospitalRank<-hospitalRank[hospitalRank$Outcome.Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!='Not Available',]
View(newhospitalRank[order(newhospitalRank$Outcome.Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,decreasing = F) ,])

View(newhospitalRank)


View(hospitalRank[hospitalRank$Outcome.Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!='Not Available',])



head(hospitalRank)




Rank<-rank(newhospitalRank$Outcome.Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
          ties.method = "first")


hospitalwithRank<-data.frame(newhospitalRank,Rank)

View(hospitalwithRank)

rankHospital<-function(state,outcomes,num){
  
  if (outcomes == 'heart attack') {
    
    heartattach2<-subset(hospitalwithRank,hospitalwithRank$Outcome.State == state & hospitalwithRank$Rank == num)
    
    heartattach2
    
  }else if (outcomes == 'heart failure'){
    
    heartFailure2<-subset(hospitalwithRank,hospitalwithRank$Outcome.State == state & hospitalwithRank$Rank == num)
    heartFailure2
    
  }else if(outcomes == 'pneumonia'){
    
    pneumonia2<-subset(hospitalwithRank,hospitalwithRank$Outcome.State == state & hospitalwithRank$Rank == num)
    pneumonia2
        
  }else{
    
    "Error Check your function attribute Three Attribute are allow -> heart attack,heart failure,pneumonia"
  }
  
}

table(hospitalwithRank$Outcome.State)

rankHospital('AL','pneumonia',1)
---------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #Question 4
  rankall<-function(outcomes,num){
    
    if (outcomes == 'heart attack') {
      
      heartattach3<-subset(hospitalwithRank,hospitalwithRank$Rank == num)
      
      heartattach3
      
    }else if (outcomes == 'heart failure'){
      
      heartFailure3<-subset(hospitalwithRank,hospitalwithRank$Rank == num)
      heartFailure3
      
    }else if(outcomes == 'pneumonia'){
      
      pneumonia3<-subset(hospitalwithRank,hospitalwithRank$Rank == num)
      pneumonia3
      
    }else{
      
      "Error Check your function attribute Three Attribute are allow -> heart attack,heart failure,pneumonia"
    }
    
  }
rankall('pneumonia',1)
------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  for (i in 1:length(table(Outcome$State))) {
   
     c(print(table(Outcome$State)[[i]]),"->",print(table(Outcome$State)[i]))      
    
  }
  

length(table(Outcome$State))

table(Outcome$State)[[2]]    





