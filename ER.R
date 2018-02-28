library(xlsx)

ERData<-read.xlsx(file = "data.xlsx",sheetIndex = 1)

ERData$Execute.Team.Name<-NULL

h<-hist(ERData$Active.Reflective)
h$counts
v<-h$counts/sum(h$counts)*100

FBratioActiveOne<-length(which(ERData$Active.Reflective==1))/length(ERData$Active.Reflective)
FBpercentActiveone <- FBratioActiveOne*100
FBpercentActiveone

FBratioReflectiveone1<-length(which(ERData$Active.Reflective==-1 ))/length(ERData$Active.Reflective)
FBpercentReflectiveone1 <- FBratioReflectiveone1*100
FBpercentReflectiveone1


FBratioActiveTwo<-length(which(ERData$Active.Reflective==2))/length(ERData$Active.Reflective)
FBpercentActiveTwo <- FBratioActiveTwo*100
FBpercentActiveTwo

FBratioReflectiveTwo1<-length(which(ERData$Active.Reflective==-2 ))/length(ERData$Active.Reflective)
FBpercentReflectiveTwo1 <- FBratioReflectiveTwo1*100
FBpercentReflectiveTwo1

FBratioActiveThree<-length(which(ERData$Active.Reflective==3))/length(ERData$Active.Reflective)
FBpercentActiveThree <- FBratioActiveThree*100
FBpercentActiveThree

FBratioReflectiveThree1<-length(which(ERData$Active.Reflective==-3 ))/length(ERData$Active.Reflective)
FBpercentReflectiveThree1 <- FBratioReflectiveThree1*100
FBpercentReflectiveThree1


FBratioActiveFour<-length(which(ERData$Active.Reflective==4))/length(ERData$Active.Reflective)
FBpercentActiveFour <- FBratioActiveFour*100
FBpercentActiveFour


FBratioReflectiveFour1<-length(which(ERData$Active.Reflective==-4 ))/length(ERData$Active.Reflective)
FBpercentReflectiveFour1<- FBratioReflectiveFour1*100
FBpercentReflectiveFour1

FBratioActiveFive<-length(which(ERData$Active.Reflective==5))/length(ERData$Active.Reflective)
FBpercentActiveFive<- FBratioActiveFive*100
FBpercentActiveFive


FBratioReflectiveFive1<-length(which(ERData$Active.Reflective==-5 ))/length(ERData$Active.Reflective)
FBpercentReflectiveFive1<- FBratioReflectiveFive1*100
FBpercentReflectiveFive1

One<-sum(FBpercentActiveone,FBpercentReflectiveone1)
Two<-sum(FBpercentActiveTwo,FBpercentReflectiveTwo1)
Three<-sum(FBpercentActiveThree,FBpercentReflectiveThree1)
Four<-sum(FBpercentActiveFour,FBpercentReflectiveFour1)
Five<-sum(FBpercentActiveFive,FBpercentReflectiveFive1)
One
Two
Three
Four
Five
sum(One,Two,Three,Four,Five)



