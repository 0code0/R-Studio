library(sqldf)
library(tcltk)
kdd<- read.csv("C:/Users/jas/Desktop/Kdd/kddcup.csv")
x0<-kdd$X0
x1<-c(1:length(kdd$X0))
kdd$X0=x1

Kddnew<-as.data.frame(kdd)

Normals<-sqldf("SELECT X0,X181,normal FROM Kddnew where normal = 'normal.' ORDER BY X0 asc")
Smurf<-sqldf("SELECT X0,X181,normal FROM Kddnew where normal = 'smurf.' ORDER BY X0 asc")
Back<-sqldf("SELECT X0,X181,normal FROM Kddnew where normal = 'back.' ORDER BY X0 asc")
Neptune<-sqldf("SELECT X0,X181,normal FROM Kddnew where normal = 'neptune.' ORDER BY X0 asc")

svd()

KddSampleData2<-KddSampleData
KddSampleData2$normal<-NULL

apply(KddSampleData,2,function(x) sum(is.na(x)))

t.test(KddSampleData2,mu = 8,alternative = "less",conf.level = 0.95)


