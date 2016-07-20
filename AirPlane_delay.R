#libraries needed
require(dplyr)
require(rpart)
require(ggplot2)



user1=read.csv("/Users/AshleyMeow/Documents/SW_MD.csv")
user2=read.csv("/Users/AshleyMeow/Documents/DL_ORD.csv")
user3=read.csv("/Users/AshleyMeow/Documents/AA_ORD.csv")
user4=read.csv("/Users/AshleyMeow/Documents/UA_ORD.csv")
user5=read.csv("/Users/AshleyMeow/Documents/UA_SF.csv")
user6=read.csv("/Users/AshleyMeow/Documents/AA_OAK.csv")
user7=read.csv("/Users/AshleyMeow/Documents/DL_OAK.csv")
user8=read.csv("/Users/AshleyMeow/Documents/SW_SJ.csv")

data=rbind(user1,user2,user3,user4,user5,user6,user7,user8)
dim(data)


data$delayflg<-ifelse(data$Arrival.Delay.Minutes>0 ,1,0)
data$Date<-as.Date(data$Date..MM.DD.YYYY.,format = "%m/%d/%y") 
data$days<-weekdays(data$Date)

data$delayflg<-as.factor(data$delayflg)
data = subset(data, delayflg==1|delayflg==0)
train_sample = sample(nrow(data), size = nrow(data)*0.66)
train_data = data[train_sample,]
test_data = data[-train_sample,]

colnames(data)[5]<-"Origin_Airport"
check<-sqldf('select delayflg, count(*) from data group by delayflg')


fit1<-rpart(delayflg ~Scheduled.Elapsed.Time.Minutes.
            +Carrier.Code+days, data=train_data,parms=list(prior=c(.5,.5)),cp=.005)
rpart.plot(fit1, tweak=1.5)
printcp(fit1)
plotcp(fit1) 


ptree<-prune(fit1,cp = fit1$cptable[which.min(fit1$cptable[,"xerror"]),"CP"])

rpart.plot(ptree, tweak=1.5)


test_data$t<-predict(ptree,type='class',test_data)
#test_data$tscore1<-predict(ptree,type='prob',test_data)
#pred1<-prediction(test_data$score1[,2],test_data$delayflg)
#perf5 <- performance(pred1,"tpr","fpr")
#table(test_data$t)
table(test_data$t,test_data$delayflg)
