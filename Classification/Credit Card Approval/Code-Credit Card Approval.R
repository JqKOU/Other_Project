####################  data cleaning  ##########################
setwd("~/Desktop")
cred.1=read.table("project.2.data.1.train.txt",sep=",",na.strings="?")
str(cred.1)
# Cleaning the data and remove the observations with missing values.
sum(is.na(cred.1))
apply(is.na(cred.1),2,sum)
id_no_miss=(apply(is.na(cred.1),1,sum)==0)
cred.2=cred.1[id_no_miss,]
sum(is.na(cred.2))
#change dependent variable value to 0 and 1
Y=rep(0,nrow(cred.2))
Y[cred.2[,16]=="+"]=1
Y[cred.2[,16]=="-"]=0
cred.3=cbind(cred.2[-16],Y)
cred.3$Y=as.factor(cred.3$Y)
str(cred.3)

#summary and data cleaning
summary(cred.3[,c(1,4,5,6,7,9,10,12,13,16)])
cred.3=cred.3[-row(cred.3)[which(cred.3$V4=="l")],]
cred.3$V4=as.numeric(cred.3$V4)
cred.3$V4[cred.3$V4==2]="u"
cred.3$V4[cred.3$V4==3]="y"
cred.3$V4=as.factor(cred.3$V4)
summary(cred.3$V4)
cred.3$V5=as.numeric(cred.3$V5)
cred.3$V5[cred.3$V5==1]="g"
cred.3$V5[cred.3$V5==3]="p"
cred.3$V5=as.factor(cred.3$V5)
summary(cred.3$V5)
cred.3=cred.3[-row(cred.3)[which(cred.3$V13=="p")],]
cred.3$V13=as.numeric(cred.3$V13)
cred.3$V13[cred.3$V13==1]="g"
cred.3$V13[cred.3$V13==3]="s"
cred.3$V13=as.factor(cred.3$V13)
summary(cred.3$V13)
##################  data visulization  ############################
#graph categorical variables vs. Y
plot(cred.3$Y, xlab="Credit Approval Results", ylab="count")
spineplot(Y~V1,data=cred.3,ylab="Credit Approval Results",xlab="V1")
spineplot(Y~V4,data=cred.3,ylab="Credit Approval Results",xlab="V4")
spineplot(Y~V5,data=cred.3,ylab="Credit Approval Results",xlab="V5")
spineplot(Y~V6,data=cred.3,ylab="Credit Approval Results",xlab="V6")
spineplot(Y~V7,data=cred.3,ylab="Credit Approval Results",xlab="V7")
spineplot(Y~V9,data=cred.3,ylab="Credit Approval Results",xlab="V9")
spineplot(Y~V10,data=cred.3,ylab="Credit Approval Results",xlab="V10")
spineplot(Y~V12,data=cred.3,ylab="Credit Approval Results",xlab="V12")
spineplot(Y~V13,data=cred.3,ylab="Credit Approval Results",xlab="V13")
#graph numeric variables vs. Y
boxplot(V2~Y,data=cred.3,xlab="Credit Approval Results",ylab="V2")
boxplot(V3~Y,data=cred.3,xlab="Credit Approval Results",ylab="V3")
boxplot(V8~Y,data=cred.3,xlab="Credit Approval Results",ylab="V8")
boxplot(V11~Y,data=cred.3,xlab="Credit Approval Results",ylab="V11")
boxplot(V14~Y,data=cred.3,xlab="Credit Approval Results",ylab="V14")
boxplot(V15~Y,data=cred.3,xlab="Credit Approval Results",ylab="V15")
#data for analysis
cred.4=cred.3
cred=cred.4[-c(6,7,13,15)]
str(cred)
#################    fit model without regularity  #######################
#fit model
fit.null=glm(Y~1,family=binomial, data=cred)
fit.full=glm(Y~.,family=binomial, data=cred)
select.1=step(fit.null, scope=list(lower=fit.null,upper=fit.full),direction="forward")
fit.1=glm(Y ~ V9 + V11 + V8 + V10 + V4, family = binomial, data = cred)
summary(fit.1)


###################     ROC    ###########################
#ROC curve
pi0=seq(1,0, length.out = 100) 
nsample=nrow(cred)
roc.1=NULL
cv.set.list=split(sample(nsample),rep(1:10,length=nsample))  #10-fold cross-validation method
for(k in 1:length(pi0))
{n_11.1=n_10.1=n_01.1=n_00.1=0
nsample=nrow(cred)
for(i in 1:length(cv.set.list))
{
  training=cred[-cv.set.list[[i]],]
  test=cred[cv.set.list[[i]],]
  fit.1.training=glm(Y~V9 + V11 + V8 + V10 + V4, data=training, family="binomial")
  for(j in 1:length(cv.set.list[[i]]))
  {if(predict(fit.1.training, test[j,], type="response")>=pi0[k])
  {Y.pred.1=1}else{Y.pred.1=0}
    if((test$Y[j]==1)&(Y.pred.1==1))
    {n_11.1=n_11.1+1}
    if((test$Y[j]==1)&(Y.pred.1==0))
    {n_10.1=n_10.1+1}
    if((test$Y[j]==0)&(Y.pred.1==1))
    {n_01.1=n_01.1+1}
    if((test$Y[j]==0)&(Y.pred.1==0))
    {n_00.1=n_00.1+1}
  }
}
sen=n_11.1/(n_10.1+n_11.1)
spe=n_00.1/(n_00.1+n_01.1)
roc.1=rbind(roc.1, c(1-spe, sen))
}
plot(roc.1, type="s",xlim=c(0,1), ylim=c(0,1), col="red", main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")
auc.1=sum(roc.1[-100, 2]*(roc.1[-1,1]-roc.1[-100,1])) 
auc.1 #AUC model.1

#prediction accuracy
pred.accuracy.1=roc.1[,2]*sum(cred$Y==1)/nsample+(1-roc.1[,1])*sum(cred$Y==0)/nsample
z.1=which.max(pred.accuracy.1)
pi0[z.1] #best cut-off points of fit.1
pred.accuracy.1[z.1]  #best prediction accuracy for fit.1


##################       testing      ############################
#testing data
cred.test=read.table("project.2.data.2.test.txt",sep=",",na.strings="?")
sum(is.na(cred.test))
str(cred.test)
summary(cred.test$V4)
cred.test=cred.test[-row(cred.test)[which(cred.test$V4=="l")],]
cred.test$V4=as.numeric(cred.test$V4)
cred.test$V4[cred.test$V4==2]="u"
cred.test$V4[cred.test$V4==3]="y"
cred.test$V4=as.factor(cred.test$V4)
Y=rep(0,nrow(cred.test))
Y[cred.test[,16]=="+"]=1
Y[cred.test[,16]=="-"]=0
cred.test.2=cbind(cred.test[-16],Y)
cred.test.2$Y=as.factor(cred.test.2$Y)

#test accuracy
test=cred.test.2
Y.pred.test.1=predict(fit.1,test, type="response")
Y.pred.test1=rep(0,nrow(test))
Y.pred.test1[Y.pred.test.1>=pi0[z.1]]=1
n_11.1=sum((test$Y==1)&(Y.pred.test1==1))
n_11.1
n_10.1=sum((test$Y==1)&(Y.pred.test1==0))
n_10.1
n_01.1=sum((test$Y==0)&(Y.pred.test1==1))
n_01.1
n_00.1=sum((test$Y==0)&(Y.pred.test1==0))
n_00.1

#sen spe acc
sen.test1=n_11.1/(n_10.1+n_11.1)
spe.test1=n_00.1/(n_00.1+n_01.1)
acc.test1=(n_11.1+n_00.1)/nrow(test)
sen.test1 #test sensitivity fit.1
spe.test1 # test specificity fit.1
acc.test1 #test prediction accuracy fit.1


#####use lasso penalty to build logistic regression model#####

y=as.factor(cred$Y)
x=model.matrix(fit.full)[,-1]
library(foreach)
library(glmnet)

fit.4=glmnet(x,y,family="binomial")
lambda.seq=fit.4$lambda
pi0=seq(0,1,length.out=100)
correct.num=matrix(0,length(lambda.seq),length(pi0))
nsample=nrow(cred)
for(i in 1:nsample) {
  x.train=x[-i,]
  x.test=matrix(x[i,],1,ncol(x))
  y.train=y[-i]
  y.test=y[i]
  fit.1.training=glmnet(x.train, y.train, family="binomial")
  for(j in 1:length(lambda.seq)) {
    pred.prob=predict(fit.1.training, newx=x.test, s=lambda.seq[j], type="response")
    for(k in 1:length(pi0)) {
      if(pred.prob>=pi0[k]) { Y.pred.1=1}else {Y.pred.1=0}
      if((y.test==1)&(Y.pred.1==1))
      {correct.num[j,k]=correct.num[j,k]+1}
      if((y.test==0)&(Y.pred.1==0))
      {correct.num[j,k]=correct.num[j,k]+1} }
  }
}
accuracy=correct.num/nsample
max(accuracy)
matrix=which(accuracy==max(accuracy), arr.ind=TRUE)
j=matrix[1,1]
k=matrix[1,2]
lambda.seq[j] # tuning parameter
pi0[k] #cut to cut off point

#fit Lasso model
lambda.opt=lambda.seq[j]
fit.lasso=glmnet(x,y,family="binomial")
coef(fit.lasso, s=lambda.opt)

#test accuracy
test=cred.test.2
x.test=data.matrix(test[,-16])
y.test=test[,16]
fit.lasso.test=glmnet(x.test,y.test,family="binomial")
Y.pred.3.1=predict(fit.lasso.test, newx=x.test, s=0.009449769, type="response")
Y.pred.3=rep(0,nrow(test))
Y.pred.3[Y.pred.3.1>0.5656566]=1
Y.pred.3
n_11.3=sum((test$Y==1)&(Y.pred.3==1))
n_11.3
n_10.3=sum((test$Y==1)&(Y.pred.3==0))
n_10.3
n_01.3=sum((test$Y==0)&(Y.pred.3==1))
n_01.3
n_00.3=sum((test$Y==0)&(Y.pred.3==0))
n_00.3

#sen spe acc
sen.3=n_11.3/(n_10.3+n_11.3)
spe.3=n_00.3/(n_00.3+n_01.3)
acc.3=(n_11.3+n_00.3)/nrow(test)
sen.3 #test sensitivity lasso 
spe.3 # test specificity lasso
acc.3 #test prediction accuracy lasso
