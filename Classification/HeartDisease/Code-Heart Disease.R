
setwd("~/Desktop")
#step1. take a look at the dataset
#read CSV file
heart1=read.table("project.1.data.1.csv",sep=",",head=T)
#preview of data
heart1[1:10,]
#check variable type
str(heart1)
#step2. Decriptive analysis
summary(heart1)
#change categorical variable to factor
heart1$sex[heart1$sex==1]="M"
heart1$sex[heart1$sex==0]="F"
heart1$sex=as.factor(heart1$sex)
heart1$fbs[heart1$fbs==1]="True"
heart1$fbs[heart1$fbs==0]="False"
heart1$fbs=as.factor(heart1$fbs)
heart1$cp[heart1$cp==1]="typical"
heart1$cp[heart1$cp==2]="atypical"
heart1$cp[heart1$cp==3]="non_anginal"
heart1$cp[heart1$cp==4]="ymptomatic"
heart1$cp=as.factor(heart1$cp)
heart1$exang[heart1$exang==1]="Yes"
heart1$exang[heart1$exang==0]="No"
heart1$exang=as.factor(heart1$exang)
heart1$restecg[heart1$restecg==0]="normal"
heart1$restecg[heart1$restecg==1]="ST_T_abnormal"
heart1$restecg[heart1$restecg==2]="probable or definite left ventricular hypertrophy"
heart1$restecg=as.factor(heart1$restecg)
heart1$thal[heart1$thal==3]="normal"
heart1$thal[heart1$thal==6]="fixed_defect"
heart1$thal[heart1$thal==7]="reversable_defect"
heart1$thal=as.factor(heart1$thal)
heart1$slope[heart1$slope==1]="up"
heart1$slope[heart1$slope==2]="flat"
heart1$slope[heart1$slope==3]="down"
heart1$slope=as.factor(heart1$slope)
heart1$target=as.factor(heart1$target)
str(heart1)
#step3. visuals
#graph numeric variables vs. "target"
boxplot(age~target,data=heart1,xlab="Heart Disease",ylab="Age")
boxplot(trestbps~target,data=heart1,xlab="Heart Disease",ylab="resting blood pressure (in mm Hg) ")
boxplot(chol~target,data=heart1,xlab="Heart Disease",ylab="serum cholestoral in mg/dl")
boxplot(thalach~target,data=heart1,xlab="Heart Disease",ylab="maximum heart rate achieved")
boxplot(oldpeak~target,data=heart1,xlab="Heart Disease",ylab="ST depression induced by exercise relative to rest")
boxplot(ca~target,data=heart1,xlab="Heart Disease",ylab="number of major vessels(0-3)")
#graph categorical variable vs. "target"
plot(heart1$target, xlab="heart disease", ylab="count")
spineplot(target~sex,data=heart1,ylab="Heart Disease",xlab="Sex")
spineplot(target~cp,data=heart1,ylab="Heart Disease",xlab="chest pain type")
spineplot(target~fbs,data=heart1,ylab="Heart Disease",xlab="fasting blood sugar > 120 mg/dl")
spineplot(target~restecg,data=heart1,ylab="Heart Disease",xlab="resting electrocardiographic")
spineplot(target~exang,data=heart1,ylab="Heart Disease",xlab="exercise induced angina")
spineplot(target~slope,data=heart1,ylab="Heart Disease",xlab="slope of the peak exercise ST segment")
spineplot(target~thal,data=heart1,ylab="Heart Disease",xlab="thal")

#Combine data with few observations
heart1=read.table("project.1.data.1.csv",sep=",",head=T)

plot(heart1$sex)
heart1$sex[heart1$sex==1]="M"
heart1$sex[heart1$sex==0]="F"
heart1$sex=as.factor(heart1$sex)

plot(heart1$fbs)
heart1$fbs[heart1$fbs==0]="False"
heart1$fbs[heart1$fbs==1]="True"
heart1$fbs=as.factor(heart1$fbs)
heart1$fbs=relevel(heart1$fbs,2)

#combine typical and atypical angina as one  
plot(heart1$cp)
heart1$cp[heart1$cp==1]="angina"
heart1$cp[heart1$cp==2]="angina"
heart1$cp[heart1$cp==3]="non_anginal"
heart1$cp[heart1$cp==4]="asymptomatic"
heart1$cp=as.factor(heart1$cp)

plot(heart1$exang)
heart1$exang[heart1$exang==1]="Yes"
heart1$exang[heart1$exang==0]="No"
heart1$exang=as.factor(heart1$exang)

#combine ST_T_abnormal and "probable or definite left ventricular hypertrophy" as abnormal
plot(heart1$restecg)
heart1$restecg[heart1$restecg==0]="normal"
heart1$restecg[heart1$restecg==1]="abnormal"
heart1$restecg[heart1$restecg==2]="abnormal"
heart1$restecg=as.factor(heart1$restecg)

#combine fixed and reversable defect as defect
plot(heart1$thal)
heart1$thal[heart1$thal==3]="normal"
heart1$thal[heart1$thal==6]="defect"
heart1$thal[heart1$thal==7]="defect"
heart1$thal=as.factor(heart1$thal)

#combine flat and down together
plot(heart1$slope)
heart1$slope[heart1$slope==1]="up"
heart1$slope[heart1$slope==2]="flat|down"
heart1$slope[heart1$slope==3]="flat|down"
heart1$slope=as.factor(heart1$slope)
heart1$target=as.factor(heart1$target)

#graph categorical variable vs. "target"
plot(heart1$target, xlab="heart disease", ylab="count")
spineplot(target~sex,data=heart1,ylab="Heart Disease",xlab="Sex")
spineplot(target~cp,data=heart1,ylab="Heart Disease",xlab="chest pain type")
spineplot(target~fbs,data=heart1,ylab="Heart Disease",xlab="fasting blood sugar > 120 mg/dl")
spineplot(target~restecg,data=heart1,ylab="Heart Disease",xlab="resting electrocardiographic")
spineplot(target~exang,data=heart1,ylab="Heart Disease",xlab="exercise induced angina")
spineplot(target~slope,data=heart1,ylab="Heart Disease",xlab="slope of the peak exercise ST segment")
spineplot(target~thal,data=heart1,ylab="Heart Disease",xlab="Thalassemia")

###############################


#step 4.model selection
#model selection without interaction
fit.1=glm(target~1,data=heart1,family="binomial")
fit.2=glm(target~.,data=heart1,family="binomial")
select.1=step(fit.2, scope=list(lower=fit.1,upper=fit.2),direction="backward")
summary(select.1)
coef(select.1)

#model selection with interaction
fit.3=glm(target ~ (sex + cp + trestbps + chol + fbs + thalach + 
                      exang + slope + ca + thal)^2, family = "binomial", data = heart1)
select.2=step(select.1, scope=list(lower=select.1,upper=fit.3),direction="forward")
summary(select.2)
coef(select.2)
library(car)# Anova
Anova(select.2) #based on this, exang not significant remove exang in fit.4
fit.final=glm(target ~ sex + cp + trestbps  + fbs  + exang + slope + ca + thal + exang:slope + sex:fbs + fbs:slope + 
                exang:ca + trestbps:exang  + fbs:ca + sex:thal, 
              data=heart1, family=binomial)
fit.final=glm(target ~ sex + cp + trestbps + exang + slope + ca + thal + exang:slope + 
                exang:ca + trestbps:exang + sex:thal, 
              data=heart1, family=binomial)
summary(fit.final)
coef(fit.final)
Anova(fit.final)

oddsratio=cbind(coef(fit.final),confint(fit.final),exp(coef(fit.final)))
colnames(oddsratio)=c("coefficient","95%CI(2.5%)","95%CI(97.5.5%)","odds ratio")
oddsratio=format(round(oddsratio,2), nsmall=2)
oddsratio

#############################
#step 5. model evalution: sensitivity/specificity/ROC/AUC
pi0=seq(1,0, length.out = 100) 
nsample=nrow(heart1)
roc=NULL
cv.set.list=split(sample(nsample),rep(1:10,length=nsample))  #10-fold cross-validation method
for(k in 1:length(pi0))
{n_11=n_10=n_01=n_00=0
nsample=nrow(heart1)
for(i in 1:length(cv.set.list))
{
  training=heart1[-cv.set.list[[i]],]
  test=heart1[cv.set.list[[i]],]
  fit.training=glm(target ~ sex + cp + trestbps    + exang + slope + ca + thal + exang:slope + 
                     exang:ca + trestbps:exang   + sex:thal, data=training, family="binomial")
  for(j in 1:length(cv.set.list[[i]]))
  {if(predict(fit.training, test[j,], type="response")>=pi0[k])
  {Y.pred.1=1}else{Y.pred.1=0}
    if((test$target[j]==1)&(Y.pred.1==1))
    {n_11=n_11+1}
    if((test$target[j]==1)&(Y.pred.1==0))
    {n_10=n_10+1}
    if((test$target[j]==0)&(Y.pred.1==1))
    {n_01=n_01+1}
    if((test$target[j]==0)&(Y.pred.1==0))
    {n_00=n_00+1}
  }
}
sen=n_11/(n_10+n_11)
spe=n_00/(n_00+n_01)
roc=rbind(roc, c(1-spe, sen))
}
plot(roc, type="s",xlim=c(0,1), ylim=c(0,1), col="red", main="ROC curve", xlab="1-Specificity", ylab="Sensitivity")
auc=sum(roc[-100, 2]*(roc[-1,1]-roc[-100,1])) 
auc #AUC area under ROC curve
pred.accuracy=roc[,2]*sum(heart1$target==1)/nsample+(1-roc[,1])*sum(heart1$target==0)/nsample
z.1=which.max(pred.accuracy)
pi0[z.1] #best cut-off points 
pred.accuracy[z.1]  #best prediction accuracy


