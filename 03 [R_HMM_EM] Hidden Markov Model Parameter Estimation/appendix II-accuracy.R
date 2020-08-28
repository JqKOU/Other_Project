#Appendix II: Prediction Accuracy 

#-------------------------2.1. prediction accuracy on overall dataset---------------------

setwd("~/Desktop")
obs=read.table("weather-test2-1000.txt",sep=",")
overall=obs[,1]
overall[1:5]
[1] rainy foggy sunny sunny  sunny
Levels: foggy rainy sunny

#------------------------- log scale lambda --------------------

P=t(matrix(log(c(0.7540976,0.06190326,0.1839992,0.1092816,0.75817739,0.1325410,0.4181348,0.13562309,0.4462422)), nrow=3,dimnames=list(c("Sunny","Rainy","Foggy"), c("Sunny","Rainy","Foggy"))))

E=matrix(log(c(0.8886417,0.1113583,0.2927066,0.7072934,0.7130629,0.2869371)), nrow=3, byrow=T,dimnames=list(c("Sunny","Rainy","Foggy"),c("no","yes")))

P
#                      Sunny           Rainy                Foggy
#Sunny    -0.2822335    -2.7821824    -1.6928239
#Rainy     -2.2138272    -0.2768379    -2.0208632
#Foggy     -0.8719514    -1.9978756    -0.8068934
E
#                        no                yes
#Sunny   -0.1180612   -2.1950023
#Rainy    -1.2285845   -0.3463097
#Foggy    -0.3381856   -1.2484923

#-------------------------A table --------------------

A=matrix(data=0, nrow=3, ncol=n-1)
A[1,1]=max(log(1/3)+P[1,1]+E[1,O[1]], log(1/3)+P[2,1]*E[2,O[1]],log(1/3)+P[3,1]+E[3,O[1]])+E[1,O[2]]
A[2,1]=max(log(1/3)+P[1,2]+E[1,O[1]], log(1/3)+P[2,2]*E[2,O[1]],log(1/3)+P[3,2]+E[3,O[1]])+E[2,O[2]]
A[3,1]=max(log(1/3)+P[1,3]+E[1,O[1]], log(1/3)+P[2,3]*E[2,O[1]],log(1/3)+P[3,3]+E[3,O[1]])+E[3,O[2]]
for (i in 2:(n-1)){
  A[1,i]=max(A[1,i-1]+P[1,1], A[2,i-1]+P[2,1], A[3,i-1]+P[3,1])+E[1,O[i+1]]
  A[2,i]=max(A[1,i-1]+P[1,2], A[2,i-1]+P[2,2], A[3,i-1]+P[3,2])+E[2,O[i+1]]
  A[3,i]=max(A[1,i-1]+P[1,3], A[2,i-1]+P[2,3], A[3,i-1]+P[3,3])+E[3,O[i+1]]
}
A[,1:3]
#                 [,1]                 [,2]               [,3]
#[1,]   -0.4500036   -0.8502982   -1.250593
#[2,]   -2.2313252   -3.7367476   -4.861065
#[3,]   -0.7369534   -1.8820324   -2.881308

#-------------------------q table --------------------
q=matrix(data=0, nrow=3,ncol=n-1)
for (i in 2:(n-1)){
  q[1,1]=which.max(c(log(1/3)+P[1,1]+E[1,O[1]], log(1/3)+P[2,1]*E[2,O[1]],log(1/3)+P[3,1]+E[3,O[1]]))
  q[2,1]=which.max(c(log(1/3)+P[1,2]+E[1,O[1]], log(1/3)+P[2,2]*E[2,O[1]],log(1/3)+P[3,2]+E[3,O[1]]))
  q[3,1]=which.max(c(log(1/3)+P[1,3]+E[1,O[1]], log(1/3)+P[2,3]*E[2,O[1]],log(1/3)+P[3,3]+E[3,O[1]]))
  q[1,i]=which.max(c(A[1,i-1]+P[1,1], A[2,i-1]+P[2,1], A[3,i-1]+P[3,1]))
  q[2,i]=which.max(c(A[1,i-1]+P[1,2], A[2,i-1]+P[2,2], A[3,i-1]+P[3,2]))
  q[3,i]=which.max(c(A[1,i-1]+P[1,3], A[2,i-1]+P[2,3], A[3,i-1]+P[3,3]))
}
q[,1:10]
#         [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#[1,]    2      1     1     1    1     1     1     2     1     1
#[2,]    2      2     1     1    1     2     2     2     2     2
#[3,]    2      3     1     1    1     3     2     2     3     3

#-----------------------estimated states --------------------

Qs=numeric(n)
Qs[n]=which.max(c(A[1,n-1],A[2,n-1],A[3,n-1]))
for (i in (n-1):1){
  if(Qs[i+1]==1) {Qs[i]=q[1,i]}
  if(Qs[i+1]==2) {Qs[i]=q[2,i]}
  if(Qs[i+1]==3) {Qs[i]=q[3,i]}
}
Qstar=character(n)
for (i in 1:n){
  if (Qs[i]==1) {Qstar[i]="sunny"}
  if (Qs[i]==2) {Qstar[i]="rainy"}
  if (Qs[i]==3) {Qstar[i]="foggy"}
}
Qstar[1:10]
#[1] "rainy" "sunny" "sunny" "sunny" "sunny" "rainy" "rainy" "rainy" "sunny" "sunny"

#-----------------------accuracy --------------------

k.2=numeric(n)
for (i in 1:n){
  k.2[i]=(overall[i]==Qstar[i])
}
acc.2=sum(k.2)/10
acc.2 #[1] 66.2


#----------------2.1. prediction accuracy on new short observation--------------

O.new=c('yes', 'no', 'no', 'no', 'no', 'yes', 'yes', 'yes', 'no', 'no')
O.new=as.factor(O.new)
O.new=as.numeric(O.new)
actual=c('rainy', 'foggy', 'sunny', 'sunny', 'sunny', 'foggy', 'foggy', 'rainy', 'foggy', 'sunny')

#------------------------- lambda --------------------

P=t(matrix(c(0.7540976,0.06190326,0.1839992,0.1092816,0.75817739,0.1325410,0.4181348,0.13562309,0.4462422), nrow=3,dimnames=list(c("Sunny","Rainy","Foggy"), c("Sunny","Rainy","Foggy"))))

E=matrix(c(0.8886417,0.1113583,0.2927066,0.7072934,0.7130629,0.2869371), nrow=3, byrow=T,dimnames=list(c("Sunny","Rainy","Foggy"),c("no","yes")))

P
#                    Sunny             Rainy               Foggy
#Sunny    0.7540976    0.06190326     0.1839992
#Rainy     0.1092816    0.75817739    0.1325410
#Foggy     0.4181348    0.13562309    0.4462422

E
#                        no                 yes
#Sunny    0.8886417    0.1113583
#Rainy     0.2927066     0.7072934
#Foggy     0.7130629     0.2869371

#-------------------------A table --------------------

A=matrix(data=0, nrow=3, ncol=9)
A[1,1]=max(1/3*P[1,1]*E[1,O.new[1]], 1/3*P[2,1]*E[2,O.new[1]],1/3*P[3,1]*E[3,O.new[1]])*E[1,O.new[2]]
A[2,1]=max(1/3*P[1,2]*E[1,O.new[1]], 1/3*P[2,2]*E[2,O.new[1]],1/3*P[3,2]*E[3,O.new[1]])*E[2,O.new[2]]
A[3,1]=max(1/3*P[1,3]*E[1,O.new[1]], 1/3*P[2,3]*E[2,O.new[1]],1/3*P[3,3]*E[3,O.new[1]])*E[3,O.new[2]]
for (i in 2:9){
  A[1,i]=max(A[1,i-1]*P[1,1], A[2,i-1]*P[2,1], A[3,i-1]*P[3,1])*E[1,O.new[i+1]]
  A[2,i]=max(A[1,i-1]*P[1,2], A[2,i-1]*P[2,2], A[3,i-1]*P[3,2])*E[2,O.new[i+1]]
  A[3,i]=max(A[1,i-1]*P[1,3], A[2,i-1]*P[2,3], A[3,i-1]*P[3,3])*E[3,O.new[i+1]]
}
A
#[,1]        [,2]         [,3]        [,4]         [,5]         [,6]         [,7]         [,8]         [,9]
#[1,] 0.04110093 0.026889229 0.0175915892 0.011508846 0.0009978161 8.651058e-05 7.500461e-06 1.762990e-05 1.153391e-05
#[2,] 0.02380408 0.004587842 0.0008842303 0.000303312 0.0005580454 3.024682e-04 1.639419e-04 3.159708e-05 6.089813e-06
#[3,] 0.02644941 0.008961413 0.0038263537 0.002503294 0.0005874667 7.139838e-05 1.145621e-05 1.731037e-05 5.864985e-06

#-------------------------q table --------------------
q=matrix(data=0, nrow=3,ncol=9)
for (i in 2:9){
  q[1,1]=which.max(c(1/3*P[1,1]*E[1,O.new[1]], 1/3*P[2,1]*E[2,O.new[1]], 1/3*P[3,1]*E[3,O.new[1]]))
  q[2,1]=which.max(c(1/3*P[1,2]*E[1,O.new[1]], 1/3*P[2,2]*E[2,O.new[1]], 1/3*P[3,2]*E[3,O.new[1]]))
  q[3,1]=which.max(c(1/3*P[1,3]*E[1,O.new[1]], 1/3*P[2,3]*E[2,O.new[1]], 1/3*P[3,3]*E[3,O.new[1]]))
  q[1,i]=which.max(c(A[1,i-1]*P[1,1], A[2,i-1]*P[2,1], A[3,i-1]*P[3,1]))
  q[2,i]=which.max(c(A[1,i-1]*P[1,2], A[2,i-1]*P[2,2], A[3,i-1]*P[3,2]))
  q[3,i]=which.max(c(A[1,i-1]*P[1,3], A[2,i-1]*P[2,3], A[3,i-1]*P[3,3]))
}
q
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9]
#[1,]    3    1    1    1    1    1    1    2    1
#[2,]    2    2    2    1    1    2    2    2    2
#[3,]    3    3    1    1    1    3    2    2    3

#-----------------------estimated states --------------------

Qs=numeric(10)
Qs[10]=which.max(c(A[1,9],A[2,9],A[3,9]))
for (i in 9:1){
  if(Qs[i+1]==1) {Qs[i]=q[1,i]}
  if(Qs[i+1]==2) {Qs[i]=q[2,i]}
  if(Qs[i+1]==3) {Qs[i]=q[3,i]}
}
Qs #[1] 3 1 1 1 1 2 2 2 1 1
Qstar=character(10)
for (i in 1:10){
  if (Qs[i]==1) {Qstar[i]="sunny"}
  if (Qs[i]==2) {Qstar[i]="rainy"}
  if (Qs[i]==3) {Qstar[i]="foggy"}
}
Qstar 
#[1] "foggy" "sunny" "sunny" "sunny" "sunny" "rainy" "rainy" "rainy" "sunny" "sunny"
#-----------------------accuracy --------------------

k=numeric(10)
for (i in 1:10){
  k[i]=(actual[i]==Qstar[i])
}
acc.1=sum(k)/10
acc.1 #[1] 0.5
