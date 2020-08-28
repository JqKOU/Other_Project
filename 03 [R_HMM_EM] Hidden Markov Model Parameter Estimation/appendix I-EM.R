#Appendix I: Parameter Estimation Using EM Algorithm

#-------------------------------------1.1. read data----------------------------------------

setwd("~/Desktop")
obs=read.table("weather-test2-1000.txt",sep=",")
o=obs[,2]
o[1:10]
O=as.numeric(o)

#-------------------------------------1.2. initial lambda---------------------------------------

p11=p22=p33=p12=p13=p21=p23=p31=p32=numeric(0)
e11=e12=e21=e22=e31=e32=numeric(0)
p11[1]=0.5;p12[1]=0.25; p13[1]=0.25; p21[1]=0.25; p22[1]=0.5; p23[1]=0.25; p31[1]=0.25; p32[1]=0.25; p33[1]=0.5   
e11[1]=0.7;e12[1]=0.3;e21[1]=0.3;e22[1]=0.7;e31[1]=e32[1]=0.5
P=t(matrix(c(p11,p12,p13,p21,p22,p23,p31,p32,p33), nrow=3,dimnames=list(c("Sunny","Rainy","Foggy"), c("Sunny","Rainy","Foggy"))))
E=matrix(c(e11,e12,e21,e22,e31,e32), nrow=3, byrow=T,dimnames=list(c("Sunny","Rainy","Foggy"),c("no","yes")))
P
#            Sunny   Rainy   Foggy
#Sunny   0.50     0.25      0.25
#Rainy   0.25      0.50      0.25
#Foggy   0.25      0.25      0.50
E
#                no     yes
#Sunny    0.7     0.3
#Rainy     0.3     0.7
#Foggy     0.5     0.5

#-------------------------------------1.3. EM Algorithm----------------------------------------

n=length(O)
elog=seq(1:1000)
dif=seq(1:1000)
j=2
while (dif[j-1]>0.00001) {
  
  #----------1.3.1 forward table--------
  
  forward=matrix(seq(1,3*n),nrow=3,dimnames=list(c("Sunny","Rainy","Foggy"),seq(1,n)))
  forward[1,1]=1/3*E[1,O[1]]
  forward[2,1]=1/3*E[2,O[1]]
  forward[3,1]=1/3*E[3,O[1]]
  for (i in 2:n) {
    forward[1,i]=(forward[1,i-1]*P[1,1]+forward[2,i-1]*P[2,1]+forward[3,i-1]*P[3,1])*E[1,O[i]]
    forward[2,i]=(forward[1,i-1]*P[1,2]+forward[2,i-1]*P[2,2]+forward[3,i-1]*P[3,2])*E[2,O[i]]
    forward[3,i]=(forward[1,i-1]*P[1,3]+forward[2,i-1]*P[2,3]+forward[3,i-1]*P[3,3])*E[3,O[i]]
  }
  
  
  #----------1.3.2 backward table--------
  
  backward=matrix(seq(1,3*n-3),nrow=3,dimnames=list(c("Sunny","Rainy","Foggy"),seq(1,n-1)))
  backward[1,n-1]=P[1,1]*E[1,O[n]]+P[1,2]*E[2,O[n]]+P[1,3]*E[3,O[n]]
  backward[2,n-1]=P[2,1]*E[1,O[n]]+P[2,2]*E[2,O[n]]+P[2,3]*E[3,O[n]]
  backward[3,n-1]=P[3,1]*E[1,O[n]]+P[3,2]*E[2,O[n]]+P[3,3]*E[3,O[n]]
  for (k in (n-2):1){
    backward[1,k]=P[1,1]*E[1,O[k+1]]*backward[1,k+1]+P[1,2]*E[2,O[k+1]]*backward[2,k+1]+P[1,3]*E[3,O[k+1]]*backward[3,k+1]
    backward[2,k]=P[2,1]*E[1,O[k+1]]*backward[1,k+1]+P[2,2]*E[2,O[k+1]]*backward[2,k+1]+P[2,3]*E[3,O[k+1]]*backward[3,k+1]
    backward[3,k]=P[3,1]*E[1,O[k+1]]*backward[1,k+1]+P[3,2]*E[2,O[k+1]]*backward[2,k+1]+P[3,3]*E[3,O[k+1]]*backward[3,k+1]
  }
  
  #----------1.3.3 P(0|Lambda) --------
  
  po=forward[1,n]+forward[2,n]+forward[3,n]
  Po=E[1,O[1]]*backward[1,1]*1/3+E[2,O[1]]*backward[2,1]*1/3+E[3,O[1]]*backward[3,1]*1/3
  
  #--1.3.4 p(qi=sunny|o,lambda),p(qi=rainny|o,lambda),p(qi=foggy|o,lambda)-
  
  Psrf=matrix(seq(1,3*n),nrow=3,dimnames=list(c("P(qi=S|O)","P(qi=R|O)","P(qi=F|O)"),seq(1,n)))
  for (l in 1:(n-1)){
    Psrf[1,l]=forward[1,l]*backward[1,l]/po
    Psrf[2,l]=forward[2,l]*backward[2,l]/po
    Psrf[3,l]=forward[3,l]*backward[3,l]/po
    Psrf[1,n]=forward[1,n]/po
    Psrf[2,n]=forward[2,n]/po
    Psrf[3,n]=forward[3,n]/po
  }
  
  
  
  #---------- 1.3.5 transition table --------
  
  transition=matrix(seq(1,(n-2)*9),nrow=9,dimnames=list(c("Pss","Psr","Psf","Prr","Prs","Prf","Pff","Pfs","Pfr"),seq(1,n-2)))
  for (t in 1:(n-2)){
    transition[1,t]=forward[1,t]*P[1,1]*E[1,O[t+1]]*backward[1,t+1]/po
    transition[2,t]=forward[1,t]*P[1,2]*E[2,O[t+1]]*backward[2,t+1]/po
    transition[3,t]=forward[1,t]*P[1,3]*E[3,O[t+1]]*backward[3,t+1]/po
    transition[4,t]=forward[2,t]*P[2,2]*E[2,O[t+1]]*backward[2,t+1]/po
    transition[5,t]=forward[2,t]*P[2,1]*E[1,O[t+1]]*backward[1,t+1]/po
    transition[6,t]=forward[2,t]*P[2,3]*E[3,O[t+1]]*backward[3,t+1]/po
    transition[7,t]=forward[3,t]*P[3,3]*E[3,O[t+1]]*backward[3,t+1]/po
    transition[8,t]=forward[3,t]*P[3,1]*E[1,O[t+1]]*backward[1,t+1]/po
    transition[9,t]=forward[3,t]*P[3,2]*E[2,O[t+1]]*backward[2,t+1]/po
  }
  
  #---------- 1.3.6 maximizer----------
  
  p11[j]=sum(transition[1,])/sum(transition[1:3,])
  p12[j]=sum(transition[2,])/sum(transition[1:3,])
  p13[j]=sum(transition[3,])/sum(transition[1:3,])
  p22[j]=sum(transition[4,])/sum(transition[4:6,])
  p21[j]=sum(transition[5,])/sum(transition[4:6,])
  p23[j]=sum(transition[6,])/sum(transition[4:6,])
  p33[j]=sum(transition[7,])/sum(transition[7:9,])
  p31[j]=sum(transition[8,])/sum(transition[7:9,])
  p32[j]=sum(transition[9,])/sum(transition[7:9,])
  
  e11[j]=sum(Psrf[1,c(which(O==1))])/sum(Psrf[1,])
  e12[j]=sum(Psrf[1,c(which(O==2))])/sum(Psrf[1,])
  e21[j]=sum(Psrf[2,c(which(O==1))])/sum(Psrf[2,])
  e22[j]=sum(Psrf[2,c(which(O==2))])/sum(Psrf[2,])
  e31[j]=sum(Psrf[3,c(which(O==1))])/sum(Psrf[3,])
  e32[j]=sum(Psrf[3,c(which(O==2))])/sum(Psrf[3,])
  
  #---------- 1.3.7 update lambda --------
  
  P=t(matrix(c(p11[j],p12[j],p13[j],p21[j],p22[j],p23[j],p31[j],p32[j],p33[j]), nrow=3,dimnames=list(c("Sunny","Rainy","Foggy"), c("Sunny","Rainy","Foggy"))))
  
  E=matrix(c(e11[j],e12[j],e21[j],e22[j],e31[j],e32[j]), nrow=3, byrow=T,dimnames=list(c("Sunny","Rainy","Foggy"),c("no","yes")))
  
  
  #---------- 1.3.8 maximized E(log likelihood)--------
  
  elog[j]=sum(e11[j]*log(e11[j]),e12[j]*log(e12[j]),e21[j]*log(e21[j]),
              e22[j]*log(e22[j]),e31[j]*log(e31[j]),e32[j]*log(e32[j]),
              p11[j]*log(p11[j]),p12[j]*log(p12[j]),p13[j]*log(p13[j]),
              p21[j]*log(p21[j]),p22[j]*log(p22[j]),p23[j]*log(p23[j]),
              p33[j]*log(p33[j]),p31[j]*log(p31[j]),p32[j]*log(p32[j]))
  
  #---------- 1.3.9 check stability --------
  
  dif[j]=abs(elog[j]-elog[j-1])
  j=j+1
}

#---------- 1.3.10 iteration time --------

dif[j-1]  #[1] 9.882749e-06
j-1   #[1] 84

#---------- 1.3.11 final lambda --------
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


