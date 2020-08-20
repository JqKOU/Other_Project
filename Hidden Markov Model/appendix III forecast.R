#Appendix III: Weather Forecast for Next 10 Days

#-------------------------------------3.1. estimated lambda-----------------------------------

setwd("~/Desktop")
obs=read.table("weather-test2-1000.txt",sep=",")
o=obs[,2]
o[1:10]
O=as.numeric(o)

P=t(matrix(c(0.7540976,0.06190326,0.1839992,0.1092816,0.75817739,0.1325410,0.4181348,0.13562309,0.4462422), nrow=3,dimnames=list(c("Sunny","Rainy","Foggy"), c("Sunny","Rainy","Foggy"))))

E=matrix(c(0.8886417,0.1113583,0.2927066,0.7072934,0.7130629,0.2869371), nrow=3, byrow=T,dimnames=list(c("Sunny","Rainy","Foggy"),c("no","yes")))

P #transition matrix
#                    Sunny             Rainy               Foggy
#Sunny    0.7540976    0.06190326     0.1839992
#Rainy     0.1092816    0.75817739    0.1325410
#Foggy     0.4181348    0.13562309    0.4462422

E #emission matrix
#                        no                 yes
#Sunny    0.8886417    0.1113583
#Rainy     0.2927066     0.7072934
#Foggy     0.7130629     0.2869371

#----------3.2 forward table--------

n=length(O)
forward=matrix(seq(1,3*n),nrow=3,dimnames=list(c("Sunny","Rainy","Foggy"),seq(1,n)))
forward[1,1]=1/3*E[1,O[1]]
forward[2,1]=1/3*E[2,O[1]]
forward[3,1]=1/3*E[3,O[1]]
for (i in 2:n) {
  forward[1,i]=(forward[1,i-1]*P[1,1]+forward[2,i-1]*P[2,1]+forward[3,i-1]*P[3,1])*E[1,O[i]]
  forward[2,i]=(forward[1,i-1]*P[1,2]+forward[2,i-1]*P[2,2]+forward[3,i-1]*P[3,2])*E[2,O[i]]
  forward[3,i]=(forward[1,i-1]*P[1,3]+forward[2,i-1]*P[2,3]+forward[3,i-1]*P[3,3])*E[3,O[i]]
}
forward[,1:3]

#                         1                      2                   3
#Sunny   0.03711943   0.08330944   0.08274024
#Rainy    0.23576447   0.05679119   0.01639889
#Foggy   0.09564570   0.05758664   0.03462179

#----------3.3 backward table--------


backward=matrix(seq(1,3*n-3),nrow=3,dimnames=list(c("Sunny","Rainy","Foggy"),seq(1,n-1)))
backward[1,n-1]=P[1,1]*E[1,O[n]]+P[1,2]*E[2,O[n]]+P[1,3]*E[3,O[n]]
backward[2,n-1]=P[2,1]*E[1,O[n]]+P[2,2]*E[2,O[n]]+P[2,3]*E[3,O[n]]
backward[3,n-1]=P[3,1]*E[1,O[n]]+P[3,2]*E[2,O[n]]+P[3,3]*E[3,O[n]]
for (k in (n-2):1){
  backward[1,k]=P[1,1]*E[1,O[k+1]]*backward[1,k+1]+P[1,2]*E[2,O[k+1]]*backward[2,k+1]+P[1,3]*E[3,O[k+1]]*backward[3,k+1]
  backward[2,k]=P[2,1]*E[1,O[k+1]]*backward[1,k+1]+P[2,2]*E[2,O[k+1]]*backward[2,k+1]+P[2,3]*E[3,O[k+1]]*backward[3,k+1]
  backward[3,k]=P[3,1]*E[1,O[k+1]]*backward[1,k+1]+P[3,2]*E[2,O[k+1]]*backward[2,k+1]+P[3,3]*E[3,O[k+1]]*backward[3,k+1]
}
backward[,1:4]

#                           1                           2                            3                       4
#Sunny   6.980523e-257   8.818381e-257   1.092718e-256   1.278231e-256
#Rainy    2.394857e-257   3.671708e-257   7.339664e-257   2.105266e-256
#Foggy   5.858813e-257   7.656819e-257    1.038727e-256   1.509113e-256

#----------3.4 P(0|Lambda) --------

po=forward[1,n]+forward[2,n]+forward[3,n]
po #[1] 1.384106e-257
Po=E[1,O[1]]*backward[1,1]*1/3+E[2,O[1]]*backward[2,1]*1/3+E[3,O[1]]*backward[3,1]*1/3
Po #[1] 1.384106e-257

#--3.5 p(qi=sunny|o,lambda),p(qi=rainny|o,lambda),p(qi=foggy|o,lambda)-

#p(qi=s|o,lamda),p(qi=r|o,lamda),p(qi=f|o,lamda)
Psrf=matrix(seq(1,3*n),nrow=3,dimnames=list(c("P(qi=S|O)","P(qi=R|O)","P(qi=F|O)"),seq(1,n)))
for (l in 1:(n-1)){
  Psrf[1,l]=forward[1,l]*backward[1,l]/po
  Psrf[2,l]=forward[2,l]*backward[2,l]/po
  Psrf[3,l]=forward[3,l]*backward[3,l]/po
  Psrf[1,n]=forward[1,n]/po
  Psrf[2,n]=forward[2,n]/po
  Psrf[3,n]=forward[3,n]/po
}
Psrf[,n]

#P(qi=S|O) P(qi=R|O) P(qi=F|O) 
#0.1251379 0.6929691 0.1818930 

#----------3.5 predict next ten days weather --------
#predict state
predict.state=matrix(seq(1,3*10),nrow=3,dimnames=list(c("P(qi+1=S|O)","P(qi+1=R|O)","P(qi+1=F|O)"),seq(1,10)))
for(i in 2:10){
  predict.state[1,1]=Psrf[1,n]*P[1,1]+Psrf[2,n]*P[2,1]+Psrf[3,n]*P[3,1]
  predict.state[2,1]=Psrf[1,n]*P[1,2]+Psrf[2,n]*P[2,2]+Psrf[3,n]*P[3,2]
  predict.state[3,1]=Psrf[1,n]*P[1,3]+Psrf[2,n]*P[2,3]+Psrf[3,n]*P[3,3]
  predict.state[1,i]=predict.state[1,i-1]*P[1,1]+predict.state[2,i-1]*P[2,1]+predict.state[3,i-1]*P[3,1]
  predict.state[2,i]=predict.state[1,i-1]*P[1,2]+predict.state[2,i-1]*P[2,2]+predict.state[3,i-1]*P[3,2]
  predict.state[3,i]=predict.state[1,i-1]*P[1,3]+predict.state[2,i-1]*P[2,3]+predict.state[3,i-1]*P[3,3]
}
predict.state
#                 1         2         3         4         5         6         7         8         9        10
#P(qi+1=S|O) 0.2461508 0.3285513 0.3849784 0.4237064 0.4503107 0.4685929 0.4811581 0.4897945 0.4957306 0.4998108
#P(qi+1=R|O) 0.5578088 0.4647432 0.4007302 0.3567189 0.3264644 0.3056681 0.2913735 0.2815481 0.2747945 0.2701525
#P(qi+1=F|O) 0.1960404 0.2067056 0.2142915 0.2195748 0.2232251 0.2257392 0.2274686 0.2286577 0.2294752 0.2300371

#----------3.6 predict next ten days observation --------


predict.obs=matrix(seq(1,2*10),nrow=2,dimnames=list(c("P(Oi+1=no|O)","P(Oi+1=yes|O)"),seq(1,10)))
for(i in 1:10){
  predict.obs[1,i]=predict.state[1,i]*E[1,1]+predict.state[2,i]*E[2,1]+predict.state[3,i]*E[3,1]
  predict.obs[2,i]=predict.state[1,i]*E[1,2]+predict.state[2,i]*E[2,2]+predict.state[3,i]*E[3,2]
}
predict.obs
#                   1         2         3         4         5         6         7         8         9        10
#P(Oi+1=no|O)  0.5218033 0.5753918 0.6122075 0.6375078 0.6548967 0.6668485 0.6750635 0.6807101 0.6845913 0.6872591
#P(Oi+1=yes|O) 0.4781967 0.4246082 0.3877925 0.3624923 0.3451035 0.3331517 0.3249367 0.3192902 0.3154090 0.3127413

