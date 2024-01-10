x=rnorm(50)   
r=acf(x)      
y=x[1:49]+x[2:50] 
ry=acf(y)     
rho=ry

n=5000; 
x=5*rnorm(n)-2   
hist(x,nclass=50)  
qqnorm(x)     
y=runif(100,-4,0)  
qqnorm(y)      
dn=ks.test(x,"pnorm",-2,5)    
dn
ddn=ks.test(y,"punif",1,4)     
ddn
ks.test(x,y)    
Box.test(x, lag = 8)$p.value

Box.test(x, lag = 8,"Ljung-Box")
X=x[2:n]-0.5*x[1:(n-1)] 
acf(X)
Box.test(X, lag = 8,"Ljung-Box")

###################################
####    Exercice
###################################

rep=1000
PVALx=matrix(0,rep,4)
PVALX=matrix(0,rep,4)  
for (p in c(1:rep))
  {n=100
   x=5*rnorm(n)-2 
   X=x[2:n]-0.1*x[1:(n-1)]
   q=0
   for (j in c(5,10,round(log(n)),round(sqrt(n))))
   {q=q+1
   PVALx[p,q]=Box.test(x,lag=j)$p.value
   PVALX[p,q]=Box.test(X,lag=j)$p.value}
}
mean(PVALx[,1])
sum(PVALx[,1]>=0.05)
mean(PVALx[,2])
sum(PVALx[,2]>=0.05)
mean(PVALx[,3])
sum(PVALx[,3]>=0.05)
mean(PVALx[,4])
sum(PVALx[,4]>=0.05)
mean(PVALX[,1])
sum(PVALX[,1]<0.05)
mean(PVALX[,2])
sum(PVALX[,2]<0.05)
mean(PVALX[,3])
sum(PVALX[,3]<0.05)
mean(PVALX[,4])
sum(PVALX[,4]<0.05)
min(PVALx[,1])  
min(PVALx[,2])
min(PVALx[,3])
min(PVALx[,4])
max(PVALX[,1])  
max(PVALX[,2])
max(PVALX[,3])
max(PVALX[,4])


m=100; n=100; Z=0
eps=runif(n+m+1,-2,2)
for (i in c(1:(n+m)))
Z[i+1]=-0.3*Z[i]+eps[i+1]
ZZ=Z[(m+1):(m+n)]
ts.plot(ZZ)
acf(ZZ)
Box.test(ZZ, lag = 8,"Ljung-Box")


n=100; m=100; X=0 
epsi=3*rnorm(n+m) 
for (j in c(1:(n+m))) 
  X[j+1]=-0.3*X[j]+epsi[j+1]+0.7*epsi[j]  
x=X[c((m+1):(n+m+1))]; tsplot(x)   
XX=arima.sim(100,model=list(ar=-.3,ma=.7)) 
X=arima.sim(1000,model=list(ar=-.3,ma=.7),rand.gen = function(n, ...) runif(n,-1,1))  
R=acf(X); R[1]           
Res=c()
for (j in c(1:200))
{X=arima.sim(1000,model=list(ar=-.3,ma=.7)) 
R=acf(X)
Res=c(Res,as.numeric(R[1]$acf))}
hist(Res)

arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
          rand.gen = function(n, ...) runif(n,-1,1))
