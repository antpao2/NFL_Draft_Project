# Adapt dataset to include only >1 targets
nr = nr[nr$targets >1,]
all = all[all$targets > 1,]
#plot histograms and overlay normal curve for yards per target for each round.
#It shows that the data is roughly normal
for (i in 1:7){
  hist(nr$yards_per_target[nr$round==i],probability =T)
  x = seq(0,20,by=.1)
  sd = sqrt(var(nr$yards_per_target[nr$round==i]))
  mean = mean(nr$yards_per_target[nr$round==i])
  print(mean)
  y = dnorm(x,mean = mean,sd = sd)
  lines(x,y)
}
#Set priors based off of rookie data
mu0 = c()
s20 = c()
for (group in 1:7){
  mu0 = c(mu0, mean(all$yards_per_target[all$round==group]))
  s20 = c(s20, var(all$yards_per_target[all$round==group]))
}
k0 = 1
nu0 = 1
muns = c()
s2ns = c()
#update values for each group
for (i in 1:7){
  dat = nr$yards_per_target[nr$round==i]
  n= length(dat)
  ybar = mean(dat)
  s2 = var(dat)
  kn = k0+n
  nun = nu0+n
  mun = (k0*mu0[i] + n*ybar )/kn
  s2n = (nu0*s20[i] + (n-1)*s2+k0*n*(ybar-mu0[i])^2/kn)/nun
  muns = c(muns,mun)
  s2ns = c(s2ns,s2n)
}
thetak = seq(4,13.5,by=.1)
sigma2l = seq(.001,.3,by = .001)
df <- data.frame(matrix(ncol = length(sigma2l), nrow = length(thetak)))
library(fields)
for (group in 1:7){
  for (i in 1:length(thetak)){
    for (j in 1:length(sigma2l)){
      val = dnorm(thetak[i],muns[group],1/sqrt(10*sigma2l[j]))*dgamma(sigma2l[j],10/2,10*s2ns[group]/2)
      df[i,j] = val
    }
  }
  newdf = as.matrix(df)
  image.plot(thetak,sigma2l,newdf,xlab = group)
}

#Distribution for only theta:
s2Mat = matrix(nrow = 10000,ncol = 7)
thetaMat = matrix(nrow = 10000,ncol = 7)
for (group in 1:7){
  s2Mat[,group] = 1/rgamma(10000, nun/2, s2ns[group]*nun/2)
  thetaMat[,group] <- rnorm(10000,muns[group],sqrt(s2Mat[,group]/kn))
  
}
hist(thetaMat[,1])
hist(thetaMat[,2])
hist(thetaMat[,3])
hist(thetaMat[,4])
hist(thetaMat[,5])
hist(thetaMat[,6])
hist(thetaMat[,7])

#prob that mean of 1>other groups:
for (i in 2:7){
  print(mean(thetaMat[,1]>thetaMat[,i]))
}

#plot the output with the data. I think this is the posterior predictive Distribution
x = seq(0,20,by = .1)
for (group in 1:7){
  y = dnorm(x,mean = muns[group],sd = sqrt(s2ns)[group])
  hist(nr$yards_per_target[nr$round==group],probability = T)
  lines(x,y)
}
#sample people from the posterior predictive distribution
sample = matrix(ncol = 7,nrow = 10000)
for (group in 1:7){
  sample[,group] = rnorm(10000,mean = muns[group],sd = sqrt(s2ns[group]))
}
#This is the probability that a player from round 1 will be better than the later group!
for (group in 2:7){
  print(mean(sample[,1]> sample[,group]))
}

