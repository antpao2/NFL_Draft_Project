# Adapt dataset to include yards per target stat
nr$ypt = nr$yards/nr$targets
nr = na.omit(nr)
#plot histograms and overlay normal curve for yards per target for each round 
for (i in 1:7){
  hist(nr$ypt[nr$round==i],probability =T)
  x = seq(0,20,by=.1)
  sd = sqrt(var(nr$ypt[nr$round==i]))
  mean = mean(nr$ypt[nr$round==i])
  print(mean)
  y = dnorm(x,mean = mean,sd = sd)
  lines(x,y)
}
#Set priors
mu0 = 10
k0 = 1
s20 = .5
nu0 = 1
muns = c()
s2ns = c()
#update values for each group
for (i in 1:7){
  dat = nr$ypt[nr$round==i]
  n= length(dat)
  ybar = mean(dat)
  s2 = var(dat)
  kn = k0+n
  nun = nu0+n
  mun = (k0*mu0 + n*ybar )/kn
  s2n = (nu0*s20 + (n-1)*s2+k0*n*(ybar-mu0)^2/kn)/nun
  muns = c(muns,mun)
  s2ns = c(s2ns,s2n)
}
thetak = seq(4,13.5,by=.1)
sigma2l = seq(.001,.3,by = .001)
df <- data.frame(matrix(ncol = length(sigma2l), nrow = length(thetak)))

for (i in 1:length(thetak)){
  for (j in 1:length(sigma2l)){
    val = dnorm(thetak[i],muns[1],1/sqrt(10*sigma2l[j]))*dgamma(sigma2l[j],10/2,10*s2ns[1]/2)
    df[i,j] = val
  }
}
newdf = as.matrix(df)
image(thetak,sigma2l,newdf)

#plot the output with the data (I only have round1 here)
x = seq(0,20,by = .1)
y = dnorm(x,mean = muns[1],sd = sqrt(s2ns)[1])
hist(nr$ypt[nr$round==1],probability = T)
lines(x,y)





