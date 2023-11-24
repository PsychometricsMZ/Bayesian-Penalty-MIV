install.packages("R2jags")
###############################
library(R2jags)
###############################


setwd("C:/...")

#data set (cleaned)
dat.sub3 <- readRDS("mivdat.rds")

# jagsfiles 
jagsfiles <- c("normal","normal2","horse","stabcon","stabcat2","laplace","lasso","DL")
#parameters to be extracted
params <- c("ly0","ly1","ty0","ty1","sigmay","ka1","ps1")


# define data for jagsfiles
y <- as.matrix(dat.sub3[,1:6+1])
x <- as.matrix(dat.sub3[,1:14+1+6])
# covariates 
x[,1] <- x[,1]-1 #gender scale to 0/1
x[,2:6] <- as.matrix(scale(x[,2:6])) # scale continuous covariates

# center dichotomous items
for(j in c(1,7:14)){
  x[,j] <- x[,j]-mean(x[,j])
}

#apply(x,2,mean)

#define sample size, no of items and no of covariates
N <- dim(y)[1]
p <- dim(y)[2]
q <- dim(x)[2]

#collect data
data2 <- list(N=N,y=y,x=x,p=p,q=q)
#output names
r0nom <- c("median","mean","mad","sd","low","hig","rhat1")

#either loop through all files or select one (f0)
for(f0 in 2:length(jagsfiles)){
f0 <- 2
fit1 <- jags.parallel(data=data2, 
                      parameters.to.save=params,
                      n.iter=10000, n.chains=3,n.thin=1,n.burnin=5000,
                      model.file=paste0("funs/miv_",jagsfiles[f0],"_noanc2a.txt"))
fit1
#save output
saveRDS(fit1,paste0("results_",jagsfiles[f0],".rds"))
#}







