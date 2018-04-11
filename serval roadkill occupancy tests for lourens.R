# Trying out single-species single-season occupancy models on serval roadkill data

rm(list=ls()) 

# load packages

# Load detection histoy
histAll <- read.csv("hist_1d.csv") # load detection histories (sampling occasion = 1 day)
hist <- histAll[,2:32] # keeps only detection history for first 30 sampling occasions
hist <- data.frame(hist[,-1], row.names=hist[,1]) # makes sections into row names 

# Load covariates
cov <- read.csv("cov.csv") # load covariates
cov$X = NULL # remove unwanted column
cov$section <- as.factor(cov$section) # converts section name to factor

cov.num<-cov[,sapply(cov,is.numeric)] # separate out the numeric variables for colinearity check and subsequent standatdisation
library(usdm)
vif(cov.num) # checks numeric covariates for colinearity. Remove any variables where VIF > 4. Here all VIFs are <4 so don't need to exclude any. 

# Now standardise the covariates 
library(vegan)
cov.std<-decostand(cov.num,method="standardize") # standardise the numeric variables
cov.fac<-cov[,sapply(cov,is.factor)]  # extract factor variables
covs<-data.frame(cov.fac, cov.std) # merge factors and standardised covariates back into a single dataframe
head(covs)

# Calculate naive occupancy
naive <- sum(apply(hist, 1, sum) > 0)/nrow(hist) 
naive
# Naive occupancy looks low, but I think that is to be expected. 

library(unmarked)
um1 <- unmarkedFrameOccu(y=hist,siteCovs= covs) # Create occupancy object

m0 <- occu(~1~1,um1) # fit null nodel 
m0

# Now fit other models. d1-d4 have covariates on detection probability (p), o1-o3 have covariates on occupancy (psi). Haven't yet tried fitting models with more than one covariate on either p, psi, or both p & psi, or interactions 
d1<- occu(~habitat~1,um1, starts=c(-1,0,0,0,0,0,0,0,0,0,0)) # I added the "starts" arugument to fix an error I was getting "Error: Hessian is singular.  Try providing starting values or using fewer covariates." This fixed it.
d1 # now fits without hessian error. 
d2 <- occu(~precip~1,um1,control=list(maxit=1000, trace=TRUE, REPORT=1))
d2 # Model didn't converge, so added control=list(maxit=1000, trace=TRUE, REPORT=1), which eliminated the error
d3 <- occu(~water.dist~1,um1)
d3 # no problems
d4 <- occu(~road.width~1,um1)
d4 # no problems

o1 <- occu(~1~habitat,um1, starts=c(1,0,0,0,0,0,0,0,0,0,0))
o1 # no problems
o2 <- occu(~1~precip,um1)
o2 # no problems
o3 <- occu(~1~water.dist,um1, control=list(maxit=3000, trace=TRUE, REPORT=1))
o3 # Model didn't converge, so added control argument, which eliminated the error
o4 <- occu(~1~road.width,um1, control=list(maxit=1000, trace=TRUE, REPORT=1))
o4 # Model didn't converge, so added control argument, which eliminated the error

# None of the covariates modelled had any effect on p or psi, so did not fit models that indluded more than one covariate. 

# Now compare models with null model using AIC
dlist <- fitList(Nullo = m0,d1=d1,d2=d2,d3=d3,d4=d4,o1=o1,o2=o2,o3=o3,o4=o4) 
selmod <- modSel(dlist,nullmod="Nullo")
selmod  

# So some of the models fitted, but some others had errors such as "Hessian is singular" or "Model did not converge", but after googling solutions I added some arguments that appeared to fix the problems.

# None of the models are a better fit to the data than the null model, so didn't progress further regarding predicting occupancy, mapping, model avergaing, etc. 
