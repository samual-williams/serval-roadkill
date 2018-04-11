se

# Standardise covariates 

cov.num<-cov.or[,sapply(cov.or,is.numeric)] # separate out numeric variables

library(vegan)
cov.std<-decostand(cov.num,method="standardize") #standardise numeric variables
cov.fac<-cov.or[,sapply(cov.or,is.factor)]  # extract factor variables
covs<-data.frame(cov.fac, cov.std) # put numeric and covariate variables back together
covs


library(unmarked)
umCs<-unmarkedFrameOccu(y=Cs,siteCovs= covs) # Create occupancy object, specifying the capture history (Cs) and site covariates (covs)

m0<- occu(~1~1,umCs) # occu fits single-season occupancy model. This line is the numm model, withpout covariates of detection (~1 on left) or occupancy (~1 on right)
# Now fit other models. d1-d3 have covariates on p, o1-o3 have covariates on psi (occupancy), m1-m7 have covariates on both
d1<- occu(~edge~1,umCs)
d2<- occu(~border~1,umCs)
d3<- occu(~edge+border~1,umCs)
o1<- occu(~1~border,umCs)
o2<- occu(~1~habitat,umCs)
o3<- occu(~1~habitat+border,umCs)
m1<- occu(~edge~border,umCs)         
m2<- occu(~border~border,umCs)
m3<- occu(~edge+border~border,umCs)  
m4<- occu(~edge~habitat,umCs)
m5<- occu(~border~habitat,umCs)
m6<- occu(~edge+border~habitat,umCs)
m7<- occu(~edge+border~habitat+border,umCs)

# Need to check each model for estimates, standard errors and convergence

## examine model m1 as an example:
m1
# m1 could be written as p(edge)psi(border)
# this model provided the following estimates: 
# border 0.698 (SE=0.468), probability (P, not to be confused with detection probability p) = 0.136: sign positive so as border increases so does occupancy
# edge -0.200 (SE=0.168) P = 0.234: sign negative so as edge increases, detection probability decreases
# BUT SEs are big relative to estimates and as a result the terms are non-significant (P>0.05)
# Because we used standardized covariates need to backtransform to get menaingful estimates & SEs:


backTransform(linearComb(m1, coefficients = c(1, 0), type = "det")) # sets probability of detection given that a site is occupied and border is set to 0
# Detection probabiliy estimate for m1 was 0.226 (SE 0.035)
backTransform(linearComb(m1, coefficients = c(1, 0), type = "state")) # similarly for occupancy
# occupancy estimate was 0.630 (SE 0.103)

## model selection
# dlist compares the null model (m0) with the others on the basis of minimising AIC
dlist<-fitList(Nullo = m0,d1=d1,d2=d2,d3=d3,o1=o1,o2=o2,o3=o3,m1=m1,m2=m2,m3=m3,m4=m4,m5=m5,m6=m6,m7=m7) 
selmod<-modSel(dlist,nullmod="Nullo")
selmod   
# As dAIC increases gradually there is not a single best model
# remember dAIC<2 = substantial level of empirical support; dAIC=4-7 = substantially less support; dAIC>10 = essentially no support.
# In this case the first 7 models are equally supported. Five of them are concordant in supporting the hypothesis that occupancy varies in relation to habitat. Edge and border seem to affect detection probability, as they both appear frequently in the first 7 models.
# A convenient manner to quantify the importance of the covariates is to sum the model weights for the models where they are retiained. 
# The sum of model weights for models in which occupancy is effected by habitat is 0.16+0.14+0.14+0.13+0.07=0.64 (64%). The sum for occupany models effected by border is 0.14+0.07 = 0.27. So habitat is more important than border.
# Doing the same for detection probability gives summed model weights for border = 0.55 and edge = 0.56.

# Now we want to plot the predictions of psi & p on a map using covariate data. I think this also allows us to determine the values need to substitute the equation Psi = β0 + β1covaraite1 + β2covariate2 + β3covariate3 + ...... + βncovariaten
newhab<-data.frame(habitat=c("Deciduous","Montane"))
pred<-predict(o2,type="state",newdata=newhab,appendData=T) # The Predicted values in pred give the 

ggplot(pred,aes(x=habitat,y=Predicted))+
        geom_point(size=4) +
        ylab("Predicted Psi Cercocebus sanjei") +
        theme_bw()+
        geom_errorbar(aes(ymin=Predicted-SE, ymax=Predicted+SE), width=.2)

# prepare the raster matrix with standardized covariates
map<-read.table("covs100x100.txt",h=T)  # covs100x100.txt is a matrix with the covariate values on a grid of points
mapst<-data.frame(x=map$x,y=map$y, habitat=map$habitat,   # standardize the matrix using mean and sd of covs measured at camera points
                  edge=(map$edge-mean(cov.or$edge))/sd(cov.or$edge),
                  border=(map$border-mean(cov.or$border))/sd(cov.or$border),
                  river=(map$river-mean(cov.or$river))/sd(cov.or$river))

# map
predmap<-predict(o2,type="state",newdata=mapst,appendData=T) # this takes ~ 15 sec
levelplot(predmap$Predicted ~ x + y, map, aspect="iso", xlab="Easting (m)", ylab="Northing (m)", col.regions=terrain.colors(100))
# But isn't this map of predicted occupancy just using habitat alone (model o2)? What if want to do this using model averaging?


# In this next example we will take a more exploratory approach to modelling. Examine simple models (with one variable on p or psi) after creating each one, and retain only those with significant p values in more complicated models. 
#===============================================#
# Rhynchocyon udzungwensis; Grey-faced sengi    #
#===============================================#
Ru<-shrink(mat.udz.09[["Rhynchocyon udzungwensis"]],5)
umRu<-unmarkedFrameOccu(y=Ru,siteCovs=covs)

m0<- occu(~1~1,umRu)
d1<- occu(~edge~1,umRu)
d1 # P = 0.278593, so edge is unlikely to influence detection probability - omit from future models of detection probability

d2<- occu(~border~1,umRu)
d2 # P = 0.234054, so border has limited importance on detection probability - omit from future models

o1<- occu(~1~edge,umRu)
o1 # P = 0.0266, so worth keeping in future models 

o2<- occu(~1~border,umRu)
o2 # P = 0.08350, so I would have thought not worth keeping in future models, but author kept it

o3<- occu(~1~habitat,umRu)
o3 # P = 0.02572, so worth keeping in future models 

o4<- occu(~1~river,umRu)
o4 # P = 0.85343, so distance to river unlikely to influence occupancy, so omit river from future occupancy models (o5, o6, o7)

# Construct remaining models 
o5<- occu(~1~edge+habitat+border,umRu)
o6<- occu(~1~habitat+border,umRu)
o7<- occu(~1~edge+habitat,umRu)
# Why didn't we combine psi and p (m1, m2 etc models) in this example?

dlist<-fitList(Nullo=m0,d1=d1,d2=d2,o1=o1,o2=o2,o3=o3,o4=o4,o5=o5,o6=o6,o7=o7) # organises models for model selection
sel<-modSel(dlist,nullmod="Nullo") # model selection results from an unmarkedFitList
sel 

# the top 3 models are equally supported (dAIC<2), so we will apply model averaging from these models
library(MuMIn)
best<-list(o7,o3,o5) # creates list of best models 
avgmod <- model.avg(best, fit=T) # averages the best models
summary(avgmod)

modnames<-as.character(c(o5,o3,o6)) # this, and the subsequent modavgPred lines were omitted from the book
a <- modavgPred(best,modnames=modnames,newdata=covs,parm.type="psi") # computes the model-averaged predictions, unconditional standard errors, and confidence intervals based on the entire candidate model set
a # I think this contains the predicted values for occupancy (since the top models were just for occupancy) for each camera trap sites

# map
predmap<-predict(avgmod,type="state",newdata=mapst,appendData=T) # takes about 45 sec
levelplot(predmap$fit ~ x + y, map, aspect="iso", xlab="Easting (m)", ylab="Northing (m)",col.regions=terrain.colors(100))
