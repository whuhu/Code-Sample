rm(list = ls())

setwd("C:/Users/huhu/Desktop/重构code/code/R code/Standard/R data A1")
dat <- read.csv("C:/Users/huhu/Desktop/重构code/code/R code/Standard/R data A1/R_vcm.csv")

library(np)
library(gtools)
library(fANCOVA)
library(foreign)
#source("npscoefGrad.R")

### Generating Data

yd     <- dat[,1] #isei

yp     <- dat[,8] #parent's log income (lnadinc)
ypx    <- dat[,8] #parent's log incodatme (lnadinc)

age <- dat[,2] #child's age
gender <- dat[,3] #gender
urban <- dat[,4] #urban
minzu <- dat[,5] #minzu
mid <- dat[,6] #mid
east <- dat[,7] #east

ypz   <- dat[,8] #parent's income (non logged)
ypze   <- exp(dat[,8]) #parent's income (non logged)


basic<-data.frame(yd = yd, ypx=ypx, age=age, gender=gender, urban=urban, minzu=minzu, mid=mid, east=east, ypz=ypz, ypze=ypze)

#######################################################################
# Dropping missing obs
datall <- data.frame(basic=basic)
datall <-na.exclude(datall)
n <- dim(datall)[1]
#########################################################################################################################################################

yd        <- datall[,1] #daughter's log income (lnayinc)
yd        <- as.vector(yd)

ypx       <- datall[,2] #parent's log income (lnadinc)
age    <- datall[,3] #age
gender     <- datall[,4] #gender
urban    <- datall[,5] #urban
minzu    <- datall[,6] #minzu
mid    <- datall[,7] #mid
east    <- datall[,8] #east

ypz       <- datall[,9] #parent's income (not in logs)
ypze      <- datall[,10] #parent's income (not in logs)

basic<-data.frame(yd = yd, ypx=ypx, age=age, gender=gender, urban=urban, minzu=minzu, mid=mid, east=east, ypz=ypz, ypze=ypze)

###################

########################
	# value <-runif(1000)
	# temp <- data.frame(value=value, quartile=rep(NA, 1000))

#############################################################################################################
x_basic    <- data.frame(ypx=ypx, age=age, gender=gender, urban=urban, minzu=minzu, mid=mid, east=east)
########################################################################################################################
###############    Estimation of IGM   #########################################
################################################################################

# put together the data for estimation

###########################################
#ypz <- exp(ypz)/1000
###########################################
x_dat   <- x_basic
z_dat <- ypz
yxz_dat <- cbind(yd,x_dat,ypz) 
n <- nrow(yxz_dat)

###########################################################################
################ Linear model
lm_model <- lm(yd~ypx+age+gender+urban+minzu+mid+east,data=yxz_dat)
summary(lm_model)
lm_model_coef <- lm_model$coefficients
IGM_c <- lm_model_coef[2]
save(lm_model,file="lm_model.Rda")


#########################################################################
## Varying Coefficient Model

bw_cv <- npscoefbw(yd~ypx+age+gender+urban+minzu+mid+east|ypz, data=yxz_dat, nmulti=min(5,ncol(x_dat)+1))
bw <- npscoefbw(yd~ypx+age+gender+urban+minzu+mid+east|ypz, data=yxz_dat, nmulti=min(5,ncol(x_dat)+1),bandwidth.compute=FALSE,bws=2.5*bw_cv$bw)

vcm_model <- npscoef(bw,betas=TRUE) 
gammas <- data.frame(vcm_model$beta)

trim_up <-0.01
trim_low <-0.01
temp <-0
temp <-cbind(z_dat,rep(lm_model_coef[2],n),gammas[,2])
temp <- temp[order(temp[,1]),]
matplot(temp[floor(1+n*trim_low):floor(n-n*trim_up),1],temp[floor(1+n*trim_low):floor(n-n*trim_up),2:3],type="llll",lty=c(2,1,3,3),pch=1, col = c(3,1), xlab="Parents' LogIncome", ylab="Coef.")

save(gammas,file="gammas.Rda")

# Computing the average VCM 
avcm_coef <- colMeans(coef(vcm_model))
round(avcm_coef,3)

# Computing the quintiles of VCM sorted by parents' income (ypz)
tempor <-cbind(ypz,gammas)
tempor <- tempor[order(tempor[,1]),]
mgamma_q <-matrix(NA,5,dim(gammas)[2])
d_q1[,j]


for(j in 1:dim(gammas)[2]) {
temp <- data.frame(zval = tempor[,1], vcoef.value=tempor[,j+1], quintile=rep(NA, n))
brks <- with(temp, quantile(zval, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))
temp <- within(temp, quintile <- cut(zval, breaks = brks, labels = 1:5,include.lowest = TRUE))

mgamma_q1<-mean(temp[temp[,3]==1,2])
mgamma_q2<-mean(temp[temp[,3]==2,2])
mgamma_q3<-mean(temp[temp[,3]==3,2])
mgamma_q4<-mean(temp[temp[,3]==4,2])
mgamma_q5<-mean(temp[temp[,3]==5,2])
mgamma_q[,j]<-c(mgamma_q1,mgamma_q2,mgamma_q3,mgamma_q4,mgamma_q5)



}
save(mgamma_q,file="mgamma_q.Rda")


d1_2 <- mgamma_q[1,] - mgamma_q[2,]
d1_3 <- mgamma_q[1,] - mgamma_q[3,]
d1_4 <- mgamma_q[1,] - mgamma_q[4,]
d1_5 <- mgamma_q[1,] - mgamma_q[5,]
d2_3 <- mgamma_q[2,] - mgamma_q[3,]
d2_4 <- mgamma_q[2,] - mgamma_q[4,]
d2_5 <- mgamma_q[2,] - mgamma_q[5,]
d3_4 <- mgamma_q[3,] - mgamma_q[4,]
d3_5 <- mgamma_q[3,] - mgamma_q[5,] 
d4_5 <- mgamma_q[4,] - mgamma_q[5,] 



#################################################################################################################################  Bootstrap Inference  #############################################
##########################################################################################################

num_boot <- 500  # set this to 500 or 1000

########## keep the original data  #############
yxz_dat_orig <-  yxz_dat
save(yxz_dat,file="yxz_dat.Rda")

x_dat_orig <- x_dat  
z_dat_orig <- z_dat  

############################################################################
############# Bootstrap Test the null of linear model vs. VCM (wild) ####################
set.seed(123)

fit0 <- fitted(lm_model)
res0 <- residuals(lm_model)
rss0 <- sum(res0^2)

res1 <-residuals(vcm_model) # residuals under H1: VCM
rss1 <-sum(res1^2)

GLRstat_boot <- matrix(NA,num_boot,1)
#GLRstat <- (n/2)*log(rss0/rss1) 
GLRstat <- (n/2)*((rss0-rss1)/rss1) 
res0_orig <- res0 - mean(res0)

res_mat_boot  <- wild.boot(res0_orig, nboot=num_boot)
y_mat_boot    <- matrix(rep(fit0,num_boot), ncol=num_boot) + res_mat_boot

for(b in 1:num_boot) {
	#print(b)
	# Generate boot sample: yd_star, xx_basic
      yd         <- y_mat_boot[,b]
	yxz_dat <- cbind(yd,x_dat,ypz) 

	# Obtain rss0 under the null of the linear model
	lm_model_boot <- lm(yd~ypx+age+gender+urban+minzu+mid+east,data=yxz_dat)

	res0_boot      <- residuals(lm_model_boot)
      rss0_boot      <- sum(res0_boot^2) 

	# Obtain rss1 under the alternative of the VCM
	vcm_model_boot <- npscoef(bw,betas=TRUE) 
      res1_boot <- residuals(vcm_model_boot)
      rss1_boot <- sum(res1_boot^2) 
	
	#GLRstat_boot[b,1] <- (n/2)*log(rss0_boot/rss1_boot) 
	GLRstat_boot[b,1] <- (n/2)*((rss0_boot-rss1_boot)/rss1_boot) 
}

pval <- (sum(GLRstat_boot >= GLRstat)+1)/(num_boot+1)
print("P-value (wild) for testing the null of the linear against the alternative of VCM:") 
print(pval)
saveRDS(pval,"pval1.Rda")
save(pval,file="teststat_pval.Rda")
  
######################################################################################
###############################################################################################
###### Wild bootstrap for std errors and CI of the VCM coefficients ############
set.seed(123)
res_vcm   <- residuals(vcm_model) # VCM residuals
fit_vcm   <- fitted(vcm_model)
res_vcm   <- res_vcm - mean(res_vcm)
res_mat_boot  <- wild.boot(res_vcm, nboot=num_boot)
y_mat_boot    <- matrix(rep(fit_vcm,num_boot), ncol=num_boot) + res_mat_boot

# Generate objects (the number of objects should be the same as the number of coefficients + 1 for the avgcoef)
coef_mat_boot_1 <-matrix(NA,n,num_boot)
coef_mat_boot_2 <-matrix(NA,n,num_boot)
coef_mat_boot_3 <-matrix(NA,n,num_boot)
coef_mat_boot_4 <-matrix(NA,n,num_boot)
coef_mat_boot_5 <-matrix(NA,n,num_boot)
coef_mat_boot_6 <-matrix(NA,n,num_boot)
coef_mat_boot_7 <-matrix(NA,n,num_boot)
coef_mat_boot_8 <-matrix(NA,n,num_boot)

mgamma_q_boot_1 <-matrix(NA,5,num_boot)
mgamma_q_boot_2 <-matrix(NA,5,num_boot)
mgamma_q_boot_3 <-matrix(NA,5,num_boot)
mgamma_q_boot_4 <-matrix(NA,5,num_boot)
mgamma_q_boot_5 <-matrix(NA,5,num_boot)
mgamma_q_boot_6 <-matrix(NA,5,num_boot)
mgamma_q_boot_7 <-matrix(NA,5,num_boot)
mgamma_q_boot_8 <-matrix(NA,5,num_boot)

d1_2_boot <- matrix(NA,8,num_boot)
d1_2_boot <- matrix(NA,8,num_boot)
d1_3_boot <- matrix(NA,8,num_boot)
d1_4_boot <- matrix(NA,8,num_boot)
d1_5_boot <- matrix(NA,8,num_boot)
d2_3_boot <- matrix(NA,8,num_boot)
d2_4_boot <- matrix(NA,8,num_boot)
d2_5_boot <- matrix(NA,8,num_boot)
d3_4_boot <- matrix(NA,8,num_boot)
d3_5_boot <- matrix(NA,8,num_boot)
d4_5_boot <- matrix(NA,8,num_boot)


k <- ncol(gammas)
coef_mat_boot_avg <-matrix(NA,k,num_boot)

for(b in 1:num_boot) {	
#	print(b)
    yd <- y_mat_boot[,b]
yxz_dat <- cbind(yd,x_dat,ypz) 

    vcm_model_boot <- npscoef(bws=bw,betas=TRUE) 
    gammas_boot <- data.frame(vcm_model_boot$beta)

    coef_mat_boot_1[,b] <- gammas_boot[,1]
    coef_mat_boot_2[,b] <- gammas_boot[,2]
    coef_mat_boot_3[,b] <- gammas_boot[,3]
    coef_mat_boot_4[,b] <- gammas_boot[,4]
    coef_mat_boot_5[,b] <- gammas_boot[,5]
    coef_mat_boot_6[,b] <- gammas_boot[,6]
	coef_mat_boot_7[,b] <- gammas_boot[,7]
    coef_mat_boot_8[,b] <- gammas_boot[,8]
    
    coef_mat_boot_avg[,b] <- t(colMeans(coef(vcm_model_boot)))

# Computing the quintiles of VCM sorted by parents' income (ypz)
	tempor <-cbind(ypz,gammas_boot)
	tempor <- tempor[order(tempor[,1]),]
	mgamma_q_boot <-matrix(NA,5,dim(gammas_boot)[2])
	for(j in 1:dim(gammas)[2]) {
	temp <- data.frame(zval = tempor[,1], vcoef.value=tempor[,j+1], quintile=rep(NA, n))
	brks <- with(temp, quantile(zval, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1)))
	temp <- within(temp, quintile <- cut(zval, breaks = brks, labels = 1:5,include.lowest = TRUE))
	mgamma_q1<-mean(temp[temp[,3]==1,2])
	mgamma_q2<-mean(temp[temp[,3]==2,2])
	mgamma_q3<-mean(temp[temp[,3]==3,2])
	mgamma_q4<-mean(temp[temp[,3]==4,2])
	mgamma_q5<-mean(temp[temp[,3]==5,2])
	mgamma_q_boot[,j]<-c(mgamma_q1,mgamma_q2,mgamma_q3,mgamma_q4,mgamma_q5)
	}

	mgamma_q_boot_1[,b] <- mgamma_q_boot[,1]
	mgamma_q_boot_2[,b] <- mgamma_q_boot[,2]
	mgamma_q_boot_3[,b] <- mgamma_q_boot[,3]
	mgamma_q_boot_4[,b] <- mgamma_q_boot[,4]
	mgamma_q_boot_5[,b] <- mgamma_q_boot[,5]
	mgamma_q_boot_6[,b] <- mgamma_q_boot[,6]
	mgamma_q_boot_7[,b] <- mgamma_q_boot[,7]
	mgamma_q_boot_8[,b] <- mgamma_q_boot[,8]

	d1_2_boot[,b] <- mgamma_q_boot[1,] - mgamma_q_boot[2,]
	d1_3_boot[,b] <- mgamma_q_boot[1,] - mgamma_q_boot[3,]
	d1_4_boot[,b] <- mgamma_q_boot[1,] - mgamma_q_boot[4,]
	d1_5_boot[,b] <- mgamma_q_boot[1,] - mgamma_q_boot[5,]
	d2_3_boot[,b] <- mgamma_q_boot[2,] - mgamma_q_boot[3,]
	d2_4_boot[,b] <- mgamma_q_boot[2,] - mgamma_q_boot[4,]
	d2_5_boot[,b] <- mgamma_q_boot[2,] - mgamma_q_boot[5,]
	d3_4_boot[,b] <- mgamma_q_boot[3,] - mgamma_q_boot[4,]
	d3_5_boot[,b] <- mgamma_q_boot[3,] - mgamma_q_boot[5,] 
	d4_5_boot[,b] <- mgamma_q_boot[4,] - mgamma_q_boot[5,]

}

IGM <- data.frame(IGM = IGM)
write.csv(IGM,file = "IGM_Dau1.csv")

save(coef_mat_boot_1,file="coef_mat_boot_1.Rda")
save(coef_mat_boot_2,file="coef_mat_boot_2.Rda")
save(coef_mat_boot_3,file="coef_mat_boot_3.Rda")
save(coef_mat_boot_4,file="coef_mat_boot_4.Rda")
save(coef_mat_boot_5,file="coef_mat_boot_5.Rda")
save(coef_mat_boot_6,file="coef_mat_boot_6.Rda")
save(coef_mat_boot_7,file="coef_mat_boot_7.Rda")
save(coef_mat_boot_8,file="coef_mat_boot_8.Rda")

save(coef_mat_boot_avg,file="coef_mat_boot_avg.Rda")

save(mgamma_q_boot_1,file="mgamma_q_boot_1.Rda")
save(mgamma_q_boot_2,file="mgamma_q_boot_2.Rda")
save(mgamma_q_boot_3,file="mgamma_q_boot_3.Rda")
save(mgamma_q_boot_4,file="mgamma_q_boot_4.Rda")
save(mgamma_q_boot_5,file="mgamma_q_boot_5.Rda")
save(mgamma_q_boot_6,file="mgamma_q_boot_6.Rda")
save(mgamma_q_boot_7,file="mgamma_q_boot_7.Rda")
save(mgamma_q_boot_8,file="mgamma_q_boot_8.Rda")


save(d1_2_boot,file="d1_2_boot.Rda")
save(d1_3_boot,file="d1_3_boot.Rda")
save(d1_4_boot,file="d1_4_boot.Rda")
save(d1_5_boot,file="d1_5_boot.Rda")
save(d2_3_boot,file="d2_3_boot.Rda")
save(d2_4_boot,file="d2_4_boot.Rda")
save(d2_5_boot,file="d2_5_boot.Rda")
save(d3_4_boot,file="d3_4_boot.Rda")
save(d3_5_boot,file="d3_5_boot.Rda")
save(d4_5_boot,file="d4_5_boot.Rda")

save(d1_2,file="d1_2.Rda")
save(d1_3,file="d1_3.Rda")
save(d1_4,file="d1_4.Rda")
save(d1_5,file="d1_5.Rda")
save(d2_3,file="d2_3.Rda")
save(d2_4,file="d2_4.Rda")
save(d2_5,file="d2_5.Rda")
save(d3_4,file="d3_4.Rda")
save(d3_5,file="d3_5.Rda")
save(d4_5,file="d4_5.Rda")
