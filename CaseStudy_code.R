#install.packages('ggplot2')
#install.packages('actuar')
#install.packages('Rlab')
#install.packages('MASS')
library(ggplot2)
library(actuar)
library(Rlab)
library(MASS)

#Please upload the data with 'Import Dataset'

#####Data Management##############################################################
#We have a first look at our data
#We print our data in order to get a little summary of all the different values that are contained in our data set and how many times they occur each
for (i in c(2,4,5,6,7,8)){
  print(table(data.frame(Data_set_2[[i]])))
}                     
summary(Data_set_2)

#We check if N/A values are contained in our data set
any(is.na(Data_set_2))   #It returns FALSE which means that no N/A values are contained in our dataset

##We check that all the values in CLM_FREQ are non-negative and included between 0 and 20 because we assumed that any values bigger than 20 would be inconsistent
min(Data_set_2[[2]])>=0 #It returns TRUE
#We check that all our claim frequencies are below 20
max(Data_set_2[[2]])<=20 #It returns TRUE 
#---> We confirm that the claim frequencies in CLM_FREQ are valid values 

##We check that all the values in CLM_AMT are positive, since a negative claim amount is not possible
min(Data_set_2[[3]])>=0  #It returns FALSE, which means we have to delete the row(s) containing the negative values
Data_set_2 <- Data_set_2[-c(which(Data_set_2[[3]]<0)),]  #We update our data set Data_set_2 by deleting the concerning row(s)
min(Data_set_2[[3]])>=0  #All our claim amounts are now indeed positive

max(Data_set_2[[3]]) #It returns 200'000, inconsistent with the rest of the data, we remove this row also
Data_set_2 <- Data_set_2[-c(which(Data_set_2[[3]]==200000)),]  #We update our data set Data_set_2 by removing the information concerning the policyholder with a claim amount of 200000 
max(Data_set_2[[3]]) #This command now returns 2283.425, which is now more consistent with the rest of the data

#Now that our data set is cleaned, let us store the columns for the claim frequency and the claim amounts in two different variables in order to simplify our manipulation of the code later on
CLM_FREQ <- Data_set_2[[2]]
CLM_AMT <- Data_set_2[[3]]

######summary stats################################################################
number_PolicyHolders <- nrow(Data_set_2) #This variable contians the number of policyholders
#CLM_FREQ
summary(CLM_FREQ)
mean_freq <- mean(CLM_FREQ) #This variable stores the mean of the claim frequencies
variance_freq <- var(CLM_FREQ) #This variable stores the variance of the claim frequencies
std_freq <- sqrt(variance_freq) #This variable stores the standard deviation of the claim frequencies
FREQ_histo <-   ggplot(Data_set_2, aes(x = CLM_FREQ)) +geom_bar(colour = 4, fill = "white") +
                geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, colour = "black")+
                ggtitle("Histogram of the claim frequencies")
print(FREQ_histo)
print(boxplot(CLM_FREQ))
#CLM_AMT
summary(CLM_AMT[CLM_AMT>0])
mean_amt <- mean(CLM_AMT[CLM_AMT>0]) #This variable stores the mean of all the claim amounts without taking in consideration the policyholders that have 0 claims
variance_amt <- var(CLM_AMT[CLM_AMT>0]) #This variable stores the variance of all the claim amounts without taking in consideration the policyholders that have 0 claims
std_amt <- sqrt(variance_amt) #This variable stores the standard deviation of all the claim amounts without taking in consideration the policyholders that have 0 claims
#we will create a 'derivate' dataset with only claim amounts larger than 0, that we will use in order to  generate plots
der_dataset2 <- Data_set_2[-c(which(Data_set_2[[3]]==0)),]
AMT_histo <-  ggplot(der_dataset2, aes(x = CLM_AMT)) +
              geom_histogram(aes(y = ..density..),colour = 4, fill = "white", bins = 20) +
              ggtitle("Histogram + density of claim amount")+
              geom_density(lwd = 1, colour = 4,fill = 4, alpha = 0.25)
AMT_BoxPlot <-ggplot(der_dataset2, aes(y = CLM_AMT)) + geom_boxplot()
print(AMT_histo)
print(AMT_BoxPlot)

#####Part 1: model for claims frequency############################################
#We create variables for every possible claim frequency by knowing that the maximal possible claim frequency is 8
freq_0 <- length(CLM_FREQ[CLM_FREQ==0])
freq_1 <- length(CLM_FREQ[CLM_FREQ==1])
freq_2 <- length(CLM_FREQ[CLM_FREQ==2])
freq_3 <- length(CLM_FREQ[CLM_FREQ==3])
freq_4 <- length(CLM_FREQ[CLM_FREQ==4])
freq_5 <- length(CLM_FREQ[CLM_FREQ==5])
freq_6 <- length(CLM_FREQ[CLM_FREQ==6])
freq_7 <- length(CLM_FREQ[CLM_FREQ==7])
freq_8 <- length(CLM_FREQ[CLM_FREQ==8])
#We now store this variables in a single vector
freq_vec <- c(freq_0,freq_1,freq_2,freq_3,freq_4,freq_5,freq_6,freq_7,freq_8)

###Poisson: we are going to evaluate if the Poisson distribution is a good model for the claim frequencies by performing the Chi-Square Goodness of Fit Test:
#A condition to use this test, is to check that each expected value is larger than 5, if not we have to group them 
#We need the lambda parameter
lambda <- as.double(fitdistr(CLM_FREQ,'Poisson')$estimate)
excp_freq <- dpois(0:8,lambda)*number_PolicyHolders #We generate a vector composed of the expected number of policyholders having a certain claim frequency
freq_vec_modPois <- c(freq_0,freq_1,freq_2,freq_3,freq_4,freq_5,freq_moreThan5=freq_6+freq_7+freq_8) #Here we have to group the frequencies greater than 6 together since freq_7, freq_8, freq_9 are smaller than 5
#now that the empirical frequencies are prepared, we create the corresponding vector with the density of a Poisson distribution
prob = c(dpois(0:5,mean_freq), sum(dpois(6:100,mean_freq)))
#we can now perform the Chi-Square Goodness of Fit Test:
res <- chisq.test(freq_vec_modPois, p=prob)
#result: X-squared = 10.673, df = 6, p-value = 0.09901

###Binomial
n = number_PolicyHolders
p = mean_freq/n
excp_freq <- dbinom(0:8,n,p)*number_PolicyHolders
freq_vec_modBinom <- c(freq_0,freq_1,freq_2,freq_3,freq_4,freq_5,freq_moreThan5=freq_6+freq_7+freq_8)
prob = c(dbinom(0:5,n,p), sum(dbinom(6:100,n,p)))
res <- chisq.test(freq_vec_modBinom, p=prob)
#results: X-squared = 10.881, df = 6, p-value = 0.09211

###Geometric
p <- as.double(fitdistr(CLM_FREQ, 'geometric')$estimate)
excp_freq <- dgeom(0:8,p)*number_PolicyHolders
freq_vec_modGeom <- c(freq_0,freq_1,freq_2,freq_3,freq_4,freq_5,freq_6,freq_7,freq_8)
prob = c(dgeom(0:7,p), sum(dgeom(8:100,p)))
res <- chisq.test(freq_vec_modGeom, p=prob)
#results: X-squared = 222.83, df = 8, p-value < 2.2e-16

###Negative Binomial
p <- mean_freq/variance_freq
r <- (mean_freq*p)/(1-p)
excp_freq <- dnbinom(0:8,r,p)*number_PolicyHolders
freq_vec_modNbinom <- c(freq_0,freq_1,freq_2,freq_3,freq_4,freq_5,freq_moreThan5=freq_6+freq_7+freq_8)
prob = c(dnbinom(0:5,r,p), sum(dnbinom(6:100,r,p)))
res <- chisq.test(freq_vec_modNbinom, p=prob)
#result: X-squared = 3.7869, df = 6, p-value = 0.7055

#Our best match to model our claim frequencies is the negative binomial distribution. We validate the parameter for later:
p <- mean_freq/variance_freq
r <- (mean_freq*p)/(1-p)
#we draw a plot with negative binomial mass and data mass
plot(freq_vec, type = 'l', col = 'blue', main = 'Claim frequency and Model',)
lines(dnbinom(0:8,r,p)*number_PolicyHolders, col = 'firebrick3')
legend(6.5, 320, legend=c("Data_set_2", "NegBin(13.3886,0.8949)"),
       col=c("firebrick3", "blue"), lty=1:1, cex=0.8)

#####Part 2: model for claims size############################################
#We now perform the same concepts than above for the claim amounts. This can be done by the Kolmogorov-Smirnov Goodness of Fit-Test which is applied for continuos distributions
##Exponential distribution
lambda <- as.double(fitdistr(CLM_AMT[CLM_AMT>0],'exponential')$estimate)
ks.test(CLM_AMT[CLM_AMT>0], "pexp",lambda)
#results: D = 0.5814, p-value < 2.2e-16

##Normal distribution
mu <- as.double(fitdistr(CLM_AMT[CLM_AMT>0],'normal')$estimate[1])
sigma <- as.double(fitdistr(CLM_AMT[CLM_AMT>0],'normal')$estimate[2])
ks.test(CLM_AMT[CLM_AMT>0], "pnorm",mu, sigma)
#results: D = 0.013638, p-value = 0.9987

##Log-normal distribution
mu <- as.double(fitdistr(CLM_AMT[CLM_AMT>0],'lognormal')$estimate[1])
sigma <- as.double(fitdistr(CLM_AMT[CLM_AMT>0],'lognormal')$estimate[2])
ks.test(CLM_AMT[CLM_AMT>0], "plnorm",mu, sigma)
#results: = 0.015992, p-value = 0.9888

##Gamma distribution
alpha <- as.double(fitdistr(CLM_AMT[CLM_AMT>0],'gamma')$estimate[1])
lambda <- as.double(fitdistr(CLM_AMT[CLM_AMT>0],'gamma')$estimate[2])
ks.test(CLM_AMT[CLM_AMT>0], "pgamma",alpha, lambda)
#results: D = 0.013236, p-value = 0.9992

#Our best match to model our claim amounts is the gamma distribution. We validate the parameters for later:
alpha <- as.double(fitdistr(CLM_AMT[CLM_AMT>0],'gamma')$estimate[1])
lambda <- as.double(fitdistr(CLM_AMT[CLM_AMT>0],'gamma')$estimate[2])
#We draw a Q-Q plot
qqplot(CLM_AMT[CLM_AMT>0], rgamma(1000,alpha,lambda),xlim = c(1700,2300),ylim = c(1700,2300), main = 'QQ-plot of claim amount')
abline(0,1,col='firebrick1')

#We draw a plot with the density of a gamma distribution and the density of the data
Claim_amt_larger_0_density <- density(CLM_AMT[CLM_AMT>0])
plot(dgamma(0:2500,alpha,lambda), type = 'l', col = 'firebrick3', main = 'Claim amount and Model', xlim = c(1400,2400))
lines(Claim_amt_larger_0_density, col = 'blue')
legend(1400, 0.004, legend=c("Data_set_2", " Γ(490.15,0.24)"),
       col=c("blue", "firebrick3"), lty=1:1, cex=0.8)

#####Part 3: risk premium############################################

#The total premium income 
tot_prem = sum(Data_set_2[[9]])
#The average premium per policyholder
effectiv_avg_prem = tot_prem/number_PolicyHolders

#the risk premium computed from our data
tot_claims = sum(CLM_AMT)
data_avg_risk_prem = tot_claims/number_PolicyHolders 

#The risk premium computed from our models (see paper for description of model)
set.seed(56) #We set the seed in order to generate the same sequence of random numbers when running our simulations
nb_sim = 10000
P_0claim = dnbinom(0,r,p) #computed from the model of negative binomial for claim frequencies
#This function generates nb_sim bernoulli random variables, and for those in which a claim occurs (=1), their size is then generated from a gamma distribution in order to compute the expected loss per policyholder
sim_loss_perPolicyHolder <- rcompound(nb_sim, rbern(1-P_0claim), rgamma(alpha,lambda)) 
E_loss_perPolicyHolder <- sum(sim_loss_perPolicyHolder)/nb_sim
sim_loss = rcompound(nb_sim, rbinom(number_PolicyHolders,1-P_0claim), rgamma(alpha,lambda)) #The same as above, just replacing the bernoulli distribution by a binomial with n = number_PolicyHolders, in order to compute the expected aggregate loss
Agg_E_loss = sum(sim_loss)/nb_sim

#####Part 4: Tariff###################################################
E_cashflow = tot_prem - Agg_E_loss #We calculate the expected cashflow of the insurance company
#We draw a plot in order to have a visual representation
bars = c(tot_prem, Agg_E_loss, E_cashflow)
tarrif_plot =barplot(bars,names.arg=c('Premium','E[Claim]','Diff'),
        border="firebrick2",ylim=c(-1500000,2000000), density=c(5,5,100), width = c(1,1,1), main = 'Expected cashflow for portfolio')
text(x=tarrif_plot, y = c(tot_prem,Agg_E_loss,E_cashflow), label = c(round(tot_prem,2),round(Agg_E_loss,2),round(E_cashflow,2)),pos = 3)


min_expected_agg_claim_size = min(sim_loss) 
min_expected_agg_claim_size > tot_prem #returns TRUE, meaning that the aggregate claim size will almost always be bigger than the total premium_income


#####Part 5: Value at Risk############################################
set.seed(56)
nb_sim = 100000
vector_aggregate_claim = rcompound(nb_sim, rbinom(number_PolicyHolders,1-P_0claim), rgamma(alpha,lambda))
vector_aggregate_claim_ordered = vector_aggregate_claim[order(vector_aggregate_claim)] #We order the vector of aggregate claim size in order to easily find the quantiles
VaR_0.005 = vector_aggregate_claim_ordered[99500] #99.5%-quantile
VaR_0.01 = vector_aggregate_claim_ordered[99000] #99%-quantile
Median_agg_loss = vector_aggregate_claim_ordered[50000] #The median

plot(density(vector_aggregate_claim), main = 'Value at Risk')
abline(v = VaR_0.005,col='red')
abline(v = VaR_0.01,col='purple')
abline(v = Median_agg_loss,col='blue')
legend(1430000, 1.5*10^-5, legend=c("Median", "VaR_0.005", 'Var_0.01'),
       col=c("blue", "red",'purple'), lty=1:1, cex=0.8)
