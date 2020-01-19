#-------------------------------------------------------------------------
#------------- Beta-binomial model for goal rates ------------------
#-------------------------------------------------------------------------
rm(list=ls()) # Clear the workspace
ruta = "/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/"
setwd(ruta)  
library(R2jags)
library(lattice)
library(psych)
library(rstan)
library(bayesplot)
options(mc.cores=3)
# options(mc.cores=parallel::detectCores()-1)
rstan_options(auto_write=TRUE)
# Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

## Read data
data <- read.delim("~/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/VinnieJohnsonCompleteClean.txt")
## Clean NA values from data
data_clean = na.omit(data)

# Create grouping variable for seasons
library(dummies)
s = dummy(data_clean$Season, sep = ".")
#J = length(colnames(s))
#season = matrix(0, dim(s)[1])
#for (j in 1:J)
#{
#  season = season + j*s[,j]
#}
#data_clean = cbind(data_clean,SeasonFactor=season)

# Set up the data
y = as.numeric(data_clean$FGScored)
x = as.numeric(data_clean$FGAttempts)
N = length(y)
s = data.frame(s)
x1 = s$Season.1985.1986
x2 = s$Season.1986.1987
x3 = s$Season.1987.1988
x4 = s$Season.1988.1989

#...............................................................................
#.......... STAN ...............................................................
#...............................................................................


normal_stan <- "
data{
  int<lower=1> n;
//  int<lower=0,upper=1> x1[n];
  int<lower=0,upper=1> x2[n];
//  int<lower=0,upper=1> x3[n];
  real x[n];
  real y[n];
}

parameters{
 real a;
 real b0;
// real b1;
 real b2;
// real b3;

  real<lower=0> sigma2;
}

transformed parameters {
  real<lower=0> sigma;
  sigma = sqrt(sigma2);
}

model{
for (i in 1:n) {
//y[i] ~ normal(a + b0*x[i] + b1*x1[i] + b2*x2[i] + b3*x3[i], sigma);
//y[i] ~ normal(a + b1*x1[i] + b2*x2[i] + b3*x3[i], sigma);
//y[i] ~ normal(a + b2*x2[i], sigma);
y[i] ~ normal(a + b0*x[i] +b2*x2[i] , sigma);
}

a ~ normal(0.0, 1.0E3);
b0 ~ normal(0.0, 1.0E3);
//b1 ~ normal(0.0, 1.0E3);
b2 ~ normal(0.0, 1.0E3);
//b3 ~ normal(0.0, 1.0E3);
sigma2 ~ inv_gamma(1.0E-3, 1.0E-3);
}
"


# Create the data object and call the stan program
# model_data =  list(n = N, y = y,x=x,  x1 = x1, x2 = x2, x3 = x3) 
model_data =  list(n = N, y = y,x=x, x2 = x2) 

# model_data =  list(n = N, y = y,  x1 = x1, x2 = x2, x3 = x3) 
# model_data =  list(n = N, y = y,  x2 = x2) 


normal_stan_model = stan(model_code=normal_stan, data=model_data, iter=100000, chains=3)
print(summary(normal_stan_model)$summary, digits=5)


# Plot credible intervals and save the plot as a pdf
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/ci_normal_reducedb0_season.pdf") 
# stan_plot(normal_stan_model, pars=c("a","b0", "b1", "b2","b3", "sigma"))
stan_plot(normal_stan_model, pars=c("a","b0","b2", "sigma"))
dev.off() 

# Plot chains convergence and save the plot as a pdf
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/convergence_normal_reducedb0_season.pdf") 
traceplot(normal_stan_model, pars= c("a","b0","b2", "sigma"), inc_warmup=TRUE) +
  coord_cartesian(xlim = c(0, 100000))
dev.off() 

# Plot chains distributions and save the plot as a pdf
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/density_normal_reducedb0_season.pdf") 
plot(normal_stan_model, pars=c("a","b0","b2", "sigma"), show_density=TRUE, ci_level=0.95, fill_color="blue")
dev.off() 


#...............................................................................
#.......... JAGS ...............................................................
#...............................................................................


linear_dummies = '
model
{
  # Likelihood
  for (i in 1:n) {
    mu[i] <- a + b0*x[i] + b1*x1[i] + b2*x2[i] + b3*x3[i] #
    y[i] ~ dnorm(mu[i],tau)
  }
    
  # Priors
  a ~ dnorm(0,1.0E-3) # Independent value, probably mean value
  b0 ~ dnorm(0, 1.0E-3) # Coefficient corresponding to each game season
  b1 ~ dnorm(0, 1.0E-3) # Coefficient corresponding to each game season
  b2 ~ dnorm(0, 1.0E-3) 
  b3 ~ dnorm(0, 1.0E-3) 

  # sigma ~ dt(0, 10^-2, 1)T(0,) # Variance of logit(field goal rate)
  tau ~ dgamma(0.001,0.001);
  # sigma ~ dunif(0, 1)
	sigma2 <- 1/tau
}
'

# Create the data object and call the stan program
model_data =  list(n = N, y = y, x = x, x1 = x1, x2 = x2, x3 = x3) 
parametros = c("a","b0", "b1", "b2","b3", "sigma2")

linear_model = jags(data=model_data, inits=NULL, parameters.to.save=parametros, 
                     n.chains=3, n.iter=100000, working.directory=ruta, 
                     model.file=textConnection(linear_dummies))

linear_model

linear_dummies = '
model
{
  # Likelihood
  for (i in 1:n) {
    mu[i] <- a + b0*x[i] + b2*x2[i] #
    y[i] ~ dnorm(mu[i],tau)
  }
    
  # Priors
  a ~ dnorm(0,1.0E-3) # Independent value, probably mean value
  b0 ~ dnorm(0, 1.0E-3) # Coefficient corresponding to each game season
  b2 ~ dnorm(0, 1.0E-3) 

  # sigma ~ dt(0, 10^-2, 1)T(0,) # Variance of logit(field goal rate)
  tau ~ dgamma(0.001,0.001);
  # sigma ~ dunif(0, 1)
	sigma2 <- 1/tau
}
'


# Create the data object and call the stan program
model_data =  list(n = N, y = y, x = x, x1 = x1, x2 = x2, x3 = x3) 
parametros = c("a","b0", "b2", "sigma2")

linear_model = jags(data=model_data, inits=NULL, parameters.to.save=parametros, 
                    n.chains=3, n.iter=100000, working.directory=ruta, 
                    model.file=textConnection(linear_dummies))

linear_model