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
J = length(colnames(s))
season = matrix(0, dim(s)[1])
for (j in 1:J)
{
  season = season + j*s[,j]
}
data_clean = cbind(data_clean,SeasonFactor=season)

# Set up the data
y = as.numeric(data_clean$FGScored)
x = as.numeric(data_clean$FGAttempts)
season = data_clean$SeasonFactor
N = length(y)

#...............................................................................
#.......... STAN ...............................................................
#...............................................................................
beta_bin_stan <- "
data{
  int<lower=1> N; // data size
  int<lower=0> y[N]; // Field goals
  int<lower=0> x[N]; // Goal attempts
}

parameters{
  real<lower=0> phi;
  real<lower=0, upper=1> p;
  real<lower=0, upper=1> mu;
}

model{
  phi ~ uniform(0, 1000);
  mu ~ uniform(0, 1);
  p ~ beta(mu*phi,(1-mu)*phi); // Prior density beta
  for (i in 1:N) {
     y[i] ~ binomial(x[i], p);
    }
}
"

beta_bin_stan_seasons <- "
data{
  int<lower=1> N; // data size
  int<lower=1> J; // data size
  int<lower=0> y[N]; // Field goals
  int<lower=0> x[N]; // Goal attempts
  int<lower=1, upper=4> season[N];
}

parameters{
  real<lower=0> phi[J];
  real<lower=0, upper=1> p[J];
  real<lower=0, upper=1> mu[J];
}

model{
  for (j in 1:J) {
    p[j] ~ beta(mu[j]*phi[j],(1-mu[j])*phi[j]); // Prior density beta
    mu[j] ~ uniform(0, 1);
    phi[j] ~ uniform(0, 1000);
  }
  for (i in 1:N) {
     y[i] ~ binomial(x[i], p[season[i]]);
    }
}
"

# Create the data object and call the stan program
model_data =  list(N = N, y = y, x = x, J=J, season=season)
beta_bin_stan_seasons_model = stan(model_code=beta_bin_stan_seasons, data=model_data, iter=100000, chains=3)
print(summary(beta_bin_stan_seasons_model)$summary, digits=5)

# Chequeo de convergencias
stan_plot(beta_bin_stan_seasons_model, pars=c("p", "mu"), outer_level = 0.80)

# Open a pdf file
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/convergence_betabin_season.pdf") 
traceplot(beta_bin_stan_seasons_model, pars= "p", inc_warmup=TRUE) +
coord_cartesian(xlim = c(0, 100000), ylim = c(0.3, 0.7))
# Close the pdf file
dev.off() 

pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/convergence_mu_betabin_season.pdf") 
traceplot(beta_bin_stan_seasons_model,pars=c("mu"), inc_warmup=TRUE) +
  coord_cartesian(xlim = c(0, 100000), ylim = c(0, 1))
# Close the pdf file
dev.off() 

pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/density_betabin_season.pdf") 
plot(beta_bin_stan_seasons_model, pars=c("p"), show_density=TRUE, ci_level=0.95, fill_color="blue")
dev.off() 

pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/density_mu_betabin_season.pdf") 
plot(beta_bin_stan_seasons_model, pars=c("mu"), show_density=TRUE, ci_level=0.95, fill_color="blue")
dev.off() 

pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/density_phi_betabin_season.pdf") 
plot(beta_bin_stan_seasons_model, pars=c("phi"), show_density=TRUE, ci_level=0.95, fill_color="blue")
dev.off() 

# Alternativa via Navegador
library(shinystan)
launch_shinystan(beta_bin_stan_seasons_model)

# Create the data object and call the stan program
model_data =  list(N = N, y = y, x = x)
beta_bin_stan_model = stan(model_code=beta_bin_stan, data=model_data, iter=100000, chains=3)
print(summary(beta_bin_stan_model)$summary, digits=5)
# Chequeo de convergencias
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/convergence_betabin.pdf") 
traceplot(beta_bin_stan_model, pars= "p", inc_warmup=TRUE)
dev.off() 

pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/density_betabin.pdf") 
plot(beta_bin_stan_model, pars=c("p"), show_density=TRUE, ci_level=0.95, fill_color="blue")
dev.off() 

# Alternativa via Navegador
library(shinystan)
launch_shinystan(normal_model)


#...............................................................................
#.......... JAGS ...............................................................
#...............................................................................


beta_bin_jags = "
# Beta binomial model
model{

for(i in 1:N) 
  {
  	y[i] ~ dbin(p, x[i])
  }
    p ~ dbeta(mu*phi,(1-mu)*phi) # Prior density beta
    phi ~ dunif(0, 1000)
    mu ~ dunif(0,1)
}
"

beta_bin_jags_seasons = "
# Beta binomial model
model{

for(i in 1:N) 
  {
  	y[i] ~ dbin(p[season[i]], x[i])
  }
for (j in 1:J)
  {
    p[j] ~ dbeta(mu[j]*phi[j],(1-mu[j])*phi[j]) # Prior density beta
    phi[j] ~ dunif(0, 1000)
    mu[j] ~ dunif(0,1)
    }
    
}
"
model_data =  list(N = N, y = y, x = x)
parametros = c("p", "phi", "mu")

beta_bin_jags_model = jags(data = model_data, inits=NULL, parameters.to.save = parametros, 
                                   n.chains=3, n.iter=100000, working.directory=ruta, 
                                   model.file=textConnection(beta_bin_jags_seasons))

beta_bin_jags_model

beta_bin_jags_model.mcmc = as.mcmc.list(beta_bin_jags_model$BUGSoutput)


#--------------------------------------------------------------------------------------------------
model_data =  list(N = N, y = y, x = x, J = J, season=season)
parametros = c("p", "phi", "mu")

beta_bin_jags_seasons_model = jags(data = model_data, inits=NULL, parameters.to.save = parametros, 
            n.chains=3, n.iter=100000, working.directory=ruta, 
            model.file=textConnection(beta_bin_jags_seasons))

beta_bin_jags_seasons_model

beta_bin_jags_seasons_model.mcmc = as.mcmc.list(beta_bin_jags_seasons_model$BUGSoutput)
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/convergence_betabin_season_jags.pdf", height=20, width=10) 
xyplot(beta_bin_jags_seasons_model.mcmc)
dev.off() 

pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/density_betabin_season_jags.pdf", height=9, width=10) 
densityplot(beta_bin_jags_seasons_model.mcmc)
dev.off() 
