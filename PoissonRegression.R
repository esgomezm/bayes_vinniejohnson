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

#-----------------------------------------------------------------------------------------------------
# Model evaluating season 1986-1987 and 1987-1988 with respect to seasons 1985-1986 and 1988-1989
#-----------------------------------------------------------------------------------------------------

poisson_stan <- "
data{
  int<lower=1> n;
  int<lower=0,upper=1> x1[n];
  int<lower=0,upper=1> x2[n];
  int<lower=0,upper=1> x3[n];
  int<lower=0> y[n];
}

parameters{
  real a;
  real b1;
  real b2;
  real b3;
}
model{
for (i in 1:n) {
   y[i] ~ poisson(exp(a + b1*x1[i] + b2*x2[i] + b3*x3[i]));
  }
   a ~ normal(0.0, 1.0E3);
   b1 ~ normal(0.0, 1.0E3);
   b2 ~ normal(0.0, 1.0E3);
   b3 ~ normal(0.0, 1.0E3);
}
"
# Create the data object and call the stan program
# Evaluate scored field goals
model_data =  list(n = N, y = y,  x1 = x1, x2 = x2, x3 = x3) 

# Evaluate goal attempts
model_data =  list(n = N, y = x,  x1 = x1, x2 = x2, x3 = x3) 

poisson_stan_model = stan(model_code=poisson_stan, data=model_data, iter=100000, chains=3)
print(summary(poisson_stan_model)$summary, digits=5)


# Plot credible intervals and save the plot as a pdf
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/ci_poisson_season.pdf") 
stan_plot(poisson_stan_model, pars=c("a", "b1", "b2", "b3"))
dev.off() 

# Plot chains convergence and save the plot as a pdf
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/convergence_poisson_season.pdf") 
traceplot(poisson_stan_model, pars= c("a", "b1", "b2", "b3"), inc_warmup=TRUE) +
  coord_cartesian(xlim = c(0, 100000), ylim = c(-1, 3))
dev.off() 

# Plot chains distributions and save the plot as a pdf
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/density_poisson_season.pdf") 
plot(poisson_stan_model, pars=c("a", "b1", "b2", "b3"), show_density=TRUE, ci_level=0.95, fill_color="blue")
dev.off() 

# Plot the densities of obtained values in different ways
stan_dens(poisson_stan_model)

(dens <- stan_dens(poisson_stan_model, fill = "skyblue", ))
dens <- dens + ggtitle("Kernel Density Estimates\n") + xlab("")
dens

(dens_sep <- stan_dens(poisson_stan_model, separate_chains = TRUE, alpha = 0.1))
dens_sep + scale_fill_manual(values = c("red", "blue", "green"))

(dens_sep_stack <- stan_dens(poisson_stan_model, pars = c("a", "b1", "b2", "b3"), alpha = 0.5,
                             separate_chains = TRUE, position = "stack") + 
                            scale_fill_manual(values = c("orange", "yellow", "blue")))

# Use shinystan to get a nice navigator view of the model
library(shinystan)
launch_shinystan(poisson_stan_model)

#-----------------------------------------------------------------------------------------------------
# Model evaluating season 1986-1987 and 1987-1988 with respect to seasons 1985-1986 and 1988-1989
#-----------------------------------------------------------------------------------------------------

poisson_stan <- "
data{
  int<lower=1> n;
  int<lower=0,upper=1> x2[n];
  int<lower=0,upper=1> x3[n];
  int<lower=0> y[n];
}

parameters{
  real a;
  real b2;
  real b3;
}
model{
for (i in 1:n) {
   y[i] ~ poisson(exp(a + b2*x2[i] + b3*x3[i]));
  }
   a ~ normal(0.0, 1.0E3);
   b2 ~ normal(0.0, 1.0E3);
   b3 ~ normal(0.0, 1.0E3);
}
"
# Create the data object and call the stan program
model_data =  list(n = N, y = y,  x2 = x2, x3 = x3) 
poisson_stan_model = stan(model_code=poisson_stan, data=model_data, iter=100000, chains=3)
print(summary(poisson_stan_model)$summary, digits=5)

# Plot credible intervals and save the plot as a pdf
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/ci_poisson_reduced_season.pdf") 
stan_plot(poisson_stan_model, pars=c("a", "b2", "b3"))
dev.off() 

# Plot chains convergence and save the plot as a pdf
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/convergence_poisson_reduced_season.pdf") 
traceplot(poisson_stan_model, pars= c("a", "b2", "b3"), inc_warmup=TRUE) +
  coord_cartesian(xlim = c(0, 100000), ylim = c(-1, 3))
dev.off() 

#...............................................................................
#.......... JAGS ...............................................................
#...............................................................................
poisson = "
# poisson model
model{

for(i in 1:n) 
  {
  	y[i] ~ dpois(lambda[i])
  	log(lambda[i]) <- a + b1*x1[i] + b2*x2[i] + b3*x3[i]
  }
    a ~ dnorm(0, 0.001)
    b1 ~ dnorm(0, 0.001)
    b2 ~ dnorm(0, 0.001)
    b3 ~ dnorm(0, 0.001)
}
"
# Create the data object and call the stan program
model_data =  list(n = N, y = y,  x1 = x1, x2 = x2, x3 = x3) 
parametros = c("a", "b1", "b2","b3", "lambda")

poisson_model = jags(data=model_data, inits=NULL, parameters.to.save=parametros, 
                          n.chains=3, n.iter=100000, working.directory=ruta, 
                          model.file=textConnection(poisson))

poisson_model
info_model = poisson_model[["BUGSoutput"]][["summary"]]

poisson_model.mcmc = as.mcmc.list(poisson_model$BUGSoutput)
pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/convergence_poison_season_jags.pdf", height=20, width=10) 
xyplot(poisson_model.mcmc)
dev.off() 

pdf("/home/esgomezm/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/density_poison_season_jags.pdf", height=9, width=10) 
densityplot(poisson_model.mcmc)
dev.off() 

