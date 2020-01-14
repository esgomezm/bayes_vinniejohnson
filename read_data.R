  ## Read data
  data <- read.delim("~/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/VinnieJohnsonCompleteClean.txt")
  # data_simple <- read.csv("~/Documents/INFERENCIA-BAYESIANA/vinnie_johnson/VinnieJohnsonSimple.txt")
  
  ## Clean NA values from data
  data_clean = na.omit(data)
  
  ## Calculate goals portions
  data_clean = cbind(data_clean,FGPortion=data_clean$FGScored/data_clean$FGAttempts)
  data_clean = cbind(data_clean,FTPortion=data_clean$FTScored/data_clean$FTAttempts)
  data_clean = cbind(data_clean,SeasonMonth = paste(data_clean$Season, data_clean$Month, sep="-"))
  data_clean = cbind(data_clean,SeasonMonthDay = paste(data_clean$SeasonMonth, data_clean$Day, sep="-"))
  mu = mean(data_clean$FGPortion)
  sd = sqrt(var(data_clean$FGPortion))
  
  
  mu <- ddply(data_clean, "Season", summarise, grp.mean=mean(FGPortion))
  head(mu)
  
  ## Plot field goals and free throwns rates distributions
  library(ggplot2)
  library(plyr)
  mu <- ddply(data_clean, "Season", summarise, grp.mean=mean(FGPortion))
  head(mu)
  
  #The differences are not normally distributed
  dif=data_clean$FGPortion
  qqnorm(scale(dif),main="Q-Q plot of\nFGrates standardized")
  qqline(scale(dif))
  
  #The differences are not normally distributed
  dif=data_clean$FGScored
  qqnorm(scale(dif),main="Q-Q plot of\nFG scored standardized")
  qqline(scale(dif))
  
  
  # Field goals rate per season
  ggplot(data_clean, aes(x=FGAttempts, color=Season, fill=Season)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
    geom_density(alpha=0.1)+
    # geom_vline(data=mu, aes(xintercept=grp.mean, color=Season),
    #            linetype="dashed")+
    labs(x="y", y = "Density")+
    theme_classic()
  
  # Field Goals total rates 
  ggplot(data_clean, aes(x=FGAttempts)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 30)+
    geom_density(alpha=0.6)+
    labs(x="y", y = "Density")+
    theme_classic()
  
  # Free throwns total rates 
  ggplot(data_clean, aes(x=FTPortion)) +
    geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
    geom_density(alpha=0.6)+
    labs(title="Free throwns rate histogram plot",x="Free throwns rate", y = "Density")+
    theme_classic()
  
  # linear trend + confidence interval
 ggplot(data_clean, aes(x=FGAttempts, y=FGScored)) +
    geom_point() +
    geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)+
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 25))
  
  ggplot(data_clean, aes(x=FGAttempts, y=FGScored, shape=Season, color=Season)) +
    geom_point(size=3) +
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 25))
  
  ggplot(data_clean, aes(y=FGScored,x=SeasonMonthDay, color=Season)) +
    geom_point(size=2) +
    scale_x_discrete(breaks = levels(data_clean$SeasonMonthDay)[c(T, rep(F, 90))])+
    theme(axis.title.x=element_blank())
##--------------------------------------------------------

