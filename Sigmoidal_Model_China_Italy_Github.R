#####COVID-19 model age and death_rate
require(ggplot2)
require(nlstools)


######### ######### Function for Logistic model fitting

Logistic_country_prediction <- function(country = country, low = low, high = high) {
  Death_country = China_Italy_age_CFR[China_Italy_age_CFR$Country == country,]$CFR[low:high]
  Age_country = China_Italy_age_CFR[China_Italy_age_CFR$Country == country,]$Weighted_Age[low:high]
  Death_country[which(Death_country == 0)] = 0.001
  Logistic_model=nls(Death_country ~ SSlogis(Age_country, Asym, xmid, scal), algorithm='port')
  return(Logistic_model)
}

### Data was described in our manuscript Table 1
China_Italy_age_CFR = read.csv("China_Italy_WeightedAge_0_100_CFR.csv", header = T)


### 1) model fitting
Logistic_China = Logistic_country_prediction("China", low = 1, high = 9)
Logistic_Italy = Logistic_country_prediction("Italy", low = 1, high = 10)


### 2) Logistic plot
p = ggplot(China_Italy_age_CFR, aes(x=Weighted_Age, y=CFR, color=Country), size =4) +
    scale_color_manual(values = c("China" = "red", "Italy" = "blue")) + geom_point() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    xlab("Age") + ylab("Case fatality rate") +
    theme(axis.text.x = element_text(size=8,face="plain",angle = 45, vjust=0.6),
          axis.title.x = element_text(size=12,face="plain")) +
      scale_x_continuous(breaks = round(seq(0, 99, by = 5),1))
  ###Asym/(1+exp((xmid-input)/scal))
  
  fun.China <- function(x) summary(Logistic_China)$coef[1,1]/(1+exp((summary(Logistic_China)$coef[2,1]-x)/summary(Logistic_China)$coef[3,1]))
  p2 = p + stat_function(fun = fun.China, colour = "red") 
  fun.Italy <- function(x) summary(Logistic_Italy)$coef[1,1]/(1+exp((summary(Logistic_Italy)$coef[2,1]-x)/summary(Logistic_Italy)$coef[3,1]))
  p2 + stat_function(fun = fun.Italy, colour = "blue")
  
  
#### 3) model parameters

summary(Logistic_China)$coef
summary(Logistic_Italy)$coef
  
China_residual = nlsResiduals(Logistic_Italy)
Italy_residual = nlsResiduals(Logistic_Italy)
  
#### 4) residual test on model
  test.nlsResiduals(Italy_residual)
  test.nlsResiduals(China_residual)
  
#### 5) fitness of model 
  Death_country = China_Italy_age_CFR[China_Italy_age_CFR$Country == "China",]$CFR[1:9]
  Age_country = China_Italy_age_CFR[China_Italy_age_CFR$Country == "China",]$Weighted_Age[1:9]
  print(data.frame(Age_country,Death_country, fun.China(Age_country)))
  print(ks.test(Death_country, fun.China(Age_country)))
 
  
  Death_country = China_Italy_age_CFR[China_Italy_age_CFR$Country == "Italy",]$CFR[1:10]
  Age_country = China_Italy_age_CFR[China_Italy_age_CFR$Country == "Italy",]$Weighted_Age[1:10]
  print(data.frame(Age_country,Death_country, fun.Italy(Age_country)))
  print(ks.test(Death_country, fun.Italy(Age_country)))
  

## 6) Find the correcponding age for a given CFR
Reversefun <- function(x,y) summary(y)$coef[2,1] - summary(y)$coef[3,1]*log(summary(y)$coef[1,1]/x-1)

model1 = Logistic_China
Quantile = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95)
China_CFR_quantile = c(summary(model1)$coef[1,1]*Quantile[1],summary(model1)$coef[1,1]*Quantile[2], summary(model1)$coef[1,1]*Quantile[3],summary(model1)$coef[1,1]*Quantile[4],summary(model1)$coef[1,1]*Quantile[5],summary(model1)$coef[1,1]*Quantile[6],summary(model1)$coef[1,1]*Quantile[7], summary(model1)$coef[1,1]*Quantile[8])
China_quantile_age = Reversefun(China_CFR_quantile,Logistic_China)
data.frame(Quantile,China_CFR_quantile, China_quantile_age)

model2 = Logistic_Italy
Quantile = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95)
Italy_CFR_quantile = c(summary(model2)$coef[1,1]*Quantile[1],summary(model2)$coef[1,1]*Quantile[2], summary(model2)$coef[1,1]*Quantile[3],summary(model2)$coef[1,1]*Quantile[4],summary(model2)$coef[1,1]*Quantile[5],summary(model2)$coef[1,1]*Quantile[6],summary(model2)$coef[1,1]*Quantile[7],summary(model1)$coef[1,1]*Quantile[8])
Italy_quantile_age = Reversefun.Italy(Italy_CFR_quantile)
data.frame(Quantile, Italy_CFR_quantile,Italy_quantile_age)


