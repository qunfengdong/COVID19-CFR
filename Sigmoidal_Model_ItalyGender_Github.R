
#####COVID-19 model age and CFR
require(MMAC)
require(mosaic)
require(mosaicCalc)
require(ggplot2)
require(nlstools)
library(xlsx)
#require(manipulate)

######### Function for Logistic model fitting

Logistic_gender_prediction <- function(gender = gender, low = low, high = high) {
  CFR_country = Italy_gender_age_CFR[Italy_gender_age_CFR$Gender == gender,]$CFR[low:high]
  Age_country = Italy_gender_age_CFR[Italy_gender_age_CFR$Gender == gender,]$Weighted_Age[low:high]
  #model estimation using nls
  CFR_country[which(CFR_country == 0)] = 0.001
  Logistic_model=nls(CFR_country ~ SSlogis(Age_country, Asym, xmid, scal), algorithm='port')
  return(Logistic_model)
}


### Data was described in our manuscript Table 1
Italy_gender_age_CFR = read.csv("Raw_Age_Gender_0_100_CFR_data_Italy.csv", header = T)
Italy_gender_age_CFR = Italy_gender_age_CFR[Italy_gender_age_CFR$Gender != "Total",]

### 1) model fitting
Logistic_Italy_Female = Logistic_gender_prediction(gender = "Female", low = 1, high = 10)
Logistic_Italy_Male = Logistic_gender_prediction("Male", 1, 10)




### 2) Logistic plot
p = ggplot(Italy_gender_age_CFR, aes(x= Weighted_Age, y=CFR, color=Gender), size =4) + 
scale_color_manual(values = c("Female" = "purple", "Male" = "green")) + geom_point() + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
xlab("Age") + ylab("Case fatality rate") +
theme(axis.text.x = element_text(size=8,face="plain",angle = 45, vjust=0.6), axis.title.x = element_text(size=12,face="plain")) +
  scale_x_continuous(breaks = round(seq(0, 99, by = 5),1))
  
###Asym/(1+exp((xmid-input)/scal))
fun.Female <- function(x) summary(Logistic_Italy_Female)$coef[1,1]/(1+exp((summary(Logistic_Italy_Female)$coef[2,1]-x)/summary(Logistic_Italy_Female)$coef[3,1]))
p2 = p + stat_function(fun = fun.Female, colour = "purple") 
fun.Male <- function(x) summary(Logistic_Italy_Male)$coef[1,1]/(1+exp((summary(Logistic_Italy_Male)$coef[2,1]-x)/summary(Logistic_Italy_Male)$coef[3,1]))
p2 + stat_function(fun = fun.Male, colour = "green")



#### 3) Model parameters
summary(Logistic_Italy_Female)$coef
summary(Logistic_Italy_Male)$coef

#### 4) Test normality and autocorrelation of residuals
ItalyFemale_residual = nlsResiduals(Logistic_Italy_Female)
ItalyMale_residual = nlsResiduals(Logistic_Italy_Male)

test.nlsResiduals(ItalyFemale_residual)
test.nlsResiduals(ItalyMale_residual)

#### 5) test the fitness of the model 
CFR_country = Italy_gender_age_CFR[Italy_gender_age_CFR$Gender == "Female",]$CFR[low:high]
Age_country = Italy_gender_age_CFR[Italy_gender_age_CFR$Gender == "Female",]$Weighted_Age[low:high]
data.frame(Age_country,CFR_country, fun.Female(Age_country))
ks.test(CFR_country, fun.Female(Age_country))

CFR_country = Italy_gender_age_CFR[Italy_gender_age_CFR$Gender == "Female",]$CFR[low:high]
Age_country = Italy_gender_age_CFR[Italy_gender_age_CFR$Gender == "Female",]$Weighted_Age[low:high]
data.frame(Age_country,CFR_country, fun.Male(Age_country))
ks.test(CFR_country, fun.Male(Age_country))

#### 6) Compute the corresponding age for a given CFR
Reversefun <- function(x,y) summary(y)$coef[2,1] - summary(y)$coef[3,1]*log(summary(y)$coef[1,1]/x-1)

model3 = Logistic_Italy_Female
Quantile = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95)
Female_CFR_quantile=c(summary(model3)$coef[1,1]*Quantile[1],summary(model3)$coef[1,1]*Quantile[2],summary(model3)$coef[1,1]*Quantile[3],summary(model3)$coef[1,1]*Quantile[4],summary(model3)$coef[1,1]*Quantile[5],summary(model3)$coef[1,1]*Quantile[6],summary(model3)$coef[1,1]*Quantile[7],summary(model3)$coef[1,1]*Quantile[8])
Female_quantile_age = Reversefun(ItalyFemale_CFR_quantile,Logistic_Italy_Female)
data.frame(Quantile, Female_CFR_quantile, Female_quantile_age)

model4 = Logistic_Italy_Male
Quantile = c(0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95)
MaleCFR_quantile=c(summary(model4)$coef[1,1]*Quantile[1],summary(model4)$coef[1,1]*Quantile[2], summary(model4)$coef[1,1]*Quantile[3],summary(model4)$coef[1,1]*Quantile[4],summary(model4)$coef[1,1]*Quantile[5],summary(model4)$coef[1,1]*Quantile[6],summary(model4)$coef[1,1]*Quantile[7], summary(model4)$coef[1,1]*Quantile[8])
Male_quantile_Age = Reversefun(Italy_MaleCFR_quantile,Logistic_Italy_Male)
data.frame(Quantile, MaleCFR_quantile, Male_quantile_Age)
