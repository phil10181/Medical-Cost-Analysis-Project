getwd()
setwd("C:\\Users\\philb\\OneDrive\\Documents\\Data Projects")
medicalData <- read.csv("medical_costs.csv")

View(medicalData) #opens our data in an expanded form in a new tab
str(medicalData) #provides sumamry of our variables and their associated first couple values
summary(medicalData) #provides measures of averages for each variable
#head(medicalData) allows us to view the first few rows of our data

#Data cleaning: removing missing values, ensuring numeric format, Outlier handling, standardization of data 

#this findoutliers function looks for outliers within a variable and removes them. 
#when the code is ran, all variables are checked for outliers. 
findoutliers <- function(x){
  Q1 <- quantile(x, 0.25, na.rm=TRUE)
  Q3 <- quantile(x, 0.75, na.rm=TRUE)
  IQR <- Q3 - Q1
  upperbound <- Q3+ 1.5 *IQR
  lowerbound <- Q1-1.5*IQR
  outliers <- x[x<lowerbound | x>upperbound]
  return(outliers)
}
#sapply checks each variable within medicalData for outliers. 
outliers_list <- sapply(medicalData, function(x){
  if(is.numeric(x)){
    findoutliers(x)
  } else{
    NA
  }
})

print(outliers_list)
remove(outliers_list)

#-- 

#Multicollinearity tests: determining if independent variables are correlated with one another
library(car) # the car libarary is needed to run a vif test for multicollinearity
print(vif(lm(Medical.Cost~Region+Smoker+Children+Sex+Age+BMI, data=medicalData)))

#----------------------------------------------

#Testing for normality among the subgroups of selected variables gender, smoking status, and region: 

#testing Gender for normality
qqnorm(medicalData$Medical.Cost[medicalData$Sex=="female"], main="Normal QQ plot for Females data")
qqline(medicalData$Medical.Cost[medicalData$Sex=="female"])
        #Our medical data for females is skewed. 
qqnorm(medicalData$Medical.Cost[medicalData$Sex=="male"], main="Normal QQ plot for Males data")
qqline(medicalData$Medical.Cost[medicalData$Sex=="male"])

#testing Smoker Status for normality
qqnorm(medicalData$Medical.Cost[medicalData$Smoker=="yes"], main="Normal QQ plot for smokers")
qqline(medicalData$Medical.Cost[medicalData$Smoker=="yes"])
qqnorm(medicalData$Medical.Cost[medicalData$Smoker=="no"], main="Normal QQ plot for non-smokers")
qqline(medicalData$Medical.Cost[medicalData$Smoker=="no"])

#testing Region for normality: 
qqnorm(medicalData$Medical.Cost[medicalData$Region=="northeast"], main="Normal QQ plot for Northeast")
qqline(medicalData$Medical.Cost[medicalData$Region=="northeast"])
qqnorm(medicalData$Medical.Cost[medicalData$Region=="southeast"], main="Normal QQ plot for Southeast")
qqline(medicalData$Medical.Cost[medicalData$Region=="southeast"])
qqnorm(medicalData$Medical.Cost[medicalData$Region=="southwest"], main="Normal QQ plot for Southwest")
qqline(medicalData$Medical.Cost[medicalData$Region=="southwest"])
qqnorm(medicalData$Medical.Cost[medicalData$Region=="northwest"], main="Normal QQ plot for Northwest")
qqline(medicalData$Medical.Cost[medicalData$Region=="northwest"])

#Creating EDA's in graphing the data and running hypothesis tests on my findings
library(ggplot2)

#Testing and Visualizing Gender's impact on medical costs
ggplot(medicalData, aes(x=Sex, y =Medical.Cost))+
  labs(title="Medical Cost and Gender",x="Gender", y="Costs")+
  geom_boxplot()
  print(wilcox.test(Medical.Cost~Sex, data=medicalData)) #test if medians are significant. 
  print(t.test(Medical.Cost~Sex, data=medicalData))
  summary(lm(Medical.Cost~Sex, data=medicalData)) #simple regression for gender's impact
  summary(lm(Medical.Cost~Sex+Smoker+Children+Region+Age+BMI, data=medicalData))#multiple linear regression for comparison  
  aggregate(Medical.Cost~Sex,data=medicalData,summary) #summary of spread
  print(sd(medicalData$Medical.Cost[medicalData$Sex=="male"]))
  print(sd(medicalData$Medical.Cost[medicalData$Sex=="female"]))
  
#---
  
#Testing and visualizing Smoker Status's impact on medical cost
ggplot(medicalData, aes(x=Smoker, y=Medical.Cost))+
  labs(title="Medical Cost and Smoking",x="Smoker", y="Costs")+
  stat_summary(fun=mean, geom="bar") #creating the box plot
  aggregate(Medical.Cost~Smoker, data=medicalData,summary)
  print(sd(medicalData$Medical.Cost[medicalData$Smoker=="yes"]))
  print(sd(medicalData$Medical.Cost[medicalData$Smoker=="no"]))
  
  #checking the assumption for homogeneity of variance:
  print(var(medicalData$Medical.Cost[medicalData$Smoker=="yes"]))
  print(var(medicalData$Medical.Cost[medicalData$Smoker=="no"]))
  
  #2 pair t test comparing medical costs for smokers vs non-smokers:
  print(t.test(Medical.Cost~Smoker, data=medicalData))

#---
  
#Testing and visualizing Region's impact on medical cost
ggplot(medicalData, aes(x=Region, y=Medical.Cost))+
  labs(title="Medical Cost and Region",x="Region", y="Costs")+
  geom_boxplot()
  aggregate(Medical.Cost~Region, data=medicalData,summary)
  
  #kruskal wallis test for significant differences in median medical costs by region
  kruskal_test <-kruskal.test(Medical.Cost~Region, data=medicalData)
  print((kruskal_test))
  summary(lm(Medical.Cost~Region, data=medicalData))
  summary(lm(Medical.Cost~Region+Smoker+Children+Sex+Age+BMI, data=medicalData))
  print(sd(medicalData$Medical.Cost[medicalData$Region=="northeast"]))
  print(sd(medicalData$Medical.Cost[medicalData$Region=="northwest"]))
  print(sd(medicalData$Medical.Cost[medicalData$Region=="southeast"]))
  print(sd(medicalData$Medical.Cost[medicalData$Region=="southwest"]))
  
  
  
  
  
  
#due to anomalies in gender and , we need to run a multiple regression test in R: