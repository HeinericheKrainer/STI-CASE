# Setting working directory
getwd()
# Vectors initialization
x<-c(1345) #c represents concatenation/x is a container
x=seq(1,10,by=2)
summary(x)
x
# working on a matrix
w=c(1,4,6,7)
k=c(1,4,6,8)
m1=matrix(w,nrow=2,byrow=T)
m2=matrix(k,nrow=2,byrow=F)
m3=cbind(m1,m2) #combining matrices column wise
m4=rbind(m1,m2) # combining matrices row wise
print(m1)
print(m2)
print(m3)
print(m4)

# Data frame
age=c(1,2,3,4)
gender=c("male","female","male","female")
education=c("informal","primary","secondary","university")
table=data.frame(age,gender,education)
print(table)
summary(table)
as.factor(gender) #categorizing
table$gender=as.factor(gender)
summary(table)
str(table) #structure of table
list(x,m1,table)
#installing packages

install.packages('boot') 
install.packages('car')
install.packages('caret')
install.packages('coin')
install.packages('correlation')
install.packages('corrplot')
install.packages('dplyr')
install.packages('epitools')
install.packages('factoextra')
install.packages('FactoMineR')
install.packages('finalfit')
install.packages('gapminder')
install.packages('ggfortify')
install.packages('ggplot2')
install.packages('ggpubr')
install.packages('ggsurvfit')
install.packages('gplots')
install.packages('gridExtra')
install.packages('gtsummary')
install.packages('haven')
install.packages('HH')
install.packages('InformationValue')
install.packages('irr')
install.packages('ISLR')
install.packages('ISwR')
install.packages('labelled')
install.packages('learnr')
install.packages('lessR')
install.packages('likert')
install.packages('likert')
install.packages('lsmeans')
install.packages('magrittr')
install.packages('MASS')
install.packages('multcomp')
install.packages('nlme')
install.packages('nnet')
install.packages('officer')
install.packages('PerformanceAnalytics')
install.packages('pls')
install.packages('pscl')
install.packages('psych')
install.packages('pwr')
install.packages('questionr')
install.packages('R2wd')
install.packages('RColorBrewer')
install.packages('rcompanion')
install.packages('readr')
install.packages('readxl')
install.packages('reshape2')
install.packages('rmarkdown')
install.packages('robustbase')
install.packages('rstatix')
install.packages('RVAideMemoire')
install.packages('scales')
install.packages('smd')
install.packages('survminer')
install.packages('tidycmprsk')
install.packages('tidyverse')
install.packages('tinytex')
install.packages('vcd')
install.packages('xfun')
install.packages('yaml')

# Attaching packages
library

xfun::pkg_attach(c('haven','foreign','boot','caret','dplyr','readr','readxl','ggplot2','HH','labelled','pwr','psych','MASS','officer','pls','Hmisc','gtsummary','tidyverse'))
xfun::pkg_attach(c('gplots'))

#Random number generator(RNG)reproducing copy or images of
set.seed(1000)
# importing dataset
# setting up a working directory
getwd()
setwd("C:/Users/User/OneDrive/Documents/HKL AMREC")
getwd()
library(haven)
stidata_unclean <- read_dta("R dataset/Datasets/stidata_unclean.dta")
View(stidata_unclean)
# viewing the dataset features
str(stidata_unclean)
#names of variables
names(stidata_unclean)
#checking the number of variables
ncol(stidata_unclean)
#classification
class(stidata_unclean$a1age)
class(stidata_unclean$a2occupation)
# viewing data features
glimpse(stidata_unclean) #output
#viewing the first 6 variables
head(stidata_unclean)
# viewing the last 6 variables
tail(stidata_unclean)
# sorting
sorting=order(stidata_unclean$idnumber,decreasing = F)
print(sorting)
# sorting data
stidata_sorted=stidata_unclean[order(stidata_unclean$idnumber,decreasing=FALSE),]
View(stidata_sorted)
table(stidata_unclean$idnumber)

#Data Cleaning and wrangling
 #checking inconsistencies
table(stidata_unclean$c3stiyesno)
table(stidata_unclean$casestatus)
View(stidata_unclean[stidata_unclean$casestatus==3,])
 
 #correcting inconsistencies
stidata_unclean$casestatus[stidata_unclean$idnumber==1]=1
stidata_unclean$casestatus[stidata_unclean$idnumber==31]=1
table(stidata_unclean$casestatus)
#OR
stidata_unclean$casestatus[stidata_unclean$idnumber==1|stidata_unclean$idnumber==31]=1

 #identifying & correcting duplicates
duplct=duplicated(stidata_unclean$idnumber)
stidata_unclean$idnumber[duplicated(stidata_unclean$idnumber)]
stidata_unclean=stidata_unclean[order(stidata_unclean$idnumber),]
View(stidata_unclean[stidata_unclean$idnumber==51,])

#dropping the duplicate
stidata_unclean<-stidata_unclean[!(stidata_unclean$idnumber==51& stidata_unclean$a1age==23),]
View(stidata_unclean[stidata_unclean$idnumber==51,])


 #handling missing values
table(stidata_unclean$sex)
str(stidata_unclean$sex)
View(stidata_unclean[stidata_unclean$sex=="",])     
stidata_unclean$sex[stidata_unclean$idnumber==48]="Male"     
stidata_unclean$sex[stidata_unclean$idnumber==213]="Female"
table(stidata_unclean$sex)

#generating new variable in age
table(stidata_unclean$a1age)
stidata_unclean$agecat=NA
stidata_unclean$agecat[stidata_unclean$a1age<=35]=1
stidata_unclean$agecat[stidata_unclean$a1age>35]=0
##labeling agecat
stidata_unclean$agecat=factor(stidata_unclean$agecat,levels = c(1,0),labels = c("below 35yrs","above 35yrs"))
table(stidata_unclean$agecat)

###generating new variable casestatus
table(stidata_unclean$casestatus)
stidata_unclean$case_status=NA
stidata_unclean$case_status[stidata_unclean$casestatus==1]=1
stidata_unclean$case_status[stidata_unclean$casestatus==2]=0
##labelling casestatus
stidata_unclean$case_status=factor(stidata_unclean$case_status,levels = c(1,0),labels = c("Positive","Negative"))
table(stidata_unclean$case_status)                                   
View(stidata_unclean)

##generating new variable for levels of education
table(stidata_unclean$a4levelofeducation)
stidata_unclean$educ=NA
stidata_unclean$educ[stidata_unclean$a4levelofeducation=="1 none"|stidata_unclean$a4levelofeducation=="2 primary"|stidata_unclean$a4levelofeducation=="3 secondary"]=1
stidata_unclean$educ[stidata_unclean$a4levelofeducation=="4 tertiary"]=0                                   
##labelling educ
stidata_unclean$educ=factor(stidata_unclean$educ,levels = c(1,0),labels = c("illiterate","literate"))
table(stidata_unclean$educ)

#generating new variable in sex

table(stidata_unclean$sex)
stidata_unclean$sexcut=NA
stidata_unclean$sexcut[stidata_unclean$sex=="Female"]=1
stidata_unclean$sexcut[stidata_unclean$sex=="Male"]=0
##labeling sexcut
stidata_unclean$sexcut=factor(stidata_unclean$sexcut,levels = c(1,0),labels = c("Female","Male"))
table(stidata_unclean$sexcut)

#Religion

table(stidata_unclean$a3church)
stidata_unclean$church=NA
View(stidata_unclean$church)
stidata_unclean$church[stidata_unclean$a3church=="7 roman catholic"]=1
stidata_unclean$church[stidata_unclean$a3church !="7 roman catholic"]=0
stidata_unclean$church=factor(stidata_unclean$church,levels=c(1,0),labels = c("roman catholic","non catholic"))
table(stidata_unclean$church)
class(stidata_unclean$church)

##generating new variable for  occupation
table(stidata_unclean$a2occupation)
stidata_unclean$occupation=NA
stidata_unclean$occupation[stidata_unclean$a2occupation=="3 formal"|stidata_unclean$a2occupation=="2 informal"]=1
stidata_unclean$occupation[stidata_unclean$a2occupation=="1 unemployed"|stidata_unclean$a2occupation=="4 student"]=0                                   
##labelling occupation
stidata_unclean$occupation=factor(stidata_unclean$occupation,levels = c(1,0),labels = c("employed","unemployed"))
table(stidata_unclean$occupation)

##generating new variable for  marital status
table(stidata_unclean$a5maritalstatus)
stidata_unclean$maritalstatus=NA
stidata_unclean$maritalstatus[stidata_unclean$a5maritalstatus=="2 married"|stidata_unclean$a5maritalstatus=="3 co-habiting"]=1
stidata_unclean$maritalstatus[stidata_unclean$a5maritalstatus=="1 single"|stidata_unclean$a5maritalstatus=="4 divorcee"|stidata_unclean$a5maritalstatus=="5 widowed"]=0                                   
##labelling marital status
stidata_unclean$maritalstatus=factor(stidata_unclean$maritalstatus,levels = c(1,0),labels = c("Married","Not married"))
table(stidata_unclean$maritalstatus)


#sex partner
table(stidata_unclean$sexpartner1year)
class(stidata_unclean$sexpartner1year)
stidata_unclean$sexpartner2<- as.factor(stidata_unclean$sexpartnerlife3)
class(stidata_unclean$sexpartner2)
stidata_unclean$sexpartner=NA
stidata_unclean$sexpartner[stidata_unclean$sexpartner1year==0]="No"
stidata_unclean$sexpartner[stidata_unclean$sexpartner1year==1]="Yes"
table(stidata_unclean$sexpartner)

#generating a variable for sexpartner1year
table(stidata_unclean$sexpartner1year)
stidata_unclean$sexpartner=NA#We Equating to NA  because its an empty variable and we are generating it.
stidata_unclean$sexpartner[stidata_unclean$sexpartner1year==0]=1
stidata_unclean$sexpartner[stidata_unclean$sexpartner1year==1]=0
table(stidata_unclean$sexpartner)
#Labeling sexpartner1year
stidata_unclean$sexpartner=factor(stidata_unclean$sexpartner,levels=c(1,0),labels=c("yes","No"))
table(stidata_unclean$sexpartner)


#generating a variable for used condom
table(stidata_unclean$n11usedcondom)
stidata_unclean$usedcondom = NA
stidata_unclean$usedcondom[stidata_unclean$n11usedcondom =="1 yes"] = 1
stidata_unclean$usedcondom[stidata_unclean$n11usedcondom =="2 No"] = 0
table(stidata_unclean$usedcondom)
#labeling
stidata_unclean$usedcondom = factor(stidata_unclean$usedcondom,levels = c(1,0),labels = c("Yes","No"))
table(stidata_unclean$usedcondom)
View(stidata_unclean)

#DATA ANALYSIS
#Descriptive statistics
summary(stidata_unclean$a1age)
summary(stidata_unclean$height)


#variance and std(standard deviation)
with(stidata_unclean,var(a1age))
with(stidata_unclean,sd(a1age))



#cross tabulation using the table 1 pkg
##This is done to report categorical variables 
#frequencies of categorical data
library(table1)
table1(~factor(stidata_unclean$case_status)+factor(stidata_unclean$sexcut)+factor(stidata_unclean$educ)+factor(stidata_unclean$agecat)+factor(stidata_unclean$sexpartner)+factor(stidata_unclean$church)+factor(stidata_unclean$occupation))

#short form of calling the data
attach(stidata_unclean)

#after calling the data,you have your simple code as
table1(~factor(agecat)+ factor(sexpartner) + height|case_status, data = stidata_unclean)

table1(~factor(stidata_unclean$agecat) +factor(stidata_unclean$sexcut) +factor(stidata_unclean$sexpartner) +factor(stidata_unclean$occupation) +factor(stidata_unclean$church) +height|case_status, data = stidata_unclean)
#to bring cross-tabulation 
table1(~factor(stidata_unclean$usedcondom)|case_status, data = stidata_unclean)

##labeling new variables
#require functions same as library
require(labelled)

#we are labeling to make the appearance look better
var_label(stidata_unclean$case_status) <- "sti stat"
var_label(stidata_unclean$sexcut) <- "sex categories"
var_label(stidata_unclean$educ) <- "education level"
var_label(stidata_unclean$agecat) <- "age categories"
var_label(stidata_unclean$church) <- "religious status"
var_label(stidata_unclean$maritalstatus) <- "marital status"
var_label(stidata_unclean$occupation) <- "occup"
var_label(stidata_unclean$usedcondom) <- "ever used condom"
var_label(stidata_unclean$sexpartner) <- "had sex patner in last 1 year"

#descriptive statistics using global code
#global code(%>%) used for channeling/joining different lines of command
library("gtsummary")
library("dplyr")
Vr<-stidata_unclean %>% select(church,occupation,educ,case_status,sexpartner,usedcondom,maritalstatus,agecat,sexcut)
Vr %>% 
  tbl_summary(by = case_status,missing=  'no',percent = "row") %>% 
  add_overall() %>%  #no. of variables(226)
  modify_header(label~"*Factor*") %>%
  modify_spanning_header(c("stat_1","stat_2")~"*STI Case Status*") %>% 
  bold_labels()%>%
  modify_caption("**: Demographic factors against STI prevalence**")

#a total of 226 people who participated in a study to determine whether the demographic factor influence the prevalence of sexually transmitted infection.We find that 54%,which is 22 of the study participant tested positive who were catholic while 46% which is 19 of the same group tested negative.

#Hypothesis testing
Vr<-stidata_unclean %>% select(church,occupation,educ,case_status,sexpartner,usedcondom,maritalstatus,agecat,sexcut)
Vr %>% 
  tbl_summary(by = case_status,missing=  'no',percent = "row") %>% 
  add_overall() %>% 
  modify_header(label~"*Factor*") %>%
  add_p()%>%
  modify_spanning_header(c("stat_1","stat_2")~"*STI Case Status*") %>% 
  bold_labels()%>%
  modify_caption("**: Demographic factors against STI prevalence**")

#H0:Demographic factors have no effect on the sti prevalence
#H1:Demographic factors have effect on the sti prevalence
#when p_value is <level of significance(alpha) ,the test is statistically significant.
#when p value is > level of significance ,the test is not statistically significant.
#sex partner has a p-value of <0.001 which is <0.05,therefore it's statistically significant in determining the prevalence of sti. Hence we can conclude that there was no enough information to reject Ho for all the risk factors except for had sex partner in last 1 year at 95% level of significance.

#Independent t test
#It's used to compare mean of  two independent groups
t.test(stidata_unclean$a1age~stidata_unclean$sexcut,var.equal=TRUE)
#H0: the mean between the groups is equal to zero
#H1: the mean between the groups is not equal to zero
#alpha=0.05
#p value=0.014
#Since the p value<0.05,we reject the null hypothesis and conclude that the mean between the two groups is not equal to zero.
summary(a1age)

#Paired t test   (determining difference between related variables)
#populating a new a1age2
stidata_unclean$a1age2=NA
stidata_unclean$a1age2=rnorm(226,28.06,7.2)
View(stidata_unclean$a1age2)
table(stidata_unclean$a1age2)
t.test(stidata_unclean$a1age,stidata_unclean$a1age2,paired=TRUE)
table(stidata_unclean$a1age)
#h0 is not equal to 0
#h1 is equal to 0
#with p value being 0.6141, which is greater than 0.05, we fail to reject the null hypothesis.

#Anova
 
###Analysis of variance (ANOVA) is a statistical technique used to check if the means 
####of two or more groups are significantly different from each other. ANOVA checks the 
####impact of one or more factors by comparing the means of different samples

one_way=aov(stidata_unclean$casestatus~stidata_unclean$a1age)
print(one_way)
summary(one_way)
#H0: there is no difference among the group mean
#H1: there is a difference among the group mean
#since the p value>0.05 we fail to reject H0 concluding that there is no difference among the group means

#importing dataset
library(readxl)
STIData <- read_excel("STIData.xls")
View(STIData)
#used when you have two different categorical variables and a dependent 
two_way<-aov(STIData$CaseStatus~STIData$Height+STIData$Weight+STIData$Height*STIData$Weight,data = STIData)
print(two_way)
summary(two_way)
#H0: there is no significant interaction btn the independent variables
#H1: there is significant interaction btn the two independent variables  (p value=0.141)
#since p-value>0.05,therefore we fail to reject the null hypothesis and conclude that there is no significant interaction btn the two independent variables.


#chi square test - a technique to check whether 2 categorical variables(independent) are likely to be related or not
#creating contingency table
tbl2<-table(STIData$CaseStatus,STIData$Sex...47)
tbl3<-table(STIData$CaseStatus,STIData$A1Age)
result2<-chisq.test(tbl2,correct=F)
result2
result3<-chisq.test(tbl3,correct=F)
result3

#H0: there is no association between the independent variables
#H1: there is association between the independent variables
##H0:There is no association between the independent variable
##H1:There is association between the independent variable
##On tbl3 the pvalue is 0.9989>0.05 which means we fail to reject the null hypothesis, 
##therefore there is no association between the independent variables casestatus and age.
##On tbl2 the pvalue is 0.3288>0.05 which means we fail to reject the null hypothesis, 
##therefore there is no association between the independent variables casestatus and sex...47

#correlation test
##converting new variables generated/populated into numeric 
stidata_unclean$case_status<-as.numeric(stidata_unclean$case_status)
stidata_unclean$sexcut<-as.numeric(stidata_unclean$sexcut)
stidata_unclean$agecat<-as.numeric(stidata_unclean$agecat)
result4=cor.test(stidata_unclean$sexcut,stidata_unclean$case_status)
result4

result5=cor.test(stidata_unclean$agecat,stidata_unclean$case_status)
result5
#h0:correlation is not equal to zero
#h1:correlation is equal to zero

#spearman rank correlation
result4=cor.test(stidata_unclean$sexcut,stidata_unclean$case_status,method = "spearman")
result4

result5=cor.test(stidata_unclean$agecat,stidata_unclean$case_status,method = "spearman")
result5
table(stidata_unclean$sexcut)

#linear regression
model1=lm(STIData$Weight~STIData$Height)
model1
summary(model1)
#B0=-26.4103
#b1=0.4981
#residual=11.03 on 224 -used to predict where the mean has been poorly shown in the graph
#R^2=0.1162 the variability in the independent variable that is being explained by the dependent variable
#R^2=0.1122
#F-STATISTIC=29.44
#P.value=1.493*10^-7

#multiple regression
model2=lm(STIData$Weight~STIData$Height+STIData$A1Age+STIData$Sex...47)
model2
summary(model2)
##bo:-33.3382
##b1:0.5110
##b2:0.1593                
##b1:0.6356
##F-statistic: 10.92 on 3
## p-value:0.000001029

install.packages("devtools")
library("devtools")
devtools::install_github("datalorax/equatiomatic")
library(equatiomatic)

bar1=ggplot(data=STIData,aes(x=Sex...47,y=Height,fill=Sex...47))+geom_bar(stat="identity")+annotate("text",x=1,y=1000,label="Missing")+ggtitle("Bar graph of cummulative height against sex")
bar1

box1=ggplot(data=STIData,aes(x=Sex...47 ,y=Weight,fill=Sex...47))+
  geom_boxplot()+
  ggtitle("Distributrion of weight accross sex")+ylab("Weight")
box1

hist1=ggplot(data=STIData,aes(x=Height))+
  geom_histogram()+
  ggtitle("Histogram of Height")+ylab("Weight")
hist1

scatter1=ggplot(data=STIData,aes(x=Height,y=Weight))+
  geom_point()+
  ggtitle("Relationship between Height and Weight")+ylab("Weight")+xlab("Height")
scatter1

line1= ggplot(data =Rabbit,aes(x=Dose, y=BPchange))+geom_line()
line1

library(equatiomatic)
model1=glm(case_status~sexpartner,family=binomial(link=logit),data=stidata_unclean)
equatiomatic::extract_eq(model1)
equatiomatic::extract_eq(model1,use_coefs = TRUE)
tbl_regression(model1,exponentiate=T,intercept=T)

#regression to be done with significant variable