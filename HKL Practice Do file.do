use "C:\Users\User\Desktop\Data analysis\classes\STATA PRAC\DATASETS\stidata-unclean.dta"
describe
//describes data in memory file
codebook idnumber
//describes data content
tabulate a1age
//shows the frequencies

**DATA CLEANING**
//Checking data ranges(sorting,summarizing)
//Checking inconsistencies
//Identifying missing values 
//Handling duplicate records

*Checking data ranges
*sorting
// helps in identifying data range i.e smallest to largest(ascending order)
sort a1age
gsort height
//sorting in descending order
gsort - a1age
// minimum and maximum values
codebook a1age
gsort idnumber

*Checking inconsistencies
codebook casestatus
//Making corrections on the inconsistencies
list idnumber if casestatus==3
replace casestatus=1 if (idnumber==1|idnumber==31)

*Handling missing values
//use the tab command and to include missing values add the option,missing
tab sex,missing
//in order to clean the missing values ,list the idnumber using the command
list idnumber if sex==""
replace sex="Male" if idnumber==48
replace sex="Female" if idnumber==213

*Handling duplicate records
duplicates report idnumber
//to listt duplicates by idnumber
duplicates list idnumber
//dropping duplicates
drop if idnumber==51 & a1age==23

*Saving and replacing
save hklpracticeclean,replace

**DATA MANAGEMENT**
//Creating new variables
gen agecut=""
replace agecut= "<25" if(a1age>0) & (a1age<=24)
replace agecut= "25-34" if(a1age>=25) & (a1age<=34)
replace agecut= "35-44" if(a1age>=35) & (a1age<=44)
replace agecut= "45-54" if(a1age>=45) & (a1age<=54)
replace agecut= "54-64" if(a1age>=54) & (a1age<=64)

//merging of 2 datasets
clear
use "C:\Users\User\Desktop\Data analysis\classes\STATA PRAC\DATASETS\hklpracticeclean.dta"
merge idnumber using "C:\Users\User\Desktop\Data analysis\classes\datasets-2\set1_1to1.dta" 

gen bmi=weight/((height/100)^2)
//rounding off
gen bmi2=round(bmi,.01)
gen bmi3=round(bmi,.1)

//extended generate command(egen)
*creating new variables and populating them based on a statistical calculation/mathematical function
egen meanage =mean(a1age),by(sex)
egen meanage1=mean(a1age)
egen sumage=sum(a1age)

//combining categories
*stata can create variables and populate them conditionally
*condition setting is done using if statements
*to create a variable occup where levels of occupation are merged
gen occup=1 if a2occupation=="2 informal"
replace occup=1 if a2occupation=="3 formal"
replace occup=2 if a2occupation=="1 unemployed"
replace occup=2 if a2occupation=="4 student"

//adding labels to values
label define occup1b 1 "Employed" 2 "Unemployed"
label values occup occup1b
tab occup

//converting variable types
gen gender=1 if sex=="Male"
replace gender=2 if sex=="Female"
encode sex, gen(gender_num)

//sub-setting observations
*keeping records  for females only
keep if sex=="Female"
*keeping some variables for dataset
keep idnumber a1age agecut occup
save Female,replace

//rotating data(wide to long)
reshape long temp,i(id) j(day)

*Summarizing and visualizing
//ranges
sum 
//counts
tabulate a1age

//summarize command produces a summary of measures of central tendency and dispersion for a numeric variable
sum a1age
//in order to add more details add d to the output( d means details)
sum a1age,d

//one way frequencies
//tabulating one variable
tab1 sex
tab1 sex,missing

//two way frequencies
//its tabulating more than one variable i.e two by two tables
//relationship between sexpartners(explanatory) and casestatus(outcome)
tab2 sex casestatus
//binary /categorical outcome
//checking of significant association/independence between two categorical variables via contigency tables
//A contingency table, also known as a cross-tabulation or crosstab, is a tabular summary of the joint distribution of two or more categorical variables. It displays the frequency counts or proportions of observations that belong to each combination of categories for the variables.
tab casestatus sex,chi
tab casestatus sex,row chi 
//if p value is greater than 0.05 then usecondom is insignificant and also reject H0

**Visualizing data
graph pie, over(sex) plabel(_all percent)
//testing for normality
histogram a1age 
//boxplot
graph box bmi,over(sex)
//distribution curves
kdensity bmi
//scatterplots
scatter a1age bmi

**DATA ANALYSIS IN STATA
//choosing the correct statistical test
//Independent t-test
ttest height, by(sex)

//correlation
//measures the strength and direction of the linear relationship between two continuous variables.
corr a1age, by sex

//anova
anova a1age height 

//linear regression
//a data analysis technique that predicts the value of unknown data by using another related and known data value
//relationship between the variables mentioned below
regress bmi weight height
//accuracy is determined by the adjusted r normally from 70%+

list idnumber if n12usecondom=="2 no" if idnumber=47
//pairwise correlation
//is a fundamental statistical technique used to explore the relationship between pairs of variables in a dataset. 
pwcorr alage height,sig

encode sex, gen(sex_num)
encode a2occupation, gen(a2occupation_num)
encode a3church, gen(a3church_num)
encode a4levelofeducation, gen(a4levelofeducation_num)
encode a5maritalstatus, gen(a5maritalstatus_num)
encode d2group1, gen(d2group1_num)
encode d2group2, gen(d2group2_num)
encode n10givereceiveforsex, gen(n10givereceiveforsex_num)
encode n12usecondom, gen(n12usecondom_num)
encode n11usedcondom, gen(n11usedcondom_num)
encode n13takenalcohol, gen(n13takenalcohol_num)
encode typeofsti, gen(typeofsti_num) 
encode agefirstsex, gen(agefirstsex_num)
*********************************************************************

replace casestatus=.0 if casestatus==2
//Logistic regression is a statistical method used to model the relationship between a binary dependent variable and one or more independent variables. It is commonly used when the dependent variable is categorical and binary, indicating two possible outcomes (e.g., yes/no, 0/1, true/false).
//social demographic variables only
logistic casestatus ib2.a5maritalstatus_num ib1.a2occupation_num ib3.a3church_num ib2.a4levelofeducation_num ib1.sexpartner1year ib1.sex_num
