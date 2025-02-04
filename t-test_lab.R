

#> Sourcing in the theme file via the GitHub link
source("https://raw.githubusercontent.com/ryandobson/504l_spring2025/refs/heads/main/504_plot_theme.R")
#source("504_plot_theme.R") # if you have the theme file downloaded, this does the same thing 

#> Ensure your directory is the R project:
getwd() #get working directory 

######Dependent-Sample t-test######
#import dataset
data.1 <- read.csv(file="data/weightreduce.csv",header = T)
head(data.1)
#> A Note on Data Format: 
#> Notice how in data.1 we are comparing columns where there is 
#> 1 participant per row. Each participant has a response at time 1 and
#> time 2. 
#> For paired-samples t-tests, you NEED your data in this format. 
#> You will always have to select data for a paired-samples t-test by selecting
#> two columns, both of which contain the relevant dependent variable.


#conduct dependent-sample t-test 
t.test(x=data.1$Time1, y=data.1$Time2, alternative="two.sided", paired=TRUE)

#Cohen's d for dependent sample t-test
#install.packages("lsr") #install package car for the first time
library(lsr)
cohensD(x=data.1$Time1, y=data.1$Time2, method = "paired")
library(psych)
describe(data.1)

######Independent-Sample t-test######
#import dataset
data.2 <- read.csv(file="data/literature.csv",header = T)
head(data.2)

#> A Note on Data Format: 
#> Notice how data.2 is in a different format than data.1 (well, practically speakig 
#> for this analysis anyway). 
#> Instead of two columns that both contain the dependent variable, we have 1
#> column that contains the dependent variable, and another column that indicates
#> the group. 
#> Notice how the synatax changes for the selection of this, we now use a formula
#> method and select the data separately from the formula. Rather than selecting
#> the specific columns as was done above. 


#make sure the group variable is a factor
data.2$group <- as.factor(data.2$group)
summary(data.2$group)

##Homogeneous of variance test
#Levene's Test
#install.packages("car") #install package car for the first time
library(car) #activate the package
leveneTest(y = literature ~ group, data= data.2, center=mean) 
#> null hypothesis is that the variance of the groups are equal  
#> If we were to reject the null hypothesis (i.e., p < .05), then we would go
#> with the alternative hypothesis that at least one of the group means is not 
#> equal to the other group means
#> Under homogeneity of variance, we want the group means to be equal. Although
#> the logic is bit backwards, we want a p value <.05. 

#conduct independent-sample t-test 
t.test(formula = literature ~ group, data= data.2, var.equal=TRUE)

#> Look at the linear model results and compare them to the t-test
summary(lm(literature ~ group, data = data.2))

#Cohen's d for independent sample t-test
cohensD(formula=literature~group, data=data.2, method ="pooled")
?cohen.d #look at Cohen's d function for information. Great information for
#on effect sizes in general is reported below the function information
#> The psych package cohen.d() function outputs some more useful information
psych::cohen.d(literature ~ group, data = data.2)

#conduct independent-sample t-test when the homogeneity of variance assumption is violated
t.test(formula=literature ~ group, data= data.2, var.equal=FALSE)

#> The t-test that r defaults to is the "Welch's" t-test which assumes
#> that the group variances are different. In other words, the "t.test" function
#> defaults to "var.equal = FALSE." 
#> The regular t-test where groups are assumed to have equal variance is 
#> "Student's" t-test. 




