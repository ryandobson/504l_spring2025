

#> clear environment 
rm(list = ls()) 

# load packages 
library(tidyverse)
library(psych)
library(DescTools) #contrasts 

source("504_plot_theme.R")

df <- read.csv("data/friendship.csv") |> 
  select(matches("jealous|pid|Age.Part|Relationship.Status|Gender")) |> 
  rename_with(tolower) |> #tidyverse version of renaming all variables with lowercase 
  rename(
    rel_status = use.relationship.status.participant #renaming a specific variable
  ) |> 
  dplyr::mutate(
    pid = 1:158,
    pid = factor(pid)
  )
df[, grepl("jealous", names(df))] <- lapply(df[, grepl("jealous", names(df))], as.numeric)
df[, grepl("jealous", names(df))]


df <- df |> filter(!is.na(rom.jealous) & !is.na(ssf.jealous) & !is.na(osf.jealous)) |> as_tibble()

str(df)

write.csv("df", "friendship_within") #load in this dataset

#> Wide versus Long Format Data: 
#> Long = 1 observation (e.g., dependent variable score) per line 
#> Wide = multiple observations per line 

#> Depending on my research question, my data are already in a "long enough" 
#> format. For previous analyses, I just examined scores for one emotion, with
#> one specific hypothetical situation. 
#> e.g., I looked at whether happiness ratings differed across men and women for
#> rating the romantic partner (rom.happy).
#> However, now I'm interested in whether ratings differ on jealousy, based on 
#> the hypothetical person people are imagining. 
#> Either the romantic partner (rom.jealous), opposite-sex friend (osf.jealous),
#> or the same-sex friend (ssf.jealous). 

#> If I look at the dataframe pertaining those columns, it is clear that I have 
#> multiple observations on the same line. 

#> Long story short, I need to make this dataframe into a long format. 

#> I typical denote my long data frames "dfl" for dataframe long. 
dfl <- df |> tidyr::pivot_longer(  ## "tidyr" and pivot_longer/pivot_wider are part of the tidyverse
  cols = c("rom.jealous", "ssf.jealous", "osf.jealous"),
  values_to = "score", #this is the column that will contain your numeric ratings 
  names_to = "interloper" #this column will contain character strings of the variables selected in cols = c()
) |> 
select(score, interloper, pid, gender) # I grab the few relevant variables needed here. 

#Remember, pid is the participant ID. 
#Its a simple variable that goes from 1 to 158. 

str(dfl)

#> Make the interloper variable a factor (tidyverse version of factoring variables)
dfl <- dfl |> mutate(
  interloper = factor(interloper),
  pid = factor(pid)
)

#> Running the within-subjects one-way ANOVA

#install.packages("ez")
library(ez)

#> This package does not seem to like when you have any missing data on your
#> dependent variable...
#> I'm going to remove missing cases so I can get models to run. 

dfl <- dfl |> filter(!is.na(score)) #filtering out cases that are not equal to na
  
summary(dfl)
ezDesign(dfl, score, interloper)

m1 <-ezANOVA(data = dfl, dv = .(score), wid = .(pid), within = .(interloper), 
                detailed = TRUE, return_aov = TRUE, type = 3)

#print out results
m1


#Report descriptive statistics
ezStats(data=dfl, dv=.(score), wid = .(pid), within= .(interloper))

#plot marginal means
ezPlot(data=dfl, dv=.(score), wid = .(pid), within= .(interloper), x=interloper)

#posthoc test
pairwise.t.test(dfl$score, dfl$interloper, paired = TRUE, p.adjust.method = "bonferroni")




#> Another package for reshaping data: 

#install.packages("reshape2")
library(reshape2) #I believe this package is modeled after a Python package that is
#> frequently used for reshaping data in Python. 
dfl1 <- melt(data = df[, c("osf.jealous", "ssf.jealous", "rom.jealous", "pid")], id = "pid", measured = c(osf.jealous, ssf.jealous, rom.jealous))
head(dfl1)


#> Some other ways to formulate the within-subjects ANOVA 
#> NOTE: Be careful with what sums of squares is being used. 

library(rstatix) #the function in this package also provides the extra tests
m2 <- anova_test(data = dfl, dv = score, wid = pid, within = interloper)
get_anova_table(m2)
m2

#> Need the error term to be a factor for this to return the same
#> result. 
m3 <- aov(score ~ interloper + Error(pid), data = dfl)
summary(m3)

#> You can also add in a between subjects factor (making it a mixed design)
m4 <- anova_test(data = dfl, dv = score, wid = pid, within = interloper, between = gender)
get_anova_table(m4)
m4









