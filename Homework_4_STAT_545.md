---
title: "Homework 4 STAT 545"
author: "Anita"
date: "October 4, 2018"
output: 
  html_document: 
    keep_md: yes
---

#Homework 04: Tidy data and joins


```r
suppressPackageStartupMessages(library(tidyverse))
```


# data reshaping prompts: Activity #1

Make your own cheatsheet similar to Tyler Rinker's minimal guide to tidyr.


Create a (made-up) dataframe with three countries, a unique ID for each participant, their subjective wellbeing (on a scale from 1-10 where 1 = least, 10 = most) and the countries score on the Democracy Index (https://en.wikipedia.org/wiki/Democracy_Index) 


```r
c <- tribble(
  ~country,  ~ID, ~wellbeing, ~Dem_Index,
  "USA",    1, 5, 7.98,
  "USA",    2, 3, 7.98,
  "USA",    3, 6, 7.98,
  "Canada", 4, 7, 9.15,
  "Canada", 5, 8, 9.15,
  "Canada", 6, 6, 9.15,
  "UK",     7, 7, 8.53,
  "UK",     8, 7, 8.53,
  "UK",     9, 6, 8.53
)
```



```r
knitr::kable(c)
```



country    ID   wellbeing   Dem_Index
--------  ---  ----------  ----------
USA         1           5        7.98
USA         2           3        7.98
USA         3           6        7.98
Canada      4           7        9.15
Canada      5           8        9.15
Canada      6           6        9.15
UK          7           7        8.53
UK          8           7        8.53
UK          9           6        8.53


Since this data is in long format, it can be used for analyses, such as multilevel modelng where wellbeing is predicted from the Democracy Index with a random intercept for country.


```r
library(lmerTest)
```

```
## Loading required package: lme4
```

```
## Loading required package: Matrix
```

```
## 
## Attaching package: 'Matrix'
```

```
## The following object is masked from 'package:tidyr':
## 
##     expand
```

```
## 
## Attaching package: 'lmerTest'
```

```
## The following object is masked from 'package:lme4':
## 
##     lmer
```

```
## The following object is masked from 'package:stats':
## 
##     step
```

```r
a <- lmer(wellbeing ~ Dem_Index + (1|country), c) 
summary(a)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: wellbeing ~ Dem_Index + (1 | country)
##    Data: c
## 
## REML criterion at convergence: 24.5
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -1.72378 -0.19498  0.08529  0.70955  0.98982 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  country  (Intercept) 0.1357   0.3684  
##  Residual             1.2222   1.1055  
## Number of obs: 9, groups:  country, 3
## 
## Fixed effects:
##             Estimate Std. Error       df t value Pr(>|t|)
## (Intercept) -10.6838     7.6266   1.0000  -1.401    0.395
## Dem_Index     1.9636     0.8903   1.0000   2.206    0.271
## 
## Correlation of Fixed Effects:
##           (Intr)
## Dem_Index -0.998
```


## Create a mean and median wellbeing score per country
Now, instead we might want to look at the mean self-reported wellbeing per country and correlate that with the Democracy Index (i.e., we just look at mean values)


```r
c_mean<-c %>%
  group_by(country, Dem_Index) %>%
  summarize(mu = mean(wellbeing),
            md = median(wellbeing))
knitr::kable(c_mean)
```



country    Dem_Index         mu   md
--------  ----------  ---------  ---
Canada          9.15   7.000000    7
UK              8.53   6.666667    7
USA             7.98   4.666667    5

### Now the data can be used to look at the relationship between the Democracy Index and mean/median wellbeing levels per country


```r
cor.test(c_mean$Dem_Index, c_mean$mu)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  c_mean$Dem_Index and c_mean$mu
## t = 2.2056, df = 1, p-value = 0.271
## alternative hypothesis: true correlation is not equal to 0
## sample estimates:
##       cor 
## 0.9107611
```

```r
cor.test(c_mean$Dem_Index, c_mean$md)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  c_mean$Dem_Index and c_mean$md
## t = 1.6017, df = 1, p-value = 0.3553
## alternative hypothesis: true correlation is not equal to 0
## sample estimates:
##       cor 
## 0.8482483
```

## Turn the data into wide format 


```r
b <- spread(c, key = "Dem_Index", value = "wellbeing")
knitr::kable(b)
```



country    ID   7.98   8.53   9.15
--------  ---  -----  -----  -----
Canada      4     NA     NA      7
Canada      5     NA     NA      8
Canada      6     NA     NA      6
UK          7     NA      7     NA
UK          8     NA      7     NA
UK          9     NA      6     NA
USA         1      5     NA     NA
USA         2      3     NA     NA
USA         3      6     NA     NA

## Create a data frame where participants are randomly assigned to either condition 1 or 2


```r
m <- tribble(
  ~ID, ~gender, ~condition1, ~condition2, ~wellbeing,
  1,    "female", ' ', 7.98, 7,
  2,    "female", ' ', 7.98, 6,
  3,    "male", 6, ' ', 5,
  4, "male", 7, ' ', 3, 
  5, "female", ' ', 9.15, 8, 
  6, "male", ' ', 9.15, 6,
  7,     "NA", 7, ' ', 4,
  8,     "female", 7, ' ', 5,
  9,     "female", ' ', 8.53, 8
)

knitr::kable(m)
```



 ID  gender   condition1   condition2    wellbeing
---  -------  -----------  -----------  ----------
  1  female                7.98                  7
  2  female                7.98                  6
  3  male     6                                  5
  4  male     7                                  3
  5  female                9.15                  8
  6  male                  9.15                  6
  7  NA       7                                  4
  8  female   7                                  5
  9  female                8.53                  8




```r
#I have experimental study designs where participants are randomly assigned to one condition. In the output, the columns for the seprate conditions are filled with numbers that are meaningless for my purposes (e.g., how long it took participants until they first clicked something on the page). I want to create a new column where that indicates which condition participants were in

m$Condition <- ifelse (m$condition1==' ', c('condition2'), c('condition1')) #here I create a new variable such that when participants have nothing in condition 1, then they are assigned to condition 2 and otherwise to condition 1

#how to do with tidyr instead of base R?

knitr::kable(m)
```



 ID  gender   condition1   condition2    wellbeing  Condition  
---  -------  -----------  -----------  ----------  -----------
  1  female                7.98                  7  condition2 
  2  female                7.98                  6  condition2 
  3  male     6                                  5  condition1 
  4  male     7                                  3  condition1 
  5  female                9.15                  8  condition2 
  6  male                  9.15                  6  condition2 
  7  NA       7                                  4  condition1 
  8  female   7                                  5  condition1 
  9  female                8.53                  8  condition2 

*Note: I used a base R function instead of tidyverse because I couldn't figure out how to do this in tidyverse

##Create variable of mean wellbeing by condition


```r
m %>%
  group_by(Condition) %>%
  summarize(mu = mean(wellbeing))
```

```
## # A tibble: 2 x 2
##   Condition     mu
##   <chr>      <dbl>
## 1 condition1  4.25
## 2 condition2  7
```




# Join Prompts (join, merge, look up)



```r
c <- tribble(
  ~country,  ~ID, ~wellbeing, ~Dem_Index,
  "USA",    1, 5, 7.98,
  "USA",    2, 3, 7.98,
  "USA",    3, 6, 7.98,
  "Canada", 4, 7, 9.15,
  "Canada", 5, 8, 9.15,
  "Canada", 6, 6, 9.15,
  "UK",     7, 7, 8.53,
  "UK",     8, 7, 8.53,
  "UK",     9, 6, 8.53
)
```


```r
d <- tribble(
  ~country,  ~ID, ~satisfaction, ~Dem_Index, ~trust,
  "USA",    10, 5, 7.98, 7,
  "USA",    11, 3, 7.98, 6,
  "USA",    12, 6, 7.98, 5,
  "Canada", 13, 7, 9.15, 4,
  "Canada", 14, 8, 9.15, 6,
  "Canada", 15, 6, 9.15, 8,
  "UK",     16, 7, 8.53, 9, 
  "UK",     17, 7, 8.53, 3,
  "UK",     18, 6, 8.53, 2
)
```

Data frame d varies in two ways from dataset c: The variable ellbeing is called satisfaction (but the question used is the same). It includes an additional variable, trust.

Before joining these two datasets, the variable that is the same but has different names, needs to be renamed.
The variable trust needs to be added to data frame c


```r
e <- rename(d, wellbeing=satisfaction)
#rename variable satisfaction to wellbeing in data frame d
#Note: If I don't rename the variable, then R will keep these variables separate and fill it in with 'NA' in each dataset thta doesn't contain the variable
```


```r
trust <- c(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')
# create a vector of the variable trust containing missing data
#Note: this step is not necessary; R will automatically fill tis variable in as missing data

cbind(c, trust)
```

```
##   country ID wellbeing Dem_Index trust
## 1     USA  1         5      7.98      
## 2     USA  2         3      7.98      
## 3     USA  3         6      7.98      
## 4  Canada  4         7      9.15      
## 5  Canada  5         8      9.15      
## 6  Canada  6         6      9.15      
## 7      UK  7         7      8.53      
## 8      UK  8         7      8.53      
## 9      UK  9         6      8.53
```

```r
#add the vector to data frame c
```
## Combine into a single data set


```r
(ce_list <- bind_rows(c, e))
```

```
## # A tibble: 18 x 5
##    country    ID wellbeing Dem_Index trust
##    <chr>   <dbl>     <dbl>     <dbl> <dbl>
##  1 USA         1         5      7.98    NA
##  2 USA         2         3      7.98    NA
##  3 USA         3         6      7.98    NA
##  4 Canada      4         7      9.15    NA
##  5 Canada      5         8      9.15    NA
##  6 Canada      6         6      9.15    NA
##  7 UK          7         7      8.53    NA
##  8 UK          8         7      8.53    NA
##  9 UK          9         6      8.53    NA
## 10 USA        10         5      7.98     7
## 11 USA        11         3      7.98     6
## 12 USA        12         6      7.98     5
## 13 Canada     13         7      9.15     4
## 14 Canada     14         8      9.15     6
## 15 Canada     15         6      9.15     8
## 16 UK         16         7      8.53     9
## 17 UK         17         7      8.53     3
## 18 UK         18         6      8.53     2
```

## Dataset can be used

```r
ab <- lm(wellbeing ~ Dem_Index, data = ce_list)
summary(ab)
```

```
## 
## Call:
## lm(formula = wellbeing ~ Dem_Index, data = ce_list)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.98534 -0.28270  0.01466  0.93471  1.01466 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept)  -10.684      4.472  -2.389  0.02955 * 
## Dem_Index      1.964      0.522   3.762  0.00171 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.058 on 16 degrees of freedom
## Multiple R-squared:  0.4693,	Adjusted R-squared:  0.4361 
## F-statistic: 14.15 on 1 and 16 DF,  p-value: 0.001705
```

```r
#predicting wellbeing from he Democracy Index

yx <- lm(trust ~ Dem_Index, data = ce_list)
summary(yx)
```

```
## 
## Call:
## lm(formula = trust ~ Dem_Index, data = ce_list)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.5545 -1.5826  0.4174  1.4705  3.4455 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept)   5.1672    14.6731   0.352    0.735
## Dem_Index     0.0454     1.7128   0.027    0.980
## 
## Residual standard error: 2.456 on 7 degrees of freedom
##   (9 observations deleted due to missingness)
## Multiple R-squared:  0.0001004,	Adjusted R-squared:  -0.1427 
## F-statistic: 0.0007026 on 1 and 7 DF,  p-value: 0.9796
```

```r
#predicting trust from the Democracy Index, here we have 9 issing data points because we had only data for trsut in dataset d
```


## Combine two data frames with different variables, that belong to the same people

Example: A study where participants complete different tasks in different programs, they have to be combined


```r
f <- tribble(
  ~ID, ~country,  ~wellbeing, ~Dem_Index,
  1, "USA",     5, 7.98,
  2, "USA",     3, 7.98,
  3, "USA",     6, 7.98,
  4, "Canada",  7, 9.15,
  5, "Canada",  8, 9.15,
  6, "Canada",  6, 9.15,
  7, "UK",      7, 8.53,
  8, "UK",      7, 8.53,
  9, "UK",      6, 8.53
)
```


```r
g <- tribble(
   ~ID, ~satisfaction, ~trust,
   3, 5, 6,
   1, 4, 3,
   2, 8, 2, 
   6, 5, 5, 
   9, 4, 6, 
   8, 7, 4, 
   5, 5, 5,
   7,7, 2,
   4, ' ', ' '
)
```

Problem: Each person has an ID, but the IDs are not in the same order, so combining the dataframes will result in a situation where one row contains information of two different participants


```r
cbind(f, g)
```

```
##   ID country wellbeing Dem_Index ID satisfaction trust
## 1  1     USA         5      7.98  3            5     6
## 2  2     USA         3      7.98  1            4     3
## 3  3     USA         6      7.98  2            8     2
## 4  4  Canada         7      9.15  6            5     5
## 5  5  Canada         8      9.15  9            4     6
## 6  6  Canada         6      9.15  8            7     4
## 7  7      UK         7      8.53  5            5     5
## 8  8      UK         7      8.53  7            7     2
## 9  9      UK         6      8.53  4
```

```r
#The IDs in each row should be identical which shows that the data frames have been combined such that each row represents one participants.
#As we can see, this is not the case
```

Solution: reorder one dataframe so the two dataframes are in the same order

```r
h <- arrange(g, ID)#default: ascending
```



```r
cbind(f, h)
```

```
##   ID country wellbeing Dem_Index ID satisfaction trust
## 1  1     USA         5      7.98  1            4     3
## 2  2     USA         3      7.98  2            8     2
## 3  3     USA         6      7.98  3            5     6
## 4  4  Canada         7      9.15  4                   
## 5  5  Canada         8      9.15  5            5     5
## 6  6  Canada         6      9.15  6            5     5
## 7  7      UK         7      8.53  7            7     2
## 8  8      UK         7      8.53  8            7     4
## 9  9      UK         6      8.53  9            4     6
```

```r
#Now each row corresponds to one participants
```



