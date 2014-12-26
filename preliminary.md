---
title: "Preliminary Analysis"
author: "Eduardo Ariño de la Rubia"
date: "December 25, 2014"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
library(rvest)
library(tidyr)
```

```
## Warning: package 'tidyr' was built under R version 3.1.2
```

```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
library(Hmisc)
```

```
## Warning: package 'Hmisc' was built under R version 3.1.2
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: splines
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:dplyr':
## 
##     src, summarize
## 
## The following object is masked from 'package:rvest':
## 
##     html
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
library(ggplot2)
library(maps)
data(state)
states_map <- map_data("state")
```



```r
# index.tidy.html was created using http://infohound.net/tidy/ from
# killedbypolice.net after some small manual editing (mainly deleting
# stuff that I knew would cause parsing headaches)
#
# tidy data/index.html > data/index.tidy.html

columns <- c("date", "state", "gender_race", "name_age", "kbp_link", "news_link")
killed_by_police <- html("data/index.tidy.html")
```

```
## Error in object[, i]: incorrect number of dimensions
```

```r
data <- killed_by_police %>% html_table(header=TRUE) %>% 
  .[[1]] %>%
  setNames(columns) %>%
  mutate(#first thing we have to do is split up columns w/multiple values
    gender=unlist(lapply(strsplit(as.character(gender_race), "\\/"), "[", 1)), 
    race=unlist(lapply(strsplit(as.character(gender_race), "\\/"), "[", 2)),
    name=unlist(lapply(strsplit(as.character(name_age), "\\,"), "[", 1)),
    age=unlist(lapply(strsplit(as.character(name_age), "\\, "), "[", 2)),
    first_name=unlist(lapply(strsplit(as.character(name), " "), "[", 1)),
    #throw out some cruft from some strings
    gender=substr(gender, 1, 1),
    race=substr(race, 1, 1),
    date=gsub("\\([^)]+) ", "", date),
    #make a real date object
    date=mdy(date),
    age=as.numeric(substr(age, 1, 2)),#centegenerians will be turned into teens
    gender=as.factor(gender),
    race=as.factor(race),
    first_name=as.factor(capitalize(tolower(first_name))),
    decade=as.factor(age %% 10),
    state=as.factor(state.name[match(state, state.abb)]))
```

```
## Error in eval(expr, envir, enclos): object 'killed_by_police' not found
```

```r
write.csv(data, "data/cleaned.csv", row.names=FALSE)

state_population <- read.csv("data/state_population.csv") %>%
  filter(NAME %in% state.name)

summaries_by_state <- data %>% group_by(state) %>% 
  summarise(total=n(), 
            mean_age=round(mean(age, na.rm=TRUE))) %>% 
  inner_join(state_population, by=c("state" = "NAME")) %>% 
  select(state, mean_age, total, POPESTIMATE2014, 
         POPEST18PLUS2014) %>% 
  mutate(pctg=(total/POPESTIMATE2014)*100) %>% 
  arrange(state) %>%
  mutate(state=tolower(state))
```

```
## Error in UseMethod("group_by_"): no applicable method for 'group_by_' applied to an object of class "function"
```

```r
write.csv(summaries_by_state, "data/summaries_by_state.csv", row.names=FALSE)
```

```
## Error in is.data.frame(x): object 'summaries_by_state' not found
```

```r
summaries_by_state_and_race <- data %>% group_by(state, race) %>% 
  summarise(total=n(), 
            mean_age=round(mean(age, na.rm=TRUE))) %>% 
  inner_join(state_population, by=c("state" = "NAME")) %>% 
  select(state, race, mean_age, total, POPESTIMATE2014, 
         POPEST18PLUS2014) %>% 
  mutate(pctg=(total/POPESTIMATE2014)*100) %>% 
  arrange(state) %>%
  mutate(region=state.region[match(state.name[state], state.name)])
```

```
## Error in UseMethod("group_by_"): no applicable method for 'group_by_' applied to an object of class "function"
```

```r
write.csv(summaries_by_state_and_race, "data/summaries_by_state_and_race.csv", row.names=FALSE)
```

```
## Error in is.data.frame(x): object 'summaries_by_state_and_race' not found
```

```r
pctg_map <- summaries_by_state %>% 
  inner_join(states_map, by=c("state" = "region")) %>% 
  arrange(group, order)
```

```
## Error in eval(expr, envir, enclos): object 'summaries_by_state' not found
```

```r
ggplot(pctg_map, aes(x=long, y=lat, group=group, fill=pctg)) +
  geom_polygon(colour="black") + 
  scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B", 
                       midpoint=median(pctg_map$pctg)) + 
  coord_map("polyconic")
```

```
## Error in ggplot(pctg_map, aes(x = long, y = lat, group = group, fill = pctg)): object 'pctg_map' not found
```

```r
data <- data %>% filter(date > ymd("2014/01/01"))
```

```
## Error in UseMethod("filter_"): no applicable method for 'filter_' applied to an object of class "function"
```

You can also embed plots, for example:


```
## Error: ggplot2 doesn't know how to deal with data of class function
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
