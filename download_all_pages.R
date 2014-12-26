library(httr)
library(doParallel)
registerDoParallel(cores = 4)

cleaned <- read.csv("data/cleaned.csv", stringsAsFactors=FALSE)
foreach(page=cleaned$news_link) %do% {
  data <- GET(page)
}
