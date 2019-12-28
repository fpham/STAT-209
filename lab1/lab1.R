x <- 5
library(tidyverse)
arbuthnot <- read_csv("http://www.openintro.org/stat/data/arbuthnot.csv")

## reading in from a local file
# arbuthnot <- read.csv("~/stat209/lab1/arbuthnot.csv") # reading a local file

## Load the file from the current working directory
arbuthnot <- read.csv("arbuthnot.csv")

## Load the file from a "sister" directory to the current working directory
arbuthnot <- read.csv("../data/arbuthnot.csv")

arbuthnot
glimpse(arbuthnot)
pull(arbuthnot,boys)
pull(arbuthnot,girls)
qplot(x = year, y = girls, data = arbuthnot)
qplot(x = year, y = girls, data = arbuthnot, geom = "line")
?qplot
pull(arbuthnot, boys) + pull(arbuthnot,girls)
arbuthnot <- arbuthnot %>% mutate(total = boys + girls)
qplot(x=year, y = total, data = arbuthnot, geom = "line")
qplot(x=year, y = proportion, data = arbuthnot <- arbuthnot %>% mutate(proportion = boys / total), geom = "line")
