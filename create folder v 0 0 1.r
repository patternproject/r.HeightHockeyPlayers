## `````````````````````````````````````````````
#### Read Me ####
## `````````````````````````````````````````````
## This file creates folder setup in the project directory
## 
## `````````````````````````````````````````````

## `````````````````````````````````````````````
#### Load Libraries ####
## `````````````````````````````````````````````
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse)
## `````````````````````````````````````````````


## `````````````````````````````````````````````
#### Global Settings ####
## `````````````````````````````````````````````

## ````````````````````
### Setting Working Dir
# SRC: http://stackoverflow.com/questions/17605563/efficiently-convert-backslash-to-forward-slash-in-r
# 1. At the prompt copy and paste windows path of project root folder
# x <- readline()
# 2. 
# my.dir <- gsub("\\\\", "/", x)
# 3. 
# setwd(my.dir)
# 4. check
getwd()
## ````````````````````

## ````````````````````
### Creating Sub folders or Setup Project Folders
# SRC: http://stackoverflow.com/questions/42435225/creating-folders-using-walk-and-purrr/42441681
my.folders <- data.frame(folder = c('1.Data','2.Code','3.References','4.Color Scheme','5.Extas','6.Output'))

my.folders %>%
  by_row(
    function(x)
      dir.create(as.character(x$folder),
                 showWarnings = FALSE),
    .collate = "rows",
    .to = "success"
  )

rm(my.folders)
## ````````````````````

## ````````````````````
### Create a R Project
# SRC: https://datascienceplus.com/r-for-publication-by-page-piccinini-lesson-1-r-basics/

# Since we just created our folder structure choose "Existing Directory". 
# Then use the "Browse." button to find our root folder. 
## ````````````````````

## `````````````````````````````````````````````
