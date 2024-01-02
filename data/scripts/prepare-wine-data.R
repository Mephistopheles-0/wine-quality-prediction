# this script prepares raw data from the wine quality dataset
# for use in shelby's developing data products course final project
# created by shelby bachman, 2019

# setup
rm(list = ls())
library(rprojroot)
library(data.table)
library(dplyr)

# function to source files within this project
path <- function(x) find_root_file(x, criterion = has_file('final-project.Rproj'))

# create vector of column names (see winequality.names file for full descriptions)
col_names <- c('acidity (fixed)', 'acidity', 'citric acid',
              'sugar', 'chlorides', 
              'sulfur dioxide (free)', 'sulfur dioxide (total)', 
              'density', 'pH', 'sulfates', 'alcohol', 'quality')

# read raw data
data_white <- fread(path('data/raw_data/winequality-white.csv'), quote = "")
data_red <- fread(path('data/raw_data/winequality-red.csv'), quote = "")

# set column names
colnames(data_white) <- col_names
colnames(data_red) <- col_names

# add variable indicating type (red or white)
data_white <- data_white %>%
  mutate(type = 'white')
data_red <- data_red %>%
  mutate(type = 'red')

# bind dataframes
data <- rbind(data_white, data_red)

# reorder columns such that quality is first, type is last
data <- data %>%
  select(quality, acidity, alcohol, chlorides, density, sugar, sulfates, type)

# write data to file
write.csv(data, path('data/prepared_data/wine_data.csv'), quote = FALSE, row.names = FALSE)
