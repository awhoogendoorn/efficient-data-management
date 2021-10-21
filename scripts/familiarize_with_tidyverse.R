# 20211021: https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/efficient-data-r/
# SECTION: 
# Data Management and Data Manipulation
# Getting to know the tidyverse
# # We use a code that is adjusted and based on examples provided here: https://declaredesign.org/r/fabricatr/articles/building_importing.html

# install.packages("tidyverse")
# install.packages("fabricatr")
# install.packages("magrittr")
library("tidyverse")
library("fabricatr")
library("magrittr")

# We set a seed to make our dataset reproducible.
set.seed(68159)

data <- fabricate(
  countries = add_level(
    N = 10,
    month = recycle(month.abb),
    gdppercapita = runif(N, min = 10000, max = 50000),
    unemployment_rate = 5 + runif(N, 30,50) + ((gdppercapita > 50) * 10),
    life_expectancy = 50 + runif(N, 10, 20) + ((gdppercapita > 30000) * 10)
  )
)

# Drop countries
data <- data %>% select(-countries)

# We add artificial NAs
data <- data %>%
  mutate(
    gdppercapita = ifelse(gdppercapita > 40000 &
                            gdppercapita < 44000, NA, gdppercapita),
    unemployment_rate = ifelse(
      unemployment_rate > 50 &
        unemployment_rate < 54,
      NA,
      unemployment_rate
    )
  )

# Have a look at the data
data


data %>%
  filter(month == "Jan") 

data %>%
  select(month, unemployment_rate) %>%
  head()

# Arrange in ascending order
data %>%
  select(month, unemployment_rate) %>%
  arrange(unemployment_rate)

# Arrange in descending order
data %>%
  select(month, unemployment_rate) %>%
  arrange(desc(unemployment_rate))

# AH: Groupwise operators
data %>% 
  group_by(month) %>% 
  summarise(max_unemployment = max(unemployment_rate)) 

# Get the distinct months in our dataset
data %>% 
  distinct(month)

# Sort data alphabetically
data %>% 
  distinct(month) %>% 
  arrange(month)

# Rename the variable "gdppercapita" to "gdp_per_capita"
# AH: %<>%
# AH: First %>%
data %>%
  rename(gdp_per_capita = gdppercapita)
data
# AH: Now %<>%
data %<>%
  rename(gdp_per_capita = gdppercapita)

# Create a new variable called "summer"
data %>%
  mutate(summer = ifelse(month %in% c("Jun", "Jul", "Aug"), 1, 0))

# AH: %<>%
# AH: First %>%
data %>% mutate_if(is.numeric, replace_na, 0)
data
# AH: Now %<>%
data %<>% mutate_if(is.numeric, replace_na, 0)
data
