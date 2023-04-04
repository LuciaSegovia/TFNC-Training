

# Recap from Rie last training. (They do it).

# 1.1. Loading libraries and importing the data

# Loading library
library(tidyverse)

# Loading the data

dataJ1 <- read.csv(here::here("hces-data", "HH_SEC_J1.csv"))

# check the data
head(dataJ1)
names(dataJ1)
str(dataJ1)
dim(dataJ1)

### 1.2. Cleaning the data

# change variable names

dataJ1 <- dataJ1 %>%
  rename(
    cons_yn = hh_j01,
    cons_unit = hh_j02_1,
    cons_quant = hh_j02_2,
    pur_unit = hh_j03_1,
    pur_quant = hh_j03_2,
    pur_THS = hh_j04,
    prod_unit = hh_j05_1,
    prod_quant = hh_j05_2,
    gift_unit = hh_j06_1,
    gift_quant = hh_j06_2
  )

# Check variable names
names(dataJ1)

# Checking consumption variables

dataJ1 %>% select(cons_quant, pur_quant, prod_quant, gift_quant) %>% 
  summary()

dataJ1 %>% select(gift_quant) %>% distinct()

# Changing NONE to NA

dataJ1 <- dataJ1 %>% 
  mutate(across(c(pur_quant, prod_quant, gift_quant),
         ~na_if(., "NONE")))

# Changing variable types

dataJ1 <- dataJ1 %>%
  mutate(
    itemcode = as.factor(itemcode),
    cons_yn = as.factor(cons_yn),
    cons_unit = as.factor(cons_unit),
    cons_quant = as.numeric(cons_quant),
    pur_unit = as.factor(pur_unit),
    pur_quant = as.numeric(pur_quant),
    prod_unit = as.factor(prod_unit),
    prod_quant = as.numeric(prod_quant),
    gift_unit = as.factor(gift_unit),
    gift_quant = as.numeric(gift_quant)
  )

### 1.3 Exploring the data

count(dataJ1, itemcode, cons_yn)

# Counting frequency of food consumed in the HH

count(dataJ1, itemcode, cons_yn)

# Filtering only consumed

dataJ1 %>% filter(cons_yn == "YES") %>% 
  count(itemcode)

# Filtering only consumed foods, and ordering from by most to least consumed

dataJ1 %>% filter(cons_yn == "YES") %>% 
  count(itemcode) %>% 
  arrange(desc(n))

# Creating a dataframe with: food items, count of HH consuming each food item, 
#and the percentage of HH consuming each foods. 

n_HH <- length(unique(dataJ1$sdd_hhid))

foodlist <- dataJ1 %>% filter(cons_yn == "YES") %>% 
  count(itemcode) %>% 
  arrange(desc(n))  %>%
  mutate(percentage = round((n /n_HH* 100), 2))

# check the data

head(foodlist)
names(foodlist)
str(foodlist)
dim(foodlist)

# Let's have a look at the FCT...

# 2.1 Loading the data

tzfct <- read.csv(here::here("..",  "data", "TZ08_tidied_FCT_v2.CSV"))

# check the data

head(tzfct)
names(tzfct)
str(tzfct)
dim(tzfct)

# 2.2 Exploring FCT for food matching

# Filtering of FCT, for food groups and food items.

# Checking food categories
tzfct %>% filter(Food_Group == "Cereal and Cereal products") 

# Exploring composition of the foods
tzfct %>% filter(Food_Group == "Cereal and Cereal products") %>% 
  select(fdc_id, food_description, EDIBLE, WATERg, VITA_RAEmcg)

# what components are missing? 
tzfct %>% filter(Food_Group == "Cereal and Cereal products") %>% 
  select(fdc_id, food_description, VITA_RAEmcg)

# Finding food items
tzfct %>% filter(grepl("tomato", food_description, ignore.case = TRUE))

# Exploring composition of filtered foods
tzfct %>% filter(grepl("tomato", food_description, ignore.case = TRUE)) %>% 
  select(fdc_id, food_description,  VITA_RAEmcg )

# What do you think about the results? How they compared with other FCTs? 


# Thinking about food matches between HCES food list and food composition.

