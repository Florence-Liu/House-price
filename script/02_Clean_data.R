#### Preamble ####
# Purpose: Clean datasets obtained by web scraping using Python about price of house in sale and relevant variables in 11 districts in Nanjing
# Author: Yufei Liu
# Date: 22 Nov 2023
# Contact: florence.liu@mail.utoronto.ca
# License: MIT
# Download link: https://nj.lianjia.com/ershoufang/sf1/


#### Workspace setup ####
library(tidyverse)
library(stringr)



#### Download data ####
## Scrapped 11 original datasets for 12 district in Nanjing from Lianjia.com 
## Import the datasets from input/data folder

data_gulou <- read.csv('input/data/raw data gulou.csv')
data_jianye <- read.csv('input/data/raw data jianye.csv')
data_gaochun <- read.csv('input/data/raw data gaochun.csv')
data_jiangning <- read.csv('input/data/raw data jiangning.csv')
data_lishui <- read.csv('input/data/raw data lishui.csv')
data_liuhe <- read.csv('input/data/raw data liuhe.csv')
data_pukou <- read.csv('input/data/raw data pukou.csv')
data_qinhuai <- read.csv('input/data/raw data qinhuai.csv')
data_qixia <- read.csv('input/data/raw data qixia.csv')
data_xuanwu <- read.csv('input/data/raw data xuanwu.csv')
data_yuhuatai <- read.csv('input/data/raw data yuhuatai.csv')

#### Merge data ####
### Add a variable representing district

data_gaochun$District <- rep('Gaochun', nrow(data_gaochun))
data_gulou$District <- rep('Gulou', nrow(data_gulou))
data_jiangning$District <- rep('Jiangning', nrow(data_jiangning))
data_jianye$District <- rep('Jianye', nrow(data_jianye))
data_lishui$District <- rep('Lishui', nrow(data_lishui))
data_liuhe$District <- rep('Liuhe', nrow(data_liuhe))
data_pukou$District <- rep('Pukou', nrow(data_pukou))
data_qinhuai$District <- rep('Qinhuai', nrow(data_qinhuai))
data_qixia$District <- rep('Qixia', nrow(data_qixia))
data_xuanwu$District <- rep('Xuanwu', nrow(data_xuanwu))
data_yuhuatai$District <- rep('Yuhuatai', nrow(data_yuhuatai))

### Add row names

row_title <- c('Description', 'Location', 'Structural attributes', 'Total price', 'Unit price', 'District')
colnames(data_gaochun) <- row_title
colnames(data_gulou) <- row_title
colnames(data_jiangning) <- row_title
colnames(data_jianye) <- row_title
colnames(data_lishui) <- row_title
colnames(data_liuhe) <- row_title
colnames(data_pukou) <- row_title
colnames(data_qinhuai) <- row_title
colnames(data_qixia) <- row_title
colnames(data_xuanwu) <- row_title
colnames(data_yuhuatai) <- row_title


### Merge datasets
merged_data <- data_gaochun |> full_join(data_gulou) |> 
  full_join(data_jiangning) |> full_join(data_jianye) |>
  full_join(data_lishui) |>
  full_join(data_liuhe) |> full_join(data_pukou) |>
  full_join(data_qinhuai) |> full_join(data_qixia) |>
  full_join(data_xuanwu) |> full_join(data_yuhuatai)

### Save uncleaned merged data
write_csv(merged_data, "output/data/merged_data.csv")




#### Clean data ####

### Split structural attributes into multiple variables
## First split for structural attributes based on special symbol '|'
split_1 <- as.data.frame(do.call(rbind, strsplit(merged_data$`Structural attributes`, "|", fixed = TRUE)))
split1_names <- c('Structure', 'Area', 'Face', 'Furnished', 'Floor', 'Type')
colnames(split_1) <- split1_names
merged_data <- cbind(merged_data, split_1)

## Second split for variable Structure
split_2 <- as.data.frame(do.call(rbind, strsplit(merged_data$`Structure`, "室", fixed = TRUE)))
split2_name <- c('Bedroom', 'Living/dining room')
colnames(split_2) <- split2_name
merged_data <- cbind(merged_data, split_2)

## Third split for variable Floor
floor_info <- str_match(merged_data$Floor, "([^\\(]+)\\(共(\\d+)层\\)")
merged_data$Floor_Type <- floor_info[, 2]
merged_data$Total_Floors <- as.integer(floor_info[, 3])



### Estimate detailed floor by Floor_type and Total_Floors
merged_data <- merged_data |> 
  mutate(Detailed_Floor = case_when(
    Floor_Type == " 高楼层" ~ as.integer(Total_Floors * 0.7) + 1,
    Floor_Type == " 中楼层" ~ as.integer(Total_Floors * 0.45) + 1,
    Floor_Type == " 低楼层" ~ as.integer(Total_Floors * 0.2) + 1,
    Floor_Type == " 地下室" ~ -1,
    Floor_Type == NA ~ NA,
    TRUE ~ Total_Floors  # Default condition
  ))


### Create a new variable indicating if the house is facing South
merged_data$Facing_South <- ifelse(merged_data$Face == " 南 ", 1, 0)


### Convert variable Furnished into English
merged_data <- merged_data |>
  mutate(Furnished_Eng = case_when(
    Furnished == " 精装 " ~ "Fully Furnished",
    Furnished == " 简装 " ~ "Part Furnished",
    Furnished == " 毛坯 " ~ "Not Furnished",
    Furnished == " 其他 " ~ "Other",
    TRUE ~ Furnished
  ))


### Remove characters and units
merged_data$`Living/dining room` <- gsub("厅", "", merged_data$`Living/dining room`)
merged_data$Area <- gsub("平米", "", merged_data$`Area`)
merged_data$`Unit price` <- gsub("元/平", "", merged_data$`Unit price`)
merged_data$`Unit price` <- gsub(",", "", merged_data$`Unit price`)


### Select variables of interest and change variable names
merged_data <- merged_data |>
  select(`Total price`,`Unit price`, District, Area, Furnished_Eng, Bedroom, 
         `Living/dining room`, Total_Floors, Detailed_Floor, Facing_South)

merged_data <- merged_data |>
  rename(Furnished = Furnished_Eng, Total_Price = `Total price`,
         Living_Room = `Living/dining room`, Unit_Price = `Unit price`)

### Convert variable types

merged_data$Total_Price <- as.numeric(merged_data$Total_Price)
merged_data$Unit_Price <- as.numeric(merged_data$Unit_Price)
merged_data$District <- as.character(merged_data$District)
merged_data$Area <- as.numeric(merged_data$Area)
merged_data$Furnished <- as.character(merged_data$Furnished)
merged_data$Bedroom <- as.numeric(merged_data$Bedroom)
merged_data$Living_Room <- as.numeric(merged_data$Living_Room)
merged_data$Total_Floors <- as.numeric(merged_data$Total_Floors)
merged_data$Detailed_Floor <- as.numeric(merged_data$Detailed_Floor)
merged_data$Facing_South <- as.factor(merged_data$Facing_South)


### Remove missing values

cleaned_data <- na.omit(merged_data)


#### Test data ####

### Check there is no missing values
sum(is.na(cleaned_data)) == 0


### Check variable types
cleaned_data$Total_Price |> class() == "numeric"
cleaned_data$Unit_Price |> class() == "numeric"
cleaned_data$District |> class() == "character"
cleaned_data$Area |> class() == "numeric"
cleaned_data$Furnished |> class() == "character"
cleaned_data$Bedroom |> class() == "numeric"
cleaned_data$Living_Room |> class() == "numeric"
cleaned_data$Total_Floors |> class() == "numeric"
cleaned_data$Detailed_Floor |> class() == "numeric"
cleaned_data$Facing_South |> class() == "factor"


### Check the unique values in District and Furnished
cleaned_data$District |> unique()
cleaned_data$Furnished |> unique()


### Check Total price, Unit price, and Area are positive
cleaned_data$Total_Price |> min() > 0
cleaned_data$Unit_Price |> min() > 0
cleaned_data$Area |> min() > 0


#### Save data ####
write_csv(cleaned_data, "output/data/cleaned_data.csv")
