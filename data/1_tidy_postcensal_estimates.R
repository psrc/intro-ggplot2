# This script will read-in OFM April 1 Population data and transform it to be used with ggplot2
library(openxlsx)
library(reshape2)
library(stringr)
library(lubridate)

my.dir <- "C:/Users/clam/Documents/github/intro-ggplot2-draft"

# read in excel data
raw <- read.xlsx(file.path(my.dir, "ofm_april1_population_final.xlsx"), startRow = 4)

# filter for PSRC region and remove Line column
region <- subset(raw, County %in% c("King", "Kitsap", "Pierce", "Snohomish"))
keep.cols <- setdiff(colnames(region), "Line")
region <- region[, keep.cols]

# OFM publishes data in a cross-tab (wider) format, pivot the table longer so that we have a new column called 'melted_cols'
# containing those column headers with information such as year, the data attribute (population), and data source type
id.cols <- c("Filter", "County", "Jurisdiction")
df <- melt(region, id.vars = id.cols, variable.name = "melted_cols", value.name = "Estimate")

# create separate columns for data year (20##), attribute (Population), and source (Census or Estimate)
df$Year_chr <- str_extract(df$melted_cols, "^\\d+") # year as character datatype
df$Year_dt <- ymd(as.numeric(df$Year_chr), truncated = 2L) # year as a formal date
df$Attribute <- str_extract(df$melted_cols, "(?:[^\\d+\\.])\\w+")
df$Source <- str_extract(df$melted_cols, "\\w+$")

# exclude melted_cols and re-order columns
cols.order <- c(id.cols, "Attribute", "Source", "Year_chr", "Year_dt", "Estimate")
df <- df[, cols.order]

# checkout the structure of df
str(df)

# the Estimate column should contain numeric values, not characters!
# convert the Estimates to be actual numbers and double check the structure
df$Estimate <- as.numeric(df$Estimate)
str(df)

# export table
write.xlsx(df, file.path(my.dir, "ofm_april1_population_final_demo.xlsx"))
