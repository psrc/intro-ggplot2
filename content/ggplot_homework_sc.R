# Suzanne's solution
library(ggplot2)
library(dplyr)
library(readxl)




#### Multiple Choice Section ####
title <- 'Fuel Economy of Popular Cars'
legend.title <- 'Type of Car'

# this works
ggplot(mpg, aes(displ, hwy, color = class)) + geom_point() + scale_color_discrete(name = legend.title)+
  theme(legend.title = element_text(legend.title))+labs(color = legend.title)

# this doesn't work, I think because you have to specific the legend.text in the theme?
ggplot(mpg, aes(displ, hwy, color = class)) + geom_point() + scale_color_discrete(name = legend.title)+
  theme(legend.title = element_text(legend.title))+theme(legend.text = element_text(title = legend.title))+
  labs(color = legend.title)

# works
ggplot(mpg, aes(displ, hwy, color = class)) + geom_point() +labs(title = title)
ggplot(mpg, aes(displ, hwy, color = class)) + geom_point() +ggtitle(title)
#looks kind of weird with the title in the plot, but works, I think the annotate makes it be inside the plot
ggplot(mpg, aes(displ, hwy, color = class)) + geom_point() +annotate('text', label = title, x = min(mpg$displ) + 3.5, y = max(mpg$hwy), size = 4)
#this doesn't put a title on the plot. I guess you can't use theme with title.
ggplot(mpg, aes(displ, hwy, color = class)) + geom_point() +theme(plot.title = element_text(title))

#### Cereal Section ####
cereal_file<-'C:/Users/SChildress/Documents/GitHub/intro-ggplot2/data/cereal.xlsx'
cereal_df <-read_excel(cereal_file)
glimpse(cereal_df)

cereal_df <- cereal_df %>% mutate(sugars_per_oz=sugars/weight)
ggplot(cereal_df, aes(sugars_per_oz, rating, color = mfr, shape= type))+ geom_point(size= 5, fill= 'red') +
  scale_shape_manual(values = c(1,3,5,7,9,11)) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

# the minimum sugars per ounce is -1? how can it be negative, maybe missing data
# the maximum 15

cereal_df %>% 
  filter(sugars_per_oz == max(sugars_per_oz)) %>% # filter the data.frame to keep row where x is maximum
  select(name)

# Golden Crisp and Smacks have the most sugar

cereal_df %>% 
  filter(sugars_per_oz == 0) %>% # filter the data.frame to keep row where x is maximum
  select(name)

# Lots have zero sugar like "All-Bran with Extra FIber"

ggplot(cereal_df, aes(sugars_per_oz, rating, color = mfr, shape= type))+ geom_point(size= 5, fill= 'red') +
  scale_shape_manual(values = c(1,3,5,7,9,11)) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))+
  facet_wrap(vars(shelf))+
  labs(x = "Sugars per Ounce")+
  labs(y= "Rating")+
  geom_text(data = .%>% filter(rating == max(rating)), aes(label = name), size = 2, hjust=-0.25)


### OFM data

ofm_file<-'C:/Users/SChildress/Documents/GitHub/intro-ggplot2/data/ofm_april1_population_final_tidied.xlsx'
ofm_df <- read_excel(ofm_file)
ofm_df_sm <- ofm_df %>% filter(Year_chr=='2010'| Year_chr=='2020') %>% filter(Filter=='4')

ofm_df_sm<-reshape2::dcast(ofm_df_sm, County + Jurisdiction ~ paste0("Year_", Year_chr), value.var = "Estimate")

#Create a new column calculating the difference between 2020 and 2010 estimates
#Sort the data frame so that the difference column is in descending order
#Take the top 10 rows (use head())

ofm_df_sm <- ofm_df_sm%>% mutate(Diff1020= Year_2020 - Year_2010)%>% arrange(desc(Diff1020))%>%head(10)

# Plot where cities/towns are on the x-axis and the y-axis displays total population estimate
# Add thousands separator to the Y axis column
# Add a title and source caption
# Angle the cities/town axis labels and change the appearance of other text
# Change the axis titles
# Fill the bars to reflect which county the cities and towns are in
# What order are the cities/town in? Can you reorder the cities/towns based on the value of the difference column?
#   convert the Jurisdiction column to a factor
# use reorder()



ggplot(data=ofm_df_sm, aes(x=Jurisdiction, y=Diff1020, fill=County)) +
  geom_bar(stat="identity") + scale_y_continuous(labels = scales::comma)+
  labs(title="Growth in Population by Jurisdiction",
       x ="Jurisdiction", y = "Growth in Population", caption= "source: OFM estimates")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))
  