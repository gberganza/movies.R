
library(tidyverse)

library(readr)
movies <- read_csv("https://hollywoodagegap.com/movies.csv")
View(movies)



#Number Of Couples In A Movie ###################################################
#Lets arrange release years of movies in descending order and
#show release year, movie title, and number of couples in each film.
movies    %>% group_by(`Movie Name`) %>%
  count(`Release Year`) %>% arrange(desc(`Release Year`)) %>% rename( num_couples = n) 

#Now show the number of couples in each release year.
movies    %>% 
  count(`Release Year`) %>% arrange(desc(`Release Year`)) %>% rename( num_couples = n) %>%
#get the years with most couples
  arrange(desc(num_couples)) 
#2002 and 1999 are the years with the most couples listed in this dataset.
#..does this connect to the year of romcoms?
# The early 2000s brought the peak of the romantic comedy i.e Reuters
# https://www.Reuters.com/graphics/USA-FILM/akveqmlarvr/
#need data on genres added to this dataset to statistically support this claim.

#Find the movie with the most couples listed in the dataset
movies %>% filter( `Actor 1 Gender` != `Actor 2 Gender`) %>% group_by(`Movie Name`, `Release Year`) %>%
  count(Director, sort = TRUE)
#Love Actually (2003) has the most couples in this dataset with 7 couples.

#Directors #######################################################################
#Directors with most couples listed in the dataset?
movies %>% 
  count(Director, sort = TRUE) %>% rename( num_couples = n)
#Movies directed by Woody Allen have the most couples listed in this dataset with 20 couples.

#Movies With Older Couples #################################################################
#Movies with actors 65+.
movies %>% filter( `Actor 1 Age` >= 65 & `Actor 2 Age` >= 65) %>% 
  select(`Movie Name`, `Actor 1 Age`,`Actor 2 Age`)



#Age Differences ####################################################################
#Lets look at movies with the highest age difference.
movies %>% group_by(`Movie Name`, `Release Year`) %>% 
  count(`Age Difference`) %>% arrange(desc(`Age Difference`))
#Harold and Maude (1971) has the highest age difference of couples with 52 years.

#Movies with largest age difference from an LGBT movie in this dataset?
movies %>% 
  filter( `Actor 2 Gender` == `Actor 1 Gender` ) %>% group_by(`Movie Name`, `Release Year`) %>% 
  count(`Age Difference`) %>% arrange(desc(`Age Difference`))
#Beginners (2010) has an age difference of 43 years

#Lets find the percentage of LGBT couples in the entire dataset
movies %>% 
  filter( `Actor 1 Gender` == `Actor 2 Gender` ) %>% 
  count(`Release Year`) %>% 
  summarize(lgbt_coup = sum(n)/1177)
#with 23 couples that means 1.95% of our Hollywood age gap data comes from lgbt couples in film
#meaning we cannot make conclusions about this population as there is not enough data.


#Visualizing Age Differences ####################################################
#Find the average age difference of couples each year this century?
movies %>% mutate(`Release Year` = as.factor(`Release Year`)) %>%
  filter(`Release Year` %in% c(2000:2023)) %>% group_by(`Release Year`) %>%
  summarize(avg_age_diff = mean(`Age Difference`)) 

#Lets plot the average age difference (in years) for movies each year in the 21st century
movies%>% 
  filter(`Release Year` %in% c(2000:2023),`Actor 1 Gender` != `Actor 2 Gender`,sort = TRUE) %>%
      group_by(`Release Year`)%>%
  summarize(avg_age_diff = mean(`Age Difference`)) %>%
  ggplot(aes(x=`Release Year`, y=avg_age_diff))+
  geom_col()+ xlab("Release Year")+ylab("Average Age Difference (years)")
#Average age difference has been mostly consistent over this century except 2020 and 2022
  

#Actor Birth-dates #############################################################
#Find couples/actors with same birth month
movies %>% mutate(`Actor 1 Birthmonth` = month(`Actor 1 Birthdate`)) %>% 
  mutate(`Actor 2 Birthmonth` = month(`Actor 2 Birthdate`)) %>%
  select(`Release Year`, `Actor 1 Birthmonth`, `Actor 2 Birthmonth`, 
         `Movie Name`,  `Actor 2 Name`, `Actor 1 Name`) %>%
  filter(`Actor 1 Birthmonth` == `Actor 2 Birthmonth`)%>% arrange(desc(`Release Year`)) %>%
count(`Actor 2 Birthmonth`, `Actor 1 Birthmonth`) %>% 
  mutate(`Actor 2 Birthmonth` = as.factor(`Actor 2 Birthmonth`)) %>%
#Plot the months of the year with the most couples that share a birthday month;
ggplot(aes(x=`Actor 2 Birthmonth`, y=n))+geom_col()+ xlab('Month')+ylab('Number of Couples')+
  ggtitle('Month of year where actors share same birthday month')
#August has most couples sharing birthday month in this dataset with 11 couples 

