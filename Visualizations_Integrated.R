
library("readxl")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(methods)
require(splines)


# xls files
df <- read_excel("movie_data.xlsx")

#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
######################## SINGLE VARIABLE PLOTS ############################
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


## boxplots, histograms, etc

continuous_vars = c("budget_in_millions" , "year_of_release",
                    "total_number_languages" ,   "duration_in_hours",
                    'total_number_of_actors', "total_number_of_directors" ,
                    "total_number_of_producers" ,
                    "total_number_of_production_companies",
                    "total_number_of_production_countries")



# 'Histogram of IMDB Movie Scores'
hist(imdb_score, col="dodgerblue2", breaks = 50, labels = TRUE, xlab='IMDB Score',main='Histogram of IMDB Movie Scores', axes=TRUE)


# 'Histogram of Movie Releases by Year'
hist(year_of_release, col="dodgerblue2", breaks = 40, labels = TRUE, xlab='Year of Release',main='Histogram of Movie Releases by Year', axes=TRUE, ylim=c(0,300),xlim=c(1900,2020))


# 'Box Plot of Movie Durations'
boxplot(duration_in_hours, col="dodgerblue2", labels = TRUE, ylab="Duration of Movie (in Hours)", main='Box Plot of Movie Durations', axes=TRUE)


# 'Barplot of Movie Languages'
par(las=2)  # make label text vertical to axis
counts <- table(movies$main_lang) 
barplot(counts, main="Barplot of Movie Languages", ylab="Number of Movies", horiz=FALSE, ylim=c(0,3000), col="dodgerblue2")


# 'Number of Actors in Movies',main='Histogram of the Number Actors in Movies'
hist(total_number_of_actors, col="dodgerblue2", breaks = 30, labels = TRUE, xlab='Number of Actors in Movies',main='Histogram of the Number Actors in Movies', axes=TRUE, ylim=c(0,1500), xlim=c(0,350))


# 'Pie Chart of Genres'
slices <- c(140,12,103,29,27,194,22,129,3,263,35,46,2,80,17,75,0,77,65,0,17,114,10,9)
lbls <- c("action","musical","adventure","animation","biography","comedy","history","crime","documentary","drama","family","fantasy","filmnoir","horror","music","mystery","realitytv","romance","scifi","shortfilm","sport","thriller","war","western")
pct <- round(slices/sum(slices)*100, 1)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(slices,labels = lbls, radius = 1, cex=0.6, col=rainbow(length(lbls)),main="Pie Chart of Genres")



#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
######################## RELATIONSHIPS W/ IMDB SCORE ############################
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


# Month of release ~ Score
# Looking for seasonality in the month of release. Not a significant feature
df  %>% 
  ggplot( aes(y=imdb_score, x = month_of_release)) + 
  geom_point() + 
  geom_smooth(method = 'lm', formula = y~x) + 
  xlab('Month of Release') + 
  ylab('IMDB Score') + 
  labs(title = 'Scatterplot of Month of Release vs IMDB Score',
       subtitle = "There's not an important relationship between the variables")

# total_number_of_production_companies
df  %>% 
  ggplot( aes(y=imdb_score, x = total_number_of_production_companies)) + 
  geom_point() + 
  geom_smooth(method = 'lm', 
              formula = y~bs(x, knots=c(2,5,10), degree=2)) +
  xlab('No. of Production Companies') + 
  ylab('IMDB Score') + 
  labs(title = 'Scatterplot of No. of Production Companies vs IMDB Score')


#total_number_of_producers
df  %>% 
  ggplot( aes(y=imdb_score, x = total_number_of_producers)) + 
  geom_point() + 
  geom_smooth( method = 'lm' ) + 
  xlab('No. of Producers') + 
  ylab('IMDB Score') + 
  labs(title = 'Scatterplot of No. of Producers vs IMDB Score',
       subtitle = 'No relationship found')


#budget_in_millions
df  %>% 
  ggplot( aes(y=imdb_score, x = budget_in_millions)) +
  geom_point(color = 'dodger blue 2')  +
  geom_smooth(method = 'loess',  span = 0.4, se = FALSE , color = 'orange') +
  xlab('Budget ($MM)') + 
  ylab('IMDB Score') + 
  labs(title = 'Scatterplot of Budget ($MM) vs IMDB Score',
       subtitle = 'Some relationship found')


#year_of_release with score 

df %>%
  ggplot(aes(y=imdb_score,x=year_of_release)) + 
  geom_point(colour = 'dodgerblue2')+ 
  labs(title = "Relationship between IMDB Score and Year of Release" , 
       y="IMDB Score", x = "Year Of Release")


#### Binary variables
genres = c("genre_action","genre_adventure", "genre_animation","genre_biography",
           "genre_comedy"     , "genre_crime"     ,                   
           "genre_documentary", "genre_drama"     ,                   
           "genre_family"     , "genre_fantasy"   ,                   
           "genre_filmnoir"   , "genre_history"   ,                   
           "genre_horror"     , "genre_music"     ,                   
           "genre_musical"    , "genre_mystery"   ,                   
           "genre_romance"   , "genre_scifi"      , 
           "genre_sport"      , "genre_thriller"  ,                   
           "genre_war"        , "genre_western" )

attach(df)


for (genre in c('genre_crime', 'genre_comedy', 'genre_drama',
                'genre_thriller', 'genre_action', 'genre_adventure')) {
  feat = genre
  s = stri_locate_all(pattern = '_', feat, fixed = TRUE)
  genre_name = paste( substr(feat, unlist(s)[1]+1, 100 ), 'movies', sep = ' ')
  other_bar_name = paste('Non',substr(feat, unlist(s)[1]+1, 100),'movies' , sep = ' ')
  print(feat)
  t = table(df[feat])
  n = nrow(df) 
  a1 =  t[1] / n 
  a2 =  t[2] / n
 
  plt <- ggplot(df, mapping = aes_string(x=feat, y='imdb_score' )) +
    geom_bar(stat="summary", fun = "mean",alpha=c( a1, a2), 
             fill = 'yellow2') + #brown2
    xlab('Genre')  + ylab('IMDB Score') +
    scale_x_continuous(breaks = c(0,1) , 
                       labels = c(other_bar_name , genre_name )) 
  print(plt) 
}


#Binary: main_lang (English = 1, All other languages = 0)
#Relationship of total_number_of_languages with score and main_lang

df$English <- ifelse(df$main_lang == 'English', '1', '0')

df %>%
  ggplot(df, aes(y=imdb_score,x=total_number_languages, color =English))+
  geom_point()+
  labs(title = "Relationship between IMDB Score and Total Number of Languages" , 
       y="IMDB Score", x = "Total Number of Languages")+
  scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9))


#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
################# ENGINEERED FEATURES VISUALIZATION #######################
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


names(df)

actors1 = df[c('main_actor1_name','imdb_score' )]
actors2 = df[c('main_actor2_name','imdb_score' )]
actors3 = df[c('main_actor3_name','imdb_score' )]

names(actors1)[1] = 'main_actor_name'
names(actors2)[1] = 'main_actor_name'
names(actors3)[1] = 'main_actor_name'

all_actors = rbind(actors1, actors2, actors3)

# actors that appear in more than 2 movies
freq_actors = all_actors %>% 
  group_by(main_actor_name) %>% 
  summarise(count = n()) %>% 
  filter(count > 2) %>% 
  select(main_actor_name)

cos = all_actors %>% 
  group_by(main_actor_name) %>% 
  summarise(count = n())

n_top = 350 # 200, 250, 340, 400, etc... optimized by trial-error
top_actors = all_actors %>% 
  filter(main_actor_name %in% as.list(freq_actors$main_actor_name)) %>% 
  group_by(main_actor_name) %>% 
  summarize(Mean = mean(imdb_score))  %>% 
  arrange(desc(Mean)) %>% 
  top_n(n_top) # 200

n_worst = 240
worst_actors = all_actors %>% 
  filter(main_actor_name %in% as.list(freq_actors$main_actor_name)) %>% 
  group_by(main_actor_name) %>% 
  summarize(Mean = mean(imdb_score))  %>% 
  arrange(desc(Mean)) %>% 
  top_n(-n_worst)


# dummy variables for top main actors 1, 2, 3
df$main_actor1_top = ifelse(df$main_actor1_name %in% top_actors$main_actor_name,
                            1,
                            0)

df$main_actor2_top = ifelse(df$main_actor2_name %in% top_actors$main_actor_name,
                            1,
                            0)

df$main_actor3_top = ifelse(df$main_actor3_name %in% top_actors$main_actor_name,
                            1,
                            0)

# dummy variables for worst main actors 1, 2, 3
df$main_actor1_worst = ifelse(df$main_actor1_name %in% worst_actors$main_actor_name,
                              1,
                              0)

df$main_actor2_worst  = ifelse(df$main_actor2_name %in% worst_actors$main_actor_name,
                               1,
                               0)

df$main_actor3_worst  = ifelse(df$main_actor3_name %in% worst_actors$main_actor_name,
                               1,
                               0)


df[c( 'title' , 'main_actor1_name', 'main_actor1_top',
      'main_actor2_name','main_actor2_top',
      'main_actor3_name', 'main_actor3_top')]

df$qty_top_actors = df$main_actor1_top + df$main_actor2_top +  df$main_actor3_top

df$qty_worst_actors = df$main_actor1_worst + df$main_actor2_worst +  df$main_actor3_worst


df %>% 
  ggplot(aes(x = qty_top_actors, y = imdb_score)) +
  geom_point(color = 'blue3') +
  geom_smooth(method = 'lm', formula = y~x, color = 'dodger blue 2') +
  xlab('Quantity of top actors') + ylab( 'IMDB Score') +
  labs(title = 'Scatterplot of Qty. of Top Actors vs. IMDB Score')


df %>% 
  ggplot(aes(x = qty_worst_actors, y = imdb_score)) +
  geom_point(color = 'blue3') +
  geom_smooth(method = 'lm', formula = y~poly(x,1), color = 'dodger blue 2') +
  xlab('Quantity of worst actors') + ylab( 'IMDB Score') +
  labs(title = 'Scatterplot of Qty. of Worst Actors vs. IMDB Score')







