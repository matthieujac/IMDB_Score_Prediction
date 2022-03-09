##### 
## ------------------------------------------------------------------------------------------------------
###  1. FEATURE ENGINEERING
library(readxl)
movie= read_excel("movie_data.xlsx")

#Dropping useless columns (title, url ...)
movie = movie[, -c(1:3)]

#Determining whether a movie has the top/worst actor/director
names(movie)

actors1 = movie[c('main_actor1_name','imdb_score' )]
actors2 = movie[c('main_actor2_name','imdb_score' )]
actors3 = movie[c('main_actor3_name','imdb_score' )]

names(actors1)[1] = 'main_actor_name'
names(actors2)[1] = 'main_actor_name'
names(actors3)[1] = 'main_actor_name'

all_actors = rbind(actors1, actors2, actors3)
library(tidyverse)

# actors that appear in more than 2 movies
freq_actors = all_actors %>% 
  group_by(main_actor_name) %>% 
  summarise(count = n()) %>% 
  filter(count > 2) %>% 
  select(main_actor_name)

cos = all_actors %>% 
  group_by(main_actor_name) %>% 
  summarise(count = n())

summary(cos$count)

top_actors = all_actors %>% 
  filter(main_actor_name %in% as.list(freq_actors$main_actor_name)) %>% 
  group_by(main_actor_name) %>% 
  summarize(Mean = mean(imdb_score))  %>% 
  arrange(desc(Mean)) %>% 
  top_n(200)

worst_actors = all_actors %>% 
  filter(main_actor_name %in% as.list(freq_actors$main_actor_name)) %>% 
  group_by(main_actor_name) %>% 
  summarize(Mean = mean(imdb_score))  %>% 
  arrange(desc(Mean)) %>% 
  top_n(-200)

# dummy variables for top main actors 1, 2, 3
movie$main_actor1_top = ifelse(movie$main_actor1_name %in% top_actors$main_actor_name,1,0)
movie$main_actor2_top = ifelse(movie$main_actor2_name %in% top_actors$main_actor_name,1,0)
movie$main_actor3_top = ifelse(movie$main_actor3_name %in% top_actors$main_actor_name,1,0)

# dummy variables for worst main actors 1, 2, 3
movie$main_actor1_worst = ifelse(movie$main_actor1_name %in% worst_actors$main_actor_name,1,0)
movie$main_actor2_worst  = ifelse(movie$main_actor2_name %in% worst_actors$main_actor_name,1,0)
movie$main_actor3_worst  = ifelse(movie$main_actor3_name %in% worst_actors$main_actor_name,1,0)

movie$qty_top_actors = movie$main_actor1_top + movie$main_actor2_top +  movie$main_actor3_top
movie$qty_worst_actors = movie$main_actor1_worst + movie$main_actor2_worst +  movie$main_actor3_worst
attach(movie)

# repeat top/worse logics for directors
freq_dirs = movie %>% 
  group_by(main_director_name) %>% 
  summarise(count = n()) %>% 
  filter(count >1)

freq_dirs %>%  arrange(desc(count))

n_top = 280
top_directs = movie %>% 
  group_by(main_director_name) %>% 
  summarize(Mean = mean(imdb_score)) %>% 
  filter(main_director_name %in% as.list(freq_dirs$main_director_name)  ) %>% 
  arrange(desc(Mean)) %>% 
  top_n(n_top)

n_wrst = 200
worst_directs = movie %>% 
  group_by(main_director_name) %>% 
  summarize(Mean = mean(imdb_score)) %>% 
  filter(main_director_name %in% as.list(freq_dirs$main_director_name)  ) %>% 
  arrange(desc(Mean)) %>% 
  top_n(-n_wrst )

movie$top_direct_flag  = ifelse(movie$main_director_name %in% top_directs$main_director_name,1,0)
movie$worst_direct_flag  = ifelse(movie$main_director_name %in% worst_directs$main_director_name,1,0)
attach(movie)


## ------------------------------------------------------------------------------------------------------
###  2. DATA PREPROCESSING
#Dropping categorical columns we won't be using
movie = movie[ , -which(names(movie) %in% c('main_actor1_name','main_actor2_name', 'main_actor3_name',
                                            'main_actor1_top', 'main_actor2_top', 'main_actor3_top',
                                            'main_actor1_worst', 'main_actor2_worst', 'main_actor3_worst',
                                            'main_director_name', 'main_producer_name',
                                            'editor_name', 'main_production_company', 
                                            'genre_realitytv', 'genre_shortfilm'))]

#Dummifying categorical columns we will be using
# install.packages("fastDummies")
require(fastDummies)
movie = dummy_cols(movie, select_columns = c('month_of_release','main_lang', 'main_production_country'),
                   remove_first_dummy = TRUE,
                   remove_selected_columns = TRUE)
names(movie)
attach(movie)

library(dplyr)
Rsquares = list()
Pvalues = list()
MregAdjRsquares = list()
MregPvalues = list()
var = list()
decision = list()
bestrsquared = 0

#Convert non-ASCII to ASCII in predictor's names
#install.packages("janitor")
library(janitor) 
movie = clean_names(movie, ascii=TRUE) 
attach(movie)

predictors = colnames(movie)
predictors = predictors[-1]
predictors = as.list(predictors)

#Replace all space in predictor's names with dash
for(i in 1:length(predictors)){
  predictors[i] = gsub(" ", "_", predictors[i]) 
} 


## ------------------------------------------------------------------------------------------------------
###  3. MODEL BASE-LINING

# Testing R2 for linear regression and multiple linear regression
for(i in 2:ncol(movie)){
  testlm1 = lm(imdb_score~unlist(movie[,i]))
  rsquared = summary(testlm1)$r.squared
  pvalue = summary(testlm1)$coefficients[ , 4]
  Rsquares = append(Rsquares, rsquared)
  Pvalues = append(Pvalues, pvalue)
  
  #Add new predictor to rolling list var, run multiple linear regression, and get adjusted r2 and p-value
  if(length(var)==0){
    f = as.formula(paste('imdb_score',unlist(predictors[i-1]),sep = '~'))
  } else{
    f = as.formula(paste('imdb_score',
                         paste(paste(unlist(var), collapse= '+'),
                               unlist(predictors[i-1]),
                               sep= '+'),
                         sep = '~'))
  }
  testlm2 = lm(f)
  mregAdjRsquares = summary(testlm2)$adj.r.squared
  mregPvalues = summary(testlm2)$coefficients[ , 4]
  MregAdjRsquares = append(MregAdjRsquares, mregAdjRsquares)
  MregPvalues = append(MregPvalues, mregPvalues)
  
  #Test if adding new predictor to rolling list var can improve adjusted r2
  #if r2 increases keep, otherwise drop and move on
  if (mregAdjRsquares > bestrsquared){
    var = append(var,predictors[i-1])
    bestrsquared = summary(testlm2)$adj.r.squared
    decision = append(decision,"keep")
  } else{
    decision = append(decision,"drop")
  }
}

#Write results to csv
results= do.call(rbind, Map(data.frame, Predictor = predictors, R_Squared=Rsquares, P_Value=Pvalues))
write.csv(results[1:ncol(movie)-1,],"result.csv", row.names = TRUE)

results2= do.call(rbind, Map(data.frame, Predictor = predictors, R_Squared=Rsquares, P_Value=Pvalues, MregAdjRsquares=MregAdjRsquares, MregPvalues=MregPvalues, Decision=decision))
write.csv(results2[1:ncol(movie)-1,],"result2.csv", row.names = TRUE)
print(bestrsquared)
#print(length(var))

## highest adjusted r2 = 0.51609, by keeping 62 predictors (overall after adding top/worst directors)


## ------------------------------------------------------------------------------------------------------
###  4. TESTING

# test for linearity
library(car) 
f1 = as.formula(imdb_score ~ budget_in_millions + year_of_release + duration_in_hours + 
                  total_number_languages + genre_action + genre_animation + 
                  genre_biography + genre_comedy + genre_crime + genre_documentary + 
                  genre_drama + genre_family + genre_fantasy + genre_filmnoir + 
                  genre_history + genre_horror + genre_music + genre_romance + 
                  genre_war + main_actor1_is_female + main_actor2_is_female + 
                  main_actor3_is_female + total_number_of_actors + total_number_of_directors + 
                  total_number_of_producers + total_number_of_production_companies + 
                  qty_top_actors + qty_worst_actors + top_direct_flag + worst_direct_flag + 
                  month_of_release_3 + month_of_release_6 + month_of_release_7 + 
                  month_of_release_8 + month_of_release_9 + main_lang_deutsch + 
                  main_lang_eesti + main_lang_english + main_lang_espanol + 
                  main_lang_latin + main_lang_nederlands + main_lang_portugues + 
                  main_lang_suomi + main_lang_tieng_viet + 
                  main_lang_hangug_eo_joseonmal + main_lang_guang_zhou_hua_guang_zhou_hua + 
                  main_production_country_argentina + main_production_country_belgium + 
                  main_production_country_bulgaria + main_production_country_ecuador + 
                  main_production_country_finland + main_production_country_germany + 
                  main_production_country_greece + main_production_country_hungary + 
                  main_production_country_iceland + main_production_country_netherlands + 
                  main_production_country_new_zealand + main_production_country_norway + 
                  main_production_country_philippines + main_production_country_singapore + 
                  main_production_country_switzerland + main_production_country_united_kingdom)
mreg_p1=lm(f1)
summary(mreg_p1)
residualPlots(mreg_p1)
# Tukey test: 1.727e-07 (~0)

# the higher the p-values are, the more likely it is linear
# Tukey test: probability for entire model
# Here it is low: So, No. It is not a linear model.
# Generally, if p-value is lower than 0.05, remove it.

f2 = as.formula(imdb_score ~ budget_in_millions + year_of_release +
                  total_number_languages + genre_action + genre_animation + 
                  genre_biography + genre_comedy + genre_crime + genre_documentary + 
                  genre_drama + genre_family + genre_fantasy + genre_filmnoir + 
                  genre_history + genre_horror + genre_music + genre_romance + 
                  genre_war + main_actor1_is_female + main_actor2_is_female + 
                  main_actor3_is_female + total_number_of_directors + 
                  total_number_of_production_companies + 
                  qty_top_actors + qty_worst_actors + top_direct_flag + worst_direct_flag + 
                  month_of_release_3 + month_of_release_6 + month_of_release_7 + 
                  month_of_release_8 + month_of_release_9 + main_lang_deutsch + 
                  main_lang_eesti + main_lang_english + main_lang_espanol + 
                  main_lang_latin + main_lang_nederlands + main_lang_portugues + 
                  main_lang_suomi + main_lang_tieng_viet + 
                  main_lang_hangug_eo_joseonmal + main_lang_guang_zhou_hua_guang_zhou_hua + 
                  main_production_country_argentina + main_production_country_belgium + 
                  main_production_country_bulgaria + main_production_country_ecuador + 
                  main_production_country_finland + main_production_country_germany + 
                  main_production_country_greece + main_production_country_hungary + 
                  main_production_country_iceland + main_production_country_netherlands + 
                  main_production_country_new_zealand + main_production_country_norway + 
                  main_production_country_philippines + main_production_country_singapore + 
                  main_production_country_switzerland + main_production_country_united_kingdom)
mreg_p1_2=lm(f2)
residualPlots(mreg_p1_2)
# Tukey test increased to 0.0007563, NOT good enough -> keep f1 for now and consider poly to improve.
summary(mreg_p1_2)

# test for Heteroskedasticity
ncvTest(mreg_p1_2)
# p-value = 2.22e-16 -- very low -- less than 0.05 -- Clear Heteroskedasticity
## Correcting Heteroskedasticity 
summary(mreg_p1_2)   # ORIGINAL MODEL WITH HETEROSKEDASTICITY
# vs.
coeftest(mreg_p1_2, vcov=vcovHC(mreg_p1_2, type='HC1'))    # CORRECTED MODEL WITHOUT HETEROSKEDASTICITY

summary(mreg_p1_2)

ncvTest(mreg_p1_2)
# attempted to correct but no significant improvement in p-value -> to explore non-linearity relationship

# test for Outliers
# Test 1: Visual Test
mreg_p3=lm(f1)
qqPlot(mreg_p3)  # tool from car package: qqPlot, reports outliers to you. VERY useful.

# Test 2: Bonferroni test
outlierTest(mreg_p3)

# Removing outliers
movie_out=movie[-c(633,2045,895,2310,2718,526,2610,1167), ]  # the last comma is because we are NOT deleting columns. (row,column)

# Re-do outlier tests (now without the outliers)
mreg_p3_noout=lm(f1, data = movie_out)
summary(mreg_p3_noout) # without outliers
# Multiple R-squared:  0.5399,        Adjusted R-squared:  0.5299 
# vs.
summary(mreg_p3) # orginal model with outliers
# Multiple R-squared:  0.5264,        Adjusted R-squared:   0.5161
# conclusion: our model is slightly more robust once we remove the outliers
qqPlot(mreg_p3_noout)


### Collinearity (arises when two or more predictors are highly correlated)
mreg_p4=lm(f1,data = movie_out)
vif(mreg_p4)
# if there is a variance inflation factor greater than 4, kick one out of the regression. 
# there are none here.


#Format regression table results using stargazer
#install.packages("stargazer")
summary(mreg_p4)
library(stargazer)
stargazer(mreg_p4, type="html")

## ------------------------------------------------------------------------------------------------------
### 3. TESTING

# Model Ready for Testing
summary(mreg_p4)

# K-Fold Cross Validation
# run a test with 10 folds = 90% training, 10% testing data
set.seed(13)
library(boot)
library(caTools)
fit=glm(f1,data = movie_out)
mse=cv.glm(movie_out, fit, K=10)$delta[1]  # adding parameter K=10, otherwise it assumes a LOOCV test
mse
# mse = 0.4382943 (seed=13)

# Train test split Cross Validation
set.seed(13)
for (i in 1:10) {
  sample=sample.split(movie_out$imdb_score, SplitRatio=0.7)  # 70% train, 30% test
  train=subset(movie_out, sample==TRUE)
  test=subset(movie_out, sample==FALSE)
  
  test$pred=predict(fit, test)
  test$res=(test$imdb_score - test$pred)
  
  test[c('pred','imdb_score')]
  
  MAPE = mean(abs(test$res)/test$imdb_score)
  print(MAPE)
  
  test$res_sq=(test$res)^2
  MSE=mean(test$res_sq)
}
# MAPE = ~0.08

## ------------------------------------------------------------------------------------------------------
###  5. FINE-TUNING WITH POLYNOMIAL
###Poly testing

ptm = proc.time()

allMSE = list()

combinations = list()

counter = 0

bestmse = 1
bestcomb = ""

fpoly = as.formula(imdb_score ~ 
                     poly(budget_in_millions, degree=a)
                   + poly(year_of_release, degree = b)
                   + poly(duration_in_hours, degree = c)
                   + poly(total_number_languages, degree = d)
                   + poly(total_number_of_actors, degree = e)
                   + poly(total_number_of_directors, degree = f)
                   + poly(total_number_of_producers, degree = g)
                   + poly(total_number_of_production_companies, degree = h)
                   + poly(qty_top_actors, degree = i) 
                   + poly(qty_worst_actors, degree = j) +
                     genre_action + genre_animation + 
                     genre_biography + genre_comedy + genre_crime + genre_documentary + 
                     genre_drama + genre_family + genre_fantasy + genre_filmnoir + 
                     genre_history + genre_horror + genre_music + genre_romance + 
                     genre_war + main_actor1_is_female + main_actor2_is_female + 
                     main_actor3_is_female +  
                     top_direct_flag + worst_direct_flag + 
                     month_of_release_3 + month_of_release_6 + month_of_release_7 + 
                     month_of_release_8 + month_of_release_9 + main_lang_deutsch + 
                     main_lang_eesti + main_lang_english + main_lang_espanol + 
                     main_lang_latin + main_lang_nederlands + main_lang_portugues + 
                     main_lang_suomi + main_lang_tieng_viet +  
                     main_lang_hangug_eo_joseonmal + main_lang_guang_zhou_hua_guang_zhou_hua + 
                     main_production_country_argentina + main_production_country_belgium + 
                     main_production_country_bulgaria + main_production_country_ecuador + 
                     main_production_country_finland + main_production_country_germany + 
                     main_production_country_greece + main_production_country_hungary + 
                     main_production_country_iceland + main_production_country_netherlands + 
                     main_production_country_new_zealand + main_production_country_norway + 
                     main_production_country_philippines + main_production_country_singapore + 
                     main_production_country_switzerland + main_production_country_united_kingdom)

for (a in 1:3){
  for(b in 1:3){
    for(c in 1:3){
      for(d in 1:3){
        for(e in 1:3){
          for(f in 1:3){
            for(g in 1:3){
              for(h in 1:3){
                for(i in 1:3){
                  for(j in 1:3){
                    
                    print(counter)
                    counter = counter + 1
                    
                    set.seed(13)
                    fit=glm(fpoly,data = movie_out)
                    msepoly=cv.glm(movie_out, fit, K=5)$delta[1]  # adding parameter K=5, to reduce computation time
                    
                    allMSE = append(allMSE, msepoly)
                    
                    combstr = paste(as.character(a),as.character(b),as.character(c),as.character(d),as.character(e),as.character(f),as.character(g),as.character(h),as.character(i),as.character(j))
                    combinations = append(combinations, combstr)
                    
                    if (msepoly < bestmse){
                      bestmse = msepoly
                      bestcomb = combstr
                    }
                    
                    print('Best MSE so far....')
                    print(bestmse)
                    print(bestcomb)
                    
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}

MSE_POLY_results3= do.call(rbind, Map(data.frame, CombinationMSE=allMSE, DegreeCombination=combinations))

proc.time() - ptm


#recreating best poly model : 3 1 2 1 2 3 3 2 2 1

fpoly_best = as.formula(imdb_score ~ 
                          poly(budget_in_millions, degree=3)
                        + poly(year_of_release, degree = 1)
                        + poly(duration_in_hours, degree = 2)
                        + poly(total_number_languages, degree = 1)
                        + poly(total_number_of_actors, degree = 2)
                        + poly(total_number_of_directors, degree = 3)
                        + poly(total_number_of_producers, degree = 3)
                        + poly(total_number_of_production_companies, degree = 2)
                        + poly(qty_top_actors, degree = 2) 
                        + poly(qty_worst_actors, degree = 1) +
                          genre_action + genre_animation + 
                          genre_biography + genre_comedy + genre_crime + genre_documentary + 
                          genre_drama + genre_family + genre_fantasy + genre_filmnoir + 
                          genre_history + genre_horror + genre_music + genre_romance + 
                          genre_war + main_actor1_is_female + main_actor2_is_female + 
                          main_actor3_is_female +  
                          top_direct_flag + worst_direct_flag + 
                          month_of_release_3 + month_of_release_6 + month_of_release_7 + 
                          month_of_release_8 + month_of_release_9 + main_lang_deutsch + 
                          main_lang_eesti + main_lang_english + main_lang_espanol + 
                          main_lang_latin + main_lang_nederlands + main_lang_portugues + 
                          main_lang_suomi + main_lang_tieng_viet +  
                          main_lang_hangug_eo_joseonmal + main_lang_guang_zhou_hua_guang_zhou_hua + 
                          main_production_country_argentina + main_production_country_belgium + 
                          main_production_country_bulgaria + main_production_country_ecuador + 
                          main_production_country_finland + main_production_country_germany + 
                          main_production_country_greece + main_production_country_hungary + 
                          main_production_country_iceland + main_production_country_netherlands + 
                          main_production_country_new_zealand + main_production_country_norway + 
                          main_production_country_philippines + main_production_country_singapore + 
                          main_production_country_switzerland + main_production_country_united_kingdom)


set.seed(13)
lm_poly = glm(fpoly_best, data = movie_out )
mse_best_poly = cv.glm(movie_out, lm_poly, K=5)$delta[1]
mse_best_poly


## ------------------------------------------------------------------------------------------------------
###  6. FINE-TUNING WITH SPLINES

#Spline testing
#lets plot all variables subject to poly and see if distribution is relevant for splines

plot1 = ggplot(movie_out, aes(y=imdb_score,x=budget_in_millions))
plot2 = ggplot(movie_out, aes(y=imdb_score,x=year_of_release))
plot3 = ggplot(movie_out, aes(y=imdb_score,x=duration_in_hours))
plot4 = ggplot(movie_out, aes(y=imdb_score,x=total_number_languages))
plot5 = ggplot(movie_out, aes(y=imdb_score,x=total_number_of_actors))
plot6 = ggplot(movie_out, aes(y=imdb_score,x=total_number_of_directors))
plot7 = ggplot(movie_out, aes(y=imdb_score,x=total_number_of_producers))
plot8 = ggplot(movie_out, aes(y=imdb_score,x=total_number_of_production_companies))
plot9 = ggplot(movie_out, aes(y=imdb_score,x=qty_top_actors))
plot10 = ggplot(movie_out, aes(y=imdb_score,x=qty_worst_actors))

scatter = geom_point()

library(gridExtra)
grid.arrange(plot1+scatter, 
             plot2+scatter, 
             plot3+scatter, 
             plot4+scatter, 
             plot5+scatter, 
             plot6+scatter, 
             plot7+scatter, 
             plot8+scatter, 
             plot9+scatter, 
             plot10+scatter, 
             nrow = 3)

#no jagged lines so splines dont appear as an obvious solution but let's still try with two nots at 33% and 66% quantiles
#approach - try with first variable - improved mse = keep , worse mse = leave as poly. if kept, try for quantiles 25% 50% and 75%. 
#finish by toying with degrees to see if better mse can be obtained

require(splines)

fspline = as.formula(imdb_score ~ 
                       bs(budget_in_millions, knots = c(quantile(budget_in_millions , .33),quantile(budget_in_millions , .66)), degree =5)
                     + poly(year_of_release, degree = 1)
                     + poly(duration_in_hours, degree = 2)
                     + poly(total_number_languages, degree = 1)
                     + bs(total_number_of_actors, knots = c(quantile(total_number_of_actors , .10),quantile(total_number_of_actors , .30), quantile(total_number_of_actors , .50)), degree =2)
                     + poly(total_number_of_directors, degree = 3)
                     + poly(total_number_of_producers, degree = 3)
                     + bs(total_number_of_production_companies, knots = c(quantile(total_number_of_production_companies , .33),quantile(total_number_of_production_companies , .66)), degree =2)
                     + poly(qty_top_actors, degree = 2) 
                     + poly(qty_worst_actors, degree = 1) +
                       genre_action + genre_animation + 
                       genre_biography + genre_comedy + genre_crime + genre_documentary + 
                       genre_drama + genre_family + genre_fantasy + genre_filmnoir + 
                       genre_history + genre_horror + genre_music + genre_romance + 
                       genre_war + main_actor1_is_female + main_actor2_is_female + 
                       main_actor3_is_female +  
                       top_direct_flag + worst_direct_flag + 
                       month_of_release_3 + month_of_release_6 + month_of_release_7 + 
                       month_of_release_8 + month_of_release_9 + main_lang_deutsch + 
                       main_lang_eesti + main_lang_english + main_lang_espanol + 
                       main_lang_latin + main_lang_nederlands + main_lang_portugues + 
                       main_lang_suomi + main_lang_tieng_viet +  
                       main_lang_hangug_eo_joseonmal + main_lang_guang_zhou_hua_guang_zhou_hua + 
                       main_production_country_argentina + main_production_country_belgium + 
                       main_production_country_bulgaria + main_production_country_ecuador + 
                       main_production_country_finland + main_production_country_germany + 
                       main_production_country_greece + main_production_country_hungary + 
                       main_production_country_iceland + main_production_country_netherlands + 
                       main_production_country_new_zealand + main_production_country_norway + 
                       main_production_country_philippines + main_production_country_singapore + 
                       main_production_country_switzerland + main_production_country_united_kingdom)

set.seed(13)
lm_spline = glm(fspline, data = movie_out )
mse_best_spline = cv.glm(movie_out, lm_spline, K=5)$delta[1]
mse_best_spline

#lets confirm that with a loop

fsplineloop = as.formula(imdb_score ~ 
                           bs(budget_in_millions, knots = c(quantile(budget_in_millions , .33),quantile(budget_in_millions , .66)), degree =a)
                         + poly(year_of_release, degree = 1)
                         + poly(duration_in_hours, degree = 2)
                         + poly(total_number_languages, degree = 1)
                         + bs(total_number_of_actors, knots = c(quantile(total_number_of_actors , .10),quantile(total_number_of_actors , .30), quantile(total_number_of_actors , .50)), degree =b)
                         + poly(total_number_of_directors, degree = 3)
                         + poly(total_number_of_producers, degree = 3)
                         + bs(total_number_of_production_companies, knots = c(quantile(total_number_of_production_companies , .33),quantile(total_number_of_production_companies , .66)), degree =c)
                         + poly(qty_top_actors, degree = 2) 
                         + poly(qty_worst_actors, degree = 1) +
                           genre_action + genre_animation + 
                           genre_biography + genre_comedy + genre_crime + genre_documentary + 
                           genre_drama + genre_family + genre_fantasy + genre_filmnoir + 
                           genre_history + genre_horror + genre_music + genre_romance + 
                           genre_war + main_actor1_is_female + main_actor2_is_female + 
                           main_actor3_is_female +  
                           top_direct_flag + worst_direct_flag + 
                           month_of_release_3 + month_of_release_6 + month_of_release_7 + 
                           month_of_release_8 + month_of_release_9 + main_lang_deutsch + 
                           main_lang_eesti + main_lang_english + main_lang_espanol + 
                           main_lang_latin + main_lang_nederlands + main_lang_portugues + 
                           main_lang_suomi + main_lang_tieng_viet +  
                           main_lang_hangug_eo_joseonmal + main_lang_guang_zhou_hua_guang_zhou_hua + 
                           main_production_country_argentina + main_production_country_belgium + 
                           main_production_country_bulgaria + main_production_country_ecuador + 
                           main_production_country_finland + main_production_country_germany + 
                           main_production_country_greece + main_production_country_hungary + 
                           main_production_country_iceland + main_production_country_netherlands + 
                           main_production_country_new_zealand + main_production_country_norway + 
                           main_production_country_philippines + main_production_country_singapore + 
                           main_production_country_switzerland + main_production_country_united_kingdom)

splineMSEs = list()

combinations_spline = list()

counter_spline = 0

bestmsespline = 1
bestcombspline = ""     

for (a in 1:5){
  for(b in 1:5){
    for(c in 1:5){
      
      print(counter_spline)
      counter_spline = counter_spline + 1
      
      set.seed(13)
      fit=glm(fsplineloop,data = movie_out)
      msespline=cv.glm(movie_out, fit, K=5)$delta[1]  
      
      splineMSEs = append(splineMSEs, msespline)
      
      combstr = paste(as.character(a),as.character(b),as.character(c))
      combinations_spline = append(combinations_spline, combstr)
      
      if (msespline < bestmsespline){
        bestmsespline = msespline
        bestcombspline = combstr
      }
      
      print('Best MSE so far....')
      print(bestmsespline)
      print(bestcombspline)
      
    }
  }
}

MSE_spline_results= do.call(rbind, Map(data.frame, CombinationMSE=splineMSEs, DegreeCombination=combinations_spline))



#ULTIMATE BEST MODEL
require(splines)
fbest = as.formula(imdb_score ~ 
                     bs(budget_in_millions, knots = c(quantile(budget_in_millions , .33),quantile(budget_in_millions , .66)), degree =5)
                   + poly(year_of_release, degree = 1)
                   + poly(duration_in_hours, degree = 2)
                   + poly(total_number_languages, degree = 1)
                   + bs(total_number_of_actors, knots = c(quantile(total_number_of_actors , .10),quantile(total_number_of_actors , .30), quantile(total_number_of_actors , .50)), degree =2)
                   + poly(total_number_of_directors, degree = 3)
                   + poly(total_number_of_producers, degree = 3)
                   + bs(total_number_of_production_companies, knots = c(quantile(total_number_of_production_companies , .33),quantile(total_number_of_production_companies , .66)), degree =1)
                   + poly(qty_top_actors, degree = 2) 
                   + poly(qty_worst_actors, degree = 1) +
                     genre_action + genre_animation + 
                     genre_biography + genre_comedy + genre_crime + genre_documentary + 
                     genre_drama + genre_family + genre_fantasy + genre_filmnoir + 
                     genre_history + genre_horror + genre_music + genre_romance + 
                     genre_war + main_actor1_is_female + main_actor2_is_female + 
                     main_actor3_is_female +  
                     top_direct_flag + worst_direct_flag + 
                     month_of_release_3 + month_of_release_6 + month_of_release_7 + 
                     month_of_release_8 + month_of_release_9 + main_lang_deutsch + 
                     main_lang_eesti + main_lang_english + main_lang_espanol + 
                     main_lang_latin + main_lang_nederlands + main_lang_portugues + 
                     main_lang_suomi + main_lang_tieng_viet +  
                     main_lang_hangug_eo_joseonmal + main_lang_guang_zhou_hua_guang_zhou_hua + 
                     main_production_country_argentina + main_production_country_belgium + 
                     main_production_country_bulgaria + main_production_country_ecuador + 
                     main_production_country_finland + main_production_country_germany + 
                     main_production_country_greece + main_production_country_hungary + 
                     main_production_country_iceland + main_production_country_netherlands + 
                     main_production_country_new_zealand + main_production_country_norway + 
                     main_production_country_philippines + main_production_country_singapore + 
                     main_production_country_switzerland + main_production_country_united_kingdom)

set.seed(13)
final_model=glm(fbest,data = movie_out)
final_mse=cv.glm(movie_out, final_model, K=5)$delta[1]

final_mse
## lowest mse = 0.4075092

#SAMPLE TESTING
require(caTools)
require(boot)
require(splines)
sample=sample.split(movie_out$imdb_score, SplitRatio=0.7)
train=subset(movie_out, sample==TRUE)
test=subset(movie_out, sample==FALSE)

test$pred=predict(final_model, test)
test$res=(test$imdb_score - test$pred)

test[c('pred','imdb_score')]


MAPE = mean(abs(test$res)/test$imdb_score)

test$res_sq=(test$res)^2

MSE=mean(test$res_sq)

print(MAPE)
## lowest MAPE = 0.7652

#export to excel

#install(writexl)
library("writexl")
write_xlsx(test,"some predictions.xlsx")

names(test)