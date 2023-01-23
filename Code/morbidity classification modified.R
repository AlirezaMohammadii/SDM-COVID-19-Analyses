### 
#[1] "Australia"      "Austria"        "Czech Republic" "Germany"        "Iceland"        "Mexico"        
#[7] "Netherlands"    "Spain"          "United States"  "Estonia"        "Slovenia"         
#[12] "Latvia"         "Costa Rica"     "Lithuania" 
###

#Necessary libraries and also for plotting data in a better way
install.packages("rlang")
library(gridExtra)
library(cowplot)
library(scales)
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if(!require(devtools)) install.packages("devtools")
if (!require('MASS')) install.packages('MASS'); library('MASS')
if (!require('ISLR')) install.packages('ISLR'); library('ISLR')
#devtools::install_github("kassambara/ggpubr", force = TRUE)
require(ggplot2)
dev.off()

setwd("C:/Users/mohammadia/Desktop/R")

morb = read.csv("HEALTH_STAT total.csv")

# this goes through all values in "Variable" column and cut texts with more than 20 chars
for(i in 1:length(morb$Variable)){
  if (nchar(morb$Variable[i]) > 40){
    morb$Variable[i] = strtrim(morb$Variable[i], 40)
  } 
}
table1 = morb[,c("Country","Year","Variable","Value","population")]
### to add a new column with 0 value for each row
table1[,"comorbidity"] <- 0
### replacing comorbidity values if year is 2020
table1$comorbidity = ifelse(table1$Year==2020,1,table1$comorbidity)

#check availability of 2020 data for countries
t = table1 %>% filter_all(any_vars(. %in% c(Year = 2020)))
#t1 = country[country$Year == 2010,]

#splitting dataframe based on each country (column 1)
split_data = split(table1, f=table1$Country)
################################################################################
################################################################################
################################################################################

################################################################################
################################################################################
################################################################################
# saving each country's data under its name from the dataframe split_data
################################################################################
#Australia
Australia = split_data[["Australia"]]
Australia_pop = pop_ratio(Australia)

#two methods of normalization and standardization for
#10 years of data available
znorm_ausl = znormal(Australia_pop)
test_ausl = test_results(znorm_ausl[[2]],znorm_ausl[[1]])
std_mean_ausl = mean_sd(znorm_ausl[[2]],znorm_ausl[[1]])
write.csv(znorm_ausl[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\1.Australia.csv", row.names = TRUE)
write.csv(test_ausl,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Australia.csv", row.names = TRUE)
#min_maxnorm_ausl = min_max_normal(Australia_pop)
#export data frame to csv
#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_ausl[[1]])
#country_split_19(znorm_ausl[[1]])
#country_split_20(znorm_ausl[[1]])
################################################################################
#Austria
Austria = split_data[["Austria"]]
Austria_pop = pop_ratio(Austria)
#two methods of normalization and standardization for
#10 years of data available
znorm_aust = znormal(Austria_pop)
test_aust = test_results(znorm_aust[[2]],znorm_aust[[1]])
write.csv(znorm_aust[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\2.Austria.csv", row.names = TRUE)
write.csv(test_aust,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Austria.csv", row.names = TRUE)
min_maxnorm_aust = min_max_normal(Austria_pop)
#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_aust[[1]])
#country_split_19(znorm_aust[[1]])
#country_split_20(znorm_aust[[1]])
#for(i in 1:length(Austria$Value)){
#  Austria[i,4] = Austria[i,4]/83975
#}
#Austria$pop_ratio = (Austria[4]/83975)
################################################################################
#Czech_Republic
Czech_Republic = split_data[["Czech Republic"]]
Czech_Republic_pop = pop_ratio(Czech_Republic)

#two methods of normalization and standardization for
#10 years of data available
znorm_czech = znormal(Czech_Republic_pop)
min_maxnorm_czech = min_max_normal(Czech_Republic_pop)
test_czech = test_results(znorm_czech[[2]],znorm_czech[[1]])
#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_czech[[1]])
#country_split_19(znorm_czech[[1]])
#country_split_20(znorm_czech[[1]])
write.csv(znorm_czech[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\3.Czech_Repubic.csv", row.names = TRUE)
write.csv(test_czech,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Czech.csv", row.names = TRUE)

################################################################################
#Germany 
Germany = split_data[["Germany"]]
Germany_pop = pop_ratio(Germany)

#two methods of normalization and standardization for
#10 years of data available
znorm_ger = znormal(Germany_pop)
min_maxnorm_czech = min_max_normal(Germany_pop)
test_ger = test_results(znorm_ger[[2]],znorm_ger[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_ger[[1]])
#country_split_19(znorm_ger[[1]])
#country_split_20(znorm_ger[[1]])
write.csv(znorm_ger[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\4.Germany.csv", row.names = TRUE)
write.csv(test_ger,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Germay.csv", row.names = TRUE)

################################################################################
#Iceland
Iceland = split_data[["Iceland"]]
Iceland_pop = pop_ratio(Iceland)

#two methods of normalization and standardization for
#10 years of data available
znorm_ice = znormal(Iceland_pop)
min_maxnorm_czech = min_max_normal(Iceland_pop)
test_ice = test_results(znorm_ice[[2]],znorm_ice[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_ice[[1]])
#country_split_19(znorm_ice[[1]])
#country_split_20(znorm_ice[[1]])
write.csv(znorm_ice[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\5.Iceland.csv", row.names = TRUE)
write.csv(test_ice,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Iceland.csv", row.names = TRUE)

################################################################################
#Mexico
Mexico = split_data[["Mexico"]]
Mexico_pop = pop_ratio(Mexico)

#two methods of normalization and standardization for
#10 years of data available
znorm_mex = znormal(Mexico_pop)
min_maxnorm_czech = min_max_normal(Mexico_pop)
test_mex = test_results(znorm_mex[[2]],znorm_mex[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_mex[[1]])
#country_split_19(znorm_mex[[1]])
#country_split_20(znorm_mex[[1]])
write.csv(znorm_mex[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\6.Mexico.csv", row.names = TRUE)
write.csv(test_mex,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Mexico.csv", row.names = TRUE)

################################################################################
#Netherlands
Netherlands = split_data[["Netherlands"]]
Netherlands_pop = pop_ratio(Netherlands)

#two methods of normalization and standardization for
#10 years of data available
znorm_nether = znormal(Netherlands_pop)
min_maxnorm_czech = min_max_normal(Netherlands_pop)
test_nether = test_results(znorm_nether[[2]],znorm_nether[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_nether[[1]])
#country_split_19(znorm_nether[[1]])
#country_split_20(znorm_nether[[1]])
write.csv(znorm_nether[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\7.Netherlands.csv", row.names = TRUE)
write.csv(test_nether,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Netherlands.csv", row.names = TRUE)

################################################################################
#Spain
Spain = split_data[["Spain"]]
Spain_pop = pop_ratio(Spain)

#two methods of normalization and standardization for
#10 years of data available
znorm_spain = znormal(Spain_pop)
min_maxnorm_czech = min_max_normal(Spain_pop)
test_spain = test_results(znorm_spain[[2]],znorm_spain[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_spain[[1]])
#country_split_19(znorm_spain[[1]])
#country_split_20(znorm_spain[[1]])
write.csv(znorm_spain[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\8.Spain.csv", row.names = TRUE)
write.csv(test_spain,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Spain.csv", row.names = TRUE)

################################################################################
#USA
US = split_data[["United States"]]
US_pop = pop_ratio(US)

#two methods of normalization and standardization for
#10 years of data available
znorm_usa = znormal(US_pop)
min_maxnorm_czech = min_max_normal(US_pop)
test_usa = test_results(znorm_usa[[2]],znorm_usa[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_usa[[1]])
#country_split_19(znorm_usa[[1]])
#country_split_20(znorm_usa[[1]])
write.csv(znorm_usa[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\9.USA.csv", row.names = TRUE)
write.csv(test_usa,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_USA.csv", row.names = TRUE)

################################################################################
#Estonia
Estonia = split_data[["Estonia"]]
Estonia_pop = pop_ratio(Estonia)

#two methods of normalization and standardization for
#10 years of data available
znorm_estonia = znormal(Estonia_pop)
#min_maxnorm_czech = min_max_normal(Estonia_pop)
test_estonia = test_results(znorm_estonia[[2]],znorm_estonia[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_estonia[[1]])
#country_split_19(znorm_estonia[[1]])
#country_split_20(znorm_estonia[[1]])
write.csv(znorm_estonia[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\10.Estonia.csv", row.names = TRUE)
write.csv(test_estonia,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Estonia.csv", row.names = TRUE)

################################################################################
#Slovenia
Slovenia = split_data[["Slovenia"]]
Slovenia_pop = pop_ratio(Slovenia)

#two methods of normalization and standardization for
#10 years of data available
znorm_slov = znormal(Slovenia_pop)
min_maxnorm_czech = min_max_normal(Slovenia_pop)
test_slovenia = test_results(znorm_slov[[2]],znorm_slov[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_slov[[1]])
#country_split_19(znorm_slov[[1]])
#country_split_20(znorm_slov[[1]])
write.csv(znorm_slov[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\11.Slovenia.csv", row.names = TRUE)
write.csv(test_slovenia,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Slovenia.csv", row.names = TRUE)

################################################################################
#Latvia
Latvia = split_data[["Latvia"]]
Latvia_pop = pop_ratio(Latvia)

#two methods of normalization and standardization for
#10 years of data available
znorm_Latvia = znormal(Latvia_pop)
min_maxnorm_czech = min_max_normal(Latvia_pop)
test_Latvia = test_results(znorm_Latvia[[2]],znorm_Latvia[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_Latvia[[1]])
#country_split_19(znorm_Latvia[[1]])
#country_split_20(znorm_Latvia[[1]])
write.csv(znorm_Latvia[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\12.Latvia.csv", row.names = TRUE)
write.csv(test_Latvia,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Latvia.csv", row.names = TRUE)

################################################################################
#Costa_Rica
CostaRica = split_data[["Costa Rica"]]
CostaRica_pop = pop_ratio(CostaRica)

#two methods of normalization and standardization for
#10 years of data available
znorm_CostaRica = znormal(CostaRica_pop)
min_maxnorm_czech = min_max_normal(CostaRica_pop)
test_CostaRica = test_results(znorm_CostaRica[[2]],znorm_CostaRica[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_CostaRica[[1]])
#country_split_19(znorm_CostaRica[[1]])
#country_split_20(znorm_CostaRica[[1]])
write.csv(znorm_CostaRica[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\13.CostaRica.csv", row.names = TRUE)
write.csv(test_CostaRica,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_CostaRica.csv", row.names = TRUE)

################################################################################
#Lithuania
Lithuania = split_data[["Lithuania"]]
Lithuania_pop = pop_ratio(Lithuania)

#two methods of normalization and standardization for
#10 years of data available
znorm_Lithuania = znormal(Lithuania_pop)
min_maxnorm_czech = min_max_normal(Lithuania_pop)
test_Lithuania = test_results(znorm_Lithuania[[2]],znorm_Lithuania[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_Lithuania[[1]])
#country_split_19(znorm_Lithuania[[1]])
#country_split_20(znorm_Lithuania[[1]])
write.csv(znorm_Lithuania[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\14.Lithuania.csv", row.names = TRUE)
write.csv(test_Lithuania,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Lithuania.csv", row.names = TRUE)

################################################################################
all_countries = rbind(znorm_ausl[[1]],znorm_aust[[1]],znorm_CostaRica[[1]],
           znorm_czech[[1]],znorm_estonia[[1]],znorm_ger[[1]],
           znorm_ice[[1]],znorm_Latvia[[1]],znorm_Lithuania[[1]],
           znorm_mex[[1]],znorm_nether[[1]],znorm_slov[[1]],
           znorm_spain[[1]],znorm_usa[[1]])
#split data of all countries based on each disease
diseases = split(all_countries, f=all_countries$Variable)
#run linear regression in a loop over all rows of diseases

################################################################################
t1 = all_countries[all_countries$Variable == "Mental and behavioural disorders",]
write.csv(t1,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Mental and behavioural disorders.csv", row.names = TRUE)
################################################################################

lin_reg_all = list()
for(i in 1:length(diseases)){
  lin_reg_all[[i]] = linear_reg(diseases[[i]])
                            }

#bind each row containing 4 values to make a column comprising info for 48 diseases
linear_reg_all = do.call("rbind",lin_reg_all)

#make a table of all diseases and their correlation calculated based on different tests 
test_all_countries = test_results(diseases,all_countries)

#add columns of linear regression result data to right side of test result table
compelete = data.frame(test_all_countries,linear_reg_all)

concise_compelete = compelete[,c("Variable","pearson_test_pop","p_value_pop","p_value_pop")]
######### tests ########
########################
lm_fit=lm(diseases[[1]]$`Normalized Values`~diseases[[1]]$comorbidity,data=diseases[[1]]) 
summary(lm_fit)
p_val = summary(lm_fit)$coefficients[,4][2]  
coeff = lm_fit$coefficients[2]
#ggplot(lm_fit, aes(x = diseases[[17]]$comorbidity, y = diseases[[17]]$`Normalized Values`)) + geom_point() + stat_smooth(method = "lm", col = "red")
u = unique(morb$Variable)
write.csv(u,"C:\\Users\\mohammadia\\Desktop\\R\\unique_disease_complete name.csv", row.names = TRUE)
########################
########################
write.csv(all_countries,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\15.All_Countries.csv", row.names = TRUE)
write.csv(compelete,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_All_countries.csv", row.names = TRUE)
write.csv(concise_compelete,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_All_countries_concise.csv", row.names = TRUE)

################################################################################

