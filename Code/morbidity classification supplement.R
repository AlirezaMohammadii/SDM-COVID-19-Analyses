#to check if a function already exists
exists("country_split_18")
#rm(list=ls())

country_split_18 <- function(country_name){
                                            #name = split_data[[country_name]]
                                            name_val20 = unlist(c(country_name[country_name$Year == "2018", "Normalized Values"]))
                                            name_Var20 = c(country_name[country_name$Year == "2018", "Variable"])
                                            name2 = data.frame(name_Var20,name_val20)
                                            print(ggplot(data = name2, aes(x = reorder(name_Var20, -log(name_val20)), y = log(name_val20))) + labs(y="2018 \n Death", x="Disease") +
                                                    geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
                                            return(name2)
                                          }


country_split_19 <- function(country_name){
                                            #name = split_data[[country_name]]
                                            name_val20 = unlist(c(country_name[country_name$Year == "2019", "Normalized Values"]))
                                            name_Var20 = c(country_name[country_name$Year == "2019", "Variable"])
                                            name2 = data.frame(name_Var20,name_val20)
                                            print(ggplot(data = name2, aes(x = reorder(name_Var20, -log(name_val20)), y = log(name_val20))) + labs(y="2019 \n Death", x="Disease") +
                                                    geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
                                            return(name2)
                                          }


country_split_20 <- function(country_name){
                                            #name = split_data[[country_name]]
                                            name_val20 = unlist(c(country_name[country_name$Year == "2020", "Normalized Values"]))
                                            name_Var20 = c(country_name[country_name$Year == "2020", "Variable"])
                                            name2 = data.frame(name_Var20,name_val20)
                                            print(ggplot(data = name2, aes(x = reorder(name_Var20, -log(name_val20)), y = log(name_val20))) + labs(y="2020 \n Death", x="Disease") +
                                                   geom_col() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))
                                            return(name2)
                                          }
  

#############################################################################################
#############################################################################################

#Normalization 1: z-score calculation
znormal <- function(name) {
                          #name = split_data[[name]]
                          disease_list = split(name,name$Variable)
                          
                          for(i in 1:length(disease_list)){
                              #apply the normalization
                              st <-  as.data.frame(scale(disease_list[[i]][4], center = TRUE, scale = TRUE))
                              #Add the normalized column to dataframes 
                              disease_list[[i]][,"Normalized Values"] <- (st)
                              st2 <-  as.data.frame(scale(disease_list[[i]][7], center = TRUE, scale = TRUE))
                              disease_list[[i]][,"Normalized Values pop"] <- (st2)
                              }
################################################################################
                          #it is used if I want to filter data for only 5 years
                          #for (i in 1:length(disease_list)) {
                          #  disease_list[[i]] <- disease_list[[i]][disease_list[[i]]$Year %in% 2015:2020,]
                          #}
                          #replace nan with 0
                          disease_list[[14]] <- NULL
                          #disease_list[[14]][[6]][is.nan(disease_list[[14]][[6]])] <- 0
                          #unsplit the data
                          country_norm <- do.call("rbind",disease_list)
                          #country_norm <- country_norm[country_norm$Year %in% 2018:2020,]
                          return(list(country_norm,disease_list))
                          }


#Normalization 2: Min-Max
norm_minmax <- function(x){
  (x- min(x)) /(max(x)-min(x))
}

min_max_normal <- function(name) {
                                  #split data based on diseases
                                  disease_list = split(name,name$Variable)
                                  
                                  for(i in 1:length(disease_list)){
                                      #apply the normalization
                                      min_max <-  as.data.frame(lapply(disease_list[[i]][4], norm_minmax))
                                      #Add the normalized column to dataframes 
                                      disease_list[[i]][,"Normalized Values"] <- (min_max)
                                      
                                      min_max2 <-  as.data.frame(lapply(disease_list[[i]][7], norm_minmax))
                                      disease_list[[i]][,"Normalized Values pop"] <- (min_max2)
                                  }
                                  #it is used if I want to filter data for only 5 years
                                  for (i in 1:length(disease_list)) {
                                    disease_list[[i]] <- disease_list[[i]][disease_list[[i]]$Year %in% 2015:2020,]
                                  }
                                  #replace nan with 0
                                  disease_list[[14]] <- NULL
                                  #disease_list[[14]][[6]][is.nan(disease_list[[14]][[6]])] <- 0
                                  #unsplit the data
                                  country_min_max <- do.call("rbind",disease_list)
                                  return(list(country_min_max,disease_list))
                                  }

################################################################################
################################################################################

#Preleminary test to check the test assumptions
#shapiro.test(znorm[[2]][[3]][[6]])

#Pearson correlation test

test_results = function(norm_val,norm_table) {
                                    pearson_test = list()
                                    pearson_test_pop = list()
                                    p_value = list()
                                    p_value_pop = list()
                                    
                                    kendall_rank = list()
                                    kendall_rank_pop = list()
                                    p_value_ken = list()
                                    p_value_kenPop = list()
                                    
                                    spearman_rank = list()
                                    spearman_rank_pop = list()
                                    p_value_spear = list()
                                    p_value_spearPop = list()
                                    
                                    disease_unique = unique(norm_table[3])
                                    for(i in 1:length(norm_val)){
                                                                 #pearson correlation 
                                                                 res <- cor.test(norm_val[[i]][[8]], norm_val[[i]][[6]], 
                                                                 method = "pearson",exact=FALSE)
                                                                 pearson_test[i] <- res[4]
                                                                 p_value[i] <- res[3]
                                                                 
                                                                 res_pop <- cor.test(norm_val[[i]][[9]], norm_val[[i]][[6]], 
                                                                 method = "pearson",exact=FALSE)
                                                                 pearson_test_pop[i] <- res_pop[4]
                                                                 p_value_pop[i] <- res_pop[3]
                                                                 
                                                                 #kendall_rank
                                                                 res2 <- cor.test(norm_val[[i]][[8]], norm_val[[i]][[6]],  
                                                                 method="kendall",exact=FALSE)
                                                                 kendall_rank[i] <- res2[4]
                                                                 p_value_ken[i] <- res2[3]
                                                                 
                                                                 res2_pop <- cor.test(norm_val[[i]][[9]], norm_val[[i]][[6]],  
                                                                 method="kendall",exact=FALSE)
                                                                 kendall_rank_pop[i] <- res2_pop[4]
                                                                 p_value_kenPop[i] <- res2_pop[3]
                                                                 
                                                                 #spearman rank
                                                                 res3 <-cor.test(norm_val[[i]][[8]], norm_val[[i]][[6]],  
                                                                 method = "spearman",exact=FALSE)
                                                                 spearman_rank[i] <- res3[4]
                                                                 p_value_spear[i] <- res3[3]
                                                                 
                                                                 res3_pop <-cor.test(norm_val[[i]][[9]], norm_val[[i]][[6]],  
                                                                 method = "spearman",exact=FALSE)
                                                                 spearman_rank_pop[i] <- res3_pop[4]
                                                                 p_value_spearPop[i] <- res3_pop[3]
                                                                }
                                    pearson_test = as.numeric(unlist(pearson_test))
                                    pearson_test_pop = as.numeric(unlist(pearson_test_pop))
                                    
                                    p_value = as.numeric(unlist(p_value))
                                    p_value_pop = as.numeric(unlist(p_value_pop))
                                    
                                    kendall_rank = as.numeric(unlist(kendall_rank))
                                    kendall_rank_pop = as.numeric(unlist(kendall_rank_pop))
                                    
                                    p_value_ken = as.numeric(unlist(p_value_ken))
                                    p_value_kenPop = as.numeric(unlist(p_value_kenPop))
                                    
                                    spearman_rank = as.numeric(unlist(spearman_rank))
                                    spearman_rank_pop = as.numeric(unlist(spearman_rank_pop))
                                    
                                    p_value_spear = as.numeric(unlist(p_value_spear))
                                    p_value_spearPop = as.numeric(unlist(p_value_spearPop))
                                    
                                    #gather all the columns together as a dataframe to construct a table
                                    pearson_test_plot = data.frame(disease_unique,pearson_test,p_value,pearson_test_pop,
                                                                   p_value_pop,kendall_rank,p_value_ken,kendall_rank_pop,
                                                                   p_value_kenPop,spearman_rank,p_value_spear,spearman_rank_pop,p_value_spearPop)
                                    
                                    g1 = ggplot(data = pearson_test_plot, aes(x = reorder(unlist(disease_unique), -(pearson_test)), y = (pearson_test))) + 
                                            labs(y="Pearson's test correlation", x="Disease") + geom_point()  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                    
                                    g2 = ggplot(data = pearson_test_plot, aes(x = reorder(unlist(disease_unique), -(pearson_test_pop)), y = (pearson_test_pop))) + 
                                      labs(y="Pearson's test correlation pop_death ratio", x="Disease") + geom_point()  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                    
                                    #kendall_rank_plot = data.frame(disease_unique,kendall_rank)
                                    #g2 = ggplot(data = kendall_rank_plot, aes(x = reorder(unlist(disease_unique), -(kendall_rank)), y = (kendall_rank))) + 
                                            #labs(y="Kendall's test correlation", x="Disease") + geom_point()  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                    
                                    spearman_rank_plot = data.frame(disease_unique,spearman_rank)
                                    g3 = ggplot(data = spearman_rank_plot, aes(x = reorder(unlist(disease_unique), -(spearman_rank)), y = (spearman_rank))) + 
                                            labs(y="Spearman's test correlation", x="Disease") + geom_point()  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                    
                                  print(grid.arrange(g1, g2, ncol = 2))
                                  return(pearson_test_plot)
                                  }


mean_sd <- function(norm_col,norm_table){
                              disease_unique = unique(norm_table[3])
                              mean_val = list()
                              stand_dev = list()
                              for(i in 1:length(norm_col)){ 
                                                          mean_val[i] = mean(norm_col[[i]][[4]])
                                                          stand_dev[i] = sd(norm_col[[i]][[4]])
                                                          }
                              mean_val2 = as.numeric(unlist(mean_val))
                              stand_dev2 = as.numeric(unlist(stand_dev))
                              mean_sd_disease = data.frame(disease_unique,mean_val2,stand_dev2)
                              return(mean_sd_disease)
                              }


#View(znorm[znorm$Year %in% 2018:2020,])
################################################################################
################################################################################

pop_ratio <- function(country) {
                                  #assigning 0 to a new column with prefered name
                                  country[,"value_population_ratio"] <- 0
                                  #check availability of 2020 data for countries
                                  
                                  t1 = country[country$Year == 2010,]
                                  t2 = country[country$Year == 2011,]
                                  t3 = country[country$Year == 2012,]
                                  t4 = country[country$Year == 2013,]
                                  t5 = country[country$Year == 2014,]
                                  t6 = country[country$Year == 2015,]
                                  t7 = country[country$Year == 2016,]
                                  t8 = country[country$Year == 2017,]
                                  t9 = country[country$Year == 2018,]
                                  t10 = country[country$Year == 2019,]
                                  t11 = country[country$Year == 2020,]
                                  t = list(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11)
                                  
                                  # to loop over all elements of t list and calculate death ratio and add it to new column
                                  for (j in 1:length(t)){
                                    for (i in 1:dim(t[[j]])[1]){
                                      m = as.numeric(unlist((t[[j]][t[[j]]$Variable == 'All causes of death',][5])))
                                      t[[j]]$`value_population_ratio`[i] <- t[[j]]$Value[i]/m
                                    }
                                  }
                                  t_country <- do.call("rbind",t)
                                  return(t_country)
                                }

linear_reg = function(lin_country){
                                   lm_fit_pop = list()
                                   lm_fit = list()
                                   
                                   p_val = list()
                                   p_val_pop = list()
                                   
                                   coeff = list()
                                   coeff_pop = list()
                                   
                                   lm_fit_pop=lm(lin_country$`Normalized Values pop`~lin_country$comorbidity,data=lin_country) 
                                   p_val_pop = summary(lm_fit_pop)$coefficients[,4][2]  
                                   coeff_pop = lm_fit_pop$coefficients[2]
                                   p_value_pop = as.numeric(unlist(p_val_pop))
                                   coefficient_pop = as.numeric(unlist(coeff_pop))
                                   
                                   lm_fit=lm(lin_country$`Normalized Values`~lin_country$comorbidity,data=lin_country) 
                                   p_val = summary(lm_fit)$coefficients[,4][2]  
                                   coeff = lm_fit$coefficients[2]
                                   p_value = as.numeric(unlist(p_val))
                                   coefficientz = as.numeric(unlist(coeff))

                                   r_sq_P_val = data.frame(coefficientz,p_value,coefficient_pop,p_value_pop)
                                   return(r_sq_P_val)
                                   }
                                 
################################################################################
################################################################################
#to test whether the functions work properly
#disease_list[[14]] <- NULL
res <- cor.test(znorm_ausl[[2]][[2]][[9]], znorm_ausl[[2]][[2]][[6]], 
                method = "spearman",exact=FALSE)
pearson_test <- res[3]

t1 = CostaRica[CostaRica$Year == 2010,]
t2 = CostaRica[CostaRica$Year == 2011,]
t = list(t1,t2)
m = t[[1]][t[[1]]$Variable == 'All causes of death',][5]

