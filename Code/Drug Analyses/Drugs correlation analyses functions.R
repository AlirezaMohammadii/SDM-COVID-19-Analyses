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
                                                    st2 <-  as.data.frame(scale(disease_list[[i]][4], center = TRUE, scale = TRUE))
                                                    disease_list[[i]][,"Normalized Values"] <- (st2)
                                                    sd <- lapply(disease_list[[i]][4], sd, na.rm = TRUE)
                                                    disease_list[[i]][,"standard daviation"] <- (sd)
                                                  }
  ################################################################################
                  disease_list[[14]] <- NULL
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
                                p_value = list()
                                
                                drug_unique = unique(norm_table[3])
                                for(i in 1:length(norm_val)){
                                                              #pearson correlation 
                                                              res <- cor.test(norm_val[[i]][[6]], norm_val[[i]]$comorbidity, 
                                                                                  method = "pearson",exact=FALSE)
                                                              pearson_test[i] <- res[4]
                                                              p_value[i] <- res[3]

                                                            }
                                
                                pearson_test = as.numeric(unlist(pearson_test))
                                p_value = as.numeric(unlist(p_value))
                                norm_val[[i]][[6]][is.nan(norm_val[[i]][[6]])] <- 0
                                
                                #gather all the columns together as a dataframe to construct a table
                                pearson_test = data.frame(drug_unique,pearson_test,p_value)
                                
                                g2 = ggplot(data = pearson_test, aes(x = reorder(unlist(drug_unique), -(pearson_test)), y = (pearson_test))) + 
                                  labs(y="Pearson's test correlation pop_drug consumed ratio", x="Drug") + geom_point()  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                
                                #kendall_rank_plot = data.frame(disease_unique,kendall_rank)
                                #g2 = ggplot(data = kendall_rank_plot, aes(x = reorder(unlist(disease_unique), -(kendall_rank)), y = (kendall_rank))) + 
                                #labs(y="Kendall's test correlation", x="Disease") + geom_point()  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                
                                #spearman_rank_plot = data.frame(disease_unique,spearman_rank)
                                #g3 = ggplot(data = spearman_rank_plot, aes(x = reorder(unlist(disease_unique), -(spearman_rank)), y = (spearman_rank))) + 
                                labs(y="Spearman's test correlation", x="Disease") + geom_point()  + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                                
                                print(g2)
                                return(pearson_test)
                              }


mean_sd <- function(norm_col){
                              mean_val = list()
                              stand_dev = list()
                              for(i in 1:length(norm_col)){ 
                                mean_val[i] = mean(norm_col[[i]][[7]])
                                stand_dev[i] = sd(norm_col[[i]][[7]])
                              }
                              mean_value = as.numeric(unlist(mean_val))
                              stand_dev = as.numeric(unlist(stand_dev))
                              mean_sd_disease = data.frame(mean_value,stand_dev)
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
    
                            lm_fit = list()
                            
                            p_val = list()
                            
                            coeff = list()
                            
                            lm_fit=lm(lin_country$`Normalized Values`~lin_country$comorbidity,data=lin_country) 
                            p_val = summary(lm_fit)$coefficients[,4][2]  
                            coeff = lm_fit$coefficients[2]
                            p_value = as.numeric(unlist(p_val))
                            coefficient = as.numeric(unlist(coeff))
                            
                            r_sq_val = data.frame(coefficient,p_value)
                            return(r_sq_val)
                          }
plot_bar_comparison <- function(country_name, country_title) {
  
                            # Extract necessary data
                            country_data <- country_name[!(country_name$Variable %in% disease_omitted), c(1:4)]
                            population_country <- country_name[country_name$Variable == "All causes of death",c(2,5)]
                            merged_table <- merge(country_data, population_country, by = "Year")
                            
                            # Calculate normalized population and aggregate by year
                            merged_table$Normalized_pop <- merged_table$Value / merged_table$population
                            sum_by_year <- aggregate(Value ~ Year, merged_table, sum)
                            sum_by_year_norm <- aggregate(Normalized_pop ~ Year, merged_table, sum)
                            merged_table <- merge(merged_table, sum_by_year, by = "Year")
                            merged_table <- merge(merged_table, sum_by_year_norm, by = "Year")
                            merged_table2 <- merged_table[,c(1,3,6)]
                            df_wide <- dcast(merged_table2, Variable ~ Year, value.var = "Normalized_pop.x")
                            df_wide <- df_wide[-c(7),]
                            df_wide_names <- c(df_wide[,1])
                            
                            # Generate disease code names
                            Disease_code <- c()
                            for (i in 1:nrow(df_wide)) {
                              name <- paste0("D", i)
                              Disease_code <- c(Disease_code, name)
                            }
                            df_wide <- cbind(df_wide[,1],Disease_code,df_wide[,2:12])
                            
                            # Generate plot
                            df_wide_names_trans <- df_wide[,2]
                            df_wide_concise <- df_wide[,-c(1,2)]
                            set.seed(413)
                            colours_plot = colors()[c(sample(70:140, 50, replace =  FALSE))]
                            bar_plot = barplot(as.matrix(df_wide_concise, nrow=31, ncol = 11), 
                                               col=colours_plot,
                                               border="white", 
                                               space=0.5,
                                               font.axis=4,
                                               xlab="Years",
                                               ylab="Normalized death cases over population",
                                               main= country_title,
                                               legend.text=df_wide_names_trans,
                                               args.legend=list(title="Diseases", cex=0.5, bg="white", x="right", inset=c(-.03,0.65)))
                            
                          }

###for plotting the data of each country based on peaeson correlation and 
plot_country_data <- function(country) {
  
                      # Extracting data for the given country
                      category1_death = cbind(United_states_death = alpha[,16],
                                              Mexico_death = alpha[,12],
                                              Spain_death = alpha[,15],
                                              Germany_death = alpha[,8],
                                              Netherlands_death = alpha[,13])
                      
                      category2_death = cbind(Czech_Republic_death = alpha[,6],
                                              Austria_death = alpha[,4],
                                              Slovenia_death = alpha[,14],
                                              CostaRica_death = alpha[,5],
                                              Lithiania_death = alpha[,11])
                      
                      category3_death = cbind(Australia_death = alpha[,3],
                                              Latvia_death = alpha[,10],
                                              Estonia_death = alpha[,7],
                                              Iceland_death = alpha[,9])
                      
                      death_data = switch(country,
                                          "United States" = category1_death[,1],
                                          "Mexico" = category1_death[,2],
                                          "Spain" = category1_death[,3],
                                          "Germany" = category1_death[,4],
                                          "Netherlands" = category1_death[,5],
                                          "Czech Republic" = category2_death[,1],
                                          "Austria" = category2_death[,2],
                                          "Slovenia" = category2_death[,3],
                                          "Costa Rica" = category2_death[,4],
                                          "Lithuania" = category2_death[,5],
                                          "Australia" = category3_death[,1],
                                          "Latvia" = category3_death[,2],
                                          "Estonia" = category3_death[,3],
                                          "Iceland" = category3_death[,4],
                                          stop("Invalid country name entered. Please check and try again."))
                      
                      # Extracting disease names and pearson correlations
                      disease_name = alpha[,1]
                      pearson_correlation = alpha[,2]
                      
                      # Creating the plot
                      #                                    p = ggplot(alpha, aes_string(reorder(x = disease_name, - death_data), y = death_data, color = pearson_correlation)) +
                      #                                      geom_point(size = 3) +
                      #                                      geom_text(aes(label = paste0(round(pearson_correlation, 2), ",", round(death_data))), 
                      #                                                angle = 90,hjust = 1.2, vjust = 0.5, size = 3) +
                      #                                      scale_color_gradient2(low = "darkgreen", mid = "white", high = "darkblue") +
                      #                                      xlab("Death caused by") +
                      #                                      ylab("Death Cases count") +
                      #                                      ggtitle(paste0("Correlation Between Disease and Death Cases in ", country)) +
                      #                                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
                      
                      # Creating the plot in bars
                      p = ggplot(alpha, aes_string(reorder(x = disease_name, - pearson_correlation), y = death_data, fill = pearson_correlation)) +
                      geom_bar(stat = "identity", width = 0.5) +
                      geom_text(aes(label = paste0(round(pearson_correlation, 2), ",", round(death_data))), 
                                angle = 90, hjust = 1.2, vjust = 0.5, size = 3) +
                      scale_fill_gradient2(low = "darkgreen", mid = "white", high = "darkblue") +
                      xlab("Death caused by") +
                      ylab("Death Cases count") +
                      ggtitle(paste0("Correlation Between Disease and Death Cases in ", country)) +
                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
                      return(p)
                    }

get_sorted_data <- function(country_name) {
                                            country_data <- country_name[!(country_name$Variable %in% disease_omitted), c(1:4)]
                                            population_country <- country_name[country_name$Variable == "All causes of death", c(2, 5)]
                                            merged_table <- merge(country_data, population_country, by = "Year")
                                            
                                            # Calculate normalized population and aggregate by year
                                            merged_table$Normalized_pop <- merged_table$Value / merged_table$population
                                            sum_by_year <- aggregate(Value ~ Year, merged_table, sum)
                                            sum_by_year_norm <- aggregate(Normalized_pop ~ Year, merged_table, sum)
                                            merged_table <- merge(merged_table, sum_by_year, by = "Year")
                                            merged_table <- merge(merged_table, sum_by_year_norm, by = "Year")
                                            merged_table2 <- merged_table[, c(1, 3, 4)]
                                            df_wide <- dcast(merged_table2, Variable ~ Year, value.var = "Value.x")
                                            df_wide <- df_wide[-c(7), ]
                                            df_wide_names <- c(df_wide[, 1])
                                            
                                            # Calculate the mean value of each column, starting from the second column
                                            mean_values <- colMeans(df_wide[, 2:ncol(df_wide)], na.rm = TRUE)
                                            
                                            # Iterate over columns and replace empty cells with mean values
                                            for (col_index in 2:ncol(df_wide)) {
                                              empty_cells <- is.na(df_wide[, col_index])
                                              df_wide[empty_cells, col_index] <- mean_values[col_index - 1]
                                            }
                                            df_wide$row_means <- rowMeans(df_wide[, c(2:ncol(df_wide))])
                                            df_wide[, 2:ncol(df_wide)] <- round(df_wide[, 2:ncol(df_wide)],0)
                                            
                                            # Generate disease code names
                                            Disease_code <- c()
                                            for (i in 1:nrow(df_wide)) {
                                              name <- paste0("D", i)
                                              Disease_code <- c(Disease_code, name)
                                            }
                                            
                                            df_wide <- cbind(df_wide[, 1], Disease_code, df_wide[, c(2:ncol(df_wide))])
                                            df_wide <- df_wide[, c(1, 2,13, 14)]
                                            sorted_data <- df_wide[order(-df_wide$row_means), ]
                                            
                                            return(df_wide)
}


plot_pearson_correlation <- function(drug2) {
                                            ggplot(drug2, aes(reorder(drug_names_concise, -avg_cor), avg_cor, fill = avg_cor)) +
                                              geom_bar(stat = "identity") +
                                              scale_fill_gradient(low = "green", high = "magenta") +
                                              xlab("Drugs") +
                                              ylab("Pearson Correlation Coefficient") +
                                              ggtitle("Pearson Correlation Coefficient of 28 Drugs") +
                                              theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

plot_country_drug_consumption <- function(country) {
                                                    # Define the drug data categories
                                                    category1 <- c("Spain", "Germany", "Czech_Republic")
                                                    category2 <- c("Slovenia", "Israel", "CostaRica", "Lithuania", "Korea", "Australia", "Latvia", "Estonia")
                                                    category3 <- c("Belgium", "Canada", "France", "Denmark", "Finland", "Greece", "Hungary", "Iceland", "Italy", "Luxembourg", "Norway", "Portugal", "Sweden", "Slovak_Republic")
                                                    
                                                    
                                                    # Check the category of the given country
                                                    if (country %in% category1) {
                                                      category <- 1
                                                    } else if (country %in% category2) {
                                                      category <- 2
                                                    } else if (country %in% category3) {
                                                      category <- 3
                                                    } else {
                                                      stop("Country not found in any category.")
                                                    }
                                                    
                                                    # Retrieve the corresponding drug data column based on the category and country
                                                    if (category == 1) {
                                                      drug_column <- switch(country,
                                                                            "Spain" = drug2[,24],
                                                                            "Germany" = drug2[,14],
                                                                            "Czech_Republic" = drug2[,10]
                                                      )
                                                    } else if (category == 2) {
                                                      drug_column <- switch(country,
                                                                            "Slovenia" = drug2[,21],
                                                                            "Israel" = drug2[,28],
                                                                            "CostaRica" = drug2[,9],
                                                                            "Lithuania" = drug2[,19],
                                                                            "Korea" = drug2[,22],
                                                                            "Australia" = drug2[,5],
                                                                            "Latvia" = drug2[,18],
                                                                            "Estonia" = drug2[,13]
                                                      )
                                                    } else if (category == 3) {
                                                      drug_column <- switch(country,
                                                                            "Belgium" = drug2[,6],
                                                                            "Canada" = drug2[,7],
                                                                            "France" = drug2[,8],
                                                                            "Denmark" = drug2[,11],
                                                                            "Finland" = drug2[,12],
                                                                            "Greece" = drug2[,15],
                                                                            "Hungary" = drug2[,16],
                                                                            "Iceland" = drug2[,17],
                                                                            "Italy" = drug2[,20],
                                                                            "Luxembourg" = drug2[,23],
                                                                            "Norway" = drug2[,25],
                                                                            "Portugal" = drug2[,26],
                                                                            "Sweden" = drug2[,27],
                                                                            "Slovak_Republic" = drug2[,29]
                                                      )
                                                    } else {
                                                      stop("Invalid category.")
                                                    }
                                                    
                                                    # Plot the bar plot
                                                    ggplot(drug2, aes_string(reorder(x = drug_code_concise, - avg_cor), y = drug_column, fill = avg_cor)) +
                                                      geom_bar(stat = "identity") +
                                                      geom_text(aes(label = paste0(round(avg_cor, 2), ",", drug_column)), 
                                                                angle = 90, hjust = 1.2, vjust = 0.5, size = 3) +
                                                      scale_fill_gradient(low = "red", high = "blue") +
                                                      xlab("Death caused by") +
                                                      ylab("Death Cases count") +
                                                      ggtitle(paste0("Correlation Between Disease and Death Cases in ", country)) +
                                                      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}


get_sorted_data <- function(country_name) {
                                            # Replace "country_name" with the input country name in the following code
                                            
                                            # Calculate normalized population and aggregate by year
                                            sum_by_year <- aggregate(Value ~ Year, country_name, sum)
                                            merged_table <- merge(country_name, sum_by_year, by = "Year")
                                            merged_table2 <- merged_table[, c(1, 3, 4)]
                                            df_wide <- dcast(merged_table2, Variable ~ Year, value.var = "Value.x")
                                            df_wide_names <- data.frame(df_wide[, 1])
                                            
                                            # Filter data that has p_value beyond 5 percent
                                            df_wide <- subset(df_wide, Variable %in% as.character(drug2$`drug2[, 1]`))
                                            df_wide_2020 <- data.frame(df_wide$Variable,df_wide$`2020`)
                                            if ("2021" %in% colnames(df_wide)) {
                                              # Remove the column "2021"
                                              df_wide <- df_wide[, colnames(df_wide) != "2021"]
                                            }
                                            if ("2020" %in% colnames(df_wide)) {
                                              # Remove the column "2020"
                                              df_wide <- df_wide[, colnames(df_wide) != "2020"]
                                            }
                                            mean_values <- colMeans(df_wide[, 2:ncol(df_wide)], na.rm = TRUE)
                                            
                                            # Iterate over columns and replace empty cells with mean values
                                            for (col_index in 2:ncol(df_wide)) {
                                              empty_cells <- is.na(df_wide[, col_index])
                                              df_wide[empty_cells, col_index] <- mean_values[col_index - 1]
                                            }
                                            
                                            df_wide$row_means <- rowMeans(df_wide[, c(2:ncol(df_wide))])
                                            df_wide[, 2:ncol(df_wide)] <- round(df_wide[, 2:ncol(df_wide)], 2)
                                          
                                            
                                            # Generate disease code names
                                            Disease_code <- c()
                                            for (i in 1:nrow(df_wide)) {
                                              name <- paste0("D", i)
                                              Disease_code <- c(Disease_code, name)
                                            }
                                            
                                            df_wide <- cbind(df_wide[, 1], Disease_code, df_wide[, 2:ncol(df_wide)])
                                            df_wide <- df_wide[, c(1, 2, ncol(df_wide))]
                                            sorted_data <- df_wide[order(-df_wide$row_means), ]
                                            
                                            return(list(data.frame(df_wide),df_wide_2020))
}


plot_stacked_bar <- function(country) {
                                      # Calculate normalized population and aggregate by year
                                      sum_by_year <- aggregate(Value ~ Year, get(country), sum)
                                      merged_table <- merge(get(country), sum_by_year, by = "Year")
                                      merged_table2 <- merged_table[, c(1, 3, 4)]
                                      df_wide <- reshape2::dcast(merged_table2, Variable ~ Year, value.var = "Value.x")
                                      df_wide_names <- data.frame(df_wide[, 1])
                                      
                                      # Filter data that has p_value beyond 5 percent
                                      df_wide <- subset(df_wide, Variable %in% as.character(drug2$`drug2[, 1]`))
                                      
                                      if ("2021" %in% colnames(df_wide)) {
                                        # Remove the column "2021"
                                        df_wide <- df_wide[, colnames(df_wide) != "2021"]
                                      }
                                      
                                      mean_values <- colMeans(df_wide[, 2:ncol(df_wide)], na.rm = TRUE)
                                      
                                      # Iterate over columns and replace empty cells with mean values
                                      for (col_index in 2:ncol(df_wide)) {
                                        empty_cells <- is.na(df_wide[, col_index])
                                        df_wide[empty_cells, col_index] <- mean_values[col_index - 1]
                                      }
                                      
                                      # Generate disease code names
                                      Disease_code <- paste0("D", seq_len(nrow(df_wide)))
                                      df_wide <- cbind(df_wide[, 1], Disease_code, df_wide[, 2:ncol(df_wide)])
                                      
                                      # Generate plot
                                      df_wide_names_trans <- df_wide[, 2]
                                      df_wide_concise <- df_wide[, -c(1, 2)]
                                      set.seed(413)
                                      colours_plot <- colors()[sample(70:140, 50, replace = FALSE)]
                                      barplot(as.matrix(df_wide_concise, nrow = nrow(df_wide_concise), ncol = ncol(df_wide_concise)), 
                                              col = colours_plot,
                                              border = "white", 
                                              space = 0.5,
                                              font.axis = 4,
                                              xlab = "Years",
                                              ylab = "Drug consumption over 100000 agent of the population",
                                              main = paste0(country),
                                              legend.text = df_wide_names_trans,
                                              args.legend = list(title = "Diseases", cex = 0.5, bg = "white", x = "right", inset = c(-0.03, 0.65)))
                                      }




################################################################################
################################################################################

