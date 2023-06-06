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
if (!require('reshape2')) install.packages('reshape2'); library('reshape2')
if (!require('viridis')) install.packages('viridis'); library('viridis')
if (!require('corrplot')) install.packages('corrplot'); library('corrplot')
#devtools::install_github("kassambara/ggpubr", force = TRUE)
require(ggplot2)
dev.off()

setwd("C:/Users/mohammadia/Desktop/R")

drug = read.csv("drug_consumption.csv")

# this goes through all values in "Variable" column and cut texts with more than 20 chars
for(i in 1:length(drug$Variable)){
  if (nchar(drug$Variable[i]) > 40){
    drug$Variable[i] = strtrim(drug$Variable[i], 40)
  } 
}
table1 = drug[,c("Country","Year","Variable","Value")]
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
#Australia_pop = pop_ratio(Australia)

#two methods of normalization and standardization for
#10 years of data available
znorm_ausl = znormal(Australia)
test_ausl = test_results(znorm_ausl[[2]],znorm_ausl[[1]])
#std_mean_ausl = mean_sd(znorm_ausl[[2]],znorm_ausl[[1]])

################################################################################
#Austria
Austria = split_data[["Austria"]]
#Austria_pop = pop_ratio(Austria)
#two methods of normalization and standardization for
#10 years of data available
znorm_aust = znormal(Austria)
test_aust = test_results(znorm_aust[[2]],znorm_aust[[1]])
#write.csv(znorm_aust[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\2.Austria.csv", row.names = TRUE)
#write.csv(test_aust,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Austria.csv", row.names = TRUE)
################################################################################
################################################################################
#countries which are not included in drug table

Belgium = split_data[["Belgium"]]
znorm_Belgium = znormal(Belgium)
test_Belgium = test_results(znorm_Belgium[[2]],znorm_Belgium[[1]])

Canada = split_data[["Canada"]]
znorm_Canada = znormal(Canada)
test_Canada = test_results(znorm_Canada[[2]],znorm_Canada[[1]])

Denmark = split_data[["Denmark"]]
znorm_Denmark = znormal(Denmark)
test_Denmark = test_results(znorm_Denmark[[2]],znorm_Denmark[[1]])

Finland = split_data[["Finland"]]
znorm_Finland = znormal(Finland)
test_Finland = test_results(znorm_Finland[[2]],znorm_Finland[[1]])


France = split_data[["France"]]
znorm_France = znormal(France)
test_France = test_results(znorm_France[[2]],znorm_France[[1]])

Greece = split_data[["Greece"]]
znorm_Greece = znormal(Greece)
test_Greece = test_results(znorm_Greece[[2]],znorm_Greece[[1]])


Hungary = split_data[["Hungary"]]
znorm_Hungary = znormal(Hungary)
test_Hungary = test_results(znorm_Hungary[[2]],znorm_Hungary[[1]])


Italy = split_data[["Italy"]]
znorm_Italy = znormal(Italy)
test_Italy = test_results(znorm_Italy[[2]],znorm_Italy[[1]])


Korea = split_data[["Korea"]]
znorm_Korea = znormal(Korea)
test_Korea = test_results(znorm_Korea[[2]],znorm_Korea[[1]])


Luxembourg = split_data[["Luxembourg"]]
znorm_Luxembourg = znormal(Luxembourg)
test_Luxembourg = test_results(znorm_Luxembourg[[2]],znorm_Luxembourg[[1]])


Norway = split_data[["Norway"]]
znorm_Norway = znormal(Norway)
test_Norway = test_results(znorm_Norway[[2]],znorm_Norway[[1]])


Portugal = split_data[["Portugal"]]
znorm_Portugal = znormal(Portugal)
test_Portugal = test_results(znorm_Portugal[[2]],znorm_Portugal[[1]])


Sweden = split_data[["Sweden"]]
znorm_Sweden = znormal(Sweden)
test_Sweden = test_results(znorm_Sweden[[2]],znorm_Sweden[[1]])

#not enough finite observations
Türkiye = split_data[["Türkiye"]]
znorm_Türkiye = znormal(Türkiye)
test_Türkiye = test_results(znorm_Türkiye[[2]],znorm_Türkiye[[1]])

#not enough finite observations
Chile = split_data[["Chile"]]
znorm_Chile = znormal(Chile)
test_Chile = test_results(znorm_Chile[[2]],znorm_Chile[[1]])


Slovak_Republic = split_data[["Slovak Republic"]]
znorm_Slovak_Republic = znormal(Slovak_Republic)
test_Slovak_Republic = test_results(Slovak_Republic[[2]],Slovak_Republic[[1]])


Israel = split_data[["Israel"]]
znorm_Israel = znormal(Israel)
test_Israel = test_results(znorm_Israel[[2]],znorm_Israel[[1]])

#not enough finite observations
Ireland = split_data[["Ireland"]]
znorm_Ireland = znormal(Ireland)
test_Ireland = test_results(znorm_Ireland[[2]],znorm_Ireland[[1]])

#not enough finite observations
Poland = split_data[["Poland"]]
znorm_Poland = znormal(Poland)
test_Poland = test_results(znorm_Poland[[2]],znorm_Poland[[1]])

#not enough finite observations
Japan = split_data[["Japan"]]
znorm_Japan = znormal(Japan)
test_Japan = test_results(znorm_Japan[[2]],znorm_Japan[[1]])


Lithuania = split_data[["Lithuania"]]
znorm_Lithuania = znormal(Lithuania)
test_Lithuania = test_results(znorm_Lithuania[[2]],znorm_Lithuania[[1]])

Latvia = split_data[["Latvia"]]
znorm_Latvia = znormal(Latvia)
test_Latvia = test_results(znorm_Latvia[[2]],znorm_Latvia[[1]])

#not enough finite observations
Croatia = split_data[["Croatia"]]
znorm_Croatia = znormal(Croatia)
test_Croatia = test_results(znorm_Croatia[[2]],znorm_Croatia[[1]])


#not enough finite observations
Bulgaria = split_data[["Bulgaria"]]
znorm_Bulgaria = znormal(Bulgaria)
test_Bulgaria = test_results(znorm_Bulgaria[[2]],znorm_Bulgaria[[1]])

#not enough finite observations
Romania = split_data[["Romania"]]
znorm_Romania = znormal(Romania)
test_Romania = test_results(znorm_Romania[[2]],znorm_Romania[[1]])



################################################################################
#Czech_Republic
Czech_Republic = split_data[["Czech Republic"]]
#Czech_Republic_pop = pop_ratio(Czech_Republic)

#two methods of normalization and standardization for
#10 years of data available
znorm_czech = znormal(Czech_Republic)
#min_maxnorm_czech = min_max_normal(Czech_Republic_pop)
test_czech = test_results(znorm_czech[[2]],znorm_czech[[1]])
#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_czech[[1]])
#country_split_19(znorm_czech[[1]])
#country_split_20(znorm_czech[[1]])
#write.csv(znorm_czech[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\3.Czech_Repubic.csv", row.names = TRUE)
#write.csv(test_czech,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Czech.csv", row.names = TRUE)

################################################################################
#Germany 
Germany = split_data[["Germany"]]
#Germany_pop = pop_ratio(Germany)

#two methods of normalization and standardization for
#10 years of data available
znorm_ger = znormal(Germany)
#min_maxnorm_czech = min_max_normal(Germany_pop)
test_ger = test_results(znorm_ger[[2]],znorm_ger[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_ger[[1]])
#country_split_19(znorm_ger[[1]])
#country_split_20(znorm_ger[[1]])
#write.csv(znorm_ger[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\4.Germany.csv", row.names = TRUE)
#write.csv(test_ger,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Germay.csv", row.names = TRUE)

################################################################################
#Iceland
Iceland = split_data[["Iceland"]]
#Iceland_pop = pop_ratio(Iceland)

#two methods of normalization and standardization for
#10 years of data available
znorm_ice = znormal(Iceland)
#min_maxnorm_czech = min_max_normal(Iceland_pop)
test_ice = test_results(znorm_ice[[2]],znorm_ice[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_ice[[1]])
#country_split_19(znorm_ice[[1]])
#country_split_20(znorm_ice[[1]])
#write.csv(znorm_ice[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\5.Iceland.csv", row.names = TRUE)
#write.csv(test_ice,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Iceland.csv", row.names = TRUE)

################################################################################

#Netherlands
Netherlands = split_data[["Netherlands"]]
#Netherlands_pop = pop_ratio(Netherlands)

#two methods of normalization and standardization for
#10 years of data available
znorm_nether = znormal(Netherlands)
#min_maxnorm_czech = min_max_normal(Netherlands_pop)
test_nether = test_results(znorm_nether[[2]],znorm_nether[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_nether[[1]])
#country_split_19(znorm_nether[[1]])
#country_split_20(znorm_nether[[1]])
#write.csv(znorm_nether[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\7.Netherlands.csv", row.names = TRUE)
#write.csv(test_nether,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Netherlands.csv", row.names = TRUE)

################################################################################
#Spain
Spain = split_data[["Spain"]]
#Spain_pop = pop_ratio(Spain)

#two methods of normalization and standardization for
#10 years of data available
znorm_spain = znormal(Spain)
#min_maxnorm_czech = min_max_normal(Spain_pop)
test_spain = test_results(znorm_spain[[2]],znorm_spain[[1]])

################################################################################
################################################################################
#Estonia
Estonia = split_data[["Estonia"]]
#Estonia_pop = pop_ratio(Estonia)

#two methods of normalization and standardization for
#10 years of data available
znorm_estonia = znormal(Estonia)
#min_maxnorm_czech = min_max_normal(Estonia_pop)
test_estonia = test_results(znorm_estonia[[2]],znorm_estonia[[1]])

#plot data for 3 years in a sequence 18,19,20
#country_split_18(znorm_estonia[[1]])
#country_split_19(znorm_estonia[[1]])
#country_split_20(znorm_estonia[[1]])
#write.csv(znorm_estonia[[1]],"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\10.Estonia.csv", row.names = TRUE)
#write.csv(test_estonia,"C:\\Users\\mohammadia\\Desktop\\R\\Countries\\Cor_Estonia.csv", row.names = TRUE)

################################################################################
#Slovenia
Slovenia = split_data[["Slovenia"]]
#Slovenia_pop = pop_ratio(Slovenia)

#two methods of normalization and standardization for
#10 years of data available
znorm_slov = znormal(Slovenia)
#min_maxnorm_czech = min_max_normal(Slovenia_pop)
test_slovenia = test_results(znorm_slov[[2]],znorm_slov[[1]])

################################################################################
#Latvia
Latvia = split_data[["Latvia"]]
#Latvia_pop = pop_ratio(Latvia)

#two methods of normalization and standardization for
#10 years of data available
znorm_Latvia = znormal(Latvia)
#min_maxnorm_czech = min_max_normal(Latvia_pop)
test_Latvia = test_results(znorm_Latvia[[2]],znorm_Latvia[[1]])


################################################################################
#Costa_Rica
CostaRica = split_data[["Costa Rica"]]
#CostaRica_pop = pop_ratio(CostaRica)

#two methods of normalization and standardization for
#10 years of data available
znorm_CostaRica = znormal(CostaRica)
#min_maxnorm_czech = min_max_normal(CostaRica_pop)
test_CostaRica = test_results(znorm_CostaRica[[2]],znorm_CostaRica[[1]])

################################################################################
#Lithuania
Lithuania = split_data[["Lithuania"]]
#Lithuania_pop = pop_ratio(Lithuania)

#two methods of normalization and standardization for
#10 years of data available
znorm_Lithuania = znormal(Lithuania)
#min_maxnorm_czech = min_max_normal(Lithuania_pop)
test_Lithuania = test_results(znorm_Lithuania[[2]],znorm_Lithuania[[1]])

################################################################################
all_countries = rbind(znorm_ausl[[1]],znorm_Belgium[[1]],znorm_Canada[[1]],znorm_France[[1]],
                      znorm_CostaRica[[1]],znorm_czech[[1]],znorm_Denmark[[1]],znorm_Finland[[1]],
                      znorm_estonia[[1]],znorm_ger[[1]],znorm_Greece[[1]],znorm_Hungary[[1]],
                      znorm_ice[[1]],znorm_Latvia[[1]],znorm_Lithuania[[1]],znorm_Italy[[1]],
                      znorm_slov[[1]],znorm_Korea[[1]],znorm_Luxembourg[[1]],
                      znorm_spain[[1]],znorm_Norway[[1]],znorm_Portugal[[1]],znorm_Sweden[[1]],
                      znorm_Israel[[1]],znorm_Slovak_Republic[[1]])

write.csv(all_countries,"C:\\Users\\mohammadia\\Desktop\\R\\Drugs tables\\all_countries.csv", row.names=T)

#split data of all countries based on each drug
drug = split(all_countries, f=all_countries$Variable)
#d = rbind(unique(drug[[1]]$Country),unique(drug[[1]]$`standard daviation`))
#d2 = rbind(unique(drug[[2]]$Country),unique(drug[[2]]$`standard daviation`))
#d3 = rbind(unique(drug[[3]]$Country),unique(drug[[3]]$`standard daviation`))
#d4 = rbind(unique(drug[[4]]$Country),unique(drug[[4]]$`standard daviation`))
#d5 = rbind(unique(drug[[5]]$Country),unique(drug[[5]]$`standard daviation`))
#d6 = rbind(unique(drug[[6]]$Country),unique(drug[[6]]$`standard daviation`))
#d7 = rbind(unique(drug[[7]]$Country),unique(drug[[7]]$`standard daviation`))
#d8 = rbind(unique(drug[[8]]$Country),unique(drug[[8]]$`standard daviation`))
#d9 = rbind(unique(drug[[9]]$Country),unique(drug[[9]]$`standard daviation`))
#d10 = rbind(unique(drug[[10]]$Country),unique(drug[[10]]$`standard daviation`))
#d11 = rbind(unique(drug[[11]]$Country),unique(drug[[11]]$`standard daviation`))
#d12 = rbind(unique(drug[[12]]$Country),unique(drug[[12]]$`standard daviation`))
#d13 = rbind(unique(drug[[13]]$Country),unique(drug[[13]]$`standard daviation`))
#d14 = rbind(unique(drug[[14]]$Country),unique(drug[[14]]$`standard daviation`))
#d15 = rbind(unique(drug[[15]]$Country),unique(drug[[15]]$`standard daviation`))
#d16 = rbind(unique(drug[[16]]$Country),unique(drug[[16]]$`standard daviation`))
#d17 = rbind(unique(drug[[17]]$Country),unique(drug[[17]]$`standard daviation`))
#d18 = rbind(unique(drug[[18]]$Country),unique(drug[[18]]$`standard daviation`))
#d19 = rbind(unique(drug[[19]]$Country),unique(drug[[19]]$`standard daviation`))
#d20 = rbind(unique(drug[[20]]$Country),unique(drug[[20]]$`standard daviation`))
#d21 = rbind(unique(drug[[21]]$Country),unique(drug[[21]]$`standard daviation`))
#d22 = rbind(unique(drug[[22]]$Country),unique(drug[[22]]$`standard daviation`))
#d23 = rbind(unique(drug[[23]]$Country),unique(drug[[23]]$`standard daviation`))
#d24 = rbind(unique(drug[[24]]$Country),unique(drug[[24]]$`standard daviation`))
#d25 = rbind(unique(drug[[25]]$Country),unique(drug[[25]]$`standard daviation`))
#d26 = rbind(unique(drug[[26]]$Country),unique(drug[[26]]$`standard daviation`))
#d27 = rbind(unique(drug[[27]]$Country),unique(drug[[27]]$`standard daviation`))
#d28 = rbind(unique(drug[[28]]$Country),unique(drug[[28]]$`standard daviation`))

#a = data.frame(matrix(NA, nrow = 25, ncol = 28))
#l = strsplit(unique(all_countries$Country)," ")
#for(i in 1:length(drug)){
#  if (length(unique(drug[[i]]$Country)) == 25){
#    a[1:length(unique(drug[[i]]$`standard daviation`)),i] = unique(drug[[i]]$`standard daviation`)
#  }
#  else {
#    for (j in 1:length(l)){
#      if (l[j] %in% unique(drug[[i]]$Country)) {
#        a[1:length(unique(drug[[i]]$`standard daviation`)),i] = unique(drug[[i]]$`standard daviation`)
#      }
#    }
#  }
#  names(a)[i] <- unique(all_countries$Variable)[i]
#}
#write.csv(d13,"C:\\Users\\mohammadia\\Desktop\\R\\Drugs tables\\C09-Agents acting on the Renin-Angiotens.csv", row.names = TRUE)
country_name = unique(all_countries$Country)
a1 = t(a)
colnames(a1) <- country_name

#run linear regression in a loop over all rows of drugs
lin_reg_all = list()
for(i in 1:length(drug)){
  lin_reg_all[[i]] = linear_reg(drug[[i]])                         
}

#bind each row containing 4 values to make a column comprising info for 28 drugs
linear_reg_all = do.call("rbind",lin_reg_all)
write.csv(linear_reg_all,"C:\\Users\\mohammadia\\Desktop\\R\\Drugs tables\\linear regression.csv", row.names = TRUE)

#make a table of all drugs and their correlation calculated based on different tests 
test_all_countries2 = test_results(drug,all_countries)
#add columns of linear regression result data to right side of test result table
compelete = data.frame(test_all_countries2,linear_reg_all)

concise_compelete = compelete[,c("Variable","pearson_test","p_value","coefficient")]
write.csv(concise_compelete,"C:\\Users\\mohammadia\\Desktop\\R\\Drugs tables\\complete_drug_analyses_table.csv", row.names = TRUE)

#alpha double prime is coefficient_pop, we transpose it to be able to multiply by each row of sigma prime
alpha = c(concise_compelete$coefficient)
write.csv(alpha,"C:\\Users\\mohammadia\\Desktop\\R\\Drugs tables\\alpha.csv", row.names = TRUE)


##########################################################################
#data visualization
# convert correlation matrix to long format
# create sample data
drug2 = read.csv("C:\\Users\\mohammadia\\Desktop\\R\\Drugs tables\\1.Drug_comorbidity_prediction latest version.csv")

drug_names <- data.frame(drug2$Variable)
# create bar plot using ggplot2

drug_code_concise<- c()
for (i in 1:nrow(drug2)) {
  name <- paste0("Drug", i)
  drug_code_concise <- c(drug_code_concise, name)
}
drug2 <- cbind(drug2[,1],drug_code_concise,drug2[,2:length(drug2)])
avg_cor <- drug2$pearson_test
drug_names_concise <- drug2$drug_code_concise

#ploting the pearson correlation of drugs
plot_pearson_correlation(drug2)

#category 1
plot_country_drug_consumption("Spain")
plot_country_drug_consumption("Germany")
plot_country_drug_consumption("Czech_Republic")

#category 2
plot_country_drug_consumption("Slovenia")
plot_country_drug_consumption("Israel")
plot_country_drug_consumption("CostaRica")
plot_country_drug_consumption("Lithuania")
plot_country_drug_consumption("Korea")
plot_country_drug_consumption("Latvia")
plot_country_drug_consumption("Estonia")

#category 3
plot_country_drug_consumption("Belgium")
plot_country_drug_consumption("Canada")
plot_country_drug_consumption("France")
plot_country_drug_consumption("Denmark")
plot_country_drug_consumption("Finland")
plot_country_drug_consumption("Greece")
plot_country_drug_consumption("Hungary")
plot_country_drug_consumption("Iceland")
plot_country_drug_consumption("Italy")
plot_country_drug_consumption("Luxembourg")
plot_country_drug_consumption("Norway")
plot_country_drug_consumption("Portugal")
plot_country_drug_consumption("Sweden")
plot_country_drug_consumption("Slovak_Republic")

################################################################################
################################################################################
#category 1
plot_stacked_bar("Spain")
plot_stacked_bar("Germany")
plot_stacked_bar("Czech_Republic")

#category 2
plot_stacked_bar("Slovenia")
plot_stacked_bar("Israel")
plot_stacked_bar("CostaRica")
plot_stacked_bar("Lithuania")
plot_stacked_bar("Korea")
plot_stacked_bar("Latvia")
plot_stacked_bar("Estonia")

#category 3
plot_stacked_bar("Belgium")
plot_stacked_bar("Canada")
plot_stacked_bar("France")
plot_stacked_bar("Denmark")
plot_stacked_bar("Finland")
plot_stacked_bar("Greece")
plot_stacked_bar("Hungary")
plot_stacked_bar("Iceland")
plot_stacked_bar("Italy")
plot_stacked_bar("Luxembourg")
plot_stacked_bar("Norway")
plot_stacked_bar("Portugal")
plot_stacked_bar("Sweden")
plot_stacked_bar("Slovak_Republic")
################################################################################
#mean values for 2010 to 2019
#category 1
Spain_sorted = get_sorted_data(Spain)[[1]]
Germany_sorted = get_sorted_data(Germany)[[1]]
Czech_Republic_sorted = get_sorted_data(Czech_Republic)[[1]]

#category 2
Slovenia_sorted = get_sorted_data(Slovenia)[[1]]
Israel_sorted = get_sorted_data(Israel)[[1]]
CostaRica_sorted = get_sorted_data(CostaRica)[[1]]
Lithuania_sorted = get_sorted_data(Lithuania)[[1]]
Korea_sorted = get_sorted_data(Korea)[[1]]
Latvia_sorted = get_sorted_data(Latvia)[[1]]
Estonia_sorted = get_sorted_data(Estonia)[[1]]

#category 3
Belgium_sorted = get_sorted_data(Belgium)[[1]]
Canada_sorted = get_sorted_data(Canada)[[1]]
France_sorted = get_sorted_data(France)[[1]]
Denmark_sorted = get_sorted_data(Denmark)[[1]]
Finland_sorted = get_sorted_data(Finland)[[1]]
Greece_sorted = get_sorted_data(Greece)[[1]]
Hungary_sorted = get_sorted_data(Hungary)[[1]]
Iceland_sorted = get_sorted_data(Iceland)[[1]]
Italy_sorted = get_sorted_data(Italy)[[1]]
Luxembourg_sorted = get_sorted_data(Luxembourg)[[1]]
Norway_sorted = get_sorted_data(Norway)[[1]]
Portugal_sorted = get_sorted_data(Portugal)[[1]]
Sweden_sorted = get_sorted_data(Sweden)[[1]]
Slovak_Republic_sorted = get_sorted_data(Slovak_Republic)[[1]]

#Plot the data based on spearsman
# Create a list of correlation matrices (14 tables)
tables <- list(Spain_sorted,Germany_sorted,Czech_Republic_sorted,Slovenia_sorted,Israel_sorted,CostaRica_sorted,Lithuania_sorted,Korea_sorted,
                    Latvia_sorted,Estonia_sorted,Belgium_sorted,Canada_sorted,France_sorted,Denmark_sorted,Finland_sorted,Greece_sorted,Hungary_sorted,
                    Iceland_sorted,Italy_sorted,Luxembourg_sorted,Norway_sorted,Portugal_sorted,Sweden_sorted,Slovak_Republic_sorted)  

# Get the unique drug names from all the tables
all_drug_names <- unique(unlist(lapply(tables, function(table) table[, 1])))

# Create an empty matrix with 20 rows and 25 columns
result_matrix <- matrix(NA, nrow =  length(all_drug_names), ncol = length(tables)+1)

# Set the drug names as the first column of the result_matrix
result_matrix[, 1] <- all_drug_names

# Loop through each table and fill in the matrix
for (i in 1:length(tables)) {
  # Get the current table
  current_table <- tables[[i]]
  
  # Get the drug names column
  drug_names <- current_table[, 1]
  
  # Get the drug consumption column
  drug_consumption <- current_table[, 3]
  
  # Find the row indices in the matrix for the current table's drug names
  row_indices <- match(drug_names, result_matrix[, 1])
  
  # Fill in the matrix with the drug consumption data for the current table
  result_matrix[row_indices, i + 1] <- drug_consumption
}

result_matrix2 =  as.matrix(result_matrix[,2:dim(result_matrix)[2]])


my_matrix <- apply(result_matrix2, 2, function(x) as.numeric(as.character(x)))

column_means <- colMeans(my_matrix, na.rm = TRUE)

# Loop through each column of the table
for (i in 1:ncol(my_matrix)) {
  # Replace NA values in the current column with corresponding values from the vector
  my_matrix[is.na(my_matrix[, i]), i] <- column_means[i]
}

my_matrix <- round(my_matrix,2)


# Create an empty correlation matrix for the final plot
cor_matrix <- cor(my_matrix, method = "spearman")

# Set up the color palette for positive and negative correlations
col_palette <- colorRampPalette(c("red", "white", "blue"))(n = 100)

# Plot the correlation matrix with circles
plot(1:dim(cor_matrix)[1], 1:dim(cor_matrix)[1], type = "n", xlim = c(0.5, 14.5), ylim = c(0.5, 14.5), xlab = "Country", ylab = "Country",
     main = "Spearman's Rho Correlation Matrix")

for (i in 1:dim(cor_matrix)[1]) {
  for (j in 1:dim(cor_matrix)[1]) {
    if (i != j) {
      rho <- cor_matrix[i, j]
      radius <- abs(rho) * 0.4 + 0.2  # Adjust the size based on correlation
      color <- ifelse(rho >= 0, hsv(0, 1, 1 - rho), hsv(0.67, 1, 1 + rho))  # Adjust the color gradient
      symbols(i, j, circles = radius, inches = 0.2, bg = color, add = TRUE)
    }
  }
}

# Plot the correlation matrix
corrplot(cor_matrix, method = "circle", tl.col = "black", col = col_palette,
         pch.cex = abs(final_cor_matrix) * 10, type = "lower")

corrplot(cor_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


################################################################################
#the values for 2020
#category 1
Spain_sorted_2020 = get_sorted_data(Spain)[[2]]
Germany_sorted_2020 = get_sorted_data(Germany)[[2]]
Czech_Republic_sorted_2020 = get_sorted_data(Czech_Republic)[[2]]

#category 2
Slovenia_sorted_2020 = get_sorted_data(Slovenia)[[2]]
Israel_sorted_2020 = get_sorted_data(Israel)[[2]]
CostaRica_sorted_2020 = get_sorted_data(CostaRica)[[2]]
Lithuania_sorted_2020 = get_sorted_data(Lithuania)[[2]]
Korea_sorted_2020 = get_sorted_data(Korea)[[2]]
Latvia_sorted_2020 = get_sorted_data(Latvia)[[2]]
Estonia_sorted_2020 = get_sorted_data(Estonia)[[2]]

#category 3
Belgium_sorted_2020 = get_sorted_data(Belgium)[[2]]
Canada_sorted_2020 = get_sorted_data(Canada)[[2]]
France_sorted_2020 = get_sorted_data(France)[[2]]
Denmark_sorted_2020 = get_sorted_data(Denmark)[[2]]
Finland_sorted_2020 = get_sorted_data(Finland)[[2]]
Greece_sorted_2020 = get_sorted_data(Greece)[[2]]
Hungary_sorted_2020 = get_sorted_data(Hungary)[[2]]
Iceland_sorted_2020 = get_sorted_data(Iceland)[[2]]
Italy_sorted_2020 = get_sorted_data(Italy)[[2]]
Luxembourg_sorted_2020 = get_sorted_data(Luxembourg)[[2]]
Norway_sorted_2020 = get_sorted_data(Norway)[[2]]
Portugal_sorted_2020 = get_sorted_data(Portugal)[[2]]
Sweden_sorted_2020 = get_sorted_data(Sweden)[[2]]
Slovak_Republic_sorted_2020 = get_sorted_data(Slovak_Republic)[[2]]

#Plot the data based on spearsman
# Create a list of correlation matrices (14 tables)
tables_2020 <- list(Spain_sorted_2020,Germany_sorted_2020,Czech_Republic_sorted_2020,Slovenia_sorted_2020,Israel_sorted_2020,CostaRica_sorted_2020,Lithuania_sorted_2020,
               Korea_sorted_2020,Latvia_sorted_2020,Estonia_sorted_2020,Belgium_sorted_2020,Canada_sorted_2020,France_sorted_2020,Denmark_sorted_2020,Finland_sorted_2020,Greece_sorted_2020,
               Hungary_sorted_2020, Iceland_sorted_2020,Italy_sorted_2020,Luxembourg_sorted_2020,Norway_sorted_2020,Portugal_sorted_2020,Sweden_sorted_2020,Slovak_Republic_sorted_2020)  

# Get the unique drug names from all the tables
all_drug_names_2020 <- unique(unlist(lapply(tables_2020, function(table) table[, 1])))

# Create an empty matrix with 20 rows and 25 columns
result_matrix_2020 <- matrix(NA, nrow =  length(all_drug_names_2020), ncol = length(tables_2020)+1)

# Set the drug names as the first column of the result_matrix
result_matrix_2020[, 1] <- all_drug_names_2020

# Loop through each table and fill in the matrix
for (i in 1:length(tables_2020)) {
                            # Get the current table
                            current_table_2020 <- tables_2020[[i]]
                            
                            # Get the drug names column
                            drug_names_2020 <- current_table_2020[, 1]
                            
                            # Get the drug consumption column
                            drug_consumption_2020 <- current_table_2020[,2]
                            
                            # Find the row indices in the matrix for the current table's drug names
                            row_indices_2020 <- match(drug_names_2020, result_matrix_2020[, 1])
                            
                            # Fill in the matrix with the drug consumption data for the current table
                            result_matrix_2020[row_indices_2020, i + 1] <- drug_consumption_2020
                          }
                          
                          result_matrix_2020 =  as.matrix(result_matrix_2020[,2:dim(result_matrix_2020)[2]])

                          my_matrix_2020 <- apply(result_matrix_2020, 2, function(x) as.numeric(as.character(x)))
                          
                          column_means_2020 <- colMeans(my_matrix_2020, na.rm = TRUE)
                          
                          # Loop through each column of the table
                          for (i in 1:ncol(my_matrix_2020)) {
                            # Replace NA values in the current column with corresponding values from the vector
                            my_matrix_2020[is.na(my_matrix_2020[, i]), i] <- column_means_2020[i]
                          }

my_matrix_2020 <- round(my_matrix_2020,2)


# Create an empty correlation matrix for the final plot
cor_matrix_2020 <- cor(my_matrix_2020, method = "spearman")

# Set up the color palette for positive and negative correlations
col_palette <- colorRampPalette(c("red", "white", "blue"))(n = 100)

# Plot the correlation matrix with circles
plot(1:dim(cor_matrix_2020)[1], 1:dim(cor_matrix_2020)[1], type = "n", xlim = c(0.5, 14.5), ylim = c(0.5, 14.5), xlab = "Country", ylab = "Country",
     main = "Spearman's Rho Correlation Matrix")

for (i in 1:dim(cor_matrix_2020)[1]) {
  for (j in 1:dim(cor_matrix_2020)[1]) {
    if (i != j) {
      rho <- cor_matrix_2020[i, j]
      radius <- abs(rho) * 0.4 + 0.2  # Adjust the size based on correlation
      color <- ifelse(rho >= 0, hsv(0, 1, 1 - rho), hsv(0.67, 1, 1 + rho))  # Adjust the color gradient
      symbols(i, j, circles = radius, inches = 0.2, bg = color, add = TRUE)
    }
  }
}

# Plot the correlation matrix
corrplot(cor_matrix_2020, method = "circle", tl.col = "black", col = col_palette,
         pch.cex = abs(final_cor_matrix) * 10, type = "lower")

corrplot(cor_matrix_2020, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


