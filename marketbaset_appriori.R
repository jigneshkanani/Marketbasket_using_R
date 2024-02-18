#install.packages("arules",dependencies = T)
library(arules)

# Using reshape2 package
#install.packages("reshape2", dependencies = T)
library(reshape2)

library(tidyverse)
library(dplyr)
library(magrittr)

library(doParallel)

all_cores <- parallel::detectCores(logical = T)

registerDoParallel(cores = all_cores)


setwd("~/R")

library(readxl)
datatest_product <- read_excel("datatest_product.xlsx", 
                               col_types = c("date", "text", "numeric"))
View(datatest_product)


wide_data <- datatest_product |> 
              pivot_wider(id_cols = invoice_date, names_from = product_name, values_from = qty, values_fill = 0) |> 
              mutate_at(vars(-("invoice_date")),function(x) ifelse(x>0, T, F))
              
head(wide_data)

wide_data$invoice_date <- as.Date(wide_data$invoice_date)

str(wide_data)


set.seed(9869)

# Calculate the number of unique values for each column
df_wide_sample <- wide_data[, sapply(wide_data, function(x) length(unique(x)) > 1)]

# Select only the columns you want to consider (excluding "invoice_date")
selected_columns <- df_wide_sample[, -which(names(df_wide_sample) == "invoice_date")]

# Calculate the number of unique values for each row
unique_counts <- apply(selected_columns, 1, function(x) length(unique(x)))

# Combine the unique counts with the original data frame
wide_data_selected <- cbind(df_wide_sample, Unique_Count = unique_counts)

#filter unique value i.e occurring more than 1
sample_data_selected <- wide_data_selected |> 
                      filter(Unique_Count > 1) |> 
                      select(-Unique_Count)

# select sample size as per memory avaliable.
wide_sample <- sample_data_selected[sample(nrow(sample_data_selected), 0.01 * nrow(sample_data_selected)), ]

# convert into transaction format.
transactions <- wide_sample |> 
          select(-invoice_date) |> 
          as("transactions")

# Example data (replace this with your own data)
#transactions <- read.transactions("Transaction.csv", sep = ",", format = "basket")

# Inspect the transactions
inspect(transactions)

#create rules
rules <- apriori(transactions, parameter = list(support = 0.85, confidence = 0.95,minlen=2, maxlen=5))


#inspect rules.
head(inspect(rules),10)

#install.packages("arulesViz", dependencies = T)
library(arulesViz)
plot(rules)
