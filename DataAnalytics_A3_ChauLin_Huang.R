# Clear all stored variables
rm(list = ls())

# Load in all necessary libraries
if (!require(nortest)) install.packages("nortest")
if (!require('gridExtra')) install.packages('gridExtra')
library(nortest) # Anderson-Darlin test
library(ggplot2)
library(gridExtra)
library(readxl)

# =================================
# Functions
# =================================
get_df_list <- function(src_list) {
  out_df_list = list()
  
  for (dir in src_list) {
    out_df_list <- c( read.csv(dir) )
  }
  
  return(out_df_list)
} # End of get_df_list

# Q1a
draw_boxplot <- function(df, key, title = '') {
  ggplot(data = df, aes(x = key)) + geom_boxplot() + ggtitle(title)
  } # End of draw_boxplot

# Q1b 
draw_histogram <- function(df, key, title = '') {
  histogram <- ggplot(data = df, aes(x = key)) + geom_histogram(binwidth = 3) + ggtitle(title)
} # End of draw_histogram

# Q1c: Empirical Cumulative Distribution Function, ECDFs
draw_ECDF <- function(df, key, title = '') {
  ggplot(df, aes(x = key)) + stat_ecdf(geom = "step")+ ggtitle(title)
  
  
} # End of draw_ECDF

# Q1d Significance testing
sigTest <- function(df, key) {
  # Shapiro-Wilk test for the first 5000 data
  sigTest <- shapiro.test(df$key[0:5000])
  
  # Anderson-Darlin test for all data
  AD_test <- ad.test(df$Impressions)
  
  pValueList = list(sigTest, AD_test)
  return(pValueList)
} # End of sigTest

single_pipeline <- function(csv_dir, dfName, save_plot_dir) {
  df <- read.csv(csv_dir)
  print('Read in dataframe')
  
  # Q1a
  boxplot_title <- paste(dfName, ': Boxplot for Age')
  box_fileName <- paste(save_plot_dir, dfName, '_boxplot.png', sep = '')
  # png( box_fileName )
  ggplot(data = df, aes(x = Age)) + geom_boxplot() + ggtitle( boxplot_title )
  # dev.off() # save the file
  print( paste('plotted and saved boxplot to ', box_fileName, sep = '')  )
  
  # Q1b
  hist_title <- paste(dfName, 'Histogram for Age')
  hist_fileName <- paste(save_plot_dir, dfName, '_histogram.png', sep = '')
  ggplot(data = df, aes(x = Age)) + geom_histogram(binwidth = 3) + ggtitle(hist_title)
  print(paste('plotted histogram and saved to', hist_fileName))
  
  # Q1c
  # Empirical Cumulative Distribution Function, ECDFs
  ggplot(df, aes(Age)) + stat_ecdf(geom = "step")+ ggtitle('ECDF for Age')
  
  # Q1d Significance testing
  sigTest <- shapiro.test(df$Impressions[0:5000])
  AD_test <- ad.test(df$Impressions)
  
  
  # Filter the distribution with one or more additional variables
  # Replace Age = 0 with the mean of the column (Imputation)
  # imput_df <- df
  age_mean <- as.integer( mean( df$Age, na.rm = TRUE) )
  df$Age <- ifelse( (df$Age == 0), age_mean,  df$Age)
  
  
} # End of single_pipeline
# only retain data with click > 1
imp_filt_df <- subset( df, (df['Clicks'] > 1)  )

# =================================


# =================================
# Main pipeline
# =================================
# Import all csv files to dataframe
dataset_dir <- "./nyt_datasets/"
nyt2_dir <- paste(dataset_dir, "nyt2.csv", sep = "")
nyt3_dir <- paste(dataset_dir, "nyt3.csv", sep = "")
nyt4_dir <- paste(dataset_dir, "nyt4.csv", sep = "")
nyt5_dir <- paste(dataset_dir, "nyt5.csv", sep = "")
nyt6_dir <- paste(dataset_dir, "nyt6.csv", sep = "")

raw_nyt_dirs <- list(nyt2_dir, nyt3_dir, nyt4_dir, nyt5_dir, nyt6_dir)

save_dir <- './plots/'

single_pipeline( nyt2_dir, 'nyt2', save_dir)
# =================================

