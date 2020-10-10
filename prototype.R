rm(list = ls())

# !Note! Make sure to get to the right workspace before proceeding
# setwd("/home/charly_huang/Documents/RPI_ITWS_coursework/Fall_2020/Data_Analytics/Assignments/Assignment_3")
getwd()

if (!require(nortest)) install.packages("nortest")
if (!require('gridExtra')) install.packages('gridExtra')
library(nortest) # Anderson-Darlin test
library(ggplot2)
library(gridExtra)

dataset_dir <- "./nyt_datasets/"
nyt2_dir <- paste(dataset_dir, "nyt6.csv", sep = "")

proto_df <- read.csv(nyt2_dir)

str(proto_df)

# Draw boxplot
png('./plots/boxplot_age.png')
age_box <- ggplot(data = proto_df, aes(x=Age)) + geom_boxplot()
imp_box <- ggplot(data = proto_df, aes(x=Impressions)) + geom_boxplot()
grid.arrange(age_box, imp_box, ncol= 2, top = 'boxplots for Age (left) and Impression (Right)' )
dev.off()
print('Boxplots saved')

# # Draw histogram
png('./plots/histogram_age.png')
ggplot(data = proto_df, aes(x = Age)) + geom_histogram(binwidth = 3) + ggtitle('Histogram for Age')
dev.off()
print('histogram for age saved')
png('./plots/histogram_impressions.png')
ggplot(data = proto_df, aes(x = Impressions)) + geom_histogram(binwidth = 1) + ggtitle('Histogram for Impressions')
dev.off()
print('histogram for impressions saved')
# Cannot use grid.arrange with histograms

# The compiler wll yield at you with the following command
# hist(proto_df['Age'])

# Empirical Cumulative Distribution Function, ECDFs
png('./plots/ecdf_age.png')
ggplot(proto_df, aes(Age)) + stat_ecdf(geom = "step")+ ggtitle('ECDF for Age')
dev.off()
png('./plots/ecdf_impressions.png')
ggplot(proto_df, aes(Impressions)) + stat_ecdf(geom = "step")+ ggtitle('ECDF for Impressions')
dev.off()

# Significance testing
# Shapiro-Wilk test for the first 5000 data
# Original Age Shapiro
sigTest.age <- shapiro.test(proto_df$Age[0:5000])
sigTest.impressions <- shapiro.test(proto_df$Impressions[0:5000])

# Anderson-Darlin test for all data
AD_test.age <-ad.test(proto_df$Age)
AD_test.Impressions <- ad.test(proto_df$Impressions)

# ============== Q2 ==================
# Filter the distribution with one or more additional variables
# Replace Age = 0 with the mean of the column (Imputation)
imput_df <- proto_df
age_mean <- as.integer( mean( proto_df$Age, na.rm = TRUE) )
imput_df <- ifelse( (proto_df$Age == 0), age_mean,  proto_df$Age)


# only retain data with click > 1
imp_filt_df <- as.data.frame( subset( proto_df, (proto_df['Clicks'] > 1)  ) )

# Repeat Q1b
png('./plots/histogram_filt_age.png')
ggplot(data = imp_filt_df, aes(x = Age)) + geom_histogram(binwidth = 3) + ggtitle('Histogram for Age')
dev.off()
print('filtered histogram age saved')
png('./plots/histogram_filt_impressions.png')
ggplot(data = imp_filt_df, aes(x = Impressions)) + geom_histogram(binwidth = 1) + ggtitle('Histogram for Impressions')
dev.off()
print('filtered histogram impressions saved')

# Repeat Q1c
png('./plots/ecdf_filt_age.png')
ggplot(imp_filt_df, aes(Age)) + stat_ecdf(geom = "step")+ ggtitle('ECDF for Age')
dev.off()
print('Filtered ECDF for age saved')
png('./plots/ecdf_filt_impressions.png')
ggplot(imp_filt_df, aes(Impressions)) + stat_ecdf(geom = "step")+ ggtitle('ECDF for Impressions')
dev.off()
print('Filtered ECDF for Impressions saved')

# Repeat Q1d Significance testing
# Shapiro-Wilk testing
sigTest.filt_age <- shapiro.test(imp_filt_df$Age[0:5000])
sigTest.filt_impressions <- shapiro.test(imp_filt_df$Impressions[0:5000])

# Anderson-Darlin test
AD_test.filt_Age <- ad.test(imp_filt_df$Age)
AD_test.filt_Impressions <- ad.test(imp_filt_df$Impressions)




print('=====================')
print('Report on Significance testing:')
print('')
print( paste('original Age Shapiro:', sigTest.age$p.value))
print( paste('Original Impre Shapiro:', sigTest.impressions$p.value))
print(paste('original Age AD:', AD_test.age$p.value))
print(paste('Original Impre AD:', AD_test.Impressions$p.value))
print('')
print(paste('Filtered Age Shapiro:', sigTest.filt_age$p.value))
print(paste('Filtered Impre Shapiro:', sigTest.filt_impressions$p.value))
print(paste('Filtered Age AD:', AD_test.filt_Age$p.value))
print(paste('Filtered Impre AD:', AD_test.filt_Impressions$p.value))
print('=====================')