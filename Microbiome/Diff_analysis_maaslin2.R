library(rJava)
library(devtools)
library(usethis)
library(Maaslin2)



rm(list=ls())
getwd()
setwd(".\\metagenome\\metagenome")
dir.create("diffanalysis") # Create a new directory
setwd("diffanalysis") # Change the current working directory 
getwd() #check if directory has been successfully changed



#Saving inputs as data frames

df_input_data = read.table(".\\combine metagenome\\corr\\species profile.txt", header = TRUE, sep = "\t",
                           row.names = 1,
                           stringsAsFactors = FALSE)
df_input_data[1:5, 1:5]
df_input_data2=data.frame(t(df_input_data))

df_input_metadata = read.table(".\\metagenome\\metagenome\\metadata-incRA.txt", header = TRUE, sep = "\t",
                               row.names = 1,
                               stringsAsFactors = FALSE)

df_input_metadata[1:5, ]


table(df_input_metadata$Group)

# OA vs. control
fit_data7 = Maaslin2(
  input_data = df_input_data2, 
  input_metadata = df_input_metadata, 
  output = "species-LOG0.1-none-cplm-include RA combine TBA", 
  fixed_effects = c("Group", "SEX", "age", "bmi","drink_cat" ,"meat_eggs_fq2", "dairy_fq2", "veg_fq2"),
  min_abundance = 0.001,
  analysis_method = "CPLM",
  min_prevalence = 0.1,
  transform="none",
  normalization = "none",
  standardize = T,
  plot_heatmap = F,
  plot_scatter=F,
  reference = "Group,Control")




###############Indicators of OA severity############
df_input_data = read.table(".\\metagenome\\metagenome\\sig species.txt", header = TRUE, sep = "\t",
                           row.names = 1,
                           stringsAsFactors = FALSE)
df_input_data[1:5, 1:5]


#Laterality

fit_data7 = Maaslin2(
  input_data = df_input_data, 
  input_metadata = df_input_metadata, 
  output = "sigspecies-LOG0.1-none-cplm-include RA combine TBA-unibi", 
  fixed_effects = c("unibi_skoa", "SEX", "age", "bmi","drink_cat" ,"meat_eggs_fq2", "dairy_fq2", "veg_fq2"),
  min_abundance = 0.001,
  analysis_method = "CPLM",
  min_prevalence = 0.1,
  transform="none",
  normalization = "none",
  standardize = T,
  plot_heatmap = F,
  plot_scatter=F)


#KL grade
fit_data7 = Maaslin2(
  input_data = df_input_data, 
  input_metadata = df_input_metadata, 
  output = "sigspecies-LOG0.1-none-cplm-include RA combine TBA-klcat", 
  fixed_effects = c("KLcat4", "SEX", "age", "bmi","drink_cat" ,"meat_eggs_fq2", "dairy_fq2", "veg_fq2"),
  min_abundance = 0.001,
  analysis_method = "CPLM",
  min_prevalence = 0.1,
  transform="none",
  normalization = "none",
  standardize = T,
  plot_heatmap = F,
  plot_scatter=F)



#Pain severity

df_input_metadata = read.table(".\\metagenome\\metagenome\\vas metadata.txt", header = TRUE, sep = "\t",
                               row.names = 1,
                               stringsAsFactors = FALSE)

df_input_metadata[1:5, ]



fit_data7 = Maaslin2(
  input_data = df_input_data, 
  input_metadata = df_input_metadata, 
  output = "sigspecies-LOG0.1-none-cplm-include RA combine TBA-vasCAT", 
  fixed_effects = c("vas_cat", "SEX", "age", "bmi","drink_cat" ,"meat_eggs_fq2", "dairy_fq2", "veg_fq2"),
  min_abundance = 0.001,
  analysis_method = "CPLM",
  min_prevalence = 0.1,
  transform="none",
  normalization = "none",
  standardize = T,
  plot_heatmap = F,
  plot_scatter=F)




#OST

df_input_metadata = read.table(".\\metagenome\\metagenome\\metadata.txt", header = TRUE, sep = "\t",
                               row.names = 1,
                               stringsAsFactors = FALSE)

df_input_metadata[1:5, ]


fit_data7 = Maaslin2(
  input_data = df_input_data, 
  input_metadata = df_input_metadata, 
  output = "sigspecies-LOG0.1-none-cplm-include RA combine TBA-OST", 
  fixed_effects = c("OST", "SEX", "age", "bmi","drink_cat" ,"meat_eggs_fq2", "dairy_fq2", "veg_fq2"),
  min_abundance = 0.001,
  analysis_method = "CPLM",
  min_prevalence = 0.1,
  transform="none",
  normalization = "none",
  standardize = T,
  plot_heatmap = F,
  plot_scatter=F)




#JSN

fit_data7 = Maaslin2(
  input_data = df_input_data, 
  input_metadata = df_input_metadata, 
  output = "sigspecies-LOG0.1-none-cplm-include RA combine TBA-JSN", 
  fixed_effects = c("JSN", "SEX", "age", "bmi","drink_cat" ,"meat_eggs_fq2", "dairy_fq2", "veg_fq2"),
  min_abundance = 0.001,
  analysis_method = "CPLM",
  min_prevalence = 0.1,
  transform="none",
  normalization = "none",
  standardize = T,
  plot_heatmap = F,
  plot_scatter=F)









