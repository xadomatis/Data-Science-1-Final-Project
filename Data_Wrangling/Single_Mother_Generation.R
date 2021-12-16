#FDIC has permitted to release output and code for my single mother variables, but not the whole supplimental person level dataset
#this process sorts out mother-child households from mother-child-parent, mother-child-boyfriend, etc.
#these households that are only mother child are of utmost concern to the FDIC



#Part 1: Preformatting the data to reduce PERRP types

#read in the personl level data from the banking survey

pp <- read_csv("pp_plus_2019.csv")

pp$hrhhid3 <- paste(pp$hrhhid,pp$hrhhid2)

#load relevant packages

library(tidyverse)

#create new column for formating

pp$xtype <- data.frame(xtype = rep(NA, length(pp$hrhhid3)))

typer <- function(rel,age,gdr) {
  
  if(rel == -1) t <- 13 #converts to blank
  
  if(rel == 1 & gdr == 1) t <- 1 #male respondent
  
  if(rel == 2 & gdr == 1) t <- 1 #male respondent
  
  if(rel == 1 & gdr == 2) t <- 2 #female respondent
  
  if(rel == 2 & gdr == 2) t <- 2 #female respondent
  
  if(rel == 3 & gdr == 1) t <- 3 #male spouse
  
  if(rel == 3 & gdr == 2) t <- 4 #female spouse
  
  if(rel == 4 & age < 18 & age > -1) t <- 7 #child children remain the same
  
  if(rel == 4 & age > 17) t <- 8 #adult children are now relative adults
  
  if(rel == 5 & age > 17) t <- 8 #converts adult grandchild into relative adult
  
  if(rel == 5 & age < 18 & age > -1) t <- 9 #converts child grandchild into relative child
  
  if(rel == 6) t <- 8 #converts parents into relative adults
  
  if(rel == 7 & age > 17) t <- 8 #converts adult sibling into relative adult
  
  if(rel == 7 & age < 18 & age > -1) t <- 9 #converts child sibling into relative child
  
  if(rel == 8 & age > 17) t <- 8 #converts adult relative into relative adult
  
  if(rel == 8 & age < 18 & age > -1) t <- 9 #converts child relative into relative child
  
  if(rel == 9 & age > 17) t <- 10 #converts adult foster child into non-relative adult
  
  if(rel == 9 & age < 18 & age > -1) t <- 11 #converts foster child into non-relative child
  
  if(rel == 13 & gdr == 1) t <- 5 #male partner
  
  if(rel == 13 & gdr == 2) t <- 6 #female partner
  
  if(rel == 14 & gdr == 1) t <- 5 #male partner
  
  if(rel == 14 & gdr == 2) t <- 6 #female partner
  
  if(rel == 10 & age > 17) t <- 10 #converts adult foster child into non-relative adult
  
  if(rel == 10 & age < 18 & age > -1) t <- 11 #converts foster child into non-relative child
  
  if(rel == 12 & age > 17) t <- 10 #converts adult foster child into non-relative adult
  
  if(rel == 12 & age < 18 & age > -1) t <- 11 #converts foster child into non-relative child
  
  if(rel == 15) t <- 12 #converts to cohabitant
  
  if(rel == 16) t <- 12 #converts to cohabitant
  
  if(rel == 17) t <- 12 #converts to cohabitant
  
  if(rel == 18) t <- 12 #converts to cohabitant
  
  return(t)
  
}

#first, ensure function works outside of applications

typer(pp$perrp[1],pp$prtage[1],pp$pesex[1])

#make simpler dataset

pps <- data.frame(pp$hrhhid3,pp$perrp,pp$prtage,pp$pesex,pp$xtype)

names(pps)[1] <- "hrhhid3"

names(pps)[2] <- "rel"

names(pps)[3] <- "age"

names(pps)[4] <- "gdr"

#fill xtype with the function

pps$xtype <- mapply(typer,pps$rel,pps$age,pps$gdr)

table(pps$xtype)



#create an xtype key

code <- c(1:13) 

key <- c("res_m","res_f","spouse_m","spouse_f","partner_m","partner_f","child","rel_adult","rel_child","nr_adult","nr_child","cohabitant","blank")

xtype.key <- data.frame(code,key)



#simplify dataset further

pp_simplest <- data.frame(pps$hrhhid3,pps$xtype)

names(pp_simplest)[1] <- "hrhhid3"

names(pp_simplest)[2] <- "xtype"

#use the pivot_wider function to create  organize the dataset by household id

ft_original <- pp_simplest %>%
  
  pivot_wider(names_from = hrhhid3, values_from = xtype, values_fn=NULL)

#fix a series of formating issues before they occur

#reorient because it's in a row; add column name, make df, create column with hrhhid3 instead of label

ft_original <- t(ft_original)

colnames(ft_original)[1] <- 'ft'

ft_original <- data.frame(ft_original)

#create a list of all possible family types, shows a total of all possible combos

ft_unique <- unique(ft_original)

#make the rownames a column instead of a title

hrhhid3 <- rownames(ft_original)

rownames(ft_original) <- NULL

ft_original <- cbind(hrhhid3,ft_original)

#ensure that hrhhid's are in the same format

ft_original$hrhhid3 <- as.character(ft_original$hrhhid3)

#create an object to identify the number of unique households within pp_plus_2019

hh_unique <- unique(pp$hrhhid3)

#because the length of the unique households matches from both pp_plus_2019 and ft_original, the households are correctly sorted



#load in the main data file

hh <- read_csv("hh2019_analys.csv")

hh$hrhhid3 <- paste(hh$hrhhid,hh$hrhhid2)

hh$hrhhid3 <- as.character(hh$hrhhid3)

hh <- select(filter(hh, hhsupwgt > 0),c(hrhhid:hrhhid3))

#join that dataset to the family type dataset

df_ft <- hh %>% inner_join(ft_original, by = c("hrhhid3" = "hrhhid3"))

#find unique types of ft

df_ft_unique <- unique(df_ft$ft)

#far to many to categorize by hand, let's simplify



#first, create a variable with the unique elements (uqel) within the ft list

df_ft$ft_uqel <- data.frame(uqel = rep(NA, length(df_ft$hrhhid3)))

df_ft$ft_uqel <- as.list(df_ft$ft_uqel)

#create a loop that reads the unique elements within the lists in the ft column

#run it again with sort function

for(ii in 1:length(df_ft$ft)){
  
  df_ft$ft_uqel[[ii]] <- as.list(sort(unique(df_ft$ft[[ii]])))
  
}

#duplicate columns

df_ft$uq_mbr <- df_ft$ft_uqel

#find out how many unique family types exist within ufhf

df_ft_unique_sort <- unique(df_ft$ft_uqel)

#unnest the lists to get more workable data

df_ft_unnested <- df_ft %>% unnest_wider(uq_mbr, names_sep = ".")

#simplify naming

dfx <- df_ft_unnested



#create a simple variable representing the possible family types categorically

dfx$ftnum <- data.frame(uqel = rep(NA, length(dfx$ft)))

dfx$uq_mbr.1[is.na(dfx$uq_mbr.1)] <- 0

dfx$uq_mbr.2[is.na(dfx$uq_mbr.2)] <- 0

dfx$uq_mbr.3[is.na(dfx$uq_mbr.3)] <- 0

dfx$uq_mbr.4[is.na(dfx$uq_mbr.4)] <- 0

dfx$uq_mbr.5[is.na(dfx$uq_mbr.5)] <- 0

dfx$uq_mbr.6[is.na(dfx$uq_mbr.6)] <- 0

dfx$uq_mbr.1[dfx$uq_mbr.1 == 13] <- 0

dfx$uq_mbr.2[dfx$uq_mbr.2 == 13] <- 0

dfx$uq_mbr.3[dfx$uq_mbr.3 == 13] <- 0

dfx$uq_mbr.4[dfx$uq_mbr.4 == 13] <- 0

dfx$uq_mbr.5[dfx$uq_mbr.5 == 13] <- 0

dfx$uq_mbr.6[dfx$uq_mbr.6 == 13] <- 0

dfx$uq_mbr.1[dfx$uq_mbr.1 == 12] <- 0

dfx$uq_mbr.2[dfx$uq_mbr.2 == 12] <- 0

dfx$uq_mbr.3[dfx$uq_mbr.3 == 12] <- 0

dfx$uq_mbr.4[dfx$uq_mbr.4 == 12] <- 0

dfx$uq_mbr.5[dfx$uq_mbr.5 == 12] <- 0

dfx$uq_mbr.6[dfx$uq_mbr.6 == 12] <- 0

#combine columns into ftnum

library(tidyverse)

dfx$ftnum <- paste(".",dfx$uq_mbr.1,".",dfx$uq_mbr.2,".",dfx$uq_mbr.3,".",dfx$uq_mbr.4,".",dfx$uq_mbr.5,".",dfx$uq_mbr.6,".")

#create a single mother variable

dfx$single_mother <- data.frame(single_mother = rep(NA, length(dfx$ft)))

mother_isolator <- function(ftnum) {
  
  if(ftnum == ". 2 . 7 . 0 . 0 . 0 . 0 .") m <- 1 #converts to 1
  
  else m <- 0 #converts to 0
  
  return(m)
  
}

dfx$single_mother <- mapply(mother_isolator, dfx$ftnum)

#check number of households

total_unique_households <- unique(dfx$ftnum)

#make a table

library(janitor)

library(dplyr)

t1 <- dfx %>%
  
  tabyl(ftnum)

rowidx <- order(t1[, "n"], decreasing = TRUE, na.last = FALSE)

t1_sorted<-t1[rowidx, c(1:3), drop = FALSE]

library(xlsx)

library("writexl")

xport <- data.frame(dfx$hrhhid3,dfx$single_mother)

write_xlsx(xport, "hh_2019_new.xlsx")