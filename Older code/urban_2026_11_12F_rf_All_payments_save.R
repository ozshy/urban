# Code for gas_v?.tex titled: "How Consumers Pay for Gasoline"
# This code uses the 2021 to 2025 data

# Packages used
#library(haven)# may be needed to handle haven labelled when data is converted from other software e.g. Stata
library(ggplot2); theme_set(theme_bw())# for graphics
library(scales)# to have % in ggplot axes and tables and for commas , in data frame numbers
#library(nnet)# multinomial regressions
library("xtable") #exporting to LaTeX
library(dplyr)# for sample_n
#library(ineq)# GINI coefficient
#library(Hmisc)# for cutting data into bins

# regression packages
#library(gtools)# for stars.pval function
#library(brglm2)# modifies glm. Useful when glm warns about fitting 0 and 1
#library(logistf)# stronger bias reduction to eliminate the glm warning (when brglm2 cannot eliminate it)
library(mfx)# binomial logit marginal effects
#library(stargazer) # for displaying multinomial coefficients. Does not work with mfx
library(texreg) # for displaying multinomial coefficients. Works with mfx (unlike stargazer). Also displays multiple regression.
#library(huxtable)#displays multiple regressions as table => advantage, since the table can be edited in R => Problem: built-in to_latex output does not run in LaTeX in my experience. 

# machine learning packages
#library(caret)#for confusionMatrix. Note: it flips the orientation so it makes actual (reference) as columns and predicted (data) as rows => unconventional. 
# ML packages
#library(rpart)# for classification/regression trees
#library(rpart.plot)
#library(partykit)# modifies rpart tree plot
#library("randomForest")# random forest original package (swithced to ranger)
#library("missForest")#to impute NAs, preparation for random forest (not used because cannot impute based on selected variables (no var selection.)
#library(mice)# for imputations. => cannot impute a large number of NA?
library(ranger)# replacing randomForest b/c it can use multiple cores.
library(missRanger)# for imputations of one variable based on some others
library(caret)# for confusion matrix
library(parallel)# to set the number of cores in ranger. num.threads = detectCores() - 1

#2025 data load
setwd("C:/Oz_local_workplace_3_Urban/urban_R")
dir()
# Read transaction dataset
trans_1.df = readRDS("dcpc_2025_tranlevel_public.rds")
dim(trans_1.df)
names(trans_1.df)
(sampled_num_trans = nrow(trans_1.df))

# Read individual dataset
indiv_1.df = readRDS("dcpc_2025_indlevel_public.rds")
dim(indiv_1.df)
names(indiv_1.df)
(sampled_num_resp = nrow(indiv_1.df))

#Refining the indiv dataset
# delete unneeded variables from indiv dataset
i_2.df = subset(indiv_1.df, select = c(id, ind_weight_all, income_hh, hhincome, age, cc_adopt, dc_adopt, cc_rewards, urban_cat, gender, highest_education, race, hispaniclatino, hh_size))
names(i_2.df)
sum(is.na(i_2.df))
sum(is.na(i_2.df$ind_weight_all))
sum(is.na(i_2.df$id))
sum(is.na(i_2.df$income_hh))#continuous income
sum(is.na(i_2.df$hhincome))#16 income cat
sum(is.na(i_2.df$age))
sum(is.na(i_2.df$cc_adopt))
sum(is.na(i_2.df$dc_adopt))
sum(is.na(i_2.df$cc_rewards))#missing the most
sum(is.na(i_2.df$urban_cat))
sum(is.na(i_2.df$gender))
sum(is.na(i_2.df$highest_education))
sum(is.na(i_2.df$race))
sum(is.na(i_2.df$hispaniclatino))
sum(is.na(i_2.df$hh_size))

#
# rename urban categories
i_3.df = i_2.df
table(i_3.df$urban_cat, useNA = "always")
str(i_3.df$urban_cat)
i_3.df$urban_cat = as.factor(i_3.df$urban_cat)
levels(i_3.df$urban_cat)
levels(i_3.df$urban_cat) = c("Rural", "Mixed", "Urban")
str(i_3.df$urban_cat)
table(i_3.df$urban_cat, useNA = "always")
# rename urban_cat to Area
i_3.df = i_3.df %>% rename("Area" = "urban_cat")
#
# condense education categories => new variable = Education
# rename urban categories
table(i_3.df$highest_education, useNA = "always")
str(i_3.df$highest_education)
i_4.df = i_3.df

i_4.df$Education = NA
i_4.df = i_4.df %>%
  mutate(Education = ifelse(highest_education < 11, "High_school_or_less", Education))# including some college
i_4.df = i_4.df %>%
  mutate(Education = ifelse(highest_education == 11 | highest_education == 12, "Associate_degree", Education))
i_4.df = i_4.df %>%
  mutate(Education = ifelse(highest_education == 13, "College_degree", Education))
i_4.df = i_4.df %>%
  mutate(Education = ifelse(highest_education > 13 , "Graduate_degree", Education))
table(i_4.df$Education, useNA = "always")
i_4.df$Education = as.factor(i_4.df$Education)
levels(i_4.df$Education)
i_4.df$Education = factor(i_4.df$Education, levels = c("High_school_or_less", "Associate_degree", "College_degree", "Graduate_degree"))

# rename Gender category
table(i_4.df$gender, useNA = "always")
i_4.df = i_4.df %>% 
  mutate(gender = ifelse(gender==0, "Female", gender))
i_4.df = i_4.df %>% 
  mutate(gender = ifelse(gender==1, "Male", gender))
table(i_4.df$gender, useNA = "always")
i_4.df = i_4.df %>% rename("Gender" = "gender")
i_4.df$Gender = as.factor(i_4.df$Gender)
names(i_4.df)

# rename and condense race categories
table(i_4.df$race, useNA = "always")
i_4.df = i_4.df %>% 
  mutate(race = ifelse(race==1, "White", race))
i_4.df = i_4.df %>% 
  mutate(race = ifelse(race==2, "Black", race))
i_4.df = i_4.df %>% 
  mutate(race = ifelse(race==4, "Asian", race))
i_4.df = i_4.df %>% 
  mutate(race = ifelse(race==3 | race==5 | race==6, "Other/combo", race))
i_4.df = i_4.df %>% rename("Race" = "race")
i_4.df$Race = as.factor(i_4.df$Race)
table(i_4.df$Race, useNA = "always")
levels(i_4.df$Race)
i_4.df$Race = factor(i_4.df$Race, levels = c("White", "Asian", "Black", "Other/combo"))

# rename and condense hispanic/latino categories
table(i_4.df$hispaniclatino, useNA = "always")
i_4.df = i_4.df %>% 
  mutate(hispaniclatino = ifelse(hispaniclatino==1, "Yes", hispaniclatino))
i_4.df = i_4.df %>% 
  mutate(hispaniclatino = ifelse(hispaniclatino==0, "No", hispaniclatino))
i_4.df = i_4.df %>% rename("Hispanic" = "hispaniclatino")
i_4.df$Hispanic = as.factor(i_4.df$Hispanic)

# Renaming all other indiv variables
names(i_4.df)
i_5.df = i_4.df
i_5.df = i_5.df %>% rename("Own_credit" = "cc_adopt", "Credit_reward" = "cc_rewards",  "Own_debit" = "dc_adopt", "HH_income" = "income_hh", "HH_income_cat" = "hhincome", "Age" = "age", "HH_size" = "hh_size")
names(i_5.df)
#
table(i_5.df$Own_debit, useNA = "always")
i_5.df = i_5.df %>% 
  mutate(Own_debit = case_when(
    Own_debit == 0 ~ "No",
    Own_debit == 1 ~ "Yes"
  ))
table(i_5.df$Own_debit, useNA = "always")
#
table(i_5.df$Own_credit, useNA = "always")
i_5.df = i_5.df %>% 
   mutate(Own_credit = case_when(
     Own_credit == 0 ~ "No",
     Own_credit == 1 ~ "Yes"
   ))
table(i_5.df$Own_credit, useNA = "always")
#
table(i_5.df$Credit_reward, useNA = "always")
i_5.df = i_5.df %>% 
  mutate(Credit_reward = case_when(
    Credit_reward == 0 ~ "No",
    Credit_reward == 1 ~ "Yes"
  ))
table(i_5.df$Credit_reward, useNA = "always")
# need to fix the NAs for credit_reward:
# First, respondents who do not have credit cards also don't have reward cards. Verify:
table(i_5.df$Own_credit, i_5.df$Credit_reward, useNA = "always", dnn = c("Own Credit", "Own Reward"))
nrow(subset(i_5.df, Own_credit=="No" & is.na(Credit_reward)))
# Make resp who do not have credit cards also not to have reward cards:
i_6.df = i_5.df
i_6.df$Credit_reward = ifelse(i_6.df$Own_credit=="No", "No", i_6.df$Credit_reward)
table(i_6.df$Credit_reward, useNA = "always")
table(i_6.df$Own_credit, i_6.df$Credit_reward, useNA = "always", dnn = c("Own Credit", "Own Reward"))

#make HH_size an integer (instead of numeric) => not used, insignificant
summary(i_5.df$HH_size)
str(i_5.df$HH_size)
i_5.df$HH_size = as.integer(i_5.df$HH_size)
str(i_5.df$HH_size)

# select variables for the trans dataset
t_2.df = subset(trans_1.df, select = c(id, pi, merch, amnt, device, mobile_app, in_person, bill ))
dim(t_2.df)

# merge indiv data into trans data
m1.df = left_join(t_2.df, i_6.df, by = "id")
dim(m1.df)
names(m1.df)

# remove payments with missing obs of: pi, amnt,ind_weight_all. Also, delete transactions with amount = 0 (if any)
sum(is.na(m1.df$pi))
sum(is.na(m1.df$amnt))
nrow(subset(m1.df, amnt <=0))
sum(is.na(m1.df$ind_weight_all))
sum(is.na(m1.df$Area))
sum(is.na(m1.df$in_person))
sum(is.na(m1.df$bill))
sum(is.na(m1.df$HH_size))
sum(is.na(m1.df$merch))
dim(m1.df)
# removed payments with missing obs listed above
m2.df = subset(m1.df, !is.na(m1.df$pi) & !is.na(m1.df$amnt) & m1.df$amnt >0 & !is.na(m1.df$ind_weight_all) & !is.na(m1.df$Area)  & !is.na(m1.df$in_person)  & !is.na(m1.df$bill) & !is.na(m1.df$merch))
nrow(m2.df)-nrow(m1.df)# num payments removed
dim(m2.df)

# rescale the weights to sum up to num obs: add column w
m3.df = m2.df
nrow(m3.df)
sum(m3.df$ind_weight_all)
m3.df$w = nrow(m3.df) * m3.df$ind_weight_all/sum(m3.df$ind_weight_all)
sum(m3.df$w)# verify w sum to num obs

## subsetting w.r.t. urban/mixed/rural
m4.df = m3.df
table(m4.df$Area, useNA = "always")

# rural data frame
rural1.df = subset(m4.df, Area == "Rural")
dim(rural1.df)
sum(rural1.df$w)
# # make rural weights: wr => Rescaling not needed
# rural1.df$wr = nrow(rural1.df)*rural1.df$w / sum(rural1.df$w)
# sum(rural1.df$wr)
# nrow(rural1.df)

# mixed data frame
mixed1.df = subset(m4.df, Area == "Mixed")
dim(mixed1.df)
sum(mixed1.df$w)
# # make mixed weights: wm  => Rescaling not needed
# mixed1.df$wm = nrow(mixed1.df)*mixed1.df$w / sum(mixed1.df$w)
# sum(mixed1.df$wm)
# nrow(mixed1.df)

# urban data frame
urban1.df = subset(m4.df, Area == "Urban")
dim(urban1.df)
sum(urban1.df$w)
# # make urban weights: wm => Rescaling not needed
# urban1.df$wm = nrow(urban1.df)*urban1.df$w / sum(urban1.df$w)
# sum(urban1.df$wm)
# nrow(urban1.df)

# verify sum payments
nrow(m4.df)
nrow(rural1.df) +nrow(mixed1.df) +nrow(urban1.df)
# verify num respondents
length(unique(m4.df$id))
length(unique((rural1.df$id))) +length(unique((mixed1.df$id))) +length(unique((urban1.df$id)))

#End: Reading, merging, renaming, refining data####

#+++++++++++++++++++++++

#Begin: Table 1: Type of payments####
# Starting urban_v5.tex 2026-11-2, Table 1 displays only weighted values (not the unweighted data computed below.)

# extracting info from rural
(rural_num_total = nrow(rural1.df))# num rural payments
length(unique(rural1.df$id))# num resp
# rural in-person
table(rural1.df$in_person, useNA = "always")
(rural_num_ip = nrow(subset(rural1.df, in_person==1)))
(rural_num_not_ip = nrow(subset(rural1.df, in_person==0)))
(rural_frac_ip = nrow(subset(rural1.df, in_person==1))/rural_num_total)
(rural_frac_not_ip = nrow(subset(rural1.df, in_person==0))/rural_num_total)
#rural bill
table(rural1.df$bill)
(rural_num_bill = nrow(subset(rural1.df, bill==1)))
(rural_num_not_bill = nrow(subset(rural1.df, bill==0)))
(rural_frac_bill = nrow(subset(rural1.df, bill==1))/rural_num_total)
(rural_frac_not_bill = nrow(subset(rural1.df, bill==0))/rural_num_total)
#rural in-person & bill
(rural_num_ip_bill = nrow(subset(rural1.df, in_person==1 & bill==1)))
(rural_num_ip_not_bill = nrow(subset(rural1.df, in_person==1 & bill==0)))
(rural_frac_ip_bill = nrow(subset(rural1.df, in_person==1 & bill==1))/rural_num_ip)
(rural_frac_ip_not_bill = nrow(subset(rural1.df, in_person==1 & bill==0))/rural_num_ip)
#rural not in-person & bill
(rural_num_not_ip_bill = nrow(subset(rural1.df, in_person==0 & bill==1)))
(rural_num_not_ip_not_bill = nrow(subset(rural1.df, in_person==0 & bill==0)))
(rural_frac_not_ip_bill = nrow(subset(rural1.df, in_person==0 & bill==1))/rural_num_not_ip)
(rural_frac_not_ip_not_bill = nrow(subset(rural1.df, in_person==0 & bill==0))/rural_num_not_ip)

# extracting info from mixed
(mixed_num_total = nrow(mixed1.df))# num mixed payments
length(unique(mixed1.df$id))# num resp
# mixed in-person
table(mixed1.df$in_person, useNA = "always")
(mixed_num_ip = nrow(subset(mixed1.df, in_person==1)))
(mixed_num_not_ip = nrow(subset(mixed1.df, in_person==0)))
(mixed_frac_ip = nrow(subset(mixed1.df, in_person==1))/mixed_num_total)
(mixed_frac_not_ip = nrow(subset(mixed1.df, in_person==0))/mixed_num_total)
#mixed bill
table(mixed1.df$bill)
(mixed_num_bill = nrow(subset(mixed1.df, bill==1)))
(mixed_num_not_bill = nrow(subset(mixed1.df, bill==0)))
(mixed_frac_bill = nrow(subset(mixed1.df, bill==1))/mixed_num_total)
(mixed_frac_not_bill = nrow(subset(mixed1.df, bill==0))/mixed_num_total)
#mixed in-person & bill
(mixed_num_ip_bill = nrow(subset(mixed1.df, in_person==1 & bill==1)))
(mixed_num_ip_not_bill = nrow(subset(mixed1.df, in_person==1 & bill==0)))
(mixed_frac_ip_bill = nrow(subset(mixed1.df, in_person==1 & bill==1))/mixed_num_ip)
(mixed_frac_ip_not_bill = nrow(subset(mixed1.df, in_person==1 & bill==0))/mixed_num_ip)
#mixed not in-person & bill
(mixed_num_not_ip_bill = nrow(subset(mixed1.df, in_person==0 & bill==1)))
(mixed_num_not_ip_not_bill = nrow(subset(mixed1.df, in_person==0 & bill==0)))
(mixed_frac_not_ip_bill = nrow(subset(mixed1.df, in_person==0 & bill==1))/mixed_num_not_ip)
(mixed_frac_not_ip_not_bill = nrow(subset(mixed1.df, in_person==0 & bill==0))/mixed_num_not_ip)

# extracting info from urban
(urban_num_total = nrow(urban1.df))# num urban payments
length(unique(urban1.df$id))# num resp
# urban in-person
table(urban1.df$in_person, useNA = "always")
(urban_num_ip = nrow(subset(urban1.df, in_person==1)))
(urban_num_not_ip = nrow(subset(urban1.df, in_person==0)))
(urban_frac_ip = nrow(subset(urban1.df, in_person==1))/urban_num_total)
(urban_frac_not_ip = nrow(subset(urban1.df, in_person==0))/urban_num_total)
#urban bill
table(urban1.df$bill)
(urban_num_bill = nrow(subset(urban1.df, bill==1)))
(urban_num_not_bill = nrow(subset(urban1.df, bill==0)))
(urban_frac_bill = nrow(subset(urban1.df, bill==1))/urban_num_total)
(urban_frac_not_bill = nrow(subset(urban1.df, bill==0))/urban_num_total)
#urban in-person & bill
(urban_num_ip_bill = nrow(subset(urban1.df, in_person==1 & bill==1)))
(urban_num_ip_not_bill = nrow(subset(urban1.df, in_person==1 & bill==0)))
(urban_frac_ip_bill = nrow(subset(urban1.df, in_person==1 & bill==1))/urban_num_ip)
(urban_frac_ip_not_bill = nrow(subset(urban1.df, in_person==1 & bill==0))/urban_num_ip)
#urban not in-person & bill
(urban_num_not_ip_bill = nrow(subset(urban1.df, in_person==0 & bill==1)))
(urban_num_not_ip_not_bill = nrow(subset(urban1.df, in_person==0 & bill==0)))
(urban_frac_not_ip_bill = nrow(subset(urban1.df, in_person==0 & bill==1))/urban_num_not_ip)
(urban_frac_not_ip_not_bill = nrow(subset(urban1.df, in_person==0 & bill==0))/urban_num_not_ip)

#preparing columns for urban.df
#in person yes and no (two columns)
(in_person.vec = c(rural_num_ip, mixed_num_ip, urban_num_ip, rural_frac_ip, mixed_frac_ip, urban_frac_ip))
(not_in_person.vec = c(rural_num_not_ip, mixed_num_not_ip, urban_num_not_ip, rural_frac_not_ip, mixed_frac_not_ip, urban_frac_not_ip))
#bill yes and no (2 columnes)
(bill.vec = c(rural_num_bill, mixed_num_bill, urban_num_bill, rural_frac_bill, mixed_frac_bill, urban_frac_bill))
(not_bill.vec = c(rural_num_not_bill, mixed_num_not_bill, urban_num_not_bill, rural_frac_not_bill, mixed_frac_not_bill, urban_frac_not_bill))
# in person & bill versus nonbill (2 columns)
(inperson_bill.vec = c(rural_num_ip_bill, mixed_num_ip_bill, urban_num_ip_bill, rural_frac_ip_bill, mixed_frac_ip_bill, urban_frac_ip_bill))
(inperson_not_bill.vec = c(rural_num_ip_not_bill, mixed_num_ip_not_bill, urban_num_ip_not_bill, rural_frac_ip_not_bill, mixed_frac_ip_not_bill, urban_frac_ip_not_bill))
# not in person & bill versus nonbill (2 columns)
(not_inperson_bill.vec = c(rural_num_not_ip_bill, mixed_num_not_ip_bill, urban_num_not_ip_bill, rural_frac_not_ip_bill, mixed_frac_not_ip_bill, urban_frac_not_ip_bill))
(not_inperson_not_bill.vec = c(rural_num_not_ip_not_bill, mixed_num_not_ip_not_bill, urban_num_not_ip_not_bill, rural_frac_not_ip_not_bill, mixed_frac_not_ip_not_bill, urban_frac_not_ip_not_bill))

# Combine the above into a data frame
# Starting urban_v5.tex 2026-11-2, only weighted values are displayed in Table 1. The data frame below will be replaced with weighted values.
(pay_type.df = data.frame(ip_yes = in_person.vec, ip_no = not_in_person.vec, bill_yes = bill.vec, bill_no = not_bill.vec, ip_yes_bill_yes = inperson_bill.vec, ip_yes_bill_no = inperson_not_bill.vec, ip_no_bill_yes = not_inperson_bill.vec, ip_no_bill_no = not_inperson_not_bill.vec))

# verifying the above table (sum to total?)
rural_num_total
mixed_num_total
urban_num_total
pay_type.df$ip_yes + pay_type.df$ip_no
pay_type.df$bill_yes + pay_type.df$bill_no
#
pay_type.df$ip_yes
pay_type.df$ip_yes_bill_yes + pay_type.df$ip_yes_bill_no
#
pay_type.df$ip_no
pay_type.df$ip_no_bill_yes + pay_type.df$ip_no_bill_no
#

## Construct a version with weights (to be appended below the above)
# Starting urban_v5.tex 2026-11-2: Table 1 displays only weighted values
# rural (weighted)
(rural_num_total = nrow(rural1.df))# num rural payments
length(unique(rural1.df$id))# num resp
(rural_num_total_w =sum(rural1.df$w))
# rural in-person
table(rural1.df$in_person)
(rural_num_ip_w = sum(subset(rural1.df, in_person==1)$w))
(rural_num_not_ip_w = sum(subset(rural1.df, in_person==0)$w))
(rural_frac_ip_w = rural_num_ip_w/sum(rural1.df$w))
(rural_frac_not_ip_w = rural_num_not_ip_w/sum(rural1.df$w))
rural_frac_ip_w + rural_frac_not_ip_w
#rural bill
table(rural1.df$bill)
(rural_num_bill_w = sum(subset(rural1.df, bill==1)$w))
(rural_num_not_bill_w = sum(subset(rural1.df, bill==0)$w))
(rural_frac_bill_w = rural_num_bill_w/sum(rural1.df$w))
(rural_frac_not_bill_w = rural_num_not_bill_w/sum(rural1.df$w))
rural_frac_bill_w + rural_frac_not_bill_w
#rural in-person & bill
(rural_num_ip_bill_w = sum(subset(rural1.df, in_person==1 & bill==1)$w))
(rural_num_ip_not_bill_w = sum(subset(rural1.df, in_person==1 & bill==0)$w))
(rural_frac_ip_bill_w = rural_num_ip_bill_w/rural_num_ip_w )
(rural_frac_ip_not_bill_w = rural_num_ip_not_bill_w/rural_num_ip_w )
rural_frac_ip_bill_w + rural_frac_ip_not_bill_w
#rural not in-person & bill
(rural_num_not_ip_bill_w = sum(subset(rural1.df, in_person==0 & bill==1)$w))
(rural_num_not_ip_not_bill_w = sum(subset(rural1.df, in_person==0 & bill==0)$w))
(rural_frac_not_ip_bill_w = rural_num_not_ip_bill_w/rural_num_not_ip_w)
(rural_frac_not_ip_not_bill_w = rural_num_not_ip_not_bill_w/rural_num_not_ip_w)
rural_frac_not_ip_bill_w + rural_frac_not_ip_not_bill

# mixed (weighted)
(mixed_num_total = nrow(mixed1.df))# num mixed payments
length(unique(mixed1.df$id))# num resp
(mixed_num_total_w = sum(mixed1.df$w))
# mixed in-person
table(mixed1.df$in_person)
(mixed_num_ip_w = sum(subset(mixed1.df, in_person==1)$w))
(mixed_num_not_ip_w = sum(subset(mixed1.df, in_person==0)$w))
(mixed_frac_ip_w = mixed_num_ip_w/sum(mixed1.df$w))
(mixed_frac_not_ip_w = mixed_num_not_ip_w/sum(mixed1.df$w))
mixed_frac_ip_w + mixed_frac_not_ip_w
#mixed bill
table(mixed1.df$bill)
(mixed_num_bill_w = sum(subset(mixed1.df, bill==1)$w))
(mixed_num_not_bill_w = sum(subset(mixed1.df, bill==0)$w))
(mixed_frac_bill_w = mixed_num_bill_w/sum(mixed1.df$w))
(mixed_frac_not_bill_w = mixed_num_not_bill_w/sum(mixed1.df$w))
mixed_frac_bill_w + mixed_frac_not_bill_w
#mixed in-person & bill
(mixed_num_ip_bill_w = sum(subset(mixed1.df, in_person==1 & bill==1)$w))
(mixed_num_ip_not_bill_w = sum(subset(mixed1.df, in_person==1 & bill==0)$w))
(mixed_frac_ip_bill_w = mixed_num_ip_bill_w/mixed_num_ip_w )
(mixed_frac_ip_not_bill_w = mixed_num_ip_not_bill_w/mixed_num_ip_w )
mixed_frac_ip_bill_w + mixed_frac_ip_not_bill_w
#mixed not in-person & bill
(mixed_num_not_ip_bill_w = sum(subset(mixed1.df, in_person==0 & bill==1)$w))
(mixed_num_not_ip_not_bill_w = sum(subset(mixed1.df, in_person==0 & bill==0)$w))
(mixed_frac_not_ip_bill_w = mixed_num_not_ip_bill_w/mixed_num_not_ip_w)
(mixed_frac_not_ip_not_bill_w = mixed_num_not_ip_not_bill_w/mixed_num_not_ip_w)
mixed_frac_not_ip_bill_w + mixed_frac_not_ip_not_bill

# urban (weighted)
(urban_num_total = nrow(urban1.df))# num urban payments
length(unique(urban1.df$id))# num resp
(urban_num_total_w = sum(urban1.df$w))
# urban in-person
table(urban1.df$in_person)
(urban_num_ip_w = sum(subset(urban1.df, in_person==1)$w))
(urban_num_not_ip_w = sum(subset(urban1.df, in_person==0)$w))
(urban_frac_ip_w = urban_num_ip_w/sum(urban1.df$w))
(urban_frac_not_ip_w = urban_num_not_ip_w/sum(urban1.df$w))
urban_frac_ip_w + urban_frac_not_ip_w
#urban bill
table(urban1.df$bill)
(urban_num_bill_w = sum(subset(urban1.df, bill==1)$w))
(urban_num_not_bill_w = sum(subset(urban1.df, bill==0)$w))
(urban_frac_bill_w = urban_num_bill_w/sum(urban1.df$w))
(urban_frac_not_bill_w = urban_num_not_bill_w/sum(urban1.df$w))
urban_frac_bill_w + urban_frac_not_bill_w
#urban in-person & bill
(urban_num_ip_bill_w = sum(subset(urban1.df, in_person==1 & bill==1)$w))
(urban_num_ip_not_bill_w = sum(subset(urban1.df, in_person==1 & bill==0)$w))
(urban_frac_ip_bill_w = urban_num_ip_bill_w/urban_num_ip_w )
(urban_frac_ip_not_bill_w = urban_num_ip_not_bill_w/urban_num_ip_w )
urban_frac_ip_bill_w + urban_frac_ip_not_bill_w
#urban not in-person & bill
(urban_num_not_ip_bill_w = sum(subset(urban1.df, in_person==0 & bill==1)$w))
(urban_num_not_ip_not_bill_w = sum(subset(urban1.df, in_person==0 & bill==0)$w))
(urban_frac_not_ip_bill_w = urban_num_not_ip_bill_w/urban_num_not_ip_w)
(urban_frac_not_ip_not_bill_w = urban_num_not_ip_not_bill_w/urban_num_not_ip_w)
urban_frac_not_ip_bill_w + urban_frac_not_ip_not_bill

# starting urban_v5.tex 2026-11-2, only weighted values in Table 1
#
#in person yes and no (2 columns)
(in_person2.vec = c(rural_num_ip_w, mixed_num_ip_w, urban_num_ip_w, rural_frac_ip_w, mixed_frac_ip_w, urban_frac_ip_w))
#
(not_in_person2.vec = c(rural_num_not_ip_w, mixed_num_not_ip_w, urban_num_not_ip_w, rural_frac_not_ip_w, mixed_frac_not_ip_w, urban_frac_not_ip_w))
#bill yes and no (2 columnes)
(bill2.vec = c(rural_num_bill_w, mixed_num_bill_w, urban_num_bill_w,  rural_frac_bill_w, mixed_frac_bill_w, urban_frac_bill_w))
#
(not_bill2.vec = c(rural_num_not_bill_w, mixed_num_not_bill_w, urban_num_not_bill_w, rural_frac_not_bill_w, mixed_frac_not_bill_w, urban_frac_not_bill_w))
# in person & bill versus nonbill (2 columns)
(inperson_bill2.vec = c(rural_num_ip_bill_w, mixed_num_ip_bill_w, urban_num_ip_bill_w, rural_frac_ip_bill_w, mixed_frac_ip_bill_w, urban_frac_ip_bill_w))
#
(inperson_not_bill2.vec = c(rural_num_ip_not_bill_w, mixed_num_ip_not_bill_w, urban_num_ip_not_bill_w, rural_frac_ip_not_bill_w, mixed_frac_ip_not_bill_w, urban_frac_ip_not_bill_w))
# not in person & bill versus nonbill (2 columns)
(not_inperson_bill2.vec = c(rural_num_not_ip_bill_w, mixed_num_not_ip_bill_w, urban_num_not_ip_bill_w, rural_frac_not_ip_bill_w, mixed_frac_not_ip_bill_w, urban_frac_not_ip_bill_w))
#
(not_inperson_not_bill2.vec = c(rural_num_not_ip_not_bill_w, mixed_num_not_ip_not_bill_w, urban_num_not_ip_not_bill_w, rural_frac_not_ip_not_bill_w, mixed_frac_not_ip_not_bill_w, urban_frac_not_ip_not_bill_w))

# make it data frame (all weighted)
(pay_type2.df = data.frame(ip_yes = in_person2.vec, ip_no = not_in_person2.vec, bill_yes = bill2.vec, bill_no2 = not_bill.vec, ip_yes_bill_yes = inperson_bill2.vec, ip_yes_bill_no = inperson_not_bill2.vec, ip_no_bill_yes = not_inperson_bill2.vec, ip_no_bill_no = not_inperson_not_bill2.vec))

# verifying the above table (sum to total?)
rural_num_total# unweighted
mixed_num_total
urban_num_total
round(pay_type2.df$ip_yes + pay_type2.df$ip_no,0)# weighted
round(pay_type2.df$bill_yes + pay_type2.df$bill_no,0)
#
rural_num_total+mixed_num_total+urban_num_total# total sample unweighted
round(sum(pay_type2.df$ip_yes + pay_type2.df$ip_no,0))# total weighed

#
round(pay_type2.df$ip_yes,0)
round(pay_type2.df$ip_yes_bill_yes + pay_type2.df$ip_yes_bill_no,0)
#
round(pay_type.df$ip_no,0)
round(pay_type.df$ip_no_bill_yes + pay_type.df$ip_no_bill_no,0)
#

# add p-values and CI at 99% level
pay_type2.df
#p-values for z-test percentages comparing rural with urban only
#comparing FRAC IN-PERSON rural vs. urban => sig
# two-proportion z-test (pooled) In person
rural_num_ip_w# rural in person
urban_num_ip_w# urban in person
rural_num_total_w# total rural
urban_num_total_w# total urban
(test_ip_w = prop.test(x = c(rural_num_ip_w, urban_num_ip_w),
          n = c(rural_num_total_w, urban_num_total_w),
          correct = FALSE, conf.level = 0.99))   # disable Yates correction to get pure z-test
(test_ip_w_pval = test_ip_w$p.value)
(test_ip_w_pval_yn = ifelse(test_ip_w$p.value < 0.01, "Yes", "No"))
(test_ip_w_ci = test_ip_w$conf.int)
(test_ip_w_ci_lower = test_ip_w$conf.int[1])
(test_ip_w_ci_upper = test_ip_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
in_person2.vec
(in_person3.vec = as.numeric(c(in_person2.vec, test_ip_w_ci_lower, test_ip_w_ci_upper)))

# two-proportion z-test (pooled) Not in person
rural_num_not_ip_w# rural in person
urban_num_not_ip_w# urban in person
rural_num_total_w# total rural
urban_num_total_w# total urban
(test_not_ip_w = prop.test(x = c(rural_num_not_ip_w, urban_num_not_ip_w),
                       n = c(rural_num_total_w, urban_num_total_w),
                       correct = FALSE, conf.level = 0.99))   # disable Yates correction to get pure z-test
(test_not_ip_w_pval = test_not_ip_w$p.value)
(test_not_ip_w_pval_yn = ifelse(test_not_ip_w$p.value < 0.01, "Yes", "No"))
(test_not_ip_w_ci = test_not_ip_w$conf.int)
(test_not_ip_w_ci_lower = test_not_ip_w$conf.int[1])
(test_not_ip_w_ci_upper = test_not_ip_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
not_in_person2.vec
(not_in_person3.vec = as.numeric(c(not_in_person2.vec, test_not_ip_w_ci_lower, test_not_ip_w_ci_upper)))

#
#comparing FRAC BILLS rural vs. urban => not sig
# two-proportion z-test (pooled)
rural_num_bill_w# rural bills
urban_num_bill_w# urban bills
rural_num_total_w# total rural
urban_num_total_w# total urban
(test_bill_w = prop.test(x = c(rural_num_bill_w, urban_num_bill_w),
          n = c(rural_num_total_w, urban_num_total_w),
          correct = FALSE, conf.level = 0.99))   # disable Yates correction to get pure z-test
(test_bill_w_pval = test_bill_w$p.value)
(test_bill_w_pval_yn = ifelse(test_bill_w$p.value < 0.01, "Yes", "No"))
(test_bill_w_ci = test_bill_w$conf.int)
(test_bill_w_ci_lower = test_bill_w$conf.int[1])
(test_bill_w_ci_upper = test_bill_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
bill2.vec
(bill3.vec = as.numeric(c(bill2.vec, test_bill_w_ci_lower, test_bill_w_ci_upper)))

#
#comparing FRAC Non-BILLS rural vs. urban => not sig
# two-proportion z-test (pooled)
rural_num_not_bill_w# rural bills
urban_num_not_bill_w# urban bills
rural_num_total_w# total rural
urban_num_total_w# total urban
(test_not_bill_w = prop.test(x = c(rural_num_not_bill_w, urban_num_not_bill_w),
                         n = c(rural_num_total_w, urban_num_total_w),
                         correct = FALSE, conf.level = 0.99))   # disable Yates correction to get pure z-test
(test_not_bill_w_pval = test_not_bill_w$p.value)
(test_not_bill_w_pval_yn = ifelse(test_not_bill_w$p.value < 0.01, "Yes", "No"))
(test_not_bill_w_ci = test_not_bill_w$conf.int)
(test_not_bill_w_ci_lower = test_not_bill_w$conf.int[1])
(test_not_bill_w_ci_upper = test_not_bill_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
not_bill2.vec
(not_bill3.vec = as.numeric(c(not_bill2.vec, test_not_bill_w_ci_lower, test_not_bill_w_ci_upper)))

#comparing inperson: FRAC BILLs rural vs. urban => not sig
# two-proportion z-test (pooled)
rural_num_ip_bill_w
urban_num_ip_bill_w# 
rural_num_ip_w# total rural in person
urban_num_ip_w# total urban in person
(test_ip_bill_w = prop.test(x = c(rural_num_ip_bill_w, urban_num_ip_bill_w),
          n = c(rural_num_ip_w, urban_num_ip_w),
          correct = FALSE, conf.level = 0.99))   # disable Yates correction to get pure z-test
(test_ip_bill_w_pval = test_ip_bill_w$p.value)
(test_ip_bill_w_pval_yn = ifelse(test_ip_bill_w$p.value < 0.01, "Yes", "No"))
(test_ip_bill_w_ci = test_ip_bill_w$conf.int)
(test_ip_bill_w_ci_lower = test_ip_bill_w$conf.int[1])
(test_ip_bill_w_ci_upper = test_ip_bill_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
inperson_bill2.vec
(inperson_bill3.vec = as.numeric(c(inperson_bill2.vec, test_ip_bill_w_ci_lower, test_ip_bill_w_ci_upper)))

#comparing inperson: FRAC Non-BILLs rural vs. urban => not sig
# two-proportion z-test (pooled)
rural_num_ip_not_bill_w
urban_num_ip_not_bill_w# 
rural_num_ip_w# total rural in person
urban_num_ip_w# total urban in person
(test_ip_not_bill_w = prop.test(x = c(rural_num_ip_not_bill_w, urban_num_ip_not_bill_w), n = c(rural_num_ip_w, urban_num_ip_w), correct = FALSE, conf.level = 0.99))   # disable Yates correction to get pure z-test
(test_ip_not_bill_w_pval = test_ip_not_bill_w$p.value)
(test_ip_not_bill_w_pval_yn = ifelse(test_ip_not_bill_w$p.value < 0.01, "Yes", "No"))
(test_ip_not_bill_w_ci = test_ip_not_bill_w$conf.int)
(test_ip_not_bill_w_ci_lower = test_ip_not_bill_w$conf.int[1])
(test_ip_not_bill_w_ci_upper = test_ip_not_bill_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
inperson_not_bill2.vec
(inperson_not_bill3.vec = as.numeric(c(inperson_not_bill2.vec, test_ip_not_bill_w_ci_lower, test_ip_not_bill_w_ci_upper)))

#comparing remote: FRAC BILLs rural vs. urban => sig
# two-proportion z-test (pooled)
rural_num_not_ip_bill_w
urban_num_not_ip_bill_w# 
rural_num_not_ip_w# total rural remote
urban_num_not_ip_w# total urban remote
(test_not_ip_bill_w = prop.test(x = c(rural_num_not_ip_bill_w, urban_num_not_ip_bill_w),
          n = c(rural_num_not_ip_w, urban_num_not_ip_w),
          correct = FALSE, conf.level = 0.99))   # disable Yates correction to get pure z-test
(test_not_ip_bill_w_pval = test_not_ip_bill_w$p.value)
(test_not_ip_bill_w_pval_yn = ifelse(test_not_ip_bill_w$p.value < 0.01, "Yes", "No"))
(test_not_ip_bill_w_ci = test_not_ip_bill_w$conf.int)
(test_not_ip_bill_w_ci_lower = test_not_ip_bill_w$conf.int[1])
(test_not_ip_bill_w_ci_upper = test_not_ip_bill_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
not_inperson_bill2.vec
(not_inperson_bill3.vec = as.numeric(c(not_inperson_bill2.vec, test_not_ip_bill_w_ci_lower, test_not_ip_bill_w_ci_upper)))

#comparing Remote: FRAC Non-BILLs rural vs. urban => not sig
# two-proportion z-test (pooled)
rural_num_not_ip_not_bill_w
urban_num_not_ip_not_bill_w# 
rural_num_not_ip_w# total rural in person
urban_num_not_ip_w# total urban in person
(test_not_ip_not_bill_w = prop.test(x = c(rural_num_not_ip_not_bill_w, urban_num_not_ip_not_bill_w),
  n = c(rural_num_not_ip_w, urban_num_not_ip_w), correct = FALSE, conf.level = 0.99))   
(test_not_ip_not_bill_w_pval = test_not_ip_not_bill_w$p.value)
(test_not_ip_not_bill_w_pval_yn = ifelse(test_not_ip_not_bill_w$p.value < 0.01, "Yes", "No"))
(test_not_ip_not_bill_w_ci = test_not_ip_not_bill_w$conf.int)
(test_not_ip_not_bill_w_ci_lower = test_not_ip_not_bill_w$conf.int[1])
(test_not_ip_not_bill_w_ci_upper = test_not_ip_not_bill_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
not_inperson_not_bill2.vec
(not_ip_not_bill3.vec = as.numeric(c(not_inperson_not_bill2.vec, test_not_ip_not_bill_w_ci_lower, test_not_ip_not_bill_w_ci_upper)))

# construct new data frame to replace pay_type2.df but  w/o the yn (chr) (to be added manually)
pay_type2.df
# 1st column with area
(area.vec = c("Rural", "Mixed", "Urban", "Rural", "Mixed", "Urban","Rural", "Mixed", "Urban", "Lower CI", "Upper CI"))
(pay_type3.df = data.frame(Area = area.vec, ip_yes = c(in_person.vec[1:3], in_person3.vec), ip_no = c(not_in_person.vec[1:3], not_in_person3.vec), bill_yes = c(bill.vec[1:3], bill3.vec), bill_no = c(not_bill.vec[1:3], not_bill3.vec), ip_yes_bill_yes = c(inperson_bill.vec[1:3], inperson_bill3.vec), ip_yes_bill_no = c(inperson_not_bill.vec[1:3], inperson_not_bill3.vec), ip_no_bill_yes = c(not_inperson_bill.vec[1:3], not_inperson_bill3.vec), ip_no_bill_no = c(not_inperson_not_bill.vec[1:3], not_ip_not_bill3.vec) ))
dim(pay_type3.df)
str(pay_type3.df)
class(pay_type3.df)

# round and add % sign
(pay_type4.df = pay_type3.df)
dim(pay_type4.df)
# round the first 3 rows (weighed number of payments)
(pay_type4.df[1:6, 2:9] = round(pay_type4.df[1:6, 2:9],0))
# round the next 3 rows (% of payments)
(pay_type4.df[7:9, 2:9] = round(pay_type4.df[7:9, 2:9],3))
# round the next 3 rows (CI of % of payments)
(pay_type4.df[10:11, 2:9] = round(pay_type4.df[10:11, 2:9],3))

pay_type4.df
str(pay_type4.df)

#using library scales to add % sign
pay_type5.df = pay_type4.df
pay_type5.df[7:11 , 2:9] = lapply(pay_type5.df[7:11 , 2:9], percent)
pay_type5.df
str(pay_type5.df)

# Add bottom row with p<0.01
(pval.vec = c("p<0.01", test_ip_w_pval_yn, test_not_ip_w_pval_yn,  test_bill_w_pval_yn, test_not_bill_w_pval_yn, test_ip_bill_w_pval_yn,  test_ip_not_bill_w_pval_yn, test_not_ip_bill_w_pval_yn,  test_not_ip_not_bill_w_pval_yn))
#
(pay_type6.df = rbind(pay_type5.df, pval.vec))

# Create a digits matrix: (rows + 0) × (columns + 1)
# First row of digits is for rownames
(digits_matrix = matrix(0, nrow = nrow(pay_type6.df)+ 0, ncol = ncol(pay_type6.df) + 1))
digits_matrix[1:6, 2:9] = 0
# Rows 7–8 get 1 digits (shifted by +1 because of the rownames)
digits_matrix[7:11, 2:9] = 2

digits_matrix

#print(xtable(pay_type5.df, digits=digits_matrix), include.rownames = F, hline.after = c(0,3,6))

print(xtable(pay_type6.df), include.rownames = F, hline.after = c(0,3,6))

# info for table 1:
nrow(m4.df)# num payments
length(unique(m4.df$id))# total num respondents
length(unique(rural1.df$id))
length(unique(mixed1.df$id))
length(unique(urban1.df$id))

#End: Table 1: Type of payments####

#++++++++++++++++++++++++++++++

#Begin: Table 2: Adoption of credit and debit cards####
# It is sufficient to use the indiv dataset only => more respondents, although more missing data.
names(i_6.df)
nrow(i_6.df)# num resp
length(unique(i_6.df$id))# num resp
i_7.df = i_6.df
# divide indiv resp by area
table(i_7.df$Area, useNA = "always")
i_7.df = subset(i_7.df, !is.na(Area))# remove 2 area NAs
table(i_7.df$Area, useNA = "always")
#
# rescale weights to equal num resp
nrow(i_7.df)
sum(i_7.df$ind_weight_all)
# new weights, call it wi before splitting into area
i_7.df$wi = nrow(i_7.df)*i_7.df$ind_weight_all/sum(i_7.df$ind_weight_all)
sum(i_7.df$wi)

# impute NAs for Own_credit, Own_debit, and Credit_reward (better to impute to preserve consistent sample size) => Don't impute, because it assigns all NAs to YES. Instead, display NAs
names(i_7.df)
table(i_7.df$Own_credit, useNA = "always")
table(i_7.df$Own_debit, useNA = "always")
table(i_7.df$Credit_reward, useNA = "always")
# imputation not needed: (num NAs are low! and mostly by the same resp)
# i_7.df = missRanger(data = i_7.df, formula = Own_credit ~ HH_income_cat + Age + Area + Gender + highest_education)
# i_7.df = missRanger(data = i_7.df, formula = Own_debit ~ HH_income_cat + Age + Area + Gender + highest_education)
# i_7.df = missRanger(data = i_7.df, formula = Credit_reward ~ HH_income_cat + Age + Area + Gender + highest_education)
# table(i_7.df$Own_credit, useNA = "always")
# table(i_7.df$Own_debit, useNA = "always")
# table(i_7.df$Credit_reward, useNA = "always")

# instead of imputation, delete the above NAs
dim(i_7.df)
i_8.df = subset(i_7.df, !is.na(Own_credit) & !is.na(Own_debit) & !is.na(Credit_reward) )
dim(i_8.df)

#splitting into areas
irural1.df = subset(i_8.df, Area == "Rural")
dim(irural1.df)
imixed1.df = subset(i_8.df, Area == "Mixed")
dim(imixed1.df)
iurban1.df = subset(i_8.df, Area == "Urban")
dim(iurban1.df)

# rural Own debit
table(irural1.df$Own_debit, useNA = "always")#Own debit
(irural_num_debit_yes = nrow(subset(irural1.df, Own_debit == "Yes")))
(irural_num_debit_no = nrow(subset(irural1.df, Own_debit == "No")))
(irural_num_debit_na = nrow(subset(irural1.df, is.na(Own_debit))))
(irural_frac_debit_yes = irural_num_debit_yes/nrow(irural1.df))
(irural_frac_debit_no = irural_num_debit_no/nrow(irural1.df))
(irural_frac_debit_na = irural_num_debit_na/nrow(irural1.df))
(irural_num_debit_yes_wi = round(sum(subset(irural1.df, Own_debit == "Yes")$wi)))#weighted
(irural_num_debit_no_wi = sum(subset(irural1.df, Own_debit == "No")$wi))
(irural_num_debit_na_wi = nrow(subset(irural1.df, is.na(Own_debit))))
(irural_frac_debit_yes_wi = irural_num_debit_yes_wi/sum(irural1.df$wi))
(irural_frac_debit_no_wi = irural_num_debit_no_wi/sum(irural1.df$wi))
(irural_frac_debit_na_wi = irural_num_debit_na_wi/sum(irural1.df$wi))

# rural Own credit
table(irural1.df$Own_credit, useNA = "always")#Own credit
(irural_num_credit_yes = nrow(subset(irural1.df, Own_credit == "Yes")))
(irural_num_credit_no = nrow(subset(irural1.df, Own_credit == "No")))
(irural_num_credit_na = nrow(subset(irural1.df, is.na(Own_credit))))
(irural_frac_credit_yes = irural_num_credit_yes/nrow(irural1.df))
(irural_frac_credit_no = irural_num_credit_no/nrow(irural1.df))
(irural_frac_credit_na = irural_num_credit_na/nrow(irural1.df))
(irural_num_credit_yes_wi = round(sum(subset(irural1.df, Own_credit == "Yes")$wi)))#weighted
(irural_num_credit_no_wi = sum(subset(irural1.df, Own_credit == "No")$wi))
(irural_num_credit_na_wi = nrow(subset(irural1.df, is.na(Own_credit))))
(irural_frac_credit_yes_wi = irural_num_credit_yes_wi/sum(irural1.df$wi))
(irural_frac_credit_no_wi = irural_num_credit_no_wi/sum(irural1.df$wi))
(irural_frac_credit_na_wi = irural_num_credit_na_wi/sum(irural1.df$wi))

# rural reward
table(irural1.df$Credit_reward, useNA = "always")#credit reward
(irural_num_reward_yes = nrow(subset(irural1.df, Credit_reward == "Yes")))
(irural_num_reward_no = nrow(subset(irural1.df, Credit_reward == "No")))
(irural_num_reward_na = nrow(subset(irural1.df, is.na(Credit_reward))))
(irural_frac_reward_yes = irural_num_reward_yes/nrow(irural1.df))
(irural_frac_reward_no = irural_num_reward_no/nrow(irural1.df))
(irural_frac_reward_na = irural_num_reward_na/nrow(irural1.df))
(irural_num_reward_yes_wi = round(sum(subset(irural1.df, Credit_reward == "Yes")$wi)))#weighted
(irural_num_reward_no_wi = sum(subset(irural1.df, Credit_reward == "No")$wi))
(irural_num_reward_na_wi = nrow(subset(irural1.df, is.na(Credit_reward))))
(irural_frac_reward_yes_wi = irural_num_reward_yes_wi/sum(irural1.df$wi))
(irural_frac_reward_no_wi = irural_num_reward_no_wi/sum(irural1.df$wi))
(irural_frac_reward_na_wi = irural_num_reward_na_wi/sum(irural1.df$wi))

# mixed Own debit
table(imixed1.df$Own_debit, useNA = "always")#Own debit
(imixed_num_debit_yes = nrow(subset(imixed1.df, Own_debit == "Yes")))
(imixed_num_debit_no = nrow(subset(imixed1.df, Own_debit == "No")))
(imixed_num_debit_na = nrow(subset(imixed1.df, is.na(Own_debit))))
(imixed_frac_debit_yes = imixed_num_debit_yes/nrow(imixed1.df))
(imixed_frac_debit_no = imixed_num_debit_no/nrow(imixed1.df))
(imixed_frac_debit_na = imixed_num_debit_na/nrow(imixed1.df))
(imixed_num_debit_yes_wi = round(sum(subset(imixed1.df, Own_debit == "Yes")$wi)))#weighted
(imixed_num_debit_no_wi = sum(subset(imixed1.df, Own_debit == "No")$wi))
(imixed_num_debit_na_wi = nrow(subset(imixed1.df, is.na(Own_debit))))
(imixed_frac_debit_yes_wi = imixed_num_debit_yes_wi/sum(imixed1.df$wi))
(imixed_frac_debit_no_wi = imixed_num_debit_no_wi/sum(imixed1.df$wi))
(imixed_frac_debit_na_wi = imixed_num_debit_na_wi/sum(imixed1.df$wi))

# mixed Own credit
table(imixed1.df$Own_credit, useNA = "always")#Own credit
(imixed_num_credit_yes = nrow(subset(imixed1.df, Own_credit == "Yes")))
(imixed_num_credit_no = nrow(subset(imixed1.df, Own_credit == "No")))
(imixed_num_credit_na = nrow(subset(imixed1.df, is.na(Own_credit))))
(imixed_frac_credit_yes = imixed_num_credit_yes/nrow(imixed1.df))
(imixed_frac_credit_no = imixed_num_credit_no/nrow(imixed1.df))
(imixed_frac_credit_na = imixed_num_credit_na/nrow(imixed1.df))
(imixed_num_credit_yes_wi = round(sum(subset(imixed1.df, Own_credit == "Yes")$wi)))#weighted
(imixed_num_credit_no_wi = sum(subset(imixed1.df, Own_credit == "No")$wi))
(imixed_num_credit_na_wi = nrow(subset(imixed1.df, is.na(Own_credit))))
(imixed_frac_credit_yes_wi = imixed_num_credit_yes_wi/sum(imixed1.df$wi))
(imixed_frac_credit_no_wi = imixed_num_credit_no_wi/sum(imixed1.df$wi))
(imixed_frac_credit_na_wi = imixed_num_credit_na_wi/sum(imixed1.df$wi))

# mixed reward
table(imixed1.df$Credit_reward, useNA = "always")#credit reward
(imixed_num_reward_yes = nrow(subset(imixed1.df, Credit_reward == "Yes")))
(imixed_num_reward_no = nrow(subset(imixed1.df, Credit_reward == "No")))
(imixed_num_reward_na = nrow(subset(imixed1.df, is.na(Credit_reward))))
(imixed_frac_reward_yes = imixed_num_reward_yes/nrow(imixed1.df))
(imixed_frac_reward_no = imixed_num_reward_no/nrow(imixed1.df))
(imixed_frac_reward_na = imixed_num_reward_na/nrow(imixed1.df))
(imixed_num_reward_yes_wi = round(sum(subset(imixed1.df, Credit_reward == "Yes")$wi)))#weighted
(imixed_num_reward_no_wi = sum(subset(imixed1.df, Credit_reward == "No")$wi))
(imixed_num_reward_na_wi = nrow(subset(imixed1.df, is.na(Credit_reward))))
(imixed_frac_reward_yes_wi = imixed_num_reward_yes_wi/sum(imixed1.df$wi))
(imixed_frac_reward_no_wi = imixed_num_reward_no_wi/sum(imixed1.df$wi))
(imixed_frac_reward_na_wi = imixed_num_reward_na_wi/sum(imixed1.df$wi))

# urban Own debit
table(iurban1.df$Own_debit, useNA = "always")#Own debit
(iurban_num_debit_yes = nrow(subset(iurban1.df, Own_debit == "Yes")))
(iurban_num_debit_no = nrow(subset(iurban1.df, Own_debit == "No")))
(iurban_num_debit_na = nrow(subset(iurban1.df, is.na(Own_debit))))
(iurban_frac_debit_yes = iurban_num_debit_yes/nrow(iurban1.df))
(iurban_frac_debit_no = iurban_num_debit_no/nrow(iurban1.df))
(iurban_frac_debit_na = iurban_num_debit_na/nrow(iurban1.df))
(iurban_num_debit_yes_wi = round(sum(subset(iurban1.df, Own_debit == "Yes")$wi)))#weighted
(iurban_num_debit_no_wi = sum(subset(iurban1.df, Own_debit == "No")$wi))
(iurban_num_debit_na_wi = nrow(subset(iurban1.df, is.na(Own_debit))))
(iurban_frac_debit_yes_wi = iurban_num_debit_yes_wi/sum(iurban1.df$wi))
(iurban_frac_debit_no_wi = iurban_num_debit_no_wi/sum(iurban1.df$wi))
(iurban_frac_debit_na_wi = iurban_num_debit_na_wi/sum(iurban1.df$wi))

# urban Own credit
table(iurban1.df$Own_credit, useNA = "always")#Own credit
(iurban_num_credit_yes = nrow(subset(iurban1.df, Own_credit == "Yes")))
(iurban_num_credit_no = nrow(subset(iurban1.df, Own_credit == "No")))
(iurban_num_credit_na = nrow(subset(iurban1.df, is.na(Own_credit))))
(iurban_frac_credit_yes = iurban_num_credit_yes/nrow(iurban1.df))
(iurban_frac_credit_no = iurban_num_credit_no/nrow(iurban1.df))
(iurban_frac_credit_na = iurban_num_credit_na/nrow(iurban1.df))
(iurban_num_credit_yes_wi = round(sum(subset(iurban1.df, Own_credit == "Yes")$wi)))#weighted
(iurban_num_credit_no_wi = sum(subset(iurban1.df, Own_credit == "No")$wi))
(iurban_num_credit_na_wi = nrow(subset(iurban1.df, is.na(Own_credit))))
(iurban_frac_credit_yes_wi = iurban_num_credit_yes_wi/sum(iurban1.df$wi))
(iurban_frac_credit_no_wi = iurban_num_credit_no_wi/sum(iurban1.df$wi))
(iurban_frac_credit_na_wi = iurban_num_credit_na_wi/sum(iurban1.df$wi))

# urban reward
table(iurban1.df$Credit_reward, useNA = "always")#credit reward
(iurban_num_reward_yes = nrow(subset(iurban1.df, Credit_reward == "Yes")))
(iurban_num_reward_no = nrow(subset(iurban1.df, Credit_reward == "No")))
(iurban_num_reward_na = nrow(subset(iurban1.df, is.na(Credit_reward))))
(iurban_frac_reward_yes = iurban_num_reward_yes/nrow(iurban1.df))
(iurban_frac_reward_no = iurban_num_reward_no/nrow(iurban1.df))
(iurban_frac_reward_na = iurban_num_reward_na/nrow(iurban1.df))
(iurban_num_reward_yes_wi = round(sum(subset(iurban1.df, Credit_reward == "Yes")$wi)))#weighted
(iurban_num_reward_no_wi = sum(subset(iurban1.df, Credit_reward == "No")$wi))
(iurban_num_reward_na_wi = nrow(subset(iurban1.df, is.na(Credit_reward))))
(iurban_frac_reward_yes_wi = iurban_num_reward_yes_wi/sum(iurban1.df$wi))
(iurban_frac_reward_no_wi = iurban_num_reward_no_wi/sum(iurban1.df$wi))
(iurban_frac_reward_na_wi = iurban_num_reward_na_wi/sum(iurban1.df$wi))

# prepare vector columns for data frame
(debit_yes.vec = c(irural_num_debit_yes, imixed_num_debit_yes, iurban_num_debit_yes, c(irural_num_debit_yes_wi, imixed_num_debit_yes_wi, iurban_num_debit_yes_wi), irural_frac_debit_yes_wi, imixed_frac_debit_yes_wi, iurban_frac_debit_yes_wi))
#
(debit_no.vec = c(irural_num_debit_no, imixed_num_debit_no, iurban_num_debit_no, irural_num_debit_no_wi, imixed_num_debit_no_wi, iurban_num_debit_no_wi, irural_frac_debit_no_wi, imixed_frac_debit_no_wi, iurban_frac_debit_no_wi))
#
(debit_na.vec = c(irural_num_debit_na, imixed_num_debit_na, iurban_num_debit_na, irural_num_debit_na_wi, imixed_num_debit_na_wi, iurban_num_debit_na_wi, irural_frac_debit_na_wi, imixed_frac_debit_na_wi, iurban_frac_debit_na_wi))
#verify sum of debit columns
debit_yes.vec + debit_no.vec + debit_na.vec
c(nrow(irural1.df), nrow(imixed1.df), nrow(iurban1.df))
#
(credit_yes.vec = c(irural_num_credit_yes, imixed_num_credit_yes, iurban_num_credit_yes, irural_num_credit_yes_wi, imixed_num_credit_yes_wi, iurban_num_credit_yes_wi, irural_frac_credit_yes_wi, imixed_frac_credit_yes_wi, iurban_frac_credit_yes_wi))
#
(credit_no.vec = c(irural_num_credit_no, imixed_num_credit_no, iurban_num_credit_no, irural_num_credit_no_wi, imixed_num_credit_no_wi, iurban_num_credit_no_wi, irural_frac_credit_no_wi, imixed_frac_credit_no_wi, iurban_frac_credit_no_wi))
#
(credit_na.vec = c(irural_num_credit_na, imixed_num_credit_na, iurban_num_credit_na, irural_num_credit_na_wi, imixed_num_credit_na_wi, iurban_num_credit_na_wi, irural_frac_credit_na_wi, imixed_frac_credit_na_wi, iurban_frac_credit_na_wi))
#verify sum of credit columns
credit_yes.vec + credit_no.vec + credit_na.vec
c(nrow(irural1.df), nrow(imixed1.df), nrow(iurban1.df))
#
(reward_yes.vec = c(irural_num_reward_yes, imixed_num_reward_yes, iurban_num_reward_yes, irural_num_reward_yes_wi, imixed_num_reward_yes_wi, iurban_num_reward_yes_wi, irural_frac_reward_yes_wi, imixed_frac_reward_yes_wi, iurban_frac_reward_yes_wi))
#
(reward_no.vec = c(irural_num_reward_no, imixed_num_reward_no, iurban_num_reward_no, irural_num_reward_no_wi, imixed_num_reward_no_wi, iurban_num_reward_no_wi, irural_frac_reward_no_wi, imixed_frac_reward_no_wi, iurban_frac_reward_no_wi))
#
(reward_na.vec = c(irural_num_reward_na, imixed_num_reward_na, iurban_num_reward_na, irural_num_reward_na_wi, imixed_num_reward_na_wi, iurban_num_reward_na_wi, irural_frac_reward_na_wi, imixed_frac_reward_na_wi, iurban_frac_reward_na_wi))
#verify sum of reward columns
reward_yes.vec + reward_no.vec + reward_na.vec
c(nrow(irural1.df), nrow(imixed1.df), nrow(iurban1.df))

# make it data frame
#(adopt1.df = data.frame(debit_yes.vec, debit_no.vec, debit_na.vec, credit_yes.vec, credit_no.vec, credit_na.vec, reward_yes.vec, reward_no.vec, reward_na.vec))# Don't display NAs
(adopt1.df = data.frame(debit_yes.vec, debit_no.vec, credit_yes.vec, credit_no.vec, reward_yes.vec, reward_no.vec))

#p-values for z-test percentages comparing rural with urban only
#comparing FRAC IN-PERSON rural vs. urban => sig
# two-proportion z-test (pooled)
irural_num_debit_yes_wi# rural debit yes
iurban_num_debit_yes_wi# urban debit yes
sum(irural1.df$wi)# total rural
sum(iurban1.df$wi)# total urban
test_debit_yes_wi =prop.test(x = c(irural_num_debit_yes_wi, iurban_num_debit_yes_wi),
  n = c(sum(irural1.df$wi), sum(iurban1.df$wi)), correct = FALSE, conf.level = 0.99)

(test_debit_yes_wi_pval = test_debit_yes_wi$p.value)
(test_debit_yes_wi_pval_yn = ifelse(test_debit_yes_wi$p.value < 0.01, "Yes", "No"))
(test_debit_yes_wi_ci = test_debit_yes_wi$conf.int)
(test_debit_yes_wi_ci_lower = test_debit_yes_wi$conf.int[1])
(test_debit_yes_wi_ci_upper = test_debit_yes_wi$conf.int[2])
# Append the above to columns w/o the yn (chr)
debit_yes.vec
(debit_yes2.vec = as.numeric(c(debit_yes.vec, test_debit_yes_wi_ci_lower, test_debit_yes_wi_ci_upper)))

#
irural_num_debit_no_wi# rural debit no
iurban_num_debit_no_wi# urban debit no
sum(irural1.df$wi)# total rural
sum(iurban1.df$wi)# total urban
test_debit_no_wi =prop.test(x = c(irural_num_debit_no_wi, iurban_num_debit_no_wi),
  n = c(sum(irural1.df$wi), sum(iurban1.df$wi)), correct = FALSE, conf.level = 0.99)
(test_debit_no_wi_pval = test_debit_no_wi$p.value)
(test_debit_no_wi_pval_yn = ifelse(test_debit_no_wi$p.value < 0.01, "Yes", "No"))
(test_debit_no_wi_ci = test_debit_no_wi$conf.int)
(test_debit_no_wi_ci_lower = test_debit_no_wi$conf.int[1])
(test_debit_no_wi_ci_upper = test_debit_no_wi$conf.int[2])
# Append the above to columns w/o the yn (chr)
debit_no.vec
(debit_no2.vec = as.numeric(c(debit_no.vec, test_debit_no_wi_ci_lower, test_debit_no_wi_ci_upper)))

#
irural_num_credit_yes_wi# rural credit yes
iurban_num_credit_yes_wi# urban credit yes
sum(irural1.df$wi)# total rural
sum(iurban1.df$wi)# total urban
test_credit_yes_wi =prop.test(x = c(irural_num_credit_yes_wi, iurban_num_credit_yes_wi),
  n = c(sum(irural1.df$wi), sum(iurban1.df$wi)), correct = FALSE, conf.level = 0.99)

(test_credit_yes_wi_pval = test_credit_yes_wi$p.value)
(test_credit_yes_wi_pval_yn = ifelse(test_credit_yes_wi$p.value < 0.01, "Yes", "No"))
(test_credit_yes_wi_ci = test_credit_yes_wi$conf.int)
(test_credit_yes_wi_ci_lower = test_credit_yes_wi$conf.int[1])
(test_credit_yes_wi_ci_upper = test_credit_yes_wi$conf.int[2])
# Append the above to columns w/o the yn (chr)
credit_yes.vec
(credit_yes2.vec = as.numeric(c(credit_yes.vec, test_credit_yes_wi_ci_lower, test_credit_yes_wi_ci_upper)))

#
irural_num_credit_no_wi# rural credit no
iurban_num_credit_no_wi# urban credit no
sum(irural1.df$wi)# total rural
sum(iurban1.df$wi)# total urban
test_credit_no_wi =prop.test(x = c(irural_num_credit_no_wi, iurban_num_credit_no_wi),
  n = c(sum(irural1.df$wi), sum(iurban1.df$wi)), correct = FALSE, conf.level = 0.99)
(test_credit_no_wi_pval = test_credit_no_wi$p.value)
(test_credit_no_wi_pval_yn = ifelse(test_credit_no_wi$p.value < 0.01, "Yes", "No"))
(test_credit_no_wi_ci = test_credit_no_wi$conf.int)
(test_credit_no_wi_ci_lower = test_credit_no_wi$conf.int[1])
(test_credit_no_wi_ci_upper = test_credit_no_wi$conf.int[2])
# Append the above to columns w/o the yn (chr)
credit_no.vec
(credit_no2.vec = as.numeric(c(credit_no.vec, test_credit_no_wi_ci_lower, test_credit_no_wi_ci_upper)))

#
irural_num_reward_yes_wi# rural reward yes
iurban_num_reward_yes_wi# urban reward yes
sum(irural1.df$wi)# total rural
sum(iurban1.df$wi)# total urban
(test_reward_yes_wi =prop.test(x = c(irural_num_reward_yes_wi, iurban_num_reward_yes_wi),
  n = c(sum(irural1.df$wi), sum(iurban1.df$wi)), correct = FALSE, conf.level = 0.99))
(test_reward_yes_wi_pval = test_reward_yes_wi$p.value)
(test_reward_yes_wi_pval_yn = ifelse(test_reward_yes_wi$p.value < 0.01, "Yes", "No"))
(test_reward_yes_wi_ci = test_reward_yes_wi$conf.int)
(test_reward_yes_wi_ci_lower = test_reward_yes_wi$conf.int[1])
(test_reward_yes_wi_ci_upper = test_reward_yes_wi$conf.int[2])
# Append the above to columns w/o the yn (chr)
reward_yes.vec
(reward_yes2.vec = as.numeric(c(reward_yes.vec, test_reward_yes_wi_ci_lower, test_reward_yes_wi_ci_upper)))
#
irural_num_reward_no_wi# rural reward no
iurban_num_reward_no_wi# urban reward no
sum(irural1.df$wi)# total rural
sum(iurban1.df$wi)# total urban
test_reward_no_wi = prop.test(x = c(irural_num_reward_no_wi, iurban_num_reward_no_wi),
  n = c(sum(irural1.df$wi), sum(iurban1.df$wi)), correct = FALSE, conf.level = 0.99)
(test_reward_no_wi_pval = test_reward_no_wi$p.value)
(test_reward_no_wi_pval_yn = ifelse(test_reward_no_wi$p.value < 0.01, "Yes", "No"))
(test_reward_no_wi_ci = test_reward_no_wi$conf.int)
(test_reward_no_wi_ci_lower = test_reward_no_wi$conf.int[1])
(test_reward_no_wi_ci_upper = test_reward_no_wi$conf.int[2])
# Append the above to columns w/o the yn (chr)
reward_no.vec
(reward_no2.vec = as.numeric(c(reward_no.vec, test_reward_no_wi_ci_lower, test_reward_no_wi_ci_upper)))

# add test to adopt1.df (rewrite)
adopt1.df
(adopt2.df = data.frame(debit_yes2.vec, debit_no2.vec, credit_yes2.vec, credit_no2.vec, reward_yes2.vec, reward_no2.vec))
str(adopt2.df)

# round
(adopt3.df =adopt2.df)
dim(adopt3.df)
str(adopt3.df)
# round the 3 rows (weighed number of payments)
(adopt3.df[4:6, ] = round(adopt3.df[4:6, ],0))

#using library scales to add % sign
adopt4.df = adopt3.df
dim(adopt4.df)
adopt4.df[7:11, ] = lapply(adopt4.df[7:11, ], percent)
adopt4.df
str(adopt4.df)

# add 1st column, recall
area.vec
(adopt5.df = cbind(area.vec, adopt4.df))

# Add bottom row with p<0.01
(adopt_pval.vec = c("p<0.01", test_debit_yes_wi_pval_yn, test_debit_no_wi_pval_yn, test_credit_yes_wi_pval_yn, test_credit_no_wi_pval_yn, test_reward_yes_wi_pval_yn, test_reward_no_wi_pval_yn ))
#
length(adopt_pval.vec)
dim(adopt5.df)
(adopt6.df = rbind(adopt5.df, adopt_pval.vec))

print(xtable(adopt6.df), include.rownames = F, hline.after = c(0,3,6,9))

# info for Table 2 (notes)
nrow(i_8.df)# num resp
nrow(irural1.df)# num rural respondents
sum(irural1.df$wi) # weighted num respondents
nrow(imixed1.df)# num mixed respondents
sum(imixed1.df$wi) # weighted num respondents
nrow(iurban1.df)# num urban respondents
nrow(irural1.df) + nrow(imixed1.df) + nrow(iurban1.df)# verify sum

#End: Table 2: Adoption of credit and debit cards####

#++++++++++++++++++++++++++++++

#Begin: Table 3: Adoption regressions####

names(i_8.df)# count missing obs
sum(is.na(i_8.df$Area))
sum(is.na(i_8.df$HH_income))
with(i_8.df, table(is.na(HH_income), Area))# missing HH_income by Area => more missing in Urban, but numbers are small
sum(is.na(i_8.df$Age))
sum(is.na(i_8.df$Gender))
sum(is.na(i_8.df$Race))
sum(is.na(i_8.df$Hispanic))
sum(is.na(i_8.df$Education))
sum(is.na(i_8.df$HH_size))
sum(is.na(i_8.df$Own_debit))#dependent var
with(i_8.df, table(is.na(Own_debit), Area))# evenly across areas (small)
sum(is.na(i_8.df$Own_credit))#dependent var
with(i_8.df, table(is.na(Own_credit), Area))# evenly across areas (small)
sum(is.na(i_8.df$Credit_reward))#dependent var (large number)
with(i_8.df, table(is.na(Credit_reward), Area))# relatively more missing @ rural (large)
table(i_8.df$Race, useNA = "always")
table(i_8.df$Hispanic, useNA = "always")

#need to rename dependent var to 0-1
i_7_reg.df = i_8.df
table(i_7_reg.df$Own_debit, useNA = "always")
i_7_reg.df = i_7_reg.df %>% mutate(Own_debit = case_when(
  Own_debit ==  "No" ~ 0,
  Own_debit ==  "Yes" ~ 1
  ))
table(i_7_reg.df$Own_debit, useNA = "always")
#
table(i_7_reg.df$Own_credit, useNA = "always")
i_7_reg.df = i_7_reg.df %>% mutate(Own_credit = case_when(
  Own_credit ==  "No" ~ 0,
  Own_credit ==  "Yes" ~ 1
))
table(i_7_reg.df$Own_credit, useNA = "always")
#
table(i_7_reg.df$Credit_reward, useNA = "always")
i_7_reg.df = i_7_reg.df %>% mutate(Credit_reward = case_when(
  Credit_reward ==  "No" ~ 0,
  Credit_reward ==  "Yes" ~ 1
))
table(i_7_reg.df$Credit_reward, useNA = "always")
#
# Construct a new variable HH_income_10k measuring incomes in $10k units
i_7_reg.df$HH_income_10k = as.numeric(i_7_reg.df$HH_income/10000)
summary(i_7_reg.df$HH_income_10k)

# verify factor variables
str(i_7_reg.df)
i_7_reg.df$Age = as.integer(i_7_reg.df$Age)
i_7_reg.df$Own_credit = as.factor(i_7_reg.df$Own_credit)
i_7_reg.df$Own_debit = as.factor(i_7_reg.df$Own_debit)
i_7_reg.df$Credit_reward = as.factor(i_7_reg.df$Credit_reward)
i_7_reg.df$Gender = as.factor(i_7_reg.df$Gender)
i_7_reg.df$Race = as.factor(i_7_reg.df$Race)
i_7_reg.df$Hispanic = as.factor(i_7_reg.df$Hispanic)
i_7_reg.df$HH_size = as.integer(i_7_reg.df$HH_size)

# 3 logit regression models: Own debit, own credit, own reward
debit.model = Own_debit ~ Area + HH_income_10k + Age +Gender + Education + Race + Hispanic 
#+ HH_size not used, not sig
#
credit.model = Own_credit ~ Area + HH_income_10k + Age +Gender + Education + Race + Hispanic 
#+ HH_size not used, not sig
#
reward.model = Credit_reward ~ Area + HH_income_10k + Age +Gender + Education + Race + Hispanic 
#+ HH_size not used, not sig

#Own debit regression R mfx yielding 0,1 warning
(debit.reg = logitmfx(debit.model, data = i_7_reg.df, atmean = F))
#Own credit regression R mfx yielding 0,1 warning
(credit.reg = logitmfx(credit.model, data = i_7_reg.df, atmean = F))
#Own credit regression R mfx yielding 0,1 warning
(reward.reg = logitmfx(reward.model, data = i_7_reg.df, atmean = F))

#construct a LaTeX table
screenreg(list(debit.reg, credit.reg, reward.reg), digits = 3,
          stars  = c(0.001, 0.01, 0.05, 0.10),  # add 10% cutoff
          symbol = ".",
          custom.model.names = c("Own debit", "Own credit", "Credit reward"))#texreg package => screen
#
texreg(list(debit.reg, credit.reg, reward.reg), digits = 3,
       stars  = c(0.001, 0.01, 0.05, 0.10),  # add 10% cutoff
       symbol = ".",
       custom.model.names = c("Own debit", "Own credit", "Credit reward"))#texreg package => LaTeX

# Reference variables (factor variables)
levels(i_7_reg.df$Area)
levels(i_7_reg.df$Gender)
levels(i_7_reg.df$Education)

#End: Table 3: Adoption regressions####

#++++++++++++++++++++++++++++++

#Begin: Table 4: All payments####
m5.df = m4.df
names(m5.df)
# verify weights sum to num payments
nrow(m5.df)# num payments
sum(m5.df$w)

# PI renaming 
table(m5.df$pi, useNA = "always")
round(prop.table(table(m5.df$pi, useNA = "always")),2)

m5.df$pi = as.numeric(m5.df$pi)
m5.df = m5.df %>% mutate(pi = ifelse(pi==0, "Other", pi))#combination
m5.df = m5.df %>% mutate(pi = ifelse(pi==1, "Cash", pi))
m5.df = m5.df %>% mutate(pi = ifelse(pi==2, "Check", pi))
m5.df = m5.df %>% mutate(pi = ifelse(pi==3, "Credit", pi))
m5.df = m5.df %>% mutate(pi = ifelse(pi==4, "Debit", pi))
m5.df = m5.df %>% mutate(pi = ifelse(pi==5, "Prepaid", pi))
m5.df = m5.df %>% mutate(pi = ifelse(pi==6, "Bank", pi))#acc num
m5.df = m5.df %>% mutate(pi = ifelse(pi==7, "Bank", pi))#Online bill
m5.df = m5.df %>% mutate(pi = ifelse(pi==8, "Other", pi))#money order
m5.df = m5.df %>% mutate(pi = ifelse(pi==10, "Mobile", pi))
m5.df = m5.df %>% mutate(pi = ifelse(pi==11, "Bank", pi))#A2A
m5.df = m5.df %>% mutate(pi = ifelse(pi==13, "Other", pi))#other
m5.df = m5.df %>% mutate(pi = ifelse(pi==14, "Other", pi))#income deduction
#
round(prop.table(table(m5.df$pi, useNA = "always")),2)
m5.df$pi = as.factor(m5.df$pi)
levels(m5.df$pi)
m5.df$pi = factor(m5.df$pi, levels = c("Cash", "Credit", "Debit", "Prepaid", "Check", "Mobile", "Bank", "Other" ))
levels(m5.df$pi)
table(m5.df$pi)

# split payments by area
names(m5.df)
table(m5.df$Area)
rural5.df = subset(m5.df, Area == "Rural")
mixed5.df = subset(m5.df, Area == "Mixed")
urban5.df = subset(m5.df, Area == "Urban")

# num payments by PI by area
(rural_num.vec = unname(table(rural5.df$pi)))#unname the table
(mixed_num.vec = unname(table(mixed5.df$pi)))#unname the table
(urban_num.vec = unname(table(urban5.df$pi)))#unname the table

# frac payments by PI by area
(rural_frac.vec = rural_num.vec/nrow(rural5.df) )
(mixed_frac.vec = mixed_num.vec/nrow(mixed5.df) )
(urban_frac.vec = urban_num.vec/nrow(urban5.df) )
# verify sum to 1
sum(rural_frac.vec)
sum(mixed_frac.vec)
sum(urban_frac.vec)

# weighted num payments rural
table(rural5.df$pi, useNA = "always")
(rural_num_cash_w = sum(subset(rural5.df, pi=="Cash")$w))
(rural_num_credit_w = sum(subset(rural5.df, pi=="Credit")$w))
(rural_num_debit_w = sum(subset(rural5.df, pi=="Debit")$w))
(rural_num_prepaid_w = sum(subset(rural5.df, pi=="Prepaid")$w))
(rural_num_check_w = sum(subset(rural5.df, pi=="Check")$w))
(rural_num_mobile_w = sum(subset(rural5.df, pi=="Mobile")$w))
(rural_num_bank_w = sum(subset(rural5.df, pi=="Bank")$w))
(rural_num_other_w = sum(subset(rural5.df, pi=="Other")$w))
#Verify sum
sum(rural5.df$w)
rural_num_cash_w + rural_num_credit_w +rural_num_debit_w +rural_num_prepaid_w + rural_num_check_w +rural_num_mobile_w +rural_num_bank_w +rural_num_other_w 

# weighted fraction payments rural
table(rural5.df$pi)
(rural_frac_cash_w = sum(subset(rural5.df, pi=="Cash")$w)/sum(rural5.df$w))
(rural_frac_credit_w = sum(subset(rural5.df, pi=="Credit")$w)/sum(rural5.df$w))
(rural_frac_debit_w = sum(subset(rural5.df, pi=="Debit")$w)/sum(rural5.df$w))
(rural_frac_prepaid_w = sum(subset(rural5.df, pi=="Prepaid")$w)/sum(rural5.df$w))
(rural_frac_check_w = sum(subset(rural5.df, pi=="Check")$w)/sum(rural5.df$w))
(rural_frac_mobile_w = sum(subset(rural5.df, pi=="Mobile")$w)/sum(rural5.df$w))
(rural_frac_bank_w = sum(subset(rural5.df, pi=="Bank")$w)/sum(rural5.df$w))
(rural_frac_other_w = sum(subset(rural5.df, pi=="Other")$w)/sum(rural5.df$w))
# verify sum up to 1
rural_frac_cash_w +rural_frac_credit_w +rural_frac_debit_w +rural_frac_prepaid_w+rural_frac_check_w +rural_frac_mobile_w +rural_frac_bank_w +rural_frac_other_w

# weighted num payments mixed
table(mixed5.df$pi, useNA = "always")
(mixed_num_cash_w = sum(subset(mixed5.df, pi=="Cash")$w))
(mixed_num_credit_w = sum(subset(mixed5.df, pi=="Credit")$w))
(mixed_num_debit_w = sum(subset(mixed5.df, pi=="Debit")$w))
(mixed_num_prepaid_w = sum(subset(mixed5.df, pi=="Prepaid")$w))
(mixed_num_check_w = sum(subset(mixed5.df, pi=="Check")$w))
(mixed_num_mobile_w = sum(subset(mixed5.df, pi=="Mobile")$w))
(mixed_num_bank_w = sum(subset(mixed5.df, pi=="Bank")$w))
(mixed_num_other_w = sum(subset(mixed5.df, pi=="Other")$w))
#Verify sum
sum(mixed5.df$w)
mixed_num_cash_w + mixed_num_credit_w +mixed_num_debit_w +mixed_num_prepaid_w + mixed_num_check_w +mixed_num_mobile_w +mixed_num_bank_w +mixed_num_other_w 

# weighted fraction payments mixed
table(mixed5.df$pi)
(mixed_frac_cash_w = sum(subset(mixed5.df, pi=="Cash")$w)/sum(mixed5.df$w))
(mixed_frac_credit_w = sum(subset(mixed5.df, pi=="Credit")$w)/sum(mixed5.df$w))
(mixed_frac_debit_w = sum(subset(mixed5.df, pi=="Debit")$w)/sum(mixed5.df$w))
(mixed_frac_prepaid_w = sum(subset(mixed5.df, pi=="Prepaid")$w)/sum(mixed5.df$w))
(mixed_frac_check_w = sum(subset(mixed5.df, pi=="Check")$w)/sum(mixed5.df$w))
(mixed_frac_mobile_w = sum(subset(mixed5.df, pi=="Mobile")$w)/sum(mixed5.df$w))
(mixed_frac_bank_w = sum(subset(mixed5.df, pi=="Bank")$w)/sum(mixed5.df$w))
(mixed_frac_other_w = sum(subset(mixed5.df, pi=="Other")$w)/sum(mixed5.df$w))
# verify sum up to 1
mixed_frac_cash_w +mixed_frac_credit_w +mixed_frac_debit_w +mixed_frac_prepaid_w+mixed_frac_check_w +mixed_frac_mobile_w +mixed_frac_bank_w +mixed_frac_other_w

# weighted num payments urban
table(urban5.df$pi, useNA = "always")
(urban_num_cash_w = sum(subset(urban5.df, pi=="Cash")$w))
(urban_num_credit_w = sum(subset(urban5.df, pi=="Credit")$w))
(urban_num_debit_w = sum(subset(urban5.df, pi=="Debit")$w))
(urban_num_prepaid_w = sum(subset(urban5.df, pi=="Prepaid")$w))
(urban_num_check_w = sum(subset(urban5.df, pi=="Check")$w))
(urban_num_mobile_w = sum(subset(urban5.df, pi=="Mobile")$w))
(urban_num_bank_w = sum(subset(urban5.df, pi=="Bank")$w))
(urban_num_other_w = sum(subset(urban5.df, pi=="Other")$w))
#Verify sum
sum(urban5.df$w)
urban_num_cash_w + urban_num_credit_w +urban_num_debit_w +urban_num_prepaid_w + urban_num_check_w +urban_num_mobile_w +urban_num_bank_w +urban_num_other_w 

# weighted fraction payments urban
table(urban5.df$pi)
(urban_frac_cash_w = sum(subset(urban5.df, pi=="Cash")$w)/sum(urban5.df$w))
(urban_frac_credit_w = sum(subset(urban5.df, pi=="Credit")$w)/sum(urban5.df$w))
(urban_frac_debit_w = sum(subset(urban5.df, pi=="Debit")$w)/sum(urban5.df$w))
(urban_frac_prepaid_w = sum(subset(urban5.df, pi=="Prepaid")$w)/sum(urban5.df$w))
(urban_frac_check_w = sum(subset(urban5.df, pi=="Check")$w)/sum(urban5.df$w))
(urban_frac_mobile_w = sum(subset(urban5.df, pi=="Mobile")$w)/sum(urban5.df$w))
(urban_frac_bank_w = sum(subset(urban5.df, pi=="Bank")$w)/sum(urban5.df$w))
(urban_frac_other_w = sum(subset(urban5.df, pi=="Other")$w)/sum(urban5.df$w))
# verify sum up to 1
urban_frac_cash_w +urban_frac_credit_w +urban_frac_debit_w +urban_frac_prepaid_w+urban_frac_check_w +urban_frac_mobile_w +urban_frac_bank_w +urban_frac_other_w

# construct column vectors for data frame
(cash.vec = c(rural_num.vec[1], mixed_num.vec[1], urban_num.vec[1], rural_num_cash_w, mixed_num_cash_w, urban_num_cash_w, rural_frac_cash_w, mixed_frac_cash_w, urban_frac_cash_w))
#
(credit.vec = c(rural_num.vec[2], mixed_num.vec[2], urban_num.vec[2], rural_num_credit_w, mixed_num_credit_w, urban_num_credit_w, rural_frac_credit_w, mixed_frac_credit_w, urban_frac_credit_w))
#
(debit.vec = c(rural_num.vec[3], mixed_num.vec[3], urban_num.vec[3], rural_num_debit_w, mixed_num_debit_w, urban_num_debit_w, rural_frac_debit_w, mixed_frac_debit_w, urban_frac_debit_w))
#
(prepaid.vec = c(rural_num.vec[4], mixed_num.vec[4], urban_num.vec[4], rural_num_prepaid_w, mixed_num_prepaid_w, urban_num_prepaid_w, rural_frac_prepaid_w, mixed_frac_prepaid_w, urban_frac_prepaid_w))
#
(check.vec = c(rural_num.vec[5], mixed_num.vec[5], urban_num.vec[5], rural_num_check_w, mixed_num_check_w, urban_num_check_w, rural_frac_check_w, mixed_frac_check_w, urban_frac_check_w))
#
(mobile.vec = c(rural_num.vec[6], mixed_num.vec[6], urban_num.vec[6], rural_num_mobile_w, mixed_num_mobile_w, urban_num_mobile_w, rural_frac_mobile_w, mixed_frac_mobile_w, urban_frac_mobile_w))
#
(bank.vec = c(rural_num.vec[7], mixed_num.vec[7], urban_num.vec[7], rural_num_bank_w, mixed_num_bank_w, urban_num_bank_w, rural_frac_bank_w, mixed_frac_bank_w, urban_frac_bank_w))
#
(other.vec = c(rural_num.vec[8], mixed_num.vec[8], urban_num.vec[8], rural_num_other_w, mixed_num_other_w, urban_num_other_w, rural_frac_other_w, mixed_frac_other_w, urban_frac_other_w))
#
# verify sum
cash.vec + credit.vec + debit.vec + prepaid.vec + check.vec + mobile.vec +bank.vec + other.vec
nrow(rural1.df)
nrow(mixed1.df)
nrow(urban1.df)
sum(rural1.df$w)
sum(mixed1.df$w)
sum(urban1.df$w)

# Make it a data frame
(area.vec = c("Rural", "Mixed", "Urban","Rural", "Mixed", "Urban","Rural", "Mixed", "Urban"))
#
(pay1.df = data.frame(Area = area.vec, Cash = cash.vec, Credit = credit.vec, Debit = debit.vec, Prepaid = prepaid.vec, Check = check.vec, Mobile = mobile.vec, Bank=bank.vec, Other = other.vec))
dim(pay1.df)

#p-values for z-test percentages comparing rural with urban only
#comparing rural vs. urban => sig
# two-proportion z-test (pooled)
# cash difference
rural_num_cash_w# rural cash payments
urban_num_cash_w# urban cash payments
sum(rural1.df$w)# total rural payments
sum(urban1.df$w)# total urban payments
(test_cash_w = prop.test(x = c(rural_num_cash_w, urban_num_cash_w), n = c(sum(rural1.df$w), sum(urban1.df$w)), correct = FALSE, conf.level = 0.99))
(test_cash_w_pval = test_cash_w$p.value)
(test_cash_w_pval_yn = ifelse(test_cash_w$p.value < 0.01, "Yes", "No"))
(test_cash_w_ci = test_cash_w$conf.int)
(test_cash_w_ci_lower = test_cash_w$conf.int[1])
(test_cash_w_ci_upper = test_cash_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
cash.vec
(cash2.vec = as.numeric(c(cash.vec, test_cash_w_ci_lower, test_cash_w_ci_upper)))

# credit difference
rural_num_credit_w# rural credit payments
urban_num_credit_w# urban credit payments
sum(rural1.df$w)# total rural payments
sum(urban1.df$w)# total urban payments
(test_credit_w =prop.test(x = c(rural_num_credit_w, urban_num_credit_w), n = c(sum(rural1.df$w), sum(urban1.df$w)), correct = FALSE, conf.level = 0.99))
(test_credit_w_pval = test_credit_w$p.value)
(test_credit_w_pval_yn = ifelse(test_credit_w$p.value < 0.01, "Yes", "No"))
(test_credit_w_ci = test_credit_w$conf.int)
(test_credit_w_ci_lower = test_credit_w$conf.int[1])
(test_credit_w_ci_upper = test_credit_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
credit.vec
(credit2.vec = as.numeric(c(credit.vec, test_credit_w_ci_lower, test_credit_w_ci_upper)))

# debit difference
rural_num_debit_w# rural debit payments
urban_num_debit_w# urban debit payments
sum(rural1.df$w)# total rural payments
sum(urban1.df$w)# total urban payments
(test_debit_w =prop.test(x = c(rural_num_debit_w, urban_num_debit_w), n = c(sum(rural1.df$w), sum(urban1.df$w)), correct = FALSE, conf.level = 0.99))
(test_debit_w_pval = test_debit_w$p.value)
(test_debit_w_pval_yn = ifelse(test_debit_w$p.value < 0.01, "Yes", "No"))
(test_debit_w_ci = test_debit_w$conf.int)
(test_debit_w_ci_lower = test_debit_w$conf.int[1])
(test_debit_w_ci_upper = test_debit_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
debit.vec
(debit2.vec = as.numeric(c(debit.vec, test_debit_w_ci_lower, test_debit_w_ci_upper)))

# Prepaid card difference
rural_num_prepaid_w# rural prepaid payments
urban_num_prepaid_w# urban prepaid payments
sum(rural1.df$w)# total rural payments
sum(urban1.df$w)# total urban payments
(test_prepaid_w =prop.test(x = c(rural_num_prepaid_w, urban_num_prepaid_w), n = c(sum(rural1.df$w), sum(urban1.df$w)), correct = FALSE, conf.level = 0.99))
(test_prepaid_w_pval = test_prepaid_w$p.value)
(test_prepaid_w_pval_yn = ifelse(test_prepaid_w$p.value < 0.01, "Yes", "No"))
(test_prepaid_w_ci = test_prepaid_w$conf.int)
(test_prepaid_w_ci_lower = test_prepaid_w$conf.int[1])
(test_prepaid_w_ci_upper = test_prepaid_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
prepaid.vec
(prepaid2.vec = as.numeric(c(prepaid.vec, test_prepaid_w_ci_lower, test_prepaid_w_ci_upper)))

# check difference
rural_num_check_w# rural check payments
urban_num_check_w# urban check payments
sum(rural1.df$w)# total rural payments
sum(urban1.df$w)# total urban payments
(test_check_w =prop.test(x = c(rural_num_check_w, urban_num_check_w), n = c(sum(rural1.df$w), sum(urban1.df$w)), correct = FALSE, conf.level = 0.99)) 
(test_check_w_pval = test_check_w$p.value)
(test_check_w_pval_yn = ifelse(test_check_w$p.value < 0.01, "Yes", "No"))
(test_check_w_ci = test_check_w$conf.int)
(test_check_w_ci_lower = test_check_w$conf.int[1])
(test_check_w_ci_upper = test_check_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
check.vec
(check2.vec = as.numeric(c(check.vec, test_check_w_ci_lower, test_check_w_ci_upper)))

# mobile difference
rural_num_mobile_w# rural mobile payments
urban_num_mobile_w# urban mobile payments
sum(rural1.df$w)# total rural payments
sum(urban1.df$w)# total urban payments
(test_mobile_w =prop.test(x = c(rural_num_mobile_w, urban_num_mobile_w), n = c(sum(rural1.df$w), sum(urban1.df$w)), correct = FALSE, conf.level = 0.99)) 
(test_mobile_w_pval = test_mobile_w$p.value)
(test_mobile_w_pval_yn = ifelse(test_mobile_w$p.value < 0.01, "Yes", "No"))
(test_mobile_w_ci = test_mobile_w$conf.int)
(test_mobile_w_ci_lower = test_mobile_w$conf.int[1])
(test_mobile_w_ci_upper = test_mobile_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
mobile.vec
(mobile2.vec = as.numeric(c(mobile.vec, test_mobile_w_ci_lower, test_mobile_w_ci_upper)))

# bank difference
rural_num_bank_w# rural bank payments
urban_num_bank_w# urban bank payments
sum(rural1.df$w)# total rural payments
sum(urban1.df$w)# total urban payments
(test_bank_w =prop.test(x = c(rural_num_bank_w, urban_num_bank_w), n = c(sum(rural1.df$w), sum(urban1.df$w)), correct = FALSE, conf.level = 0.99)) 
(test_bank_w_pval = test_bank_w$p.value)
(test_bank_w_pval_yn = ifelse(test_bank_w$p.value < 0.01, "Yes", "No"))
(test_bank_w_ci = test_bank_w$conf.int)
(test_bank_w_ci_lower = test_bank_w$conf.int[1])
(test_bank_w_ci_upper = test_bank_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
bank.vec
(bank2.vec = as.numeric(c(bank.vec, test_bank_w_ci_lower, test_bank_w_ci_upper)))

# other difference
rural_num_other_w# rural other payments
urban_num_other_w# urban other payments
sum(rural1.df$w)# total rural payments
sum(urban1.df$w)# total urban payments
(test_other_w =prop.test(x = c(rural_num_other_w, urban_num_other_w), n = c(sum(rural1.df$w), sum(urban1.df$w)), correct = FALSE, conf.level = 0.99))
(test_other_w_pval = test_other_w$p.value)
(test_other_w_pval_yn = ifelse(test_other_w$p.value < 0.01, "Yes", "No"))
(test_other_w_ci = test_other_w$conf.int)
(test_other_w_ci_lower = test_other_w$conf.int[1])
(test_other_w_ci_upper = test_other_w$conf.int[2])
# Append the above to columns w/o the yn (chr)
other.vec
(other2.vec = as.numeric(c(other.vec, test_other_w_ci_lower, test_other_w_ci_upper)))

# reconstruct the data frame
(area2.vec = c("Rural", "Mixed", "Urban", "Rural", "Mixed", "Urban","Rural", "Mixed", "Urban", "Lower CI", "Upper CI"))
#
(pay2.df = data.frame(Area = area2.vec, Cash = cash2.vec, Credit = credit2.vec, Debit = debit2.vec, Prepaid = prepaid2.vec, Check = check2.vec, Mobile = mobile2.vec, Bank=bank2.vec, Other = other2.vec))
dim(pay2.df)

# round numbers
(pay3.df =pay2.df)
dim(pay3.df)
str(pay3.df)
# round the 3 rows (weighted number of payments)
(pay3.df[4:6, 2:9] = round(pay3.df[4:6, 2:9],0))
# round the % rows (weighted number of payments)
(pay3.df[7:11, 2:9] = round(pay3.df[7:11, 2:9],3))

#using library scales to add % sign
(pay4.df = pay3.df)
dim(pay4.df)
pay4.df[7:11, 2:9] = lapply(pay4.df[7:11, 2:9], percent)
pay4.df
str(pay4.df)

# Add bottom row with p<0.01
(pval.vec = c("p<0.01", test_cash_w_pval_yn, test_credit_w_pval_yn,  test_debit_w_pval_yn, test_prepaid_w_pval_yn, test_check_w_pval_yn,  test_mobile_w_pval_yn, test_bank_w_pval_yn, test_other_w_pval_yn))
length(pval.vec)
#
(pay5.df = rbind(pay4.df, pval.vec))

print(xtable(pay5.df), include.rownames = F, hline.after = c(0,3,6,9))

# info for Table 4
nrow(rural1.df)# num payments
nrow(mixed1.df)# num payments
nrow(urban1.df)# num payments
#
length(unique(rural1.df$id))# num respondents
length(unique(mixed1.df$id))# num respondents
length(unique(urban1.df$id))# num respondents

#End: Table 4: In-person payments####

#++++++++++++++++++++++++++++++

#Begin: Figure 1: VIP plot: random forest (full sample)####
# Used in an earlier draft. Instead of VIP table, I switched to confusion matrices (see next section)

m6.df = m5.df
names(m6.df)# entire indiv and trans data combined
str(m6.df)
table(m6.df$pi, useNA = "always")

str(m6.df$amnt)
m6.df=m6.df %>% rename(Amount = amnt)
m6.df=m6.df %>% rename(Merchant = merch)
str(m6.df$HH_income)
str(m6.df$Age)
str(m6.df$Gender)
str(m6.df$Area)
str(m6.df$Race)
str(m6.df$Hispanic)
str(m6.df$Education)
str(m6.df$Own_credit)
str(m6.df$Merchant)
m6.df$Merchant = as.factor(m6.df$Merchant)
str(m6.df$Merchant)
m6.df$Own_credit = as.factor(m6.df$Own_credit)
str(m6.df$Own_debit)
m6.df$Own_debit = as.factor(m6.df$Own_debit)
str(m6.df$Credit_reward)
m6.df$Credit_reward = as.factor(m6.df$Credit_reward)
str(m6.df$pi)
str(m6.df)

# random forest model with Area
names(m6.df)
rf_pi.model = pi ~ Amount + Merchant + in_person + bill + HH_income + Age + Gender + Area +Education +Race +Hispanic + Own_credit + Own_debit  + Credit_reward

# ranger instead of randomForest
(rf_output = ranger(rf_pi.model, data = m6.df, importance= "permutation", num.threads = detectCores() - 1, seed = 1955))# imprtance is needed for VIP

(imp = data.frame(
  Variable = names(rf_output$variable.importance),
  Importance = rf_output$variable.importance
))
graphics.off()
ggplot(imp, aes(x = reorder(Variable, Importance), y = Importance)) + geom_col() + coord_flip()

# rough rule about absolute interpretation of the VIP
# > 0.05 => strong predictor
# 0.01-0.05 => moderate predictor
# 0.00-0.01 => weak predictor
# < 0 => Likely noise

# info about the above VIP table and chart
nrow(m6.df)# num of payments
length(unique(m6.df$id))# num respondents

#End: Figure 1: VIP plot: In-person random forest (full sample)####

#++++++++++++++++++++++++++++++

#Begin: Table 5: In-person confusion matrices with random forest####
names(m6.df)

#split data into 80% training and 20% testing by unique respondent ID
dim(m6.df)# num payments
length(unique(m6.df$id))# num resp
id_m6.vec = unique(m6.df$id)
length(id_m6.vec)
#
set.seed(1955)
id_train.vec = sample(id_m6.vec, size = floor(0.8 * length(id_m6.vec)))
length(id_train.vec)#num resp in the training data
length(id_m6.vec)-length(id_train.vec)# num resp in testing data
#construct a training data frame of payments
train.df = m6.df[m6.df$id %in% id_train.vec, ]
test.df = m6.df[!(m6.df$id %in% id_train.vec), ]
dim(train.df)#num payments
dim(test.df)# num payments
nrow(train.df)/nrow(m6.df)# verify that the 0.8 split by ID approx splits the payments in a similar ratio
nrow(test.df)/nrow(m6.df)# verify that the 0.8 split by ID approx splits the payments in a similar ratio
nrow(m6.df)
nrow(train.df)+nrow(test.df)

# random forest: model with Area
rf_area.model = pi ~ Amount + Merchant + in_person + bill + HH_income + Age + Gender + Area +Education +Race +Hispanic + Own_credit + Own_debit  + Credit_reward
# RF model without Area
rf_ip_no_area.model = pi ~ Amount + Merchant + in_person + bill + HH_income + Age + Gender + Education +Race +Hispanic + Own_credit + Own_debit  + Credit_reward

# ranger instead of randomForest
(rf_area_output = ranger(rf_area.model, data = train.df, importance= "permutation", num.threads = detectCores() - 1, seed = 1955))

#Start predictions and building confusion matrix (bottom of Table 5)
rf_area_pred=predict(rf_area_output, data = test.df)$predictions# predict test data

# can make simple confusion table. caret below profit also the necessary stats
#(rf_ip_area_table = table(Actual = ip_test.df$pi, Predicted = rf_ip_area_pred))# w/o caret package

#caret confusion matrix: data=predicted, reference=actual
(rf_area_confusion = confusionMatrix(data = factor(rf_area_pred), reference = factor(test.df$pi)))
#separate the table part and the stats part, and then append the two parts
(rf_area_confusion_table1.df =  as.data.frame.matrix( rf_area_confusion$table))
#
# the stats part
(rf_area_confusion_stat1.df =  as.data.frame.matrix(rf_area_confusion$byClass))
# transpose the stats part
(rf_area_confusion_stat2.df = t(rf_area_confusion_stat1.df))
# select stats for the final table
rownames(rf_area_confusion_stat2.df)
(rf_area_confusion_stat3.df = as.data.frame(rf_area_confusion_stat2.df[c(1,3,11) ,]))
#
#bind the confusion table with the stats table
dim(rf_area_confusion_table1.df)
dim(rf_area_confusion_stat3.df)
#
str(rf_area_confusion_table1.df)
names(rf_area_confusion_table1.df)
str(rf_area_confusion_stat3.df)
names(rf_area_confusion_stat3.df)
#match column names
(rf_pi_names = c("Cash", "Credit", "Debit", "Prepaid", "Check", "Mobile", "Bank", "Other"))

rf_area_confusion_stat4.df = rf_area_confusion_stat3.df
(names(rf_area_confusion_stat4.df) = rf_pi_names)
(names(rf_area_confusion_table1.df) = rf_pi_names)
# bind
(rf_area_confusion_table2.df = rbind(rf_area_confusion_table1.df, rf_area_confusion_stat4.df))

# revise row names
rownames(rf_area_confusion_table2.df)
rf_area_confusion_table3.df = rf_area_confusion_table2.df
#
(rownames(rf_area_confusion_table3.df) = c(rf_pi_names, "Sensitivity", "Pos Pred Value",  "Balanced Accuracy"))

# change stats (bottom 3 rows) to %
dim(rf_area_confusion_table3.df)
names(rf_area_confusion_table3.df)
str(rf_area_confusion_table3.df)
rf_area_confusion_table4.df = rf_area_confusion_table3.df
rf_area_confusion_table4.df[9:11, ] = lapply(rf_area_confusion_table4.df[9:11, ], percent, accuracy = 0.1)
rf_area_confusion_table4.df

##Start confusion table without Area
# ranger instead of randomForest
(rf_no_area_output = ranger(rf_ip_no_area.model, data = train.df, importance= "permutation", num.threads = detectCores() - 1, seed = 1955))

#Start predictions and building confusion matrix (bottom of Table 5)
rf_no_area_pred=predict(rf_no_area_output, data = test.df)$predictions# predict test data

# can make simple confusion table. caret below profit also the necessary stats
#(rf_ip_no_area_table = table(Actual = ip_test.df$pi, Predicted = rf_ip_no_area_pred))# w/o caret package

#caret confusion matrix: data=predicted, reference=actual
(rf_no_area_confusion = confusionMatrix(data = factor(rf_no_area_pred), reference = factor(test.df$pi)))
#separate the table part and the stats part, and then append the two parts
(rf_no_area_confusion_table1.df =  as.data.frame.matrix( rf_no_area_confusion$table))
#
# the stats part
(rf_no_area_confusion_stat1.df =  as.data.frame.matrix(rf_no_area_confusion$byClass))
# transpose the stats part
(rf_no_area_confusion_stat2.df = t(rf_no_area_confusion_stat1.df))
# select stats for the final table
rownames(rf_no_area_confusion_stat2.df)
(rf_no_area_confusion_stat3.df = as.data.frame( rf_no_area_confusion_stat2.df[c(1,3,11) ,]))
#
#bind the confusion table with the stats table
dim(rf_no_area_confusion_table1.df)
dim(rf_no_area_confusion_stat3.df)
#
str(rf_no_area_confusion_table1.df)
names(rf_no_area_confusion_table1.df)
str(rf_no_area_confusion_stat3.df)
names(rf_no_area_confusion_stat3.df)
#match column names
(rf_pi_names = c("Cash", "Credit", "Debit", "Prepaid", "Check", "Mobile", "Bank", "Other"))

rf_no_area_confusion_stat4.df = rf_no_area_confusion_stat3.df
(names(rf_no_area_confusion_stat4.df) = rf_pi_names)
(names(rf_no_area_confusion_table1.df) = rf_pi_names)
# bind
(rf_no_area_confusion_table2.df = rbind(rf_no_area_confusion_table1.df, rf_no_area_confusion_stat4.df))

# revise row names
rownames(rf_no_area_confusion_table2.df)
rf_no_area_confusion_table3.df = rf_no_area_confusion_table2.df
#
(rownames(rf_no_area_confusion_table3.df) = c(rf_pi_names, "Sensitivity", "Pos Pred Value",  "Balanced Accuracy"))

# change stats (bottom 3 rows) to %
dim(rf_no_area_confusion_table3.df)
rf_no_area_confusion_table4.df = rf_no_area_confusion_table3.df
rf_no_area_confusion_table4.df[9:11, ] = lapply(rf_no_area_confusion_table4.df[9:11, ], percent, accuracy = 0.1)
rf_no_area_confusion_table4.df

## summary of the 2 confusion matrices
rf_area_confusion_table4.df# with area
rf_no_area_confusion_table4.df# without

# Create a digits matrix: (rows + 0) × (columns + 1)
# First row of digits is for rownames
#(digits_matrix = matrix(0, nrow = nrow(rf_ip_no_area_table3.df)+ 0, ncol = ncol(rf_ip_no_area_table3.df) + 1))
#digits_matrix[1:9, ] = 0
#digits_matrix[10, ] = 2
#digits_matrix
#print(xtable(rf_ip_area_table3.df, digits=digits_matrix), include.rownames = F, hline.after = c(0,7))

#Info for table 5:


#End: Table 5: In-person confusion matrices with random forest####

#++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++
###unused code:####
# #Begin: Figure 1 (no mixed): VIP plot: random forest (mixed area removed)####
# 
# nrow(m6.df)# num payments with mixed areas
# table(m6.df$Area)
# m6_no_mixed.df = subset(m6.df, Area != "Mixed")# remove mixed area
# nrow(m6_no_mixed.df)
# table(m6_no_mixed.df$Area)
# m6_no_mixed.df$Area <- droplevels(m6_no_mixed.df$Area)# drop mixed
# table(m6_no_mixed.df$Area)
# 
# # random forest model with Area but without mixed
# names(m6_no_mixed.df)
# rf_pi.model = pi ~ Amount + Merchant + in_person + bill + HH_income + Age + Gender + Area +Education +Race +Hispanic + Own_credit + Own_debit  + Credit_reward
# 
# # ranger instead of randomForest
# (rf_output_no_mixed = ranger(rf_pi.model, data = m6_no_mixed.df, importance= "permutation", num.threads = detectCores() - 1, seed = 1955))# importance is needed for VIP
# 
# (imp_no_mixed = data.frame(
#   Variable = names(rf_output_no_mixed$variable.importance),
#   Importance = rf_output_no_mixed$variable.importance
# ))
# graphics.off()
# ggplot(imp_no_mixed, aes(x = reorder(Variable, Importance), y = Importance)) + geom_col() + coord_flip()
# 
# # rough rule about absolute interpretation of the VIP
# # > 0.05 => strong predictor
# # 0.01-0.05 => moderate predictor
# # 0.00-0.01 => weak predictor
# # < 0 => Likely noise
# 
# # info about the above VIP table and chart
# nrow(m6_no_mixed.df)# num of payments
# length(unique(m6_no_mixed.df$id))# num respondents
# 
# #End: Figure 1 (no_mixed): VIP plot: random forest (mixed removed)####


#++++++++++++++++++++++++++++++
