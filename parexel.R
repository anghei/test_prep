#### R Code For Parexel Pre-Task ####
####       October 18, 2021      ####

# Import dataset and check summary information
data = read.csv('~/Downloads/resp1.csv', sep = '@')

summary(data)

# Attach dataframe
attach(data)

# In this step I'm going to make some new variables to use them further for OR and CI calculation
response_treat1 <- data[TRTPN == '1' & responseCategory %in% c("CR", "PR"), ]
response_treat2 <- data[TRTPN == '2' & responseCategory %in% c("CR", "PR"), ]

not_response_treat1 <- data[TRTPN == '1' & !(responseCategory %in% c("CR", "PR")), ]
not_response_treat2 <- data[TRTPN == '2' & !(responseCategory %in% c("CR", "PR")), ]

# Create a matrix [2x2] with condition of Disease(+/-) and Exposure (+/-)
TAB <- matrix(
  c(
    dim(response_treat1[0])[1],
    dim(response_treat2[0])[1], 
    dim(not_response_treat1[0])[1], 
    dim(not_response_treat2[0])[1]
  ),
  
  nrow = 2, 
  byrow = F
)

# For calculate OR and CI, I'll use library "epiR"
# you might install this by command below:
##                install.packages("epiR")
library(epiR)


OR_T2T = epi.2by2(
  TAB, 
  method = "cohort.count", 
  conf.level = 0.95
)

OR_T2T
### Conclusion:
### The Odds of a treatment 1 with response on treatment are 
### 1.67 times the Odds of treatment 2 with response on treatment

# OR and CI (Male)

response_treat1_male <- data[TRTPN == '1' & gender == 'MALE' & responseCategory %in% c("CR", "PR"), ]
response_treat2_male <- data[TRTPN == '2' & gender == 'MALE' & responseCategory %in% c("CR", "PR"), ]

not_response_treat1_male <- data[TRTPN == '1' & gender == 'MALE' & !(responseCategory %in% c("CR", "PR")), ]
not_response_treat2_male <- data[TRTPN == '2' & gender == 'MALE' & !(responseCategory %in% c("CR", "PR")), ]

TAB_male <- matrix(
  c(
    dim(response_treat1_male[0])[1],
    dim(response_treat2_male[0])[1], 
    dim(not_response_treat1_male[0])[1], 
    dim(not_response_treat2_male[0])[1]
  ),
  
  nrow = 2, 
  byrow = F
)

OR_T2T_Male = epi.2by2(
  TAB_male, 
  method = "cohort.count", 
  conf.level = 0.95
)

OR_T2T_Male
### Conclusion:
### For males the Odds of a treatment 1 with response on treatment are 
### 1.99 times the Odds of treatment 2 with response on treatment

# OR and CI (Female)
response_treat1_female <- data[TRTPN == '1' & gender == 'FEMALE' & responseCategory %in% c("CR", "PR"), ]
response_treat2_female <- data[TRTPN == '2' & gender == 'FEMALE' & responseCategory %in% c("CR", "PR"), ]

not_response_treat1_female <- data[TRTPN == '1' & gender == 'FEMALE' & !(responseCategory %in% c("CR", "PR")), ]
not_response_treat2_female <- data[TRTPN == '2' & gender == 'FEMALE' & !(responseCategory %in% c("CR", "PR")), ]

TAB_female <- matrix(
  c(
    dim(response_treat1_female[0])[1],
    dim(response_treat2_female[0])[1], 
    dim(not_response_treat1_female[0])[1], 
    dim(not_response_treat2_female[0])[1]
  ),
  
  nrow = 2, 
  byrow = F
)

OR_T2T_Female = epi.2by2(
  TAB_female, 
  method = "cohort.count", 
  conf.level = 0.95
)

OR_T2T_Female
### Conclusion:
### For females the Odds of a treatment 1 with response on treatment are 
### 1.36 times the Odds of treatment 2 with response on treatment

# OR and CI (Female vs Male | Treatment 1)
response_treat1_female <- data[TRTPN == '1' & gender == 'FEMALE' & responseCategory %in% c("CR", "PR"), ]
response_treat1_male <- data[TRTPN == '1' & gender == 'MALE' & responseCategory %in% c("CR", "PR"), ]

not_response_treat1_female <- data[TRTPN == '1' & gender == 'FEMALE' & !(responseCategory %in% c("CR", "PR")), ]
not_response_treat1_male <- data[TRTPN == '1' & gender == 'MALE' & !(responseCategory %in% c("CR", "PR")), ]

TAB_femaleVSmale_treat1 <- matrix(
  c(
    dim(response_treat1_female[0])[1],
    dim(response_treat1_male[0])[1], 
    dim(not_response_treat1_female[0])[1], 
    dim(not_response_treat1_male[0])[1]
  ),
  
  nrow = 2, 
  byrow = F
)

OR_T2T_FemaleVSMale_Treat1 = epi.2by2(
  TAB_femaleVSmale_treat1, 
  method = "cohort.count", 
  conf.level = 0.95
)

OR_T2T_FemaleVSMale_Treat1
### Conclusion:
### For treatment 1 the Odds of are female with response on treatment are 
### 0.76 times the Odds of are male with response on treatment

# OR and CI (Female vs Male | Treatment 2)
response_treat2_female <- data[TRTPN == '2' & gender == 'FEMALE' & responseCategory %in% c("CR", "PR"), ]
response_treat2_male <- data[TRTPN == '2' & gender == 'MALE' & responseCategory %in% c("CR", "PR"), ]

not_response_treat2_female <- data[TRTPN == '2' & gender == 'FEMALE' & !(responseCategory %in% c("CR", "PR")), ]
not_response_treat2_male <- data[TRTPN == '2' & gender == 'MALE' & !(responseCategory %in% c("CR", "PR")), ]

TAB_femaleVSmale_treat2 <- matrix(
  c(
    dim(response_treat2_female[0])[1],
    dim(response_treat2_male[0])[1], 
    dim(not_response_treat2_female[0])[1], 
    dim(not_response_treat2_male[0])[1]
  ),
  
  nrow = 2, 
  byrow = F
)

OR_T2T_FemaleVSMale_Treat2 = epi.2by2(
  TAB_femaleVSmale_treat2, 
  method = "cohort.count", 
  conf.level = 0.95
)

OR_T2T_FemaleVSMale_Treat2
### Conclusion:
### For treatment 2 the Odds of are female with response on treatment are 
### 1.12 times the Odds of are male with response on treatment


