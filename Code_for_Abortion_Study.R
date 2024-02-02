#install.packages("tidyverse")
library(foreign)
library(haven)
library(tidyverse)

library(readr)
data <- read_csv("~/Documents/Where Anxieties Over Infertility, “Great Replacement Theory”, and Abortion Politics Intersect/Survey For Study About People's Opinions Of Controversial Behaviors_all_data_aligned_coded_regions.csv")
#inputting all the blanks with 0s
data$bmww_<- ifelse(data$bmbw==1 | data$wmww==1 | data$wmbw==1, 0, 1)
data$bmww<-data$bmww_
library(dplyr)
data$citizen_c <- recode(data$uscitizen,
                         "No, but I am a first-generation immigrant, born outside the US (enter country)" = 0,  "No, and I do not consider myself an immigrant, but a citizen of another country (enter country)" = 0, "No, but I am a 1.5 generation immigrant (came to US in before 13 years old) (enter country)"=0, "NA"=0, "Yes, I am a first-generation immigrant, born outside the US (enter country)"=1, "Yes, I am a third/fourth/fifth-generation or high generation immigrant (grandparents or earlier ancestors born outside the US)" = 1, "Yes, I am a second-generation immigrant (parents born outside the US)" = 1, "Yes, I am a 1.5 generation immigrant (came to US in before 13 years old) (enter country)"=1, "Yes, I am a third/fourth/fifth-generation or high generation immigrant (grandparents or earlier ancestors born outside the US)"=1, "Yes and I consider myself indigenous to the territory of the US."=1)
data$female<- recode(data$sex,
                     "Male" = 0, "Female"=1, "Intersex/Non-binary/self-describe"=0)

#problem with coding this variable. I need another pair of eyes to help me figure out what went wrong with it:
#data$hispanic<- ifelse(data$hispanic=="no, not of Hispanic, Latino or Spanish origin.", 0, 1) 
#Here I use the replace function instead:

#install.packages("do")
library(do)
hispanic=data$hispanic
hispanic=Replace(hispanic,"no, not of Hispanic, Latino or Spanish origin.", 0)
data$hispanic<-hispanic
data$hispanic[data$hispanic!=0]=1
data$hispanic<-as.numeric(data$hispanic)
data$white<-ifelse(data$race=="White", 1, 0)

data$black<-ifelse(data$race=="Black or African American", 1, 0)

data$religious<-ifelse(data$religpref=="None", 0, 1)

data$protestant<- ifelse(data$religpref=="Protestant", 1, 0)
data$christian<- ifelse(data$religpref=="Catholic", 1, ifelse(data$religpref=="Protestant", 1,
                                                                ifelse(data$religpref=="Christian", 1, 
                                                                       ifelse(data$religpref=="Orthodox Christian", 1, 0))))


data=data %>%
  mutate(christian_1=case_when(
    christian=="Protestant"~1,
    christian=="Catholic"~1, 
    christian=="Christian" ~1, 
    christian=="Orthodox Christian"~0, 
    christian=="Jewish"~0, 
    christian=="Muslim/Islam"~0, 
    christian=="Orthodox Christian"~0, 
    christian=="Native American"~0, 
    christian=="Other (please type)"~0, 
    christian=="Buddhism"~0, 
    christian=="Hinduism"~0
  ))
#come back to this
data$educyear<-recode(data$educ, "Master’s Degree (for example MA, MS, Meng, Med, MSW, MBA),"=22, "No schooling completed," = 0, "Nursery," = 1, "Kindergarten" = 2, "Grade 1 through 11: Specify" = 8, "12th grade—NO DIPLOMA"=13, "12th grade‚ÄîNO DIPLOMA"=13, "Regular high school diploma"=14, "GED or alternative credential,"=14, "Some college credit, but less than 1 year of college credit,"=15, "1 or more years of college credit, no degree,"=16, "Associate’s degree (for example: AA, AS)"=18, "Associate‚Äôs degree (for example: AA, AS)"=18, "Bachelor’s degree (for example: BA, BS)"=20,  "Bachelor‚Äôs degree (for example: BA, BS)"=20, "Master’s Degree (for example MA, MS, Meng, Med, MSW, MBA),"=22,"Professional degree beyond a bachelor‚Äôs degree (for example MD, DDS, DVM, LLB, JD)"=22, "Professional degree beyond a bachelor’s degree (for example MD, DDS, DVM, LLB, JD)"=22, "Doctorate degree (for example PhD, EdD)"=28)
           
data$x<-recode(data$educ, "Master’s Degree (for example MA, MS, Meng, Med, MSW, MBA),"=1)          
                          
data$collegeeduc<-ifelse(data$educyear>18, 1, 0)

data$employed_c<-recode(data$employed, "yes"=1, "no"=0)
                          
data$income_c<-recode(data$income, "$0-25,000"=12500, "$25,001-50,000"=37500, "$25,000-50,000"=37500, "$50,001-75,000"=62500, "$50,000-75,000"=62500, "$75,000-100,000"=87500,  "$75,001-100,000"=87500, "$100,000-125,000"=112500, "$100,001-125,000"=112500,"$125,000-150,000"=137500,  "$125,001-150,000"=137500, "Above $150,000"= 175000) 

data$logincome<-log(data$income_c)

data$married<-ifelse(data$relationstatus=="married", 1, 0)

data$democrat_c<-ifelse(data$polparty=="Democrat", 1, 0)
data$republican_c<-ifelse(data$polparty=="Republican", 1, 0)
data$independent_c<-ifelse(data$polparty=="Independent", 1, 0)


data$conservativeideology<-recode(data$polideology, "1 (extremely liberal)"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7 (extremely conservative)"=7)

data$conservativemediacons<-recode(data$mediaideology, "1 (extremely liberal)"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7 (extremely conservative)"=7)


data$auth1_c<-recode(data$auth1, "independent"=0, "respectful of their elders"=1)
data$auth1_c[is.na(data$auth1_c)] <- 0.5
data$auth2_c<-recode(data$auth2, "obedient"=1, "self-reliant"=0)
data$auth2_c[is.na(data$auth2_c)] <- 0.5

data$auth3_c<-recode(data$auth3, "well-behaved"=1, "considerate"=0)
data$auth3_c[is.na(data$auth3_c)] <- 0.5
data$auth4_c<-recode(data$auth4, "curious"=0, "good mannered"=1)
data$auth4_c[is.na(data$auth4_c)] <- 0.5

data$auth5_c<-recode(data$auth5, "free-spirited"=0, "polite"=1)
data$auth5_c[is.na(data$auth5_c)] <- 0.5
data$auth6_c<-recode(data$auth6, "orderly"=1, "imaginative"=0, "NA"=0.5)
data$auth6_c[is.na(data$auth6_c)] <- 0.5
data$auth7_c<-recode(data$auth7, "loyal"=1, "open-minded"=0)
data$auth7_c[is.na(data$auth7_c)] <- 0.5
data$auth8_c<-recode(data$auth8, "adaptable"=1, "disciplined"=0)
data$auth8_c[is.na(data$auth8_c)] <- 0.5
#turn NAs into 0.5

data$auth_c<-(data$auth1_c+data$auth2_c+data$auth3_c+data$auth4_c+data$auth5_c+data$auth7_c+data$auth8_c)
#auth3 has some NA values

sum(is.na(data$auth))

#racial resentment variables

data$blacksmustworkup_c<- recode(data$blacksmustworkup, "strongly disagree"=0, "disagree" = 1, "neither agree nor disagree"=2, "agree"=3, "strongly agree" = 4)

data$slaveryhurtblacks_c<- recode(data$blacksmustworkup, "strongly disagree"=4, "disagree" = 3, "neither agree nor disagree"=2, "agree"=1, "strongly agree" = 0)

data$blacksgetlessdeserve_c<- recode(data$blacksgetlessdeserve, "strongly disagree"=4, "disagree" = 3, "neither agree nor disagree"=2, "agree"=1, "strongly agree" = 0)

data$blackstryharder_c<- recode(data$blackstryharder, "strongly disagree"=0, "disagree" = 1, "neither agree nor disagree"=2, "agree"=3, "strongly agree" = 4)

data$racialresentment_c<-(data$blacksmustworkup_c+data$slaveryhurtblacks_c+data$blacksgetlessdeserve_c+data$blackstryharder_c)

dim(data)
ethnocentrism=rep(0,1218)
ethnocentrism[which(data$hispanic==1)]=data$feelinglatino[which(data$hispanic==1)]-data$feelinglatino[which(data$hispanic==1)]
ethnocentrism[which(data$hispanic==0)]=data$feelinglatino[which(data$hispanic==0)]-data$feelingnonlatino[which(data$hispanic==0)]

data<-cbind(data, ethnocentrism)

data$religionpractice_c<-recode(data$religionpractice, "never"=0, "once per year" = 1, "more once per year but less than once per month"=2, "once per month"=3, "more than once per month but less than once per week" = 4, "once per week"=5, "more than once per week"=6)

data$natimport_c<-recode(data$natimport, "very important"=3, "somewhat important" = 2, "not so important"=1, "not important at all"=0)

data$trustforeigner_c<-recode(data$trustforeigner, "trust completely"=0, "trust somewhat"=1, "distrust somewhat"=2, "do not trust at all"=3)

data$nationalism_c<-(data$trustforeigner_c+data$natimport_c)

data$genderlib1<-recode(data$genderchild, "strongly disagree"=0, "disagree"=1, "agree"=2, "strongly agree"=3)

data$genderlib2<-recode(data$genderhome, "strongly disagree"=3, "disagree"=2, "agree"=1, "strongly agree"=0)

data$genderlib3<-recode(data$genderfamily, "strongly disagree"=0, "disagree"=1, "agree"=2, "strongly agree"=3)

data$genderlib4<-recode(data$genderjob, "strongly disagree"=3, "disagree"=2, "agree"=1, "strongly agree"=0)

data$genderlib_c<-(data$genderlib1+data$genderlib2+data$genderlib3+data$genderlib4)
data$genderlib_c


data$abfamily_c<-recode(data$abfamily, "0"=0, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "8"=8, "9"=9, "10 or more"=10)

data$abinformation_c<-recode(data$abinformation, "all the information I need"=4, "most of the information I need"=3, "some of the information I need"=2, "very little information I need"=0)
#data$abinformation_c<-data$abinformation

data$abconcern_c<-recode(data$abconcern, "very concerned"=3, "somewhat concerned"=2, "not very concerned"=1, "not concerned at all"=0)

data$abfirm_c<-recode(data$abfirm, "very likely to change your opinion"=0, "somewhat likely to change"=1, "somewhat unlikely to change"=2, "very unlikely to change"=3)


data$abdiscuss_c<-recode(data$abdiscuss, "very often"=2, "sometimes"=1, "almost never"=0)

data$abweb_c<-recode(data$abweb, "yes"=1, "no"=0)

data$pabdefect_c<-recode(data$pabdefect, "yes"=1, "no"=0, "NA"=0.5)
data$pabhealth_c<-recode(data$pabhealth, "yes"=1, "no"=0, "NA"=0.5)
data$pabafford_c<-recode(data$pabafford, "yes"=1, "no"=0, "NA"=0.5)

data$ablegal_c<-recode(data$ablegal, "never legal under any circumstance"=0, "legal under certain circumstances"=1, "legal under any circumstances"=2)



data$court_c<-recode(data$court, "yes"=1, "no"=0)


data$attention_c<-ifelse(data$attention=="Earth", 1, 0)


data$pabdefect_c<-as.numeric(data$pabdefect_c)
####I can't find the "American_feeling"data--I think that was related to another survey so you 
#can ignore it
#data$American_feeling1_c <- recode(data$American_feeling1,
                          "disagree strongly" = 0, "disagree somewhat" = 1, "agree somewhat" = 2, "agree strongly" = 3)
            
                             
###data$American_feeling2_c <- recode(data$American_feeling2,
                          #"disagree strongly" = 0, "disagree somewhat" = 1, "agree somewhat" = 2, "agree strongly" = 3)
                          
                                                    
###data$American_feeling3_c <- recode(data$American_feeling3,
                         #"disagree strongly" = 0, "disagree somewhat" = 1, "agree somewhat" = 2, "agree strongly" = 3)
                          
###data$identify_with_nation_c<-(data$American_feeling1_c+data$American_feeling2_c+data$American_feeling3_c)
#reload data to recover localphysical fear values

####I can't find the "nationaleconomicpast,nationaleconomicfuture"data
#data$nationaleconomicpast_c <- recode(data$nationaleconomicpast,
                          #"worse off" = 0, "about the same" = 1, "better off" = 2)
                          
#data$nationaleconomicfuture_c <- recode(data$nationaleconomicfuture,
                          #"worse off" = 0, "about the same" = 1, "better off" = 2)
       
       
#data$localeconomicpast_c <- recode(data$localeconomicpast,
                          #"worse off" = 0, "about the same" = 1, "better off" = 2)
     
#data$localeconomicfuture_c <- recode(data$localeconomicfuture,
                          #"worse off" = 0, "about the same" = 1, "better off" = 2)
         
         
data$White_c<-ifelse(data$race=="White", 1, 0)
####I can't find the "latino"data
#data$Hispanic_c<-ifelse(data$latino=="no, not of Hispanic, Latino or Spanish origin.", 1, 0)

####I can't find the "feelingwhitenonlatinopeople,feelingnonlatinoblack"data
#data$antiblack<-(data$feelingwhitenonlatinopeople-data$feelingnonlatinoblack)
#data$male<-recode(data$sex, "Female"=0, "Male"=1, "Non-binary / another gender"=0, "Prefer not to say"=0)
#data$ethnocentrism_c<- data$feelingwhitenonlatinopeople-data$feelingwhitelatino
#find a way to reverse code this for Hispanics later
#data$ethnocentrism_c<-if(data$Hispanic_c==0) data$feelingwhitenonlatinopeople-data$feelingwhitelatino
#else {
 #data$feelingwhitenonlatinopeople-data$feelingwhitelatino

#data$ethnocentrism_c<- (data$feelingwhitenonlatinopeople-data$feelingwhitelatino)

#data$ethnocentrism_c<-  (data$feelingwhitenonlatinopeople-data$feelingwhitelatino) 

####I can't find the "partisanship"data
#data$partisanship_c<-recode(data$partisanship,
                          #"Democrat" = 0, "no party" = 1, "Independent" = 1, "Third Party" = 1, "Republican" = 2)

#data$democrat_c<-ifelse(data$partisanship=="Democrat", 1, 0)
#data$republican_c<-ifelse(data$partisanship=="Republican", 1, 0)
#data$independent_c<-ifelse(data$partisanship=="Independent", 1, 0)


data$educyear_c<-recode(data$educ,
                          "No schooling completed," = 0, "Nursery," = 1, "Kindergarten" = 2, "Grade 1 through 11: Specify" = 8, "12th grade—NO DIPLOMA"=13, "12th grade‚ÄîNO DIPLOMA"=13, "Regular high school diploma"=14, "GED or alternative credential,"=14, "Some college credit, but less than 1 year of college credit,"=15, "1 or more years of college credit, no degree,"=16, "Associate’s degree (for example: AA, AS)"=18, "Associate‚Äôs degree (for example: AA, AS)"=18, "Bachelor's degree (for example: BA, BS)"=20,  "Bachelor‚Äôs degree (for example: BA, BS)"=20, "Master's Degree (for example MA, MS, Meng, Med, MSW, MBA),"=22, "Master‚Äôs Degree (for example MA, MS, Meng, Med, MSW, MBA),"=22, "Professional degree beyond a bachelor’s degree (for example MD, DDS, DVM, LLB, JD)"=22, "Professional degree beyond a bachelor‚Äôs degree (for example MD, DDS, DVM, LLB, JD)"=22, "Doctorate degree (for example PhD, EdD)"=28)
                          
                         


data$income_c=recode(data$income, "$0-25,000"=12500, "$25,001-50,000"=37500, "$25,000-50,000"=37500, "$50,001-75,000"=62500, "$50,000-75,000"=62500, "$75,000-100,000"=87500,  "$75,001-100,000"=87500, "$100,000-125,000"=112500, "$100,001-125,000"=112500,"$125,000-150,000"=137500,  "$125,001-150,000"=137500, "Above $150,000"= 175000)

data$log_income<-log(data$income_c)




data$absingle_c<-as.numeric(data$absingle)
data$absingle_f<-as.factor(data$absingle)
data$abhusband_c<-as.numeric(data$abhusband)
data$abhusband_f<-as.factor(data$abhusband)
data$abdefect_c<-as.numeric(data$abdefect)
data$abdefect_f<-as.factor(data$abdefect)
data$abhealth_c<-as.numeric(data$abhealth)
data$abhealth_f<-as.factor(data$abhealth)
data$ablife_c<-as.numeric(data$ablife)
data$ablife_f<-as.factor(data$ablife)
data$absupport_c<-as.numeric(data$absupport)
data$absupport_f<-as.factor(data$absupport)
data$abtri_c<-as.numeric(data$abtri)
data$abtri_f<-as.factor(data$abtri)

m1_absingle<-lm(absingle_c~wmbw+bmww+bmbw, data=data)
summary(m1_absingle)
install.packages("stargazer")
library(stargazer)
library(ggplot2)
library(dplyr)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_absingle) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))

# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")

# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()



m1_abhusband<-glm(abhusband_c~wmbw+bmww+bmbw, data=data)
summary(m1_abhusband)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abhusband) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()

m1_abdefect<-glm(abdefect~wmbw+bmww+bmbw, data=data)
summary(m1_abdefect)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abdefect) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()


m1_abhealth<-glm(abhealth_c~wmbw+bmww+bmbw, data=data)
summary(m1_abhealth)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abhealth) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()


m1_ablife<-glm(ablife_c~wmbw+bmww+bmbw, data=data)
summary(m1_ablife)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_ablife) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()


m1_absupport<-glm(absupport_c~wmbw+bmww+bmbw, data=data)
summary(m1_absupport)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_absupport) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()


m1_abtri_wmww<-glm(abtri_c~wmbw+bmww+bmbw, data=data)
summary(m1_abtri_wmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abtri_wmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Table 2: Results from Model 1 For Attitudes Toward Third-Trimester Abortion")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Figure 1: Approval of Abortion of Third-Trimester Pregnancy Due to Rapes", x = "", y = "Estimate") +
  theme_bw()

#people are more likely to support abortion of a baby in a third trimester pregnancy resulting from black man
#raping a black woman than a white man raping a white woman

#relative to black man raping a black woman
m1_abtri_alt_bmbw<-glm(abtri_c~wmbw+bmww+wmww, data=data)
summary(m1_abtri_alt_bmbw)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abtri_alt_bmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table", out="Figure 1")

# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()

#relative to a baby resulting from a rape of a black woman by a black men, people are more likely to oppose abortion of a baby in a third trimester pregnancy resulting from white man
#raping a black woman, a black man raping a white woman, and a white man raping a white woman

#relative to black man raping a white woman
m1_abtri_bmww<-glm(abtri_c~wmbw+bmbw+wmww, data=data)
summary(m1_abtri_bmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abtri_bmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()

#relative to a baby resulting from a rape of a white woman by a black man, people are more likely to support abortion of a baby in a third trimester pregnancy resulting from a black man raping a black woman

#relative to black man raping a white woman
m1_abtri_wmbw<-glm(abtri_c~bmbw+bmbw+wmww, data=data)
summary(m1_abtri_wmbw)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abtri_wmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()

#relative to a baby resulting from a rape of a black woman by a white man, people are more likely to support abortion of a baby in a third trimester pregnancy resulting from a black man raping a black woman






m1_abtri_wmww<-lm(abtri_c~wmbw+bmww+bmbw, data=data)
summary(m1_abtri_wmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abtri_wmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#people are more likely to support abortion of a baby in a third trimester pregnancy resulting from black man
#raping a black woman than a white man raping a white woman


#relative to black man raping a black woman
m1_abtri_alt_bmbw<-glm(abtri_c~wmbw+bmww+wmww, data=data)
summary(m1_abtri_alt_bmbw)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abtri_alt_bmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#relative to a baby resulting from a rape of a black woman by a black men, people are more likely to oppose abortion of a baby in a third trimester pregnancy resulting from white man
#raping a black woman, a black man raping a white woman, and a white man raping a white woman


#relative to black man raping a white woman
m1_abtri_bmww<-glm(abtri_c~wmbw+bmbw+wmww, data=data)
summary(m1_abtri_bmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abtri_bmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#relative to a baby resulting from a rape of a white woman by a black man, people are more likely to support abortion of a baby in a third trimester pregnancy resulting from a black man raping a black woman


#relative to black man raping a white woman
m1_abtri_wmbw<-glm(abtri_c~bmbw+bmbw+wmww, data=data)
summary(m1_abtri_wmbw)
# Create a table with the coefficients
coef_table <- broom::tidy(m1_abtri_wmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#relative to a baby resulting from a rape of a black woman by a white man, people are more likely to support abortion of a baby in a third trimester pregnancy resulting from a black man raping a black woman








data$wouldgetabortion_c<-(data$pabdefect_c+data$pabhealth_c+data$pabafford_c)


#full models

m2_abdefect_wmww<-glm(abdefect_c~wmbw+bmww+bmbw+female+age+citizen_c+white+hispanic+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c, data=data)
summary(m2_abdefect_wmww)

m2_abhealth_wmww<-glm(abhealth_c~wmbw+bmww+bmbw+female+age+citizen_c+white+hispanic+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c, data=data)
summary(m2_abhealth_wmww)


m2_ablife_wmww<-glm(ablife_c~wmbw+bmww+bmbw+female+age+citizen_c+white+hispanic+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c, data=data)
summary(m2_abhealth_wmww)

m2_absingle_wmww<-glm(absingle_c~wmbw+bmww+bmbw+female+age+citizen_c+white+hispanic+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c, data=data)
summary(m2_absingle_wmww)

m2_absupport_wmww<-glm(absupport_c~wmbw+bmww+bmbw+female+age+citizen_c+white+hispanic+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c, data=data)
summary(m2_absupport_wmww)





m2_abtri_wmww<-glm(abtri_c~wmbw+bmww+bmbw+female+age+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c, data=data)
summary(m2_abtri_wmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_wmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2

#here I am trying to create a new plot htat shows teh confidence intervals but I cannot
# tidy(fit1, conf.int = TRUE) %>% 
#   ggplot(data = ., 
#          mapping = aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
#   geom_pointrange()

library(broom)
library(dplyr)
tidy(coef_table, conf.int = TRUE) %>% 
ggplot(data=.,  mapping = aes(x = term, y = estimate, xmin = conf.low, xmax = conf.high))+
  geom_pointrange()



ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Figure 2: Model 2 Attitude Toward Third Trimester Abortion", x = "", y = "Estimate") +
  theme_bw()

#people are more likely to support abortion of a baby in a third trimester pregnancy resulting from black man
#raping a black woman than a white man raping a white woman even after controlling all variables

#significant coefficients include:
#positive: female, educyear, conservativemediacons, wouldgetabortion
#negative: religious, married, conservativeideology, wouldget abortion, auth, racialresentment

#let's do the interations of hte variables that are theoretically interesting:
m3_abtri_wmww<-glm(abtri_c~wmbw+bmww+bmbw+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+bmbw*religionpractice_c, data=data)
summary(m3_abtri_wmww)

#note that racial resentment is almost statistically significant, so let's collect some data that is 
#more representative of the US population or maybe apply some weights nad see if we can make that signifiant.


# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_wmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()

#racial resentment seems almost below 0.1 p value


#relative to black man raping a black woman
m2_abtri_alt_bmbw<-glm(abtri_c~wmbw+bmww+wmww+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+wouldgetabortion_c, data=data)
summary(m2_abtri_alt_bmbw)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_alt_bmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#relative to a baby resulting from a rape of a black woman by a black men, people are more likely to oppose abortion of a baby in a third trimester pregnancy resulting from white man
#raping a black woman, a black man raping a white woman, and a white man raping a white woman

#coefficients that are 
#negative: female, religious, married, conservativeideology, auth_c, racialresentment, 
#positive: wouldgetabortion, educyear








# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_alt_bmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()

#educyear is almost below 0.05


#relative to black man raping a white woman
m2_abtri_bmww<-glm(abtri_c~wmbw+bmbw+wmww+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+wouldgetabortion_c, data=data)
summary(m2_abtri_bmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_bmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#significant and
#positive:educyear,  wouldgetaborition
#negative: female, religious, married, conservative ideology, auth, racialresenement , religious practice, 

#relative to a baby resulting from a rape of a white woman by a black man, people are more likely to support abortion of a baby in a third trimester pregnancy resulting from a black man raping a black woman

#examining interactions
m2_abtri_bmww<-glm(abtri_c~wmbw+bmbw+wmww+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+bmbw*educyear, data=data)
summary(m2_abtri_bmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_bmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#racial resentment and education almost below 0.1 p value


#relative to black man raping a white woman
m2_abtri_wmbw<-glm(abtri_c~bmbw+bmbw+wmww+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+wouldgetabortion_c, data=data)
summary(m2_abtri_wmbw)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_wmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#relative to a baby resulting from a rape of a black woman by a white man, people are more likely to support abortion of a baby in a third trimester pregnancy resulting from a black man raping a black woman

#find out why this drops one of hte coefficients when there is still a reference group
#significant and 
#negative: religious, female, married, conservativeideology, auth, racial resentment, religious practice, would get abortion
#posistive:educyear

#testing for interactions

#relative to black man raping a white woman
m2_abtri_wmbw<-glm(abtri_c~bmbw+bmbw+wmww+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+bmbw*educyear, data=data)
summary(m2_abtri_wmbw)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_wmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#being a citizen is statistically significant with supporting abortion of a baby between a black man and a white woman
#educyear almost signfiicant at 0.109

m2_abtri_wmww<-lm(abtri_c~wmbw+bmww+bmbw+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+wouldgetabortion_c, data=data)
summary(m2_abtri_wmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_wmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#people are more likely to support abortion of a baby in a third trimester pregnancy resulting from black man
#raping a black woman than a white man raping a white woman relative to black man raping a black woman

#signfiicant and 
#negative: female, religious, married,  conservative ideology,  auth_c, racial resentment, 
#positive: educyear, wouldgetabortion

#testing for interactions

m2_abtri_wmww<-lm(abtri_c~wmbw+bmww+bmbw+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+bmbw*abfamily_c, data=data)
summary(m2_abtri_wmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_wmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#almost staitstically signfiicant at p=0.1 racial rsentment and educyear, and democrat
#bmbw:citizen_c         0.212892   0.101981   2.088  0.03711 * 



m2_abtri_alt_bmbw<-glm(abtri_c~wmbw+bmww+wmww+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+wouldgetabortion_c, data=data)
summary(m2_abtri_alt_bmbw)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_alt_bmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#relative to a baby resulting from a rape of a black woman by a black men, people are more likely to oppose abortion of a baby in a third trimester pregnancy resulting from white man
#raping a black woman, a black man raping a white woman, and a white man raping a white woman #relative to black man raping a white woman

#signfiicant and
#negative: female, religious, married, conservativeidoelogy, auth_c, racialresentment
#positive: educyear, wouldgetabortion

#test for interactions for wwwm



m2_abtri_alt_bmbw<-glm(abtri_c~wmbw+bmww+wmww+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+wmww*educyear, data=data)
summary(m2_abtri_alt_bmbw)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_alt_bmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#educyear is almost significant

m2_abtri_bmww<-glm(abtri_c~wmbw+bmbw+wmww+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+wouldgetabortion_c, data=data)
summary(m2_abtri_bmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_bmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#relative to a baby resulting from a rape of a white woman by a black man, people are more likely to support abortion of a baby in a third trimester pregnancy resulting from a black man raping a black woman

#significant and 
#negative: female, religious, married, conservativeidoelogy, auth_c, racialresentment, religiouspractice_c
#positive: educyear, wouldgetabortion

m2_abtri_bmww<-glm(abtri_c~wmbw+bmbw+wmww+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+bmbw*educyear, data=data)
summary(m2_abtri_bmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_bmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#racial resentment and educyear interaction almost signfiicant

#relative to black man raping a white woman
m2_abtri_wmbw<-glm(abtri_c~bmbw+bmbw+wmww+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+wouldgetabortion_c, data=data)
summary(m2_abtri_wmbw)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_wmbw) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#relative to a baby resulting from a rape of a black woman by a white man, people are more likely to support abortion of a baby in a third trimester pregnancy resulting from a black man raping a black woman
#significant and
#negative: married, conservative ideology, auth_c, racialresentment,
#positive: education, wouldgetaborition

#test for interaction
m2_abtri_bmww<-glm(abtri_c~wmbw+bmbw+wmww+female+citizen_c+white+hispanic+ethnocentrism+religious+protestant+educyear+logincome+married+democrat_c+republican_c+conservativeideology+conservativemediacons+auth_c+racialresentment_c+religionpractice_c+religionimportant+genderlib_c+nationalism_c+abfamily_c+bmww*educyear, data=data)
summary(m2_abtri_bmww)
# Create a table with the coefficients
coef_table <- broom::tidy(m2_abtri_bmww) %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate_if(is.numeric, ~round(., 3))
# Print the table using stargazer
stargazer(coef_table, type = "html", title = "Coefficients Table")
# Plot the coefficients using ggplot2
ggplot(coef_table, aes(x = term, y = estimate)) +
  geom_bar(stat = "identity", fill = "blue", alpha = 0.5) +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "", y = "Estimate") +
  theme_bw()
#educyear is statistically significant




data%>%
as.numeric(c(absingle, abhusband, abdefect, abhealth, ablife, absupport, abtri))