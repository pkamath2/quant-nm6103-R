library(stringr)
library(ggplot2)
library(dplyr)

#Helper function
print_summary_stats = function(some_data) {
  m = mean(some_data)
  sd = sd(some_data)
  rg = str_c(range(some_data), collapse = "-")
  med = median(some_data)
  count = length(some_data)

  sprintf('Mean = %f, Median = %f, Std = %f, Range = %s, Total count = %f', m, med, sd, rg, count)
}

data = read.csv('group-project/group1.csv')
updated_data = data
updated_data$emotion = colnames(updated_data[5:8])[max.col(updated_data[5:8],ties.method="first")]
View(updated_data)

anger_treatment_data = updated_data[updated_data$treatment_type == 1,]
sadness_treatment_data = updated_data[updated_data$treatment_type == 2,]
excitement_treatment_data = updated_data[updated_data$treatment_type == 3,]


ai_data = updated_data[updated_data$human == 0,]
human_data = updated_data[updated_data$human == 1,]

ai_anger_treatment_data = ai_data[ai_data$treatment_type == 1,]
ai_sadness_treatment_data = ai_data[ai_data$treatment_type == 2,]
ai_excitement_treatment_data = ai_data[ai_data$treatment_type == 3,]

human_anger_treatment_data = human_data[human_data$treatment_type == 1,]
human_sadness_treatment_data = human_data[human_data$treatment_type == 2,]
human_excitement_treatment_data = human_data[human_data$treatment_type == 3,]

#NOTE: Our data is not normally distributed.
# shapiro.test(updated_data$naturalness) - low p-value, hence reject null hypothesis that data is normal.

#--------------------------- Begin Hypothesis testing ----------------------------
#H1 (we don't really have a H1 right now, since Prof changed our data to report just intensities)

human_treatment_vs_emotion = table(human_data$treatment_type, human_data$emotion)
ai_treatment_vs_emotion = table(ai_data$treatment_type, ai_data$emotion)

print(human_treatment_vs_emotion)
print(ai_treatment_vs_emotion)

chisq.test(human_treatment_vs_emotion) # low p-value. That means our treatments are working. 

chisq.test(ai_treatment_vs_emotion) #low p-value. Therefore AI treatments are also working same as human


### Conclusion for H1: There is correlation between treatments and self-reported emotions for both AI treatments 
### and human treatments. What I mean is, generally speaking treatment-1 (for anger), the participants generally reported 
### higher values for anger and similarly for treatments 2 & 3. i.e. our treatments are working.

#*****************************---------------------------*******************************
#H2

### test for anger (welch t-test are for unequal variances)
print_summary_stats(ai_anger_treatment_data$anger)
print_summary_stats(human_anger_treatment_data$anger)
boxplot(human_anger_treatment_data$anger, ai_anger_treatment_data$anger, names=c('human anger', 'ai anger'))
anger_treatment_plot_data = anger_treatment_data %>% group_by(anger, human) %>% summarize(count=n())
ggplot() + geom_col(data = anger_treatment_plot_data, aes(x = anger, y=count, fill = human), position = "dodge2") #light blue is human, dark blue is AI.

t.test(ai_anger_treatment_data$anger, human_anger_treatment_data$anger) # AI anger mean = 3.48; Human anger mean = 3.38. Almost same.

### test for sadness
print_summary_stats(ai_sadness_treatment_data$sadness)
print_summary_stats(human_sadness_treatment_data$sadness)
boxplot(human_sadness_treatment_data$sadness, ai_sadness_treatment_data$sadness, names=c('human sadness', 'ai sadness'))
sadness_treatment_plot_data = sadness_treatment_data %>% group_by(sadness, human) %>% summarize(count=n())
ggplot() + geom_col(data = sadness_treatment_plot_data, aes(x = sadness, y=count, fill = human), position = "dodge2")
t.test(ai_sadness_treatment_data$sadness, human_sadness_treatment_data$sadness) # AI sadness mean = 3.56; Human sadness mean = 3.44. Almost same.

### test for excitement
print_summary_stats(ai_excitement_treatment_data$excitement)
print_summary_stats(human_excitement_treatment_data$excitement)
boxplot(human_excitement_treatment_data$excitement, ai_excitement_treatment_data$excitement, names=c('human excitement', 'ai excitement'))
excitement_treatment_plot_data = excitement_treatment_data %>% group_by(excitement, human) %>% summarize(count=n())
ggplot() + geom_col(data = excitement_treatment_plot_data, aes(x = excitement, y=count, fill = human), position = "dodge2")
t.test(ai_excitement_treatment_data$excitement, human_excitement_treatment_data$excitement) # AI sadness mean = 3.91; Human sadness mean = 3.95. Almost same.

### Conclusion for H2: 
### 1. Looking at the plots, the distribution of intensity scores for both human and AI looks the same. 
### 2. The above t-test analysis show there is a chance of true equivalence in our findings (almost same means - cannot reject H0).
### Note: In order to confirm there is indeed true equivalence at population level 
### (and not result of sampling error i.e. we are finding true equivalence because of low sample size) should we conduct a power test?

# Power analysis will tell us if there is enough data in our data set to not reject null hypothesis.

#DO LATER - anger power analysis

# mean_anger_ai = mean(ai_anger_treatment_data$anger)
# sd_anger_ai = sd(ai_anger_treatment_data$anger)
# mean_anger_human = mean(human_anger_treatment_data$anger)
# sd_anger_human = sd(human_anger_treatment_data$anger)
# 
# sprintf('Mean anger AI = %f, Mean anger human = %f',mean_anger_ai, mean_anger_human) #"Mean anger AI = 3.480000, Mean anger human = 3.380000"
# sprintf('SD anger AI = %f, SD anger human = %f',sd_anger_ai, sd_anger_human) #"SD anger AI = 0.926381, SD anger human = 0.972137"
# 
# effect_size = (mean_anger_ai - mean_anger_human)/sd_anger_human
# sprintf('Effect size for anger = %f',effect_size)
# 
# install.packages('pwr')
# library(pwr)
# pwr.t.test(n=200, d=effect_size , sig.level = 0.05 )
#Our hypothesis is correct 


#*****************************---------------------------*******************************
#H3 - Test for naturalness

print_summary_stats(human_data$naturalness)
print_summary_stats(ai_data$naturalness)
t.test(ai_data$naturalness, human_data$naturalness)
boxplot(human_data$naturalness, ai_data$naturalness)

# To illustrate further - Calculate effect of human vs. ai on naturalness
lm_naturalness = lm(naturalness ~ human, data=updated_data) 
summary(lm_naturalness)
# Coefficient = 1.74333 and p-value very low. i.e. human vs. AI generation has effect on perceived naturalness.
# Therefore, H3 cannot be accepted. 

### Conclusion for H3: H3 cannot be accepted.  

#*****************************---------------------------*******************************

# H4a - Test for moderator on naturalness

## lang moderation - english (controlling for lang_engr)
lm_naturalness_lang_eng = lm(naturalness ~ human  * lang_eng, data=updated_data)
summary(lm_naturalness_lang_eng)
### human:lang_eng coefficient = 0.259058 and human coefficient = 0.822769 (reduced from 1.74333 from lm_naturalness above) and p-value is very low. 
### That means, lang_eng moderates perceived naturalness. 

## lang moderation - other (controlling for lang_other)
lm_naturalness_lang_other = lm(naturalness ~ human  * lang_other, data=updated_data)
summary(lm_naturalness_lang_other)
### human:lang_other coefficient = 0.019945 and human coefficient = 1.671317 (reduced from 1.74333 from lm_naturalness above) and p-value is very low. 
### That means, lang_other moderates perceived naturalness. 

### Conclusion for H4a: lang_eng and lang_other moderates perceived naturalness
-----------------------------

# H4b - Test for language as a moderator on emotional intensity

## lang moderation - english
anger_treatment_data = updated_data[updated_data$treatment_type == 1,]
sadness_treatment_data = updated_data[updated_data$treatment_type == 2,]
excitement_treatment_data = updated_data[updated_data$treatment_type == 3,]

lm_anger = lm(anger ~ human, data=anger_treatment_data)
summary(lm_anger) # p-value = 0.457
lm_anger_lang_eng = lm(anger ~ human * lang_eng, data=anger_treatment_data)
summary(lm_anger_lang_eng) # p-value = 0.463 (human) and 0.575 (human:lang_eng)
# No moderator effect evident

lm_sad = lm(sadness ~ human, data=sadness_treatment_data)
summary(lm_sad) # p-value = 0.357
lm_sad_lang_eng = lm(sadness ~ human * lang_eng, data=sadness_treatment_data)
summary(lm_sad_lang_eng) # p-value = 0.581 (human) and 0.719 (human:lang_eng)
# No moderator effect evident

lm_excitement = lm(excitement ~ human, data=excitement_treatment_data)
summary(lm_excitement) # p-value = 0.724
lm_excitement_lang_eng = lm(excitement ~ human * lang_eng, data=excitement_treatment_data)
summary(lm_excitement_lang_eng) # p-value = 0.626 (human) and 0.672 (human:lang_eng)
# No moderator effect evident

### Conclusion for H4b:  lang_eng and lang_other DO NOT moderate instensity of emotion  

#--------------------------- End of Hypothesis testing ----------------------------------






#----------------------------------------------------------------------------------------

# Exploratory analysis

# 1. What do we do for other - shall we treat it like an emotion - neutral emotion?
# ----

# 2. For treatment_type = 1, but emotion is not anger

not_anger_data = updated_data[updated_data$treatment_type == 1 & updated_data$emotion != 'anger' & updated_data$emotion != 'other',]
not_anger_plot_data = not_anger_data %>% group_by(emotion, human) %>% summarize(count=n())
ggplot() + geom_col(data = not_anger_plot_data, aes(x = emotion, y=count, fill = human), position = "dodge2")
# Ask prof - shall we clean this dataset??
# There are equivalent number of treatments that have been labelled as sad instead of anger. Probably can ignore - Need to confirm.

# 3. For treatment_type = 2, but emotion is not sadness
not_sadness_data = updated_data[updated_data$treatment_type == 2 & updated_data$emotion != 'sadness' & updated_data$emotion != 'other',]
not_sadness_plot_data = not_sadness_data %>% group_by(emotion, human) %>% summarize(count=n())
ggplot() + geom_col(data = not_sadness_plot_data, aes(x = emotion, y=count, fill = human), position = "dodge2")
# Ask prof - shall we clean this dataset??
# Probably can ignore - Need to confirm.

# 4. For treatment_type = 3, but emotion is not excitement
not_excitement_data = updated_data[updated_data$treatment_type == 3 & updated_data$emotion != 'excitement' & updated_data$emotion != 'other',]
not_excitement_plot_data = not_excitement_data %>% group_by(emotion, human) %>% summarize(count=n())
ggplot() + geom_col(data = not_excitement_plot_data, aes(x = emotion, y=count, fill = human), position = "dodge2")
# Ask prof - shall we clean this dataset??
# Probably can ignore - Need to confirm.

