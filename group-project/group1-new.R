library(stringr)
library(ggplot2)
library(dplyr)
library(sjPlot)

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
updated_data$emotion_with_affinity = '' #Initialize


# Enrich data with new columns 
anger_seq = cbind('anger','sadness','excitement','other') #Note the sequence
updated_data[updated_data$treatment_type == 1,]$emotion_with_affinity = anger_seq[max.col(updated_data[updated_data$treatment_type == 1,][1:200, anger_seq],ties.method="first")]

sadness_seq = cbind('sadness','anger','excitement','other') #Note the sequence
updated_data[updated_data$treatment_type == 2,]$emotion_with_affinity = sadness_seq[max.col(updated_data[updated_data$treatment_type == 2,][1:200,sadness_seq],ties.method="first")]

excitement_seq = cbind('excitement','sadness','anger','other') #Note the sequence
updated_data[updated_data$treatment_type == 3,]$emotion_with_affinity = excitement_seq[max.col(updated_data[updated_data$treatment_type == 3,][1:200,excitement_seq],ties.method="first")]

updated_data$emotion = colnames(updated_data[5:8])[max.col(updated_data[5:8],ties.method="first")]

updated_data$secondary_emotion_residual = 0
updated_data[updated_data$treatment_type == 1,]$secondary_emotion_residual = (updated_data[updated_data$treatment_type == 1,]$sadness + updated_data[updated_data$treatment_type == 1,]$excitement + updated_data[updated_data$treatment_type == 1,]$other) / 3
updated_data[updated_data$treatment_type == 2,]$secondary_emotion_residual = (updated_data[updated_data$treatment_type == 2,]$anger + updated_data[updated_data$treatment_type == 2,]$excitement + updated_data[updated_data$treatment_type == 2,]$other) / 3
updated_data[updated_data$treatment_type == 3,]$secondary_emotion_residual = (updated_data[updated_data$treatment_type == 3,]$anger + updated_data[updated_data$treatment_type == 3,]$sadness + updated_data[updated_data$treatment_type == 3,]$other) / 3

View(updated_data)

# Extract subsets to evaluate in the scripts below
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

# We dont need this. Kept for posterity. 
# NOTE: Our data is not normally distributed. 
# shapiro.test(updated_data$naturalness) - low p-value, hence reject null hypothesis that data is normal?
# ks.test(updated_data$naturalness, 'pnorm')??

##################
#Plots - analysis using plots
##################

# For treatment_type = 1
all_anger_data = updated_data[updated_data$treatment_type == 1,]
all_anger_plot_data = all_anger_data %>% group_by(emotion_with_affinity, factor(human)) %>% summarize(count=n())
print(all_anger_plot_data)
all_anger_plot = ggplot() + geom_col(data = all_anger_plot_data, aes(x = emotion_with_affinity, y=count, fill = `factor(human)`), position = "dodge2")
all_anger_plot = all_anger_plot + ggtitle('Treatment 1 (Anger)') + theme(plot.title = element_text(hjust = 0.5))
all_anger_plot = all_anger_plot + labs(x = 'emotion')
all_anger_plot + scale_fill_discrete(name = element_blank(), labels = c("AI", "human"))

# For treatment_type = 2
all_sadness_data = updated_data[updated_data$treatment_type == 2,]
all_sadness_plot_data = all_sadness_data %>% group_by(emotion_with_affinity, factor(human)) %>% summarize(count=n())
print(all_sadness_plot_data)
all_sadness_plot = ggplot() + geom_col(data = all_sadness_plot_data, aes(x = emotion_with_affinity, y=count, fill = `factor(human)`), position = "dodge2")
all_sadness_plot = all_sadness_plot + ggtitle('Treatment 2 (Sadness)') + theme(plot.title = element_text(hjust = 0.5))
all_sadness_plot = all_sadness_plot + labs(x = 'emotion')
all_sadness_plot + scale_fill_discrete(name = element_blank(), labels = c("AI", "human"))

# For treatment_type = 3
all_excitement_data = updated_data[updated_data$treatment_type == 3,]
all_excitement_plot_data = all_excitement_data %>% group_by(emotion, factor(human)) %>% summarize(count=n())
print(all_excitement_plot_data)
all_excitement_plot = ggplot() + geom_col(data = all_excitement_plot_data, aes(x = emotion, y=count, fill = `factor(human)`), position = "dodge2")
all_excitement_plot = all_excitement_plot + ggtitle('Treatment 3 (Excitement)') + theme(plot.title = element_text(hjust = 0.5))
all_excitement_plot = all_excitement_plot + labs(x = 'emotion')
all_excitement_plot + scale_fill_discrete(name = element_blank(), labels = c("AI", "human"))



#--------------------------- Begin Hypothesis testing ----------------------------
#H1 (we don't really have a H1 right now, since our data reports just intensities) - but using this section for validity checks.

# Face (or construct?) validity. Are our treatments working?
human_treatment_vs_emotion = table(human_data$treatment_type, human_data$emotion)
ai_treatment_vs_emotion = table(ai_data$treatment_type, ai_data$emotion)

print(human_treatment_vs_emotion)
print(ai_treatment_vs_emotion)

chisq.test(human_treatment_vs_emotion) # low p-value. That means our treatments are working. 
chisq.test(ai_treatment_vs_emotion) #low p-value. Therefore AI treatments are also working same as human

### Conclusion for face validity: There is correlation between treatments and self-reported emotions for both AI treatments 
### and human treatments. What I mean is, generally speaking treatment-1 (for anger), the participants generally reported 
### higher values for anger and similarly for treatments 2 & 3. i.e. our treatments are working.

# Convergent validity
lm_sadness_excitement_anger_treatment = lm(updated_data$sadness ~ updated_data$excitement)
summary(lm_sadness_excitement_anger_treatment)
plot(updated_data$sadness , updated_data$excitement)
abline(lm_sadness_excitement_anger_treatment)

lm_anger_excitement_treatment = lm(updated_data$anger ~ updated_data$excitement)
summary(lm_anger_excitement_treatment)
plot(updated_data$sadness , updated_data$excitement)
abline(lm_anger_excitement_treatment)

## Conclusion for convergent validity: Low p-values and negative coefficient indicate the negative correlation.
# i.e., excitement (positive emotion) reduces as anger or sadness (negative emotions) increases

#*****************************---------------------------*******************************
#H2

### test for anger (welch t-test are for unequal variances)
print_summary_stats(ai_anger_treatment_data$anger)
print_summary_stats(human_anger_treatment_data$anger)
boxplot(human_anger_treatment_data$anger, ai_anger_treatment_data$anger, names=c('human anger', 'ai anger'))
anger_treatment_plot_data = anger_treatment_data %>% group_by(anger, factor(human)) %>% summarize(count=n())
print(anger_treatment_plot_data)

anger_treatment_plot = ggplot() + geom_col(data = anger_treatment_plot_data, aes(x = anger, y=count, fill = `factor(human)`), position = "dodge2")
anger_treatment_plot = anger_treatment_plot + ggtitle('Treatment 1 (Anger) Scores Historgram') + theme(plot.title = element_text(hjust = 0.5))
anger_treatment_plot + scale_fill_discrete(name = element_blank(), labels = c("AI", "human"))

t.test(ai_anger_treatment_data$anger, human_anger_treatment_data$anger) # AI anger mean = 3.48; Human anger mean = 3.38. Almost same.


### test for sadness
print_summary_stats(ai_sadness_treatment_data$sadness)
print_summary_stats(human_sadness_treatment_data$sadness)
boxplot(human_sadness_treatment_data$sadness, ai_sadness_treatment_data$sadness, names=c('human sadness', 'ai sadness'))
sadness_treatment_plot_data = sadness_treatment_data %>% group_by(sadness, factor(human)) %>% summarize(count=n())

sadness_treatment_plot = ggplot() + geom_col(data = sadness_treatment_plot_data, aes(x = sadness, y=count, fill = `factor(human)`), position = "dodge2")
sadness_treatment_plot = sadness_treatment_plot + ggtitle('Treatment 2 (Sadness) Scores Historgram') + theme(plot.title = element_text(hjust = 0.5))
sadness_treatment_plot + scale_fill_discrete(name = element_blank(), labels = c("AI", "human"))


t.test(ai_sadness_treatment_data$sadness, human_sadness_treatment_data$sadness) # AI sadness mean = 3.56; Human sadness mean = 3.44. Almost same.

### test for excitement
print_summary_stats(ai_excitement_treatment_data$excitement)
print_summary_stats(human_excitement_treatment_data$excitement)
boxplot(human_excitement_treatment_data$excitement, ai_excitement_treatment_data$excitement, names=c('human excitement', 'ai excitement'))
excitement_treatment_plot_data = excitement_treatment_data %>% group_by(excitement, factor(human)) %>% summarize(count=n())

excitement_treatment_plot = ggplot() + geom_col(data = excitement_treatment_plot_data, aes(x = excitement, y=count, fill = `factor(human)`), position = "dodge2")
excitement_treatment_plot = excitement_treatment_plot + ggtitle('Treatment 3 (Excitement) Scores Historgram') + theme(plot.title = element_text(hjust = 0.5))
excitement_treatment_plot + scale_fill_discrete(name = element_blank(), labels = c("AI", "human"))



t.test(ai_excitement_treatment_data$excitement, human_excitement_treatment_data$excitement) # AI sadness mean = 3.91; Human sadness mean = 3.95. Almost same.

### Conclusion for H2: 
### 1. Looking at the plots, the distribution of intensity scores for all treatments for both human and AI looks the same. 
### 2. The above t-test analysis show there is a chance of true equivalence in our findings (almost same means - cannot reject H0).
### Note: In order to confirm there is indeed true equivalence at population level 
### (and not result of sampling error i.e. we are finding true equivalence because of low sample size) should we conduct a power test?


#*****************************---------------------------*******************************
#H3 - Test for naturalness

print_summary_stats(human_data$naturalness)
print_summary_stats(ai_data$naturalness)
t.test(ai_data$naturalness, human_data$naturalness)
boxplot(human_data$naturalness, ai_data$naturalness)

# To illustrate further - Calculate effect of human vs. ai on naturalness
lm_naturalness = lm(naturalness ~ human, data=updated_data) 
summary(lm_naturalness)
plot_model(lm_naturalness)
plot(updated_data$human, updated_data$naturalness)
abline(lm_naturalness)
# Coefficient = 1.74333 and p-value very low. i.e. human vs. AI generation has effect on perceived naturalness.
# Therefore, H3 cannot be accepted with our current data. 

### Conclusion for H3: H3 cannot be accepted with our current data.  

#*****************************---------------------------*******************************

# H4a - Test for moderator on naturalness

## lang moderation - english (controlling for lang_engr)
lm_naturalness_lang_eng = lm(naturalness ~ human  * lang_eng, data=updated_data)
summary(lm_naturalness_lang_eng)
plot(updated_data$human, updated_data$naturalness)
abline(lm_naturalness)
abline(lm_naturalness_lang_eng)
### human:lang_eng coefficient = 0.259058 and human coefficient = 0.822769 (reduced from 1.74333 from lm_naturalness above) and p-value is very low. 
### That means, lang_eng moderates perceived naturalness. 

## lang moderation - other (controlling for lang_other)
lm_naturalness_lang_other = lm(naturalness ~ human  * lang_other, data=updated_data)
summary(lm_naturalness_lang_other)
abline(lm_naturalness_lang_other)
### human:lang_other coefficient = 0.019945 and human coefficient = 1.671317 (reduced from 1.74333 from lm_naturalness above) and p-value is NOT very low. 
### That means, lang_other DOES NOT moderate perceived naturalness. 

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
lm_anger_lang_other = lm(anger ~ human * lang_other, data=anger_treatment_data)
summary(lm_anger_lang_other)
# No moderator effect evident
plot(anger_treatment_data$human, anger_treatment_data$anger, xlab='human/ai', ylab='anger')
abline(lm_anger, col="blue")
abline(lm_anger_lang_eng, col="red")
abline(lm_anger_lang_other, col="green")


lm_sad = lm(sadness ~ human, data=sadness_treatment_data)
summary(lm_sad) # p-value = 0.357
lm_sad_lang_eng = lm(sadness ~ human * lang_eng, data=sadness_treatment_data)
summary(lm_sad_lang_eng) # p-value = 0.581 (human) and 0.719 (human:lang_eng)
# No moderator effect evident
lm_sad_lang_other = lm(sadness ~ human * lang_other, data=sadness_treatment_data)
summary(lm_sad_lang_other)

lm_excitement = lm(excitement ~ human, data=excitement_treatment_data)
summary(lm_excitement) # p-value = 0.724
lm_excitement_lang_eng = lm(excitement ~ human * lang_eng, data=excitement_treatment_data)
summary(lm_excitement_lang_eng) # p-value = 0.626 (human) and 0.672 (human:lang_eng)
# No moderator effect evident
lm_excitement_lang_other = lm(excitement ~ human * lang_other, data=excitement_treatment_data)
summary(lm_excitement_lang_other)

### Conclusion for H4b:  lang_eng and lang_other DO NOT moderate instensity of emotion  

#--------------------------- End of Hypothesis testing ----------------------------------






#----------------------------------------------------------------------------------------

# Exploratory analysis

#1. Residual emotions analysis (Or Secondary emotions analysis? - Needs rewording)
print_summary_stats(sadness_treatment_data$sadness)
print_summary_stats(sadness_treatment_data$secondary_emotion_residual)
t.test(sadness_treatment_data$sadness, sadness_treatment_data$secondary_emotion_residual, alternative = 'greater')



