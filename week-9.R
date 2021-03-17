x1 = 1:10
y1 = 11:20
cor.test(x1, y1)



x = sample(1:100, 10)
y = sample(1:100, 10)
print(x)
print(y)
cor(x, y)

cor.test(x, y)


head(mtcars)
cor(mtcars$disp, mtcars$hp)
cor.test(mtcars$disp, mtcars$hp)

cor.test(mtcars$hp, mtcars$mpg)




migrant_salaries <- c(1100, 1200, 900, 685, 1320, 1055, 745, 1350, 1400,
                      1700, 1650, 1245, 1120, 1075, 980, 1025, 1550, 1945,
                      750, 1140, 1250, 1090, 1150, 940, 880, 950)
t.test(migrant_salaries, mu = 3940)

t.test(migrant_salaries, mu = 3940, alternative = "less")
t.test(migrant_salaries, mu = 3940, alternative = "greater")


citizen_salaries <- c(3500, 4250, 2560, 1900, 6750, 7500, 3550, 2820, 6520,
                      4570, 1700, 2510, 8440, 7510, 4580)


t.test(migrant_salaries, citizen_salaries) # there is no longer a mu
t.test(migrant_salaries, citizen_salaries, alternative = 'greater') # there is no longer a mu





migrant_salaries_201718 <- c(1100, 1200, 900, 685, 1320, 1055, 745, 1350, 1400,
                             1700, 1650, 1245, 1120, 1075, 980, 1025, 1550, 1945,
                             750, 1140, 1250, 1090, 1150, 940, 880, 950)
migrant_salaries_201819 <- c(1150, 1210, 950, 700, 1380, 1100, 800, 1380, 1450,
                             1780, 1700, 1295, 1190, 1145, 1020, 1100, 1600, 2000,
                             750, 1150, 1250, 1090, 1150, 980, 900, 980)


t.test(migrant_salaries_201718, migrant_salaries_201819, paired = TRUE, alternative='greater')

df = read.csv('media_knowledge.csv')
head(df)
View(df)
tbl = table(df$Knowledge, df$News)
print(tbl)

chisq.test(tbl)


library(MASS)
head(survey)
View(survey)
tbl_survey = table(survey$Exer, survey$Smoke)
print(tbl_survey)
chisq.test(tbl_survey)


tbl_survey = table(survey$Smoke, survey$Exer)
print(tbl_survey)
chisq.test(tbl_survey)
