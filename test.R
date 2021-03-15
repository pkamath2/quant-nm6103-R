persons = c("Michael", "Alice", "Sanjay", "Claudia", "Bob", "Jack", "Mary", "Jill")
heights = c(140, 176, 154, 143, 166, 180, 148, 178)

df = data.frame(cbind(persons, heights))
print(df$heights)

df$heights = as.numeric(df$heights)
print(df$heights > 150)
print(class(df$heights))

print(df[df$heights > 170,])

print(df[df$heights > 120,][order(heights),])


print(mean(df$heights))
print(median(df$heights))
print(sd(df$heights))


print(summary(df$heights))


df$gender = c("M", "F", "M", "F", "M", "M", "F", "F")
df

summary(df[df$gender == 'M',]$heights)

summary(df[df$gender == 'F',]$heights)


str(df)


gapminder_df = read.csv('gapminder.csv')
gapminder_df
head(gapminder_df)


View(gapminder_df)
gapminder_df_2007 = gapminder_df[gapminder_df$year == 2007,]
View(gapminder_df_2007)
