gapminder_df = read.csv('gapminder.csv')
gapminder_df
head(gapminder_df)


View(gapminder_df)
gapminder_df_2007 = gapminder_df[gapminder_df$year == 2007,]
View(gapminder_df_2007)

print(unique(gapminder_df$continent))
print(unique(gapminder_df_2007$continent))

# "Asia"     "Europe"   "Africa"   "Americas" "Oceania" 
print(mean(gapminder_df_2007[gapminder_df_2007$continent == 'Asia',]$lifeExp))
print(mean(gapminder_df_2007[gapminder_df_2007$continent == 'Europe',]$lifeExp))
print(mean(gapminder_df_2007[gapminder_df_2007$continent == 'Africa',]$lifeExp))
print(mean(gapminder_df_2007[gapminder_df_2007$continent == 'Americas',]$lifeExp))
print(mean(gapminder_df_2007[gapminder_df_2007$continent == 'Oceania',]$lifeExp))



plot(gapminder_df_2007$lifeExp, gapminder_df_2007$gdpPercap)
abline(lm(gapminder_df_2007$gdpPercap ~ gapminder_df_2007$lifeExp))


# 5th biggest population in 1957
gapminder_df_1957 = gapminder_df[gapminder_df$year == 1957,]
gapminder_df_1957 = gapminder_df_1957[order(gapminder_df_1957$pop, decreasing = TRUE),]
View(gapminder_df_1957)
print(gapminder_df_1957[5,])

# Lowest avg population in 1982
gapminder_df_1982 = gapminder_df[gapminder_df$year == 1982,]
continents_1982 = unique(gapminder_df_1982$continent)
continents_1982_avgpop = data.frame(continent = numeric(0), avg_pop=numeric(0))
for (continent in continents_1982) {
  avg_pop = mean(gapminder_df_1982[gapminder_df_1982$continent == continent, ]$pop)
  continents_1982_avgpop[nrow(continents_1982_avgpop)+1, ] = c(continent, avg_pop)
}
continents_1982_avgpop$avg_pop = as.numeric(continents_1982_avgpop$avg_pop)
continents_1982_avgpop = continents_1982_avgpop[order(continents_1982_avgpop$avg_pop, decreasing = FALSE), ]
print(continents_1982_avgpop[1,])

#Highest percapita in 1952
gapminder_df_1952 = gapminder_df[gapminder_df$year == 1952,]
gapminder_df_1952 = gapminder_df_1952[order(gapminder_df_1952$gdpPercap, decreasing = TRUE), ]
print(gapminder_df_1952[1,'country'])
View(gapminder_df_1952)
