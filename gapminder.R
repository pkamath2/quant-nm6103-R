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
