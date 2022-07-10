# Przygotowanie danych
setwd('C:/Users/Arek/Desktop/Studia_II_stopien/RMiSW/SW')
df = read.csv('life_expectancy.csv')

df$Country <- factor(df$Country)
df$Year <- factor(df$Year)
df$Status <- factor(df$Status)

# Dopasowanie modleu liniowego
fit_lm <- lm(Life.expectancy ~ . -Country -Year -Status, data = df)
summary(fit_lm) # Odrzucamy hipotezę H_0 na poziomie istotności 0.05

# Z modelu odrzucamy Alcohol, percentage.expenditure, Hepatitis.B, Measles, Polio,
# Total.expenditure, GDP, Population, thinness..1.19.years, thinness.5.9.years

new_fit_lm <- lm(Life.expectancy ~.  
                 -Country 
                 -Year 
                 -Status 
                 - Alcohol 
                 - percentage.expenditure 
                 - Hepatitis.B 
                 - Measles 
                 - Polio
                 - Total.expenditure 
                 - GDP 
                 - Population 
                 - thinness..1.19.years 
                 - thinness.5.9.years, 
                 data=df)
summary(new_fit_lm)

# Charakter wpływu:
# Adult.Mortality - ujemny, mały
# infant.deaths - dodatni, mały
# BMI - dodatni, mały
# under.five.deaths - ujemny, mały
# Diphtheria - dodatni, mały
# HIV.AIDS - ujemny, mały
# Income.composition.of.resources - dodatni, duży
# Schooling - dodatni, mały

# Dopasowanie do danych jest przyzwoite:
summary(new_fit_lm)$adj.r.squared
summary(new_fit_lm)$r.squared
