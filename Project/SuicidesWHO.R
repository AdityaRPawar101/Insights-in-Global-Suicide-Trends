library(ggplot2)
library(plotly)
library(reshape2)
library(magrittr)
library(multipanelfigure)
library(countrycode)

dataset <- read.csv("D:\\Academics\\Semester 6\\NAS1001_ASSOCIATIVE-DATA-ANALYST_Harshlata Vishwakarma\\who_suicide_statistics.csv")
dataset[sample(1:nrow(dataset),10),]

str(dataset)

# we execute these two commands so that the corresponding variables more storage 
dataset$suicides_no <- as.numeric(dataset$suicides_no)
dataset$population <- as.numeric(dataset$population)

# We can see that basically, the variables Country, Year, Sex and Age 
# work like an index in our dataset and that’s why they shouldn’t have any correlation between them. 
# And speaking of correlations between the variables, 
# let's plot a heatmap in order to find how these variables relate to each other.

num.data <- dataset[complete.cases(dataset),]
num.data$sex = as.integer(factor(num.data$sex, labels = 1:length(unique(num.data$sex))))
num.data$country = as.integer(factor(num.data$country, 
                                     labels = 1:length(unique(num.data$country))))
num.data$age = as.integer(factor(num.data$age, labels = 1:length(unique(num.data$age))))

cormat <- round(cor(num.data),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

cormat

sapply(dataset, function(x) sum(is.na(x)))

# Total rows containing NA values
na_df <- dataset[is.na(dataset$suicides_no) | is.na(dataset$population),]
nrow(na_df)

#Handling NA values
# We will try to compute some metrics in order to see if the NA values for both variables 
#(Population, Suicide Number) are seen mostly in a specific year or country.

na_population <- dataset[is.na(dataset$population),]

# how many missing rows by country due to missing population
# the purpose of the next command is linked with the plotting. Since the new dataframe 
# na_population inherits the factor variable country, the levels remain exactly the same. 
# So, despite the fact that na_population contains some of the countries, the levels of this 
# factor contains all of them. So through this way, now the levels are changed to only the 
# ones that variable contains, and when we plot the data only the present countries are 
# shown. (You can verify this by plotting the data without executing the following command)

na_population$country <- factor(na_population$country, 
                                levels = unique(na_population$country))
na_population_by_country <- as.data.frame(table(na_population$country))
colnames(na_population_by_country) <- c('country', 'frequence')
# order data frame by decreasing frequnce
na_population_by_country <- na_population_by_country[order(-na_population_by_country$frequence),]
# order factor so that we can plot in decreasing freqence
na_population_by_country$country <- factor(na_population_by_country$country, 
                                           levels = unique(na_population_by_country$country[
                                             order(-na_population_by_country$frequence, 
                                                   na_population_by_country$country)]))
# plotting na values of population by country in decreasing order
ggplot(data=na_population_by_country, aes(x=country, y=frequence, fill = country)) +
  geom_bar(stat="identity", width = 0.3) +
  theme(axis.text.x=element_blank()) + ggtitle('NA Population Values per Country')

na_population$year <- factor(na_population$year, levels = unique(na_population$year))
na_population_by_year <- as.data.frame(table(na_population$year))
colnames(na_population_by_year) <- c('year', 'frequence')
na_population_by_year$year <- factor(na_population_by_year$year, 
                                     levels = 1978:2016, ordered = T)
ggplot(data=na_population_by_year, aes(x=year, y=frequence, fill = year)) +
  geom_bar(stat="identity", width = 0.3) +
  theme(axis.text.x=element_blank()) + ggtitle('NA Population values per Year')

na_suicides <- dataset[is.na(dataset$suicides_no),]
# we remove levels from the country and year factor that are missing
na_suicides$country <- factor(na_suicides$country, levels = unique(na_suicides$country))
na_suicides_by_country <- as.data.frame(table(na_suicides$country))
colnames(na_suicides_by_country) <- c('country', 'frequence')
# order levels of countries depending on the missing rows

# order data frame by decreasing frequence
na_suicides_by_country <- na_suicides_by_country[order(-na_suicides_by_country$frequence),]
# order factor so that we can plot in decreasing freqence
na_suicides_by_country$country <- factor(na_suicides_by_country$country, 
                                         levels = unique(na_suicides_by_country$country[
                                           order(-na_suicides_by_country$frequence, 
                                                 na_suicides_by_country$country)]))


ggplot(data=na_suicides_by_country, aes(x=country, y=frequence, fill = country)) +
  geom_bar(stat="identity", width = 0.3) + 
  ggtitle('NA suicide_no values per Country') + 
  theme(axis.text.x=element_blank())

na_suicides$year <- factor(na_suicides$year, levels = unique(na_suicides$year))
na_suicides_by_year <- as.data.frame(table(na_suicides$year))
colnames(na_suicides_by_year) <- c('year', 'frequence')
na_suicides_by_year$year <- factor(na_suicides_by_year$year, 
                                   levels = 1978:2016, ordered = T)

ggplot(data=na_suicides_by_year, aes(x=year, y=frequence, fill = year)) +
  geom_bar(stat="identity", width = 0.3) + 
  ggtitle('NA suicide_no Values per Year') + 
  theme(axis.text.x=element_blank())

na_population_by_age <- as.data.frame(table(na_population$age))
na_population_by_age

na_suicides_by_age <- as.data.frame(table(na_suicides$age))
na_suicides_by_age

how_many_countries_per_year = data.frame(matrix(ncol = 2, nrow = 0))
colnames(how_many_countries_per_year) <- c('year', 'countries_number')
for(year in range(dataset$year)[1]:range(dataset$year)[2]){
  how_many_countries_per_year <- rbind(how_many_countries_per_year,data.frame(
    year = year ,countries_number = length(unique(dataset[dataset$year == year, 
                                                          'country']))))
}
ggplot(how_many_countries_per_year, aes(x = year, y = countries_number)) + 
  geom_line(size = 1.2, color = 'darkblue')

country_codes <- codelist[,c('country.name.en','iso3c')]
colnames(country_codes) <- c('country', 'code')

new_dataset <- merge(dataset, country_codes, by = 'country', all.x = T)


# find common names between the data frames
common <- intersect(unique(dataset$country), unique(country_codes$country))

# find which names are written differently in both datasets
wrong_countrynames <- setdiff(dataset$country, common)
wrong_countrynames

new_dataset[new_dataset$country == 'Bahamas', 'code'] <- 'BHM'
new_dataset[new_dataset$country == 'Falkland Islands (Malvinas)', 'code'] <- 'FLK'
new_dataset[new_dataset$country == 'Hong Kong SAR', 'code'] <- 'HKG'
new_dataset[new_dataset$country == 'Iran (Islamic Rep of)', 'code'] <- 'IRN'
new_dataset[new_dataset$country == 'Republic of Korea', 'code'] <- 'KOR'
new_dataset[new_dataset$country == 'Republic of Moldova', 'code'] <- 'MDA'
new_dataset[new_dataset$country == 'Russian Federation', 'code'] <- 'RUS'
new_dataset[new_dataset$country == 'Saint Vincent and Grenadines', 'code'] <- 'VCT'
new_dataset[new_dataset$country == 'Syrian Arab Republic', 'code'] <- 'SYR'
new_dataset[new_dataset$country == 'TFYR Macedonia', 'code'] <- 'MKD'
new_dataset[new_dataset$country == 'United States of America', 'code'] <- 'USA'
new_dataset[new_dataset$country == 'Venezuela (Bolivarian Republic of)', 'code'] <- 'VEN'
new_dataset[new_dataset$country == 'Bosnia and Herzegovina', 'code'] <- 'BIH'
new_dataset[new_dataset$country == 'Czech Republic', 'code'] <- 'CZE'

new_dataset[new_dataset$country == 'Antigua and Barbuda', 'code'] <- 'ATG'
new_dataset[new_dataset$country == 'Bru', 'code'] <- ''
new_dataset[new_dataset$country == 'Brunei Darussalam', 'code'] <- 'BRN'
new_dataset[new_dataset$country == 'Cabo Verde', 'code'] <- 'CPV'
new_dataset[new_dataset$country == 'Macau', 'code'] <- 'MAC'
new_dataset[new_dataset$country == 'Reunion', 'code'] <- 'Reu'
new_dataset[new_dataset$country == 'Saint Lucia', 'code'] <- 'LCA'
new_dataset[new_dataset$country == 'Saint Pierre and Miquelon', 'code'] <- 'SPM'
new_dataset[new_dataset$country == 'Trinidad and Tobago', 'code'] <- 'TTO'
new_dataset[new_dataset$country == 'Turks and Caicos Islands', 'code'] <- 'TCA'
new_dataset[new_dataset$country == 'Virgin Islands (USA)', 'code'] <- 'VIR'

# find all the remaining countries in the dataset with no code
# unique(new_dataset[is.na(new_dataset$code), 'country'])
sapply(new_dataset, function(x) sum(is.na(x)))

# remove countries with na codes 
new_dataset <- new_dataset[!is.na(new_dataset$code),]

# remove countries with na suicides_no
new_dataset <- new_dataset[!is.na(new_dataset$suicides_no),]

#remove countries with na population
new_dataset <- new_dataset[!is.na(new_dataset$population),]

sapply(new_dataset, function(x) sum(is.na(x)))

# compute metric to be shown
suicides_by_countries <- aggregate(cbind(Suicides = new_dataset$suicides_no, 
                                         Population = new_dataset$population), 
                                   by = list(country = new_dataset$country, 
                                             code = new_dataset$code), 
                                   FUN = sum )

suicides_by_countries$suicide_rate <- suicides_by_countries$Suicides /
  suicides_by_countries$Population * 100000

str(suicides_by_countries)

# find all the missing countries countries from the aggregated metrics
missing_countries <- codelist[codelist$continent == 'Africa' | 
                                codelist$continent == 'Asia' |
                                codelist$continent == 'Americas',c('country.name.en','iso3c')]
missing_countries <- missing_countries[! (is.na(missing_countries$iso3c)), colnames(missing_countries)]

suicides_by_countries$country <- as.character(suicides_by_countries$country)

for(i in 1:nrow(missing_countries)){
  if(!missing_countries[i,2] %in% suicides_by_countries$code ){
    suicides_by_countries <- rbind(suicides_by_countries,
                                   c(missing_countries[i, 1],missing_countries[i, 2],-1,-1,-1))
  }
  
}
suicides_by_countries$country <- factor(suicides_by_countries$country)
suicides_by_countries$Suicides <- as.numeric(suicides_by_countries$Suicides)
suicides_by_countries$Population <- as.numeric(suicides_by_countries$Population)
suicides_by_countries$suicide_rate <- as.numeric(suicides_by_countries$suicide_rate)

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

p <- plot_geo(suicides_by_countries) %>%
  add_trace(
    z = ~suicide_rate, color = ~suicide_rate, colors = 'Blues',
    text = ~country, locations = ~code, marker = list(line = l)
  ) %>%
  colorbar(title = '') %>%
  layout(
    title = 'Suicide Rate per 100K inhabitant 1985-2015',
    geo = g
  )
p

# Keeping the complete cases, i.e. the rows that do not have any NA values 
complete_dataset <- dataset[complete.cases(dataset),]

ggplot(complete_dataset, aes(x = population, y = suicides_no, color = age)) + 
  geom_point()

sum_by_age <- aggregate(cbind(suicides = complete_dataset$suicides_no, population = complete_dataset$population), 
                        by = list(age = complete_dataset$age), FUN = sum)
sum_by_age$age <- factor(sum_by_age$age, levels(sum_by_age$age)[c(4,1:3, 5:6)])
sum_by_age$ratio <- sum_by_age$suicides/sum_by_age$population
sum_by_age

complete_dataset$suicide_rate <- complete_dataset$suicides_no / 
  complete_dataset$population * 100000
ggplot(complete_dataset, aes(x = population, y = suicide_rate, color = age)) + 
  geom_point()

sum_by_year_sex <- aggregate(cbind(Suicides = complete_dataset$suicides_no, 
                                   Population = complete_dataset$population), 
                             by = list(Sex = complete_dataset$sex,
                                       Year= complete_dataset$year), 
                             FUN = sum)

# plot bar of total suicides by age separated by sex
q1 <- ggplot(data=sum_by_year_sex, aes(x=Year, y=Suicides, color = Sex)) + 
  geom_line(size = 1.5) + ggtitle('Total Suicides per Year by Sex')
q1
q2 <- ggplot(data=sum_by_year_sex, aes(x=Year, y=Population, color = Sex)) + 
  geom_line(size = 1.5) + ggtitle('Total Population per Year by Sex')
q2

# we remove groups that have less than 50 suicides 
high_suicide_countries  <- complete_dataset[complete_dataset$suicides_no > 50,]
# we order the data frame by the suicide ratio
high_suicide_countries <- high_suicide_countries[order(-high_suicide_countries$suicide_rate),]

top_300_suicide_countries <- head(high_suicide_countries, 300)
top_300_suicide_countries$country <- factor(top_300_suicide_countries$country, 
                                            levels = unique(top_300_suicide_countries$country))

head(top_300_suicide_countries, 20)

top_300_suicide_countries[top_300_suicide_countries$sex=='female',]

high_suicide_women_group <- high_suicide_countries[order(-high_suicide_countries$suicide_rate),]
high_suicide_women_group <- high_suicide_women_group[high_suicide_women_group$sex == 'female', ]
head(high_suicide_women_group)

table(top_300_suicide_countries$country)

