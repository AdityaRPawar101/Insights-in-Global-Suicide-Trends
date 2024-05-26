library(tidyverse) #General  
library(dplyr) #Data Manipulation  
library(janitor) #Data cleaning package  
library(countrycode) #To create column for continents  
library(ggthemes) #For plot themes  
library(gridExtra) #For grid arranged plots  
library(rworldmap) #For map

#Import Data
data_set <- read.csv("D:\\Academics\\Semester 6\\NAS1001_ASSOCIATIVE-DATA-ANALYST_Harshlata Vishwakarma\\master.csv")
data_set[sample(1:nrow(data_set),10),]

#Selecting columns and cleaning columns
data <- data_set %>% 
  select(-"HDI.for.year", -"suicides.100k.pop", -"country.year") %>% 
  rename(gdp_for_year = 7, gdp_per_capita = 8)

data$age <- gsub(" years", "", data$age)
data$sex <- ifelse(data$sex == "male", "Male", "Female")

#Creating column for continents
data$continent <- countrycode(sourcevar = data$country, origin = "country.name", destination = "continent")

#Making age variable ordinal
data$age <- factor(data$age, 
                   ordered = T, 
                   levels = c("5-14",
                              "15-24", 
                              "25-34", 
                              "35-54", 
                              "55-74", 
                              "75+"))

#Making generation variable ordinal
data$generation <- factor(data$generation, 
                          ordered = T, 
                          levels = c("G.I. Generation", 
                                     "Silent",
                                     "Boomers", 
                                     "Generation X", 
                                     "Millenials", 
                                     "Generation Z"))

#Global average as a yardstick
global_average <- (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population))) * 100000

#Default theme
theme <- theme(text = element_text(family = "serif", size = 12),legend.position = "none")

# **Global Analysis**
{r message = FALSE, warning = FALSE}
data %>%
  group_by(year) %>%
  summarize(population = sum(population), 
            suicides = sum(suicides_no), 
            suicides_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = year, y = suicides_per_100k)) + 
  geom_line(col = "#12a4d9", size = 1) + 
  geom_point(col = "#12a4d9", size = 2) + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey40", size = 1) +
  labs(title = "Global Suicides (per 100k)",
       subtitle = "Trend over time, 1985 - 2015.",
       caption = "Data Source: World Health Organization",
       x = "Year", 
       y = "Suicides per 100k") + 
  scale_x_continuous(breaks = seq(1985, 2015, 2)) + 
  scale_y_continuous(breaks = seq(10, 20)) + theme +
  annotate("text", x = 2003, y = 12.5, label = "Global Average", vjust = 1, size = 3, color = "grey40") +
  annotate(
    "curve",
    x = 2003, y = 12.5,
    xend = 2003, yend = 13.1,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40"
  )


# **By Continent**
{r message = FALSE, warning = FALSE}
#Grouping by continent and arranging so the plot is orderly 
continent <- data %>%
  group_by(continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(suicide_per_100k)

continent$continent <- factor(continent$continent, ordered = T, levels = continent$continent)

continent_plot <- ggplot(continent, aes(x = continent, y = suicide_per_100k, fill = continent)) + 
  geom_col() + 
  labs(title = "Global Suicides (per 100k) by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Continent") + 
  scale_y_continuous(breaks = seq(0, 20, 1), minor_breaks = F) + theme

#With Year
continent_time <- data %>%
  group_by(year, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

#To set an order to categorical variable
continent_time$continent <- factor(continent_time$continent, ordered = T, levels = continent$continent)

continent_time_plot <- ggplot(continent_time, aes(x = year, y = suicide_per_100k, col = factor(continent))) + 
  facet_grid(continent~., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time (per 100k) by Continent", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Continent") +
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F) + theme

grid.arrange(continent_plot, continent_time_plot, ncol = 2)


# **By Sex**
{r message = FALSE, warning = FALSE}
sex_plot <- data %>%
  group_by(sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = sex, y = suicide_per_100k, fill = sex)) + 
  geom_col() + 
  labs(title = "Global suicides (per 100k) by Sex",
       x = "Sex", 
       y = "Suicides per 100k") +
  scale_y_continuous(breaks = seq(0, 25), minor_breaks = F) + 
  theme

#With Year
sex_time_plot <- data %>%
  group_by(year, sex) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = sex)) + 
  facet_grid(sex ~ ., scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time, by Sex", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Sex") +
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F) + theme

grid.arrange(sex_plot, sex_time_plot, ncol = 2)

#Sex by continent
{r message = FALSE, warning = FALSE}
data %>%
  group_by(continent, sex) %>%
  summarize(n = n(), 
            suicides = sum(as.numeric(suicides_no)), 
            population = sum(as.numeric(population)), 
            suicide_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = continent, y = suicide_per_100k, fill = sex)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey40", size = 1) +
  theme + theme(legend.position = "bottom", legend.direction = "horizontal") +
  annotate("text", x = 1, y = 20, label = "Global Average", vjust = 1, size = 3, color = "grey40") +
  annotate(
    "curve",
    x = 1, y = 20,
    xend = 1.5, yend = 13.5,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40") +
  labs(title = "Gender Disparity by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "") +
  coord_flip()
  

# **By Age**
{r message = FALSE, warning = FALSE}
#Age
age_plot <- data %>%
  group_by(age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = age, y = suicide_per_100k, fill = age)) + 
  geom_col() + 
  labs(title = "Global suicides per 100k by Age",
       x = "Age", 
       y = "Suicides per 100k") +
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F) + theme

#With Year
age_time_plot <- data %>%
  group_by(year, age) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  ggplot(aes(x = year, y = suicide_per_100k, col = age)) + 
  facet_grid(age~. , scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  labs(title = "Trends Over Time (per 100k) by Age", 
       x = "Year", 
       y = "Suicides per 100k", 
       color = "Age") + 
  scale_x_continuous(breaks = seq(1985, 2015, 5), minor_breaks = F) + theme

grid.arrange(age_plot, age_time_plot, ncol = 2)

#Age group by continent
{r message = FALSE, warning = FALSE}
data %>%
  group_by(continent, age) %>%
  summarize(suicides = sum(as.numeric(suicides_no)), 
            population = sum(as.numeric(population)), 
            suicide_per_100k = (suicides / population) * 100000) %>%
  ggplot(aes(x = continent, y = suicide_per_100k, fill = age)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey40", size = 1) +
  labs(title = "Age Disparity by Continent",
       x = "Continent", 
       y = "Suicides per 100k", 
       fill = "Age") + 
  annotate("text", x = 1.9, y = 23.5, label = "Global Average", vjust = 1, size = 3, color = "grey40") +
  annotate(
    "curve",
    x = 1.5, y = 23.1,
    xend = 1.5, yend = 13.1,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40") + 
  theme(text = element_text(family = "serif", size = 12))


fig.width = 8, fig.height = 15
country <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000) %>%
  arrange(desc(suicide_per_100k))

country$country <- factor(country$country, 
                          ordered = T, 
                          levels = rev(country$country))


ggplot(country, aes(x = country, y = suicide_per_100k, fill = continent)) + 
  geom_col() + 
  geom_hline(yintercept = global_average, linetype = 2, color = "grey40", size = 1) +
  coord_flip() +
  labs(title = "Global suicides per 100k, by Country",
       x = "Country", 
       y = "Suicides per 100k", 
       fill = "Continent") +
  scale_y_continuous(breaks = seq(0, 45, 2),expand = c(0,0)) + 
  theme + theme(legend.position = "bottom", legend.direction = "horizontal") +
  labs(fill = "") + 
  annotate("text", x = 50, y = 20, label = "Global Average", vjust = 1, size = 3, color = "grey40") +
  annotate(
    "curve",
    x = 50, y = 20,
    xend = 50, yend = 13.1,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
    color = "grey40")


{r message = FALSE, warning = FALSE}
#Country heat map
country <- data %>%
  group_by(country) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000)

countrydata <- joinCountryData2Map(country, joinCode = "NAME", nameJoinColumn = "country")

{r message = FALSE, warning = FALSE}
par(mar=c(0, 0, 0, 0)) # margins
mapCountryData(countrydata, 
               nameColumnToPlot="suicide_per_100k", 
               mapTitle="", 
               colourPalette = "heat", 
               oceanCol="#5accd0", 
               missingCountryCol="grey", 
               catMethod = "pretty")


{r message = FALSE, warning = FALSE}
country_mean_gdp <- data %>%
  group_by(country, continent) %>%
  summarize(suicide_per_100k = (sum(as.numeric(suicides_no)) / sum(as.numeric(population))) * 100000, 
            gdp_per_capita = mean(gdp_per_capita))

ggplot(country_mean_gdp, aes(x = gdp_per_capita, y = suicide_per_100k, col = continent)) + 
  geom_point() + 
  scale_x_continuous(labels=scales::dollar_format(prefix="$"), breaks = seq(0, 70000, 10000)) + 
  labs(title = "Correlation between GDP (per capita) and Suicides per 100k",
       x = "GDP (per capita)", 
       y = "Suicides per 100k", 
       col = "Continent") +
  theme(text = element_text(family = "serif", size = 12))

