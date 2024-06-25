```{r}
library(tidyverse)
library(lubridate)
library(ggplot2)

# Load necessary libraries

# Load gapminder dataset
data(gapminder, package="gapminder")

# Filter the gapminder dataset to get the latest data for each country
latest_data <- gapminder %>%
  group_by(country) %>%
  filter(year == max(year))

# Create a new column 'pop_millions' by dividing population by 1e6 to represent population in millions
latest_data$pop_millions <- latest_data$pop / 1e6

# Create a scatter plot to visualize the relationship between GDP per Capita, Life Expectancy, Population (in millions), and Continent
ggplot(latest_data, aes(x = gdpPercap, y = lifeExp, size = pop_millions, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_x_log10(labels = scales::comma_format()) +  
  labs(x = "GDP per Capita", y = "Life Expectancy", size = "Population (Millions)", color = "Continent") +
  theme_minimal() +
  ggtitle("Life Expectancy vs. GDP per Capita ") +
  theme(plot.title = element_text(hjust = 0.5)
```

Discussion of Caveat: Too Many Distributions

When dealing with a dataset that contains a large number of categories, such as the Gapminder dataset with multiple countries, plotting all the data points at once can lead to a cluttered and hard-to-read visualization. This issue becomes particularly problematic when trying to compare variables like life expectancy across numerous countries.

One solution to this problem is to limit the number of distributions visualized by focusing on specific subsets of the data, such as countries from a single continent. By filtering the data to include only countries from one continent at a time, we can simplify the plot and make it more interpretable. This approach reduces visual noise and enables clearer comparisons within a more manageable dataset.

```{r}
library(ggplot2)
library(dplyr)
library(viridis)
library(gapminder)
library(patchwork)
library(hrbrthemes)

# Filter the gapminder dataset to include data for the year 2007 and select relevant columns
data <- gapminder %>% filter(year=="2007") %>% select(-year)

# Plot 1: GDP per Capita vs. Population
plot1 <- data %>%
  mutate(pop = pop / 1000000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, levels = country)) %>%
  ggplot(aes(x = gdpPercap, y = pop, size = lifeExp, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis(discrete = TRUE) +
  scale_y_log10() +
  theme_ipsum() +
  theme(legend.position = "none")

# Plot 2: Life Expectancy vs. Population
plot2 <- data %>%
  mutate(pop = pop / 1000000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, levels = country)) %>%
  ggplot(aes(x = lifeExp, y = pop, size = gdpPercap, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis(discrete = TRUE) +
  scale_y_log10() +
  theme_ipsum() +
  theme(legend.position = "none")

# Plot 3: GDP per Capita vs. Life Expectancy
plot3 <- data %>%
  mutate(pop = pop / 1000000) %>%
  arrange(desc(pop)) %>%
  mutate(country = factor(country, levels = country)) %>%
  ggplot(aes(x = gdpPercap, y = lifeExp, size = pop, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_color_viridis(discrete = TRUE) +
  scale_size_continuous(name = "Population (Million)", labels = scales::comma_format()) +
  scale_x_log10() +
  theme_minimal() +
  theme(legend.position = "none")

# Combine the three plots into a single visualization using the patchwork package
plot_combined <- (plot1 + plot2) / (plot3 + plot_spacer()) + plot_layout(ncol = 2, nrow = 2)

print(plot_combined)
```

The three individual plots offer insights into global development in 2007 using the Gapminder dataset. 

- Plot 1 (GDP per Capita vs. Population) highlights the trend that wealthier countries tend to have smaller populations and higher life expectancies, showcasing economic disparities across continents.
- Plot 2 (Life Expectancy vs. Population) reveals that countries with larger populations generally have moderate life expectancies, with noticeable regional variations.
- Plot 3 (GDP per Capita vs. Life Expectancy) effectively demonstrates the positive correlation between economic prosperity and health outcomes, emphasizing that higher GDP per capita is associated with longer life expectancy.

Among the three plots, Plot 3 stands out as it efficiently narrates the important connection between economic status and quality of life, providing a clear storyline on global health and wealth inequalities.



