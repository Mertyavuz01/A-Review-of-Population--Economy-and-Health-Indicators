---
title: "HW 02 - Data to viz"
subtitle: "Due: 24 May, 23:59 TR time"
output: 
  tufte::tufte_html:
    css: ../hw.css
    tufte_variant: "envisioned"
    highlight: pygments
link-citations: yes
---

```{r setup, include=FALSE}
library(tidyverse)
#library(dsbox)
library(lubridate)
library(ggplot2)
    
knitr::opts_chunk$set(out.width = "100%")
```

```{r load-data, message=FALSE, eval=TRUE}
# Load data
data(gapminder, package="gapminder")


```{r}

library(gapminder)
library(ggplot2)
library(dplyr)

latest_data <- gapminder %>%
  group_by(country) %>%
  filter(year == max(year))

latest_data$pop_millions <- latest_data$pop / 1e6

ggplot(latest_data, aes(x = gdpPercap, y = lifeExp, size = pop_millions, color = continent)) +
  geom_point(alpha = 0.7) +
  scale_x_log10(labels = scales::comma_format()) +  
  labs(x = "GDP per Capita", y = "Life Expectancy", size = "Population (Millions)", color = "Continent") +
  theme_minimal() +
  ggtitle("Life Expectancy vs. GDP per Capita ") +
  theme(plot.title = element_text(hjust = 0.5))



#Discussion of Caveat: Too Many Distributions

#Problem: When plotting data with a large number of categories, the plot can become overcrowded and hard to read. This is particularly an issue with the Gapminder dataset when plotting variables like life expectancy across many countries.

#Solution: To address this issue, we can limit the number of distributions by focusing on specific continents. By filtering the data to include only countries from one continent at a time, we can make the plot more readable and easier to interpret. This approach reduces visual clutter and allows for clearer comparisons within a more manageable subset of data.

```

```{r}

library(ggplot2)
library(dplyr)
library(viridis)
library(gapminder)
library(patchwork)
library(hrbrthemes)

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

plot1

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
plot2

# Plot 3: GpdPercap vs. Life Expectancy 
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

plot3



```



```{r}
plot_combined <- (plot1 + plot2) / (plot3 + plot_spacer()) + 
  plot_layout(ncol = 2, nrow = 2)

print(plot_combined)


#The three sub-plots collectively illustrate key aspects of global development in 2007 through the lens of the Gapminder dataset. Plot 1 (GDP per Capita vs. Population) reveals that wealthier countries often have smaller populations, with high life expectancy clustered among them, highlighting economic disparities across continents. Plot 2 (Life Expectancy vs. Population) shows that nations with large populations typically have moderate life expectancy, with significant regional differences evident. Plot 3 (GDP per Capita vs. Life Expectancy) effectively encapsulates the positive correlation between economic wealth and health outcomes, emphasizing that higher GDP per capita is associated with higher life expectancy. Among these, Plot 3 is the most effective in data storytelling, as it succinctly captures the crucial link between economic status and quality of life, providing a clear narrative on global health and wealth disparities.


```



