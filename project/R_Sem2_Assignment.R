install.packages("tidyverse")
install.packages("plotly")
library(tidyverse)
library(plotly)


unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")

data_join <- unicef_metadata %>%
  full_join(unicef_indicator_2, by = c("country" = "country", "year" = "time_period"))

install.packages("maps")
map_world <- map_data("world")

data_join_1960 <- data_join %>%
  filter(year == 1960)

map_data_join_1960 <- full_join(data_join_1960, map_world, by = c("country" = "region"))


#WM 1

ggplot(map_data_join_1960, aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "red", high = "yellow", name = "Life Expectancy\n(years)", na.value = "grey90") +
  labs(title = "World Life Expectancy in 1960",
       subtitle = "Data Source: UNICEF",
       caption = "Note: Some countries may be missing data") +
  xlab("Longitude") + 
  ylab("Latitude") +
  theme(
    plot.title = element_text(face = "bold", size = 24, family = "sans"),
    axis.text = element_text(face = "bold.italic"),  
    axis.title = element_text(face = "bold.italic"),
    legend.title = element_text(face = "bold"))

#WM 2 
data_join_1990 <- data_join %>%
  filter(year == 1990)
map_data_join_1990 <- full_join(data_join_1990, map_world, by = c("country" = "region"))

ggplot(map_data_join_1990, aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "red", high = "yellow", name = "Life Expectancy\n(years)", na.value = "grey90") +
  labs(title = "World Life Expectancy in 1990",
       subtitle = "Data Source: UNICEF",
       caption = "Note: Some countries may be missing data") +
  xlab("Longitude") + 
  ylab("Latitude")+
  theme(plot.title = element_text(face = "bold", size = 24, family = "sans"),
        axis.text = element_text(face = "bold.italic"),  
        axis.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold"))


#wm 3
data_join_2010 <- data_join %>%
  filter(year == 2010)

map_data_join_2010 <- full_join(data_join_2010, map_world, by = c("country" = "region"))

ggplot(map_data_join_2010, aes(x = long, y = lat, group = group, fill = `Life expectancy at birth, total (years)`)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(name = "Life Expectancy (years)",
                      low = "red",   
                      high = "yellow",  
                      na.value = "grey90") +
  labs(x = "Longitude", y = "Latitude",
       title = "World Life Expectancy in 2010",
       subtitle = "Data Source: UNICEF",
       caption = "Note: Some countries may be missing data") +
  theme(plot.title = element_text(face = "bold", size = 24, family = "sans"),
        axis.text = element_text(face = "bold.italic"),  
        axis.title = element_text(face = "bold.italic"),
        legend.title = element_text(face = "bold"))
        
#Bar Chart

data_join_filtered <- data_join %>%
  
  filter(country %in% specific_countries)

specific_countries_2 <- c("Iraq", "Thailand", "Chad", "Ethiopia", "Somalia", "Pakistan", "Argentina", "Malaysia", "Uruguay", "Paraguay", "Jamaica", "South Africa")

unicef_metadata_filtered_2 <- unicef_metadata %>%
  filter(country %in% specific_countries_2)

data_join_filtered_2 <- data_join %>%
  filter(country %in% specific_countries_2)


#timeseries Inflation


timeseries_plot <- data_join_filtered_2 %>%
  ggplot() +
  aes(year, `Inflation, consumer prices (annual %)`, color = country) +
  geom_line() +
  geom_line(linewidth = .8) +
  labs(title = "Inflation of Selected Countries 1960-2020",
       x = "Year",
       y = "Inflation (%)",
       color = "Country") +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 150)) +   
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  
    axis.title = element_text(face = "bold.italic", size = 12),  
    axis.text = element_text(size = 10),   
    legend.title = element_text(face = "bold.italic", size = 12),  
    legend.text = element_text(size = 10)) +
  scale_color_viridis_d()

ggplotly(timeseries_plot)


#timeseries ME 

timeseries_plot_2 <- data_join_filtered_2 %>%
  ggplot() +
  aes(year, `Military expenditure (% of GDP)`, color = country) +
  geom_line() +
  geom_line(linewidth = .9) +
  labs(title = "Military Expeniture (% of GDP) from 1960 - 2022",
       x = "Year",
       y = "Military expenditure (% of GDP)",
       color = "Country") +
  theme_minimal() +
  scale_y_continuous(limits = c(-5, 25))+ 
  theme(
  plot.title = element_text(face = "bold", size = 18, hjust = 0.5),  
  axis.title = element_text(face = "bold.italic", size = 10),  
  axis.text = element_text(size = 8),   
  legend.title = element_text(size = 10),  
  legend.text = element_text(size = 8)) +
  scale_color_brewer(palette = "Paired")


ggplotly(timeseries_plot_2)


#scatterplot 

filtered_data <- data_join_filtered_2 %>%
  filter(year %in% c(1960, 1980, 2000, 2010))

scatter_plot <- ggplot(filtered_data, aes(x = `GDP per capita (constant 2015 US$)`, 
                                          y = `Life expectancy at birth, total (years)`, 
                                          color = country,
                                          text = paste("Year:", year))) +
  geom_point(alpha = 0.6, size = 3) +  
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "black") +  
  labs(title = "Scatter Plot of GDP per Capita vs. Life Expectancy",
       x = "GDP per capita (constant 2015 US$)",
       y = "Life Expectancy") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(labels = scales::unit_format(unit = "K", scale = .001)) +
  theme_classic(base_family = "serif") +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  
        axis.title = element_text(face = "bold.italic", size = 12),  
        axis.text = element_text(size = 10))
interactive_scatter_plot <- ggplotly(scatter_plot)
interactive_scatter_plot


#bar Chart 

data_join_filtered_2 %>%
  group_by(country) %>%
  summarise(m_obs_value = mean(obs_value, na.rm = TRUE)) %>%
  ggplot(aes(reorder(country, m_obs_value), m_obs_value, fill = country)) + 
  geom_col(width = 0.8) +  
  geom_text(aes(label = paste0(round(m_obs_value, 1), "%")), vjust = -0.5, size = 3.5) +  
  labs(
    x = "Country",
    y = "% of Children Seeking Treatment",
    fill = "",  
    title = "% of Children Seeking Treatment for Respiratory Infection by Country"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    axis.text.x = element_blank(),
    axis.title = element_text(size = 12, face = "bold"),
    text = element_text(size = 12), 
    plot.title = element_text(size = 16, face = "bold")
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme(panel.grid.major.y = element_line(color = "gray", linetype = "dashed"))

