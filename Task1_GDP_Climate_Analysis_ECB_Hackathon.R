
setwd("C:/Users/justi/Desktop/Rstudio")  
data <- read.csv("final_dataset_full.csv", header = TRUE, sep = ",")

# Exercise 1(a) Description
print(paste(
  "Exercise 1(a): Data Description.",
  "Each row represents a specific NUTS3 region for a given year.",
  "The year column indicates that observations are recorded annually.",
  "Our dataset is in a long format.",
  "Each row represents a single observation for a given region and year.",
  "Multiple years of data are recorded in separate rows.",
  sep = " "
))

# Load dplyr library
library(dplyr)

# Removing the regions where GDP is missing for all years
data_filtered <- data %>%
  group_by(NUTS_ID) %>%
  filter(!all(is.na(gdp))) %>%
  ungroup()



# Exercise 1(B)
print("1(B)")

# Counting regions before our made adjustment
num_regions_before <- length(unique(data$NUTS_ID))
print(paste("Number of regions before adjustment:", num_regions_before))

# Counting regions after our made adjustment
num_regions_after <- length(unique(data_filtered$NUTS_ID))
print(paste("Number of regions after adjustment:", num_regions_after))





# Exercise 1(c)
print("1(c)")

# Calculating minimum and maximum GDP for each year
gdp_min_max <- data_filtered %>%
  group_by(YEAR) %>%
  summarise(
    min_GDP = ifelse(all(is.na(gdp)), NA, min(gdp, na.rm = TRUE)),
    max_GDP = ifelse(all(is.na(gdp)), NA, max(gdp, na.rm = TRUE))
  )

print(gdp_min_max)  





# Exercise 1(D)
print("1(D)")

# Filtering data for the year 2020
data_2020 <- data_filtered %>% filter(YEAR == 2020)

# Finding the region with the lowest GDP in 2020
lowest_gdp_region <- data_2020 %>% filter(gdp == min(gdp, na.rm = TRUE))

# Find the region with the highest GDP in 2020
highest_gdp_region <- data_2020 %>% filter(gdp == max(gdp, na.rm = TRUE))

# Printing the  results
print(paste("The NUTS3 region with the lowest GDP in 2020 is:", lowest_gdp_region$NUTS_ID, 
            "with GDP of", lowest_gdp_region$gdp))

print(paste("The NUTS3 region with the highest GDP in 2020 is:", highest_gdp_region$NUTS_ID, 
            "with GDP of", highest_gdp_region$gdp))





# Exercise 1(E)
print("1(E)")

# Finding the row with the highest GDP growth rate
highest_growth <- data_filtered %>% filter(growth_rate == max(growth_rate, na.rm = TRUE))

# Print results
print(paste("The highest GDP growth was in region:", highest_growth$NUTS_ID, 
            "in the year", highest_growth$YEAR, 
            "with a growth rate of", highest_growth$growth_rate, "%"))






# For ex 2. I chose SE332, SE232 and SE132. Three regions of Sweden.
# Exercise 2(a-i)
print("2(A-i)")

selected_regions <- c("SE332", "SE232", "SE123")
# Now we filter the data with our selected regions
selected_data <- data_filtered %>% filter(NUTS_ID %in% selected_regions)

# Calculating the range for climate variables separately for each region
climate_stats_per_region <- selected_data %>%
  group_by(NUTS_ID) %>%
  summarise(
    spi12_yearly_min = min(spi12_yearly, na.rm = TRUE),
    spi12_yearly_max = max(spi12_yearly, na.rm = TRUE),
    
    yearly_hw_min = min(yearly_hw, na.rm = TRUE),
    yearly_hw_max = max(yearly_hw, na.rm = TRUE),
    
    avg_hw_intensity_min = min(avg_hw_intensity, na.rm = TRUE),
    avg_hw_intensity_max = max(avg_hw_intensity, na.rm = TRUE),
    
    avg_len_hw_min = min(avg_len_hw, na.rm = TRUE),
    avg_len_hw_max = max(avg_len_hw, na.rm = TRUE)
  )

# Printing the  results
print(climate_stats_per_region, width = Inf)







# Exercise 2(a-ii)
print("2(A-ii)")

# Creating a function to calculate the mode(Since there is no default mode function in R studio)
get_mode <- function(x) {
  unique_x <- unique(na.omit(x))  # Remove NA values
  unique_x[which.max(tabulate(match(x, unique_x)))]  # Finding the most frequent value 
}

# Calculating economic variables for each regions 
economic_stats_per_region <- selected_data %>%
  group_by(NUTS_ID) %>%
  summarise(
    avg_gva_A = mean(gross_value_added_A, na.rm = TRUE),
    median_gva_A = median(gross_value_added_A, na.rm = TRUE),
    mode_gva_A = get_mode(gross_value_added_A),
    sd_gva_A = sd(gross_value_added_A, na.rm = TRUE),
    
    avg_gva_C = mean(gross_value_added_C, na.rm = TRUE),
    median_gva_C = median(gross_value_added_C, na.rm = TRUE),
    mode_gva_C = get_mode(gross_value_added_C),
    sd_gva_C = sd(gross_value_added_C, na.rm = TRUE)
  )

# Print results
print(economic_stats_per_region, width = Inf) #infinity is here to have the whole table visible.





# Exercise 2(B)
print("2(B)-Graph")

# Loading ggplot for graph creation.
library(ggplot2)

# Time series plot for SPI-yearly with a legend.
ggplot(selected_data, aes(x = YEAR, y = spi12_yearly, color = NUTS_ID, group = NUTS_ID)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  labs(title = "SPI 12-Month Standardized Precipitation Index Over Time",
       x = "Year",
       y = "SPI 12-Yearly",
       color = "NUTS3 Region") +
  theme_classic()  #Selected a simple minimalistic theme.




# Exercise 2(c)
print("2(C)")

# Description of the graph
cat("The graph shows the SPI 12-Month Standardized Precipitation Index for three NUTS3 regions in Sweden over time. 
      The data fluctuates significantly, with repeating periods of wetter and drier conditions. In
      Some years, such as around 2003 and 2019, show strong negative SPI values, indicating drier conditions, 
      while other periods show positive SPI values, suggesting wetter-than-normal conditions. 
      The trends appear to be similar across regions, with some variation in intensity.")




# Exercise 2(D)
print("2(D)-graph")

library(ggplot2)

# Time series plot for gross value added (C)
ggplot(selected_data, aes(x = YEAR, y = gross_value_added_C, color = NUTS_ID, group = NUTS_ID)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  labs(title = "Gross Value Added (C) Over Time",
       x = "Year",
       y = "Gross Value Added (C)",
       color = "NUTS3 Region") +
  theme_classic()  




# Exercise 2(E)
print("2(E)")

# Description of the graph
cat("The graph shows the trend of Gross Value Added (C) for the three selected NUTS3 regions over time. 
      SE232 has significantly higher GVA_C compared to SE123 and SE332, indicating that it might be an industrial 
      or manufacturing-driven region. In contrast, SE123 and SE332 have much lower GVA_C, suggesting they may 
      be more focused on agriculture or service-based sectors. Over time, SE232 shows noticeable growth in 
      economic output, peaking around 2015-2017, while SE123 and SE332 remain relatively stable with smaller fluctuations.")


