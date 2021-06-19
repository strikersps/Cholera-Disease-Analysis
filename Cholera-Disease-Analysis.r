library(dplyr)
library(tidyr)
library(repr)
library(ggplot2)
options(repr.plot.width = 16, repr.plot.height = 9)
library(patchwork)
library(countrycode)

# Setting The Working Directory
cholera_dataset_path <- "./Datasets"
getwd()
setwd(cholera_dataset_path)

# Loading Reported-Cases .csv file
cholera_cases_df <- read.csv("Reported-Cases-Till-2016.csv", stringsAsFactors = F)
names(cholera_cases_df) <- c('country', 'year', 'number_of_reported_cases')
filter(cholera_cases_df, number_of_reported_cases == "3 5")
cholera_df <- mutate(cholera_cases_df, 
                     number_of_reported_cases = replace(number_of_reported_cases, 
                                                        number_of_reported_cases == "3 5", 35))
cholera_cases_df <- transform(cholera_cases_df, 
                              number_of_reported_cases = as.integer(number_of_reported_cases))
head(cholera_cases_df, 10)

# Loading Number of Deaths .csv file
cholera_deaths_df <- read.csv("Number-of-Deaths-Till-2016.csv", stringsAsFactors = F)
names(cholera_deaths_df) <- c('country', 'year', 'number_of_reported_deaths')
filter(cholera_deaths_df, number_of_reported_deaths == "0 0")
cholera_deaths_df <- mutate(cholera_deaths_df, 
                            number_of_reported_deaths = replace(number_of_reported_deaths, 
                                                                number_of_reported_deaths == "0 0", NA))
cholera_deaths_df <- transform(cholera_deaths_df, number_of_reported_deaths = as.integer(number_of_reported_deaths))
head(cholera_deaths_df, 10)

# Loading Fatality Rate .csv file
cholera_fatality_rate_df <- read.csv("Fatality-Rate-Till-2016.csv", stringsAsFactors = F)
names(cholera_fatality_rate_df) <- c('country', 'year', 'fatality_rate')
filter(cholera_fatality_rate_df, fatality_rate == "Unknown" | fatality_rate == "0.0 0.0")
cholera_fatality_rate_df <- mutate(cholera_fatality_rate_df, 
                                   fatality_rate = replace(fatality_rate, (fatality_rate == "Unknown" | fatality_rate == "0.0 0.0"), NA))
cholera_fatality_rate_df <- transform(cholera_fatality_rate_df, fatality_rate = as.numeric(fatality_rate))
head(cholera_fatality_rate_df, 10)

# Merging all the three .csv files or dataframes into one single dataframe using merge() method by applying outer-join.
cholera_df <- merge(cholera_cases_df, cholera_deaths_df, 
                    by = c('country', 'year'), all = T) %>% merge(cholera_fatality_rate_df, 
                                                                  by = c('country', 'year'), all = T)
head(cholera_df, 10)


# Analysing the presence of Missing/Redundant Values in the final dataframe.
na_count <- data.frame(sapply(cholera_df, function(col_name) sum(is.na(col_name))))
names(na_count) <- "missing_value_count"
print(paste("Total Missing Values in the dataset: ", sum(na_count$missing_value_count)))
na_count


cholera_df <- replace_na(cholera_df, list(number_of_reported_cases = 0, 
                                          number_of_reported_deaths = 0, 
                                          fatality_rate = 0.0))
na_count <- data.frame(sapply(cholera_df, function(col_name) sum(is.na(col_name))))
names(na_count) <- "missing_value_count"
na_count

filter(cholera_df, country == "Argentina")

cholera_df$number_of_reported_deaths[cholera_df$country == "Argentina" & cholera_df$year == 1992] <- 0

cholera_df <- mutate(cholera_df, 
                     reported_recovered = number_of_reported_cases - number_of_reported_deaths, 
                     reported_recovery_rate = 100 * ((number_of_reported_cases - number_of_reported_deaths) / number_of_reported_cases))

na_count <- data.frame(sapply(cholera_df, function(col_name) sum(is.na(col_name))))
names(na_count) <- "missing_value_count"
print(paste("Total Number of Missing Values in the dataset:", sum(na_count)))
na_count

cholera_df <- replace_na(cholera_df, list(reported_recovery_rate = 0.0))

head(cholera_df)

na_count <- data.frame(sapply(cholera_df, function(col_name) sum(is.na(col_name))))
names(na_count) <- "Missing-Value-Count"
na_count

custom_match_country_names <- c("Micronesia (Federated States of)" = "Federated States of Micronesia", "The former state union Serbia and Montenegro" = "State Union of Serbia and Montenegro")
custom_match_country_codes <- c("State Union of Serbia and Montenegro" = "SCG")
custom_match_continent <- c("State Union of Serbia and Montenegro" = "Europe")

cholera_df$country = countrycode(cholera_df$country, 
                                 origin = 'country.name', 
                                 destination = 'cldr.name.en', 
                                 custom_match = custom_match_country_names)
cholera_df$country_iso_code = countrycode(cholera_df$country, 
                                          origin = 'country.name', 
                                          destination = 'iso3c', 
                                          custom_match = custom_match_country_codes)
cholera_df$who_region = countrycode(cholera_df$country, 
                                    origin = 'country.name', 
                                    destination = 'continent', 
                                    custom_match = custom_match_continent)
cholera_df$who_subregion = countrycode(cholera_df$country, 
                                       origin = 'country.name', 
                                       destination = 'region')

head(cholera_df, 10)

cholera_country_wise_cases <- cholera_df %>% group_by(country) %>% summarise(country_iso_code = first(country_iso_code), 
                                                                             who_region = first(who_region), 
                                                                             who_subregion = first(who_subregion), 
                                                                             total_cases = sum(number_of_reported_cases)) %>% ungroup() %>% arrange(desc(total_cases))
head(cholera_country_wise_cases)


cholera_country_wise_deaths <- cholera_df %>% group_by(country) %>% summarise(country_iso_code = first(country_iso_code), 
                                                                              who_region = first(who_region), 
                                                                              who_subregion = first(who_subregion), 
                                                                              total_deaths = sum(number_of_reported_deaths)) %>% ungroup() %>% arrange(desc(total_deaths))
head(cholera_country_wise_deaths)

cholera_country_wise_recovered <- cholera_df %>% group_by(country) %>% summarise(country_iso_code = first(country_iso_code), 
                                                                                 who_region = first(who_region), 
                                                                                 who_subregion = first(who_subregion), 
                                                                                 total_recovered = sum(reported_recovered)) %>% ungroup()
head(cholera_country_wise_recovered)

cholera_country_wise_fatality <- cholera_df %>% group_by(country) %>% summarise(country_iso_code = first(country_iso_code), who_region = first(who_region), 
                                                                                who_subregion = first(who_subregion), 
                                                                                avg_fatality = mean(fatality_rate)) %>% ungroup() %>% arrange(desc(avg_fatality))
head(cholera_country_wise_fatality)

cholera_country_wise_df <- merge(cholera_country_wise_cases, cholera_country_wise_deaths, by = c('country', 'country_iso_code', 'who_region', 'who_subregion'), all = T) %>% merge(cholera_country_wise_recovered, by = c('country', 'country_iso_code', 'who_region', 'who_subregion'), all = T) %>% merge(cholera_country_wise_fatality, by = c('country', 'country_iso_code', 'who_region', 'who_subregion'), all = T) %>% arrange(desc(total_deaths))
cholera_country_wise_df$percentage_death <- 100 * (cholera_country_wise_df$total_deaths / cholera_country_wise_df$total_cases)
cholera_country_wise_df$percentage_recovered <- 100 * (cholera_country_wise_df$total_recovered / cholera_country_wise_df$total_cases)
head(cholera_country_wise_df)

cholera_country_wise_df <- replace_na(cholera_country_wise_df, 
                                      list(percentage_death = 0.0, percentage_recovered = 100.0))

# Exploratory Data Analysis

death_color_palette <- rev(c("#67000D", "#B11217", "#CB181D", "#E32F27", "#EF3B2C", "#F34A36", "#FB7757", "#FC9F81", "#FCBBA1", "#FDD3C1"))


cholera_country_wise_cases <- cholera_country_wise_df %>% arrange(desc(total_cases)) %>% head(10) %>% ggplot(aes(x = reorder(country, total_cases), y = total_cases, fill = factor(total_cases))) + 
    geom_col(width = 0.6) + 
    labs(title = "Cholera Cases (1949-2016)", x = "Country", y = "Total Reported Cases") +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = death_color_palette) +
    theme(axis.text.x = element_text(size = 14, face = 'plain'),
          axis.text.y = element_text(size = 14, face = 'plain'),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = "none", 
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
cholera_country_wise_cases

cholera_country_wise_deaths <- cholera_country_wise_df %>% head(10) %>% ggplot(aes(x = reorder(country, total_deaths), y = total_deaths, fill = factor(total_deaths))) + 
    geom_col(width = 0.6) +
    labs(title = "Cholera Deaths (1949-2016)", x = "Country", y = "Total Reported Deaths") +
    coord_flip() +
    theme_minimal() + 
    scale_fill_manual(values = death_color_palette) +
    theme(axis.text.x = element_text(size = 14, face = 'plain'),
          axis.text.y = element_text(size = 14, face = 'plain'),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = "none", 
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
cholera_country_wise_deaths

cholera_country_recovery_rate <- cholera_country_wise_df %>% arrange(percentage_recovered) %>% head(10) %>% ggplot(aes(x = reorder(country, -percentage_recovered), y = percentage_recovered, fill = factor(percentage_recovered))) +
    geom_col(width = 0.6) +
    labs(title = "Recovery Rate (1949-2016)", x = "Country", y = "Recovery To Cases Ratio") +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = rev(death_color_palette)) +
    theme(plot.title = element_text(size = 18, face = 'bold', hjust = 0.5), 
          legend.position = 'none', 
          axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14), 
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16))
cholera_country_recovery_rate

cholera_country_death_rate <- cholera_country_wise_df %>% arrange(desc(percentage_death)) %>% head(10) %>% ggplot(aes(x = reorder(country, percentage_death), y = percentage_death, fill = factor(percentage_death))) +
    geom_col(width = 0.6) +
    labs(title = "Death Rate (1949-2016)", x = "Country", y = "Death to Cases Ratio") +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = death_color_palette) +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = "none", 
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5))
cholera_country_death_rate


(cholera_country_wise_cases | cholera_country_wise_deaths) / (cholera_country_recovery_rate | cholera_country_death_rate) + 
plot_annotation(title = "Top-10 Countries Cholera Situation (1949-2016)", 
                theme = theme(plot.title = element_text(size = 22, face = 'bold', hjust = 0.5)))

cholera_year_wise_cases = cholera_df %>% group_by(year) %>% summarise(total_cases = sum(number_of_reported_cases)) %>% ungroup() %>% arrange(desc(total_cases))
head(cholera_year_wise_cases)

cholera_year_wise_deaths <- cholera_df %>% group_by(year) %>% summarise(total_deaths = sum(number_of_reported_deaths)) %>% ungroup() %>% arrange(desc(total_deaths))
head(cholera_year_wise_deaths)

cholera_year_wise_fatality <- cholera_df %>% group_by(year) %>% summarise(avg_fatality = mean(fatality_rate)) %>% ungroup() %>% arrange(desc(avg_fatality))
head(cholera_year_wise_fatality)

cholera_year_wise_df <- merge(cholera_year_wise_cases, cholera_year_wise_deaths, by = "year", all = T) %>% merge(cholera_year_wise_fatality, by = "year", all = T) %>% arrange(desc(total_deaths))
cholera_year_wise_df$percentage_death <- 100 * (cholera_year_wise_df$total_deaths / cholera_year_wise_df$total_cases)
cholera_year_wise_df$percentage_recovered <- 100 * ((cholera_year_wise_df$total_cases - cholera_year_wise_df$total_deaths) / cholera_year_wise_df$total_cases)

head(cholera_year_wise_df)

cholera_year_cases <- cholera_year_wise_df %>% ggplot(aes(x = year, y = total_cases)) +
    geom_col(size = 0.9) +
    labs(title = "Total Cases", x = "Year", y = "Reported Cases") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14, vjust = 0.5),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          axis.line = element_line(color = 'grey'),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

cholera_year_deaths <- cholera_year_wise_df %>% ggplot(aes(x = year, y = total_deaths)) +
    geom_col(size = 0.9) +
    labs(title = "Total Deaths", x = "Year", y = "Total Reported Deaths") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14), 
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16), 
          plot.title = element_text(size = 18, face = 'bold', hjust = 0.5), 
          axis.line = element_line(color = 'grey'))

cholera_year_fatality_rate <- cholera_year_wise_df %>% ggplot(aes(x = year)) +
    geom_line(aes(y = avg_fatality), size = 0.9, color = 'red') +
    geom_line(aes(y = percentage_recovered), size = 0.7, color = 'dark green') +
    labs(title = "Fatality Rate Vs Recovered Rate", x = "Year", y = "Recorded Values") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14), 
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16), 
          plot.title = element_text(size = 18, face = 'bold', hjust = 0.5), 
          axis.line = element_line(color = 'grey'))

(cholera_year_cases | cholera_year_deaths) / cholera_year_fatality_rate + 
plot_annotation(title = "Cholera Cases Situation (1949-2016)", 
                theme = theme(plot.title = element_text(size = 22, face = 'bold', hjust = 0.5)))


cholera_top_df <- cholera_df %>% filter(country %in% head((arrange(cholera_country_wise_df, desc(total_cases)))$country, 12))
head(cholera_top_df)

cholera_top_df %>% ggplot(aes(x = year, y = number_of_reported_cases)) +
    geom_line(size = 0.8, color = "Red") +
    labs(title = "Cholera Cases for Top-12 Countries (1949-2016)", x = "Year", y = "Number of Reported Cases") +
    facet_wrap(~country, scales = "free") +
    theme_minimal() +
    theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), 
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          strip.text = element_text(size = 13, face = 'bold'),
          panel.background = element_blank(), 
          panel.border = element_blank(), 
          panel.grid.major = element_blank(),
          axis.line = element_line(color = "grey"))


cholera_who_subregion_cases <- cholera_df %>% group_by(who_subregion) %>% summarize(total_cases = sum(number_of_reported_cases)) %>% ungroup() %>% ggplot(aes(x = reorder(who_subregion, total_cases), y = total_cases, fill = factor(total_cases))) +
    geom_col(width = 0.7) +
    labs(title = "Reported Cases In WHO Sub-Region's", x = "Year", y = "Reported Cases") +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = death_color_palette) +
    theme(legend.position = "none", 
          plot.title = element_text(size = 18, hjust = 0.5), 
          axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14), 
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16))

cholera_who_subregion_deaths <- cholera_df %>% group_by(who_subregion) %>% summarise(total_deaths = sum(number_of_reported_deaths)) %>% ungroup() %>% ggplot(aes(x = reorder(who_subregion, total_deaths), y = total_deaths, fill = factor(total_deaths))) +
    geom_col(width = 0.7) +
    labs(title = "Reported Deaths In WHO Sub-Region's", x = "Year", y = "Reported Deaths") +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = death_color_palette) +
    theme(legend.position = "none", 
          axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16), 
          axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 14), 
          plot.title = element_text(size = 18, hjust = 0.5))

(cholera_who_subregion_cases | cholera_who_subregion_deaths) + 
plot_annotation(title = "WHO Sub-Region Cholera Situation (1949-2016)", 
                theme = theme(plot.title = element_text(size = 22, face = 'bold', hjust = 0.5)))

# Analysis of Last 10 Years of Cholera Disease

cholera_ten_years_df = filter(cholera_df, year > 2006)
head(cholera_ten_years_df)

countrywise_ten_years_cases <- cholera_ten_years_df %>% group_by(country) %>% summarize(total_cases = sum(number_of_reported_cases), total_deaths = sum(number_of_reported_deaths)) %>% ungroup() %>% arrange(desc(total_deaths)) %>% head(10) %>% ggplot(aes(x = reorder(country, total_cases), y = total_cases, fill = factor(total_cases))) +
    geom_col(width = 0.8) +
    labs(title = "Cholera Cases Reported", x = "Country", y = "Total Cases Reported") +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = death_color_palette) +
    theme(axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16), 
          axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14), 
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.position = "none")

country_wise_ten_years_deaths <- cholera_ten_years_df %>% group_by(country) %>% summarize(total_deaths = sum(number_of_reported_deaths)) %>% ungroup() %>% arrange(desc(total_deaths)) %>% head(10) %>% ggplot(aes(x = reorder(country, total_deaths), y = total_deaths, fill = factor(total_deaths))) +
    geom_col(width = 0.8) +
    labs(title = "Cholera Deaths Reported", x = "Country", y = "Total Deaths Reported") +
    coord_flip() +
    theme_minimal() +
    scale_fill_manual(values = death_color_palette) +
    theme(axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16), 
          axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14), 
          legend.position = 'none', 
          plot.title = element_text(size = 18, hjust = 0.5))

last_ten_year_cases <- cholera_year_wise_df %>% arrange(desc(year)) %>% head(10) %>% ggplot(aes(x = year, y = total_cases)) +
    geom_line(size = 0.8, color = 'red') +
    labs(title = "Cases of Cholera (2007-2016)", x = "Year", y = "Reported Cases") +
    theme_minimal() +
    scale_fill_manual(values = death_color_palette) +
    theme(axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size = 16),
          legend.position = "none", 
          plot.title = element_text(size = 18, hjust = 0.5))

last_ten_year_death <- cholera_year_wise_df %>% arrange(desc(year)) %>% head(10) %>% ggplot(aes(x = year, y = total_deaths)) +
    geom_line(size = 0.8, color = 'red') +
    labs(title = "Deaths From Cholera (2007-2016)", x = "Year", y = "Reported Deaths") +
    scale_fill_manual(values = death_color_palette) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "none", 
          plot.title = element_text(size = 18, hjust = 0.5))

countrywise_ten_years_cases + countrywise_ten_years_cases + 
    plot_annotation(title = "Country-Wise Last 10 Year Cholera Situation", 
                    theme = theme(plot.title = element_text(size = 22, face = 'bold', hjust = 0.5)))

last_ten_year_cases + last_ten_year_death + plot_annotation(title = "Last 10 Years Cholera Situation (2007-2016)", theme = theme(plot.title = element_text(size = 22, face = 'bold', hjust = 0.5)))

cholera_year_2010_df <- filter(cholera_df, year == 2010)
head(cholera_year_2010_df)

cholera_2010_country_wise <- cholera_year_2010_df %>% group_by(country) %>% summarize(total_cases = sum(number_of_reported_cases), 
                                                                                      total_deaths = sum(number_of_reported_deaths), 
                                                                                      avg_recovery_rate = mean(reported_recovery_rate), 
                                                                                      avg_fatality = mean(fatality_rate)) %>% ungroup() %>% arrange(desc(total_cases))
head(cholera_2010_country_wise)

cholera_2010_cases <- cholera_2010_country_wise %>% head(10) %>% ggplot(aes(x = reorder(country, total_cases), y = total_cases, fill = factor(total_cases))) +
    geom_col(width = 0.8) +
    labs(title = "Country-Wise Cholera Cases (2010)", x = "Country", y = "Reported Cases") +
    coord_flip() +
    scale_fill_manual(values = death_color_palette) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "none", 
          plot.title = element_text(size = 18, hjust = 0.5))

cholera_2010_deaths <- cholera_2010_country_wise %>% arrange(desc(total_deaths)) %>% head(10) %>% ggplot(aes(x = reorder(country, total_deaths), y = total_deaths, fill = factor(total_deaths))) +
    geom_col(width = 0.6) +
    labs(title = "Country-Wise Cholera Deaths (2010)", x = "Country", y = "Reported Deaths") +
    coord_flip() +
    scale_fill_manual(values = death_color_palette) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "none", 
          plot.title = element_text(size = 18, hjust = 0.5))

cholera_year_2011_df <- filter(cholera_df, year == 2011)
head(cholera_year_2011_df)

cholera_2011_country_wise_df = cholera_year_2011_df %>% group_by(country) %>% summarise(total_cases = sum(number_of_reported_cases), 
                                                                                        total_deaths = sum(number_of_reported_deaths), 
                                                                                        avg_recovery_rate = mean(reported_recovery_rate), 
                                                                                        avg_fatality = mean(fatality_rate)) %>% ungroup() %>% arrange(desc(total_cases))
head(cholera_2011_country_wise_df)

cholera_2011_cases <- cholera_2011_country_wise_df %>% head(10) %>% ggplot(aes(x = reorder(country, total_cases), y = total_cases, fill = factor(total_cases))) +
    geom_col(width = 0.6) +
    labs(title = "Country-Wise Cholera Cases (2011)", x = "Country", y = "Reported Cases") +
    coord_flip() +
    scale_fill_manual(values = death_color_palette) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "none", 
          plot.title = element_text(size = 18, hjust = 0.5))

cholera_2011_deaths <- cholera_2011_country_wise_df %>% arrange(desc(total_deaths)) %>% head(10) %>% ggplot(aes(x = reorder(country, total_deaths), y = total_deaths, fill = factor(total_deaths))) +
    geom_col(width = 0.6) +
    labs(title = "Country-Wise Cholera Deaths (2011)", x = "Country", y = "Reported Deaths") +
    coord_flip() +
    scale_fill_manual(values = death_color_palette) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "none", 
          plot.title = element_text(size = 18, hjust = 0.5))

(last_ten_year_cases | last_ten_year_death) / (cholera_2010_cases | cholera_2010_deaths) / (cholera_2011_cases | cholera_2011_deaths)

cholera_who_subregion_df <- cholera_ten_years_df %>% group_by(who_subregion) %>% summarise(total_cases = sum(number_of_reported_cases), 
                                                                                           total_deaths = sum(number_of_reported_deaths)) %>% ungroup() %>% arrange(desc(total_cases))
head(cholera_who_subregion_df)

cholera_who_subregion_ten_year_cases <- cholera_who_subregion_df %>% ggplot(aes(x = reorder(who_subregion, total_cases), y = total_cases, fill = factor(total_cases))) +
    geom_col(width = 0.6) +
    labs(title = "WHO Sub-Region Cholera Cases (2007-2016)", x = "WHO Sub-Region", y = "Reported Cases") +
    coord_flip() +
    scale_fill_manual(values = death_color_palette) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "none", 
          plot.title = element_text(size = 18, hjust = 0.5))

cholera_who_subregion_ten_year_deaths <- cholera_who_subregion_df %>% arrange(desc(total_deaths)) %>% ggplot(aes(x = reorder(who_subregion, total_deaths), y = total_deaths, fill = factor(total_deaths))) +
    geom_col(width = 0.6) +
    labs(title = "WHO Sub-Region Cholera Deaths (2007-2016)", x = "WHO Sub-Region", y = "Recorded Deaths") +
    coord_flip() +
    scale_fill_manual(values = death_color_palette) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 14), 
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.position = "none", 
          plot.title = element_text(size = 18, hjust = 0.5))

(cholera_who_subregion_ten_year_cases | cholera_who_subregion_ten_year_deaths)