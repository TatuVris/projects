### Data preperation:

## imports
library(readr)
library(dplyr)
library(ggplot2)

## data loading and preperation
#read the data
mergedDataFinal3 <- read_delim("~/notebooks/BDAproject/ProjectBDA/mergedDataFinal3.csv", 
                               delim = ";", escape_double = FALSE, trim_ws = TRUE)

# create the lagged dataset for the models and remove Luxemburg and OECD:
GDPusage_lag <- mergedDataFinal3 %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    Education = lag(Education, n = 5),
    `Housing and community amenities` = lag(`Housing and community amenities`, n=5),
    SocialProtection = lag(SocialProtection, n=5),
    GeneralPublicServices = lag(GeneralPublicServices, n=5),
    RecreationCultureAndReligion = lag(RecreationCultureAndReligion, n=5),
    Health = lag(Health, n=5),
    EnvironmentalProtection = lag(EnvironmentalProtection, n=5),
    PublicOrderAndSafety = lag(PublicOrderAndSafety, n=5),
    Defence = lag(Defence, n=5),
    EconomicAffairs = lag(EconomicAffairs, n=5)
  )%>% # nextremove Luxemburg and OECD:
  filter(Country != "Luxembourg") %>%
  filter(Country != "OECD")

# Replace spaces with underscores in all column names
names(GDPusage_lag) <- gsub(" ", "_", names(GDPusage_lag))


#World economic regions: North America, Western Europe, and eastern Asia, rest of the world
categorize_economic_region <- function(data) {
  data$Economic_region <- case_when(
    data$Country %in% c("United States", "United Kingdom") ~ "North America",
    data$Country %in% c("Czechia", "Austria", "Belgium", "Denmark", 
                        "Finland", "France", "Germany", "Hungary", 
                        "Iceland", "Ireland", "Italy", "Netherlands", 
                        "Norway", "Poland", "Portugal", "Slovak Rep.", 
                        "Spain", "Sweden", "Switzerland") ~ "Western Europe",
    data$Country %in% c("China", "Japan","Korea", "Philippines") ~ "East Asia",
    TRUE ~ "Rest of World"
  )
  return(data)
}

# Apply the function to the GDPusage_lag dataset
GDPusage_lag_groups <- categorize_economic_region(GDPusage_lag)

# Remove NA values
GDPusage_lag_groups <- na.omit(GDPusage_lag_groups)

## remove features with high correlation 
# correlation matrix
features <- colnames(GDPusage_lag_groups)[c(2, 4:13)]
selected_data <- GDPusage_lag_groups[features]
# Ensure all columns are numeric
numeric_data <- as.data.frame(lapply(selected_data, as.numeric))
cor_matrix <- (cor(numeric_data))
# Visualize the correlation matrix using corrplot
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper")

#remove colinearity:
GDPusage_lag_groups <- GDPusage_lag_groups %>% select(-c(4, 6, 7, 11, 12))


## bar plot:
# Count the number of entries in each economic region
region_counts <- table(GDPusage_lag_groups$Economic_region)
region_counts
# Create a bar plot of these counts
barplot(region_counts, 
        main = "Counts of Economic Regions", 
        xlab = "Economic Region", 
        ylab = "Number of Entries")

aggregate(GDP_per_capita ~ Economic_region, data = GDPusage_lag_groups, FUN = sum)


# view the final dataset:
View(GDPusage_lag_groups)

GDPusage_lag_scaled <- GDPusage_lag_groups %>%
  mutate(across(where(is.numeric), scale))
View(GDPusage_lag_scaled)

#Scale the data
#GDPusage_lag_scaled <- GDPusage_lag_groups %>%
#  mutate(
#    Year = scale(Year),
#    Defence = scale(Defence),
#    Housing_and_community_amenities = scale(Housing_and_community_amenities), # Repeat for each numeric column you want to scale
#    PublicOrderAndSafety = scale(PublicOrderAndSafety),  
#    GeneralPublicServices = scale(GeneralPublicServices),
#    Health = scale(Health)
#    )


# View the scaled dataset
View(GDPusage_lag_scaled)


GDPusage_lag_scaled <- na.omit(GDPusage_lag_scaled)
View(GDPusage_lag_scaled)
