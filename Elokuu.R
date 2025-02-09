#reading the data
library(readr)
elokuu <- read_csv("")
View(elokuu)

#turn PVM to date objects for R:
elokuu$PVM <- as.Date(elokuu$PVM, format="%m/%d/%Y")
#Turn transactions into daily spending:
daily_spending <- aggregate(Hinta ~ PVM, data=elokuu, sum)

avg_daily_spending <- mean(daily_spending$Hinta)
avg_daily_spending

#plot daily spending:
plot(daily_spending$PVM, daily_spending$Hinta, 
     type="p", 
     main="Total Spending per Day (August)",  # "ELOKUU" if you want the title in Finnish
     xlab="Date", 
     ylab="Total Spending (Euro) (avg in red)",
     lwd=2
)

#identify weekends:
weekend_dates <- daily_spending$PVM[weekdays(daily_spending$PVM) %in% c("Saturday", "Sunday")]
weekend_dates

# Highlight the weekends
for (i in seq_along(weekend_dates)) {
  rect(weekend_dates[i] - 0.5, min(daily_spending$Hinta) - 10, 
       weekend_dates[i] + 0.5, max(daily_spending$Hinta) + 10,
       col = rgb(0.9, 0.9, 0.9, 0.5), border = NA)
}


?abline
abline(h = avg_daily_spending, col = "red")

sd <- sd(daily_spending$Hinta)
sd

#cheecky visualization:
calculate_sd_in_groups <- function(data, group_size){
  n_groups <- ceiling(length(data) / group_size)
  #initialize an empty vector
  sd_values <- numeric(n_groups)
  #loop through each group and calc sd:
  for (i in 1:n_groups) {
    start_index <- (i - 1) * group_size + 1
    end_index <- min(i * group_size, length(data))
    group <- data[start_index:end_index]
    
    # Calculate and store the standard deviation for the group
    sd_values[i] <- sd(group, na.rm = TRUE)
  }
  return(sd_values)
}

grouped_sds <- calculate_sd_in_groups(daily_spending$Hinta, group_size= 5)
grouped_sds
plot(grouped_sds, type="l")
?plot
# in the plot, each point corresponds to 5 day periods




# next to categories of spending

spending_by_category <- aggregate(Hinta ~ Lisatiedot, data = elokuu, sum)

# Create a bar plot for total spending by category
?barplot
spending_by_category <- spending_by_category[order(spending_by_category$Hinta, decreasing = TRUE), ]
barplot(spending_by_category$Hinta,
        names.arg = spending_by_category$Lisatiedot,
        col = "skyblue",
        main = "Total Spending by Category",
        xlab = "Category",
        ylab = "Total Spending (Euro)",
          # Rotate category names for better readability
        cex.names = 0.8)  # Adjust the size of the category names

biggest_categ <- spending_by_category[1:5, "Lisatiedot"]
biggest_categ


# Filter the original data to include only the top 5 categories
top_5_data <- elokuu[elokuu$Lisatiedot %in% biggest_categ, ]
top_5_data

top_5_data$PVM


top_5_data$PVM <- as.Date(top_5_data$PVM, format="%m/%d/%Y")

# Set up the plot with an empty canvas
plot(NULL, xlim = range(top_5_data$PVM), ylim = range(top_5_data$Hinta),
     xlab = "Date", ylab = "Spending (Euro)",
     main = "Development of Spending in Top 5 Categories", 
     type = "l")

# Define colors for the lines (one color per category)
colors <- rainbow(length(biggest_categ))

# Loop through each category and add a line to the plot
for (i in 1:length(biggest_categ)) {
  cat_data <- top_5_data[top_5_data$Lisatiedot == biggest_categ[i], ]
  lines(cat_data$PVM, cat_data$Hinta, type = "b", col = colors[i], lwd = 2, pch = 16)
}


#bootstrapping
sample(elokuu$Hinta,11)
sample(elokuu$PVM,11)
list <- sample(elokuu$Lisatiedot,11)

boot <- c(14.915,	5.64,	2.95,	2.49,	2.43,	3.935,	7.96,	2.95,	9.2,	6.3	,28.85)
boot_days <- sample(elokuu$PVM,11)

#turn PVM to date objects for R:
boot_days <- as.Date(boot_days, format="%m/%d/%Y")

bootstrap <- data.frame(boot_days,,, boot)

# Define new data to be added


boot_data <- data.frame(
  PVM = as.Date(rep("2024-09-01", 11)), 
  Tuote =  c("","","","","","","","","","",""),
  Kategoria = c("","","","","","","","","","",""),
  Hinta = boot,
  Lisatiedot = list
)
View(boot_data)


elokuu_b <- rbind(boot_data,elokuu)
View(elokuu_b)



daily_spending_b <- aggregate(Hinta ~ PVM, data=elokuu_b, sum)
mean(daily_spending_b$Hinta)

spending_by_category_b <- aggregate(Hinta ~ Lisatiedot, data = elokuu_b, sum)
View(spending_by_category_b)
sum(spending_by_category_b$Hinta)

spending_by_category_b <- spending_by_category_b[order(spending_by_category_b$Hinta, decreasing = TRUE), ]
barplot(spending_by_category_b$Hinta,
        names.arg = spending_by_category_b$Lisatiedot,
        col = "skyblue",
        main = "Boot Total Spending by Category",
        xlab = "Category",
        ylab = "Total Spending (Euro)",
        # Rotate category names for better readability
        cex.names = 0.8)  # Adjust the size of the category names

sum(spending_by_category_b$Hinta)
spending_by_category_b

