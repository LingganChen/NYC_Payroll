# Load the required library
library(ggplot2)
library(dplyr)
library(stringr)

# Step 1: Load the dataset
df <- read.csv("Citywide_Payroll_Data__Fiscal_Year_.csv")

new_col <- c("Fiscal_Year", "Agency_Name", "Last_Name", "First_Name", "Mid_Init",
                    "Agency_Start_Date", "Work_Location_Borough", "Title_Description",
                    "Leave_Status_as_of_June_30", "Base_Salary", "Pay_Basis",
                    "Regular_Hours", "Regular_Gross_Paid", "OT_Hours",
                    "Total_OT_Paid", "Total_Other_Pay")

names(df) <- new_col

str(df)

#Convert data types
df$Fiscal_Year <- as.numeric(df$Fiscal_Year)
df$Agency_Start_Date <- as.Date(df$Agency_Start_Date, format = "%m/%d/%Y")
df$Base_Salary <- as.numeric(gsub("[$,]", "", df$Base_Salary))
df$Regular_Gross_Paid <- as.numeric(gsub("[$,]", "", df$Regular_Gross_Paid))
df$Total_OT_Paid <- as.numeric(gsub("[$,]", "", df$Total_OT_Paid))
df$Total_Other_Pay <- as.numeric(gsub("[$,]", "", df$Total_Other_Pay))

#Check for and remove duplicate rows (if needed)
df <- distinct(df)

#Check the cleaned dataset
head(df)

# Subset of Data, due to pay having negative values and zeros.
x <- df %>%
  filter(`Regular_Gross_Paid` > 10000 & `Base_Salary` > 10000)

# Calculate the mean Regular_Gross_Paid for each unique Title_Description and Fiscal_Year
mean_data <- x %>%
  group_by(`Title_Description`, `Fiscal_Year`) %>%
  summarize(mean_Regular_Gross_Paid = mean(`Regular_Gross_Paid`))

# Sort the data by mean_Regular_Gross_Paid in ascending order
sorted_data <- mean_data %>%
  arrange(mean_Regular_Gross_Paid)

# Display the top 10 lowest mean_Regular_Gross_Paid
top_10_lowest <- head(sorted_data, 10)
top_10_lowest$Title_Description <- sub("^\\?", "", top_10_lowest$Title_Description)

# Print the result with the specified format
print(top_10_lowest)

#creating the plot
ggplot(top_10_lowest, aes(x = reorder(paste(Fiscal_Year, Title_Description), mean_Regular_Gross_Paid),
                          y = mean_Regular_Gross_Paid)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +  
  labs(x = NULL, y = "$ Regular Gross Paid",title = "Lowest Paid Employees",
       subtitle = "Employees On Annual Pay above $10,000") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, hjust = 0),  # Set hjust for alignment
        axis.ticks.y = NULL,
        plot.margin = margin(5, 5, 50, 5, "pt"),
        plot.title = element_text(size = 20,hjust = 0.5),
        plot.subtitle = element_text(size = 12,hjust = 0.5)) +
  coord_flip()
options(repr.plot.width = 10, repr.plot.height = 5)

mean_data <- df %>%
  group_by(`Title_Description`, `Fiscal_Year`) %>%
  summarize(mean_Regular_Gross_Paid = mean(`Regular_Gross_Paid`))

# Sort the data by mean_Regular_Gross_Paid in ascending order
sorted_data <- mean_data %>%
  arrange(desc(mean_Regular_Gross_Paid))

# Display the top 10 highest mean_Regular_Gross_Paid
top_10_highest <- head(sorted_data, 10)
top_10_highest$Title_Description <- sub("^\\?", "", top_10_highest$Title_Description)

# Print the result with the specified format
print(top_10_highest)

#creating the plot
ggplot(top_10_highest, aes(x = reorder(paste(Fiscal_Year, Title_Description), mean_Regular_Gross_Paid),
                          y = mean_Regular_Gross_Paid)) +
  geom_bar(stat = "identity", aes(fill = -mean_Regular_Gross_Paid), width = 0.5) +  
  labs(x = NULL, y = "$ Regular Gross Paid",title = "Highest Paid Employees",
       subtitle = "Highest Annual Paid") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, hjust = 0),  # Set hjust for alignment
        axis.ticks.y = NULL,
        plot.margin = margin(5, 5, 50, 5, "pt"),
        plot.title = element_text(size = 20,hjust = 0.5),
        plot.subtitle = element_text(size = 12,hjust = 0.5),
        legend.position = "none") +
        scale_y_continuous(labels = scales::comma) +  # Specify tick positions+
  coord_flip()

options(repr.plot.width = 10, repr.plot.height = 5)

# Filter data for Fiscal_Year = 2014
data_2014 <- df[df$Fiscal_Year == 2014, ]

# Group data by Agency_Name and count employees
agency_employee_counts <- data_2014 %>%
  group_by(Agency_Name) %>%
  summarize(Employee_Count = n()) %>%
  arrange(desc(Employee_Count)) %>%
  top_n(10, wt = Employee_Count)

# Plot the top 10 agencies
ggplot(agency_employee_counts, aes(x = reorder(Agency_Name, Employee_Count), y = Employee_Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Agency Name", y = "Number of Employees", title = "Top 10 Agencies by Employee Count in 2014") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + coord_flip()

# Clean up agency names by removing extra spaces and converting to lowercase
df_clean <- df %>%
  mutate(Agency_Name = tolower(trimws(Agency_Name)))

# Group by cleaned agency name and fiscal year, calculate total number of employees
agency_employee_count <- df_clean %>%
  group_by(Agency_Name, Fiscal_Year) %>%
  summarize(total_employees = n()) %>%
  arrange(Fiscal_Year, desc(total_employees))

# Select the top 10 agencies for each fiscal year
top_10_agencies <- agency_employee_count %>%
  group_by(Fiscal_Year) %>%
  top_n(10)

# Create a bar chart
ggplot(top_10_agencies, aes(x = reorder(Agency_Name, total_employees), y = total_employees, fill = factor(Fiscal_Year))) +
  geom_bar(stat = "identity") +
  labs(x = "Agency Name", y = "Total Employees", title = "Top 10 Agencies with Most Employees (2014-2017)") +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +  # Custom colors for each year
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12,angle = 45, hjust = 1),
       axis.text.y = element_text(size = 12)) + scale_y_continuous(labels = scales::comma)+
  coord_flip()

# Filter data for each year and annual salary > 10,000
filtered_data <- df[df$Pay_Basis == " per Annum" & df$Base_Salary > 10000, ]

# Create a facetted histogram
ggplot(filtered_data, aes(x = Base_Salary)) +
  geom_histogram(binwidth = 2000, fill = "steelblue", color = "black") +
  labs(title = "Base Salary Distribution by Year",
       x = "Base Salary ($)",
       y = "Frequency") +
  theme_minimal() +  scale_x_continuous(labels = scales::comma)+
  facet_wrap(~ Fiscal_Year, ncol = 2, scales = "free_x")  # Create a 2-column grid of facets

df_2015 <- df[df$Fiscal_Year == 2015, ]
df_2015 <- df_2015 %>%
  filter(`Pay_Basis` == " per Annum" & `Base_Salary` > 20000)

# Convert Work_Location_Borough to character type
df_2015$Work_Location_Borough <- as.character(df_2015$Work_Location_Borough)
df_2015 <- df_2015 %>%
  filter(Work_Location_Borough != "")

unique_boroughs <- unique(df_2015$Work_Location_Borough)
print(unique_boroughs)


df_2015 <- df[df$Fiscal_Year == 2015, ]
df_2015 <- df_2015 %>%
  filter(`Pay_Basis` == " per Annum" & `Base_Salary` > 20000)

# Convert Work_Location_Borough to character type
df_2015$Work_Location_Borough <- as.character(df_2015$Work_Location_Borough)
df_2015 <- df_2015 %>%
  filter(Work_Location_Borough != "")

# Calculate the average gross pay for each borough
borough_avg_pay <- df_2015 %>%
  group_by(Work_Location_Borough) %>%
  summarize(mean_gross_pay = mean(Regular_Gross_Paid))

# Sort the boroughs by average gross pay in descending order
top_boroughs <- borough_avg_pay %>%
  arrange(desc(mean_gross_pay)) %>%
  head(8)

# Create a bar chart to visualize the top 8 boroughs
ggplot(top_boroughs, aes(x = reorder(Work_Location_Borough, -mean_gross_pay), y = mean_gross_pay)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Work Location Borough", y = "Average Gross Pay", title = "Top 8 Boroughs with Highest Average Gross Pay in 2015") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

df_2016 <- df[df$Fiscal_Year == 2016, ]
df_2016 <- df_2016 %>%
  filter(`Pay_Basis` == " per Annum" & `Base_Salary` > 20000)

# Convert Work_Location_Borough to character type
df_2016$Work_Location_Borough <- as.character(df_2016$Work_Location_Borough)
df_2016 <- df_2016 %>%
  filter(Work_Location_Borough != "")

# Calculate the average gross pay for each borough
borough_avg_pay <- df_2016 %>%
  group_by(Work_Location_Borough) %>%
  summarize(mean_gross_pay = mean(Regular_Gross_Paid))

# Sort the boroughs by average gross pay in descending order
top_boroughs <- borough_avg_pay %>%
  arrange(desc(mean_gross_pay)) %>%
  head(8)

# Create a bar chart to visualize the top 8 boroughs
ggplot(top_boroughs, aes(x = reorder(Work_Location_Borough, -mean_gross_pay), y = mean_gross_pay)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Work Location Borough", y = "Average Gross Pay", title = "Top 8 Boroughs with Highest Average Gross Pay in 2016") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

df_2017 <- df[df$Fiscal_Year == 2017, ]
df_2017 <- df_2017 %>%
  filter(`Pay_Basis` == " per Annum" & `Base_Salary` > 20000)

# Convert Work_Location_Borough to character type
df_2017$Work_Location_Borough <- as.character(df_2017$Work_Location_Borough)
df_2017 <- df_2017 %>%
  filter(Work_Location_Borough != "")

# Calculate the average gross pay for each borough
borough_avg_pay <- df_2017 %>%
  group_by(Work_Location_Borough) %>%
  summarize(mean_gross_pay = mean(Regular_Gross_Paid))

# Sort the boroughs by average gross pay in descending order
top_boroughs <- borough_avg_pay %>%
  arrange(desc(mean_gross_pay)) %>%
  head(8)

# Create a bar chart to visualize the top 8 boroughs
ggplot(top_boroughs, aes(x = reorder(Work_Location_Borough, -mean_gross_pay), y = mean_gross_pay)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Work Location Borough", y = "Average Gross Pay", title = "Top 8 Boroughs with Highest Average Gross Pay in 2017") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))
