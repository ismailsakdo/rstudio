# Install required packages if not already installed
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("patchwork", quietly = TRUE)) {
  install.packages("patchwork")
}

# Load the required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)

# Set the URL
url <- 'https://www.data.gov.my/data/dataset/50f58993-0d1e-4fea-80ad-9a64707b2de9/resource/14df8d5f-f7ad-4767-ac18-9caaab000b94/download/m-20220308010303_202203080103030_kes-dan-tangkapan-mengikut-pecahan-pembekalan-pemilikan-dan-pen.csv'

# Read the data from the URL
data <- readr::read_csv(url)
data_cleaned <- data %>%
  select(-c(6:9))
print(data_cleaned)

#summary statistics
summary(data_cleaned)
head(data_cleaned)
tail(data_cleaned)

# Find the row with the highest and lowest supply
highest_supply_row <- data_cleaned[which.max(data_cleaned$BEKALAN), ]
lowest_supply_row <- data_cleaned[which.min(data_cleaned$BEKALAN), ]

# Get the names from column 1 for highest and lowest rows
highest_name <- highest_supply_row$NEGERI
lowest_name <- lowest_supply_row$NEGERI

# Create a dataframe for both highest and lowest rows with names
highest_lowest_rows <- data.frame(
  NEGERI = c(highest_name, lowest_name),
  BEKALAN = c(highest_supply_row$BEKALAN, lowest_supply_row$BEKALAN)
)
print(highest_lowest_rows)

# Create a bar plot
plot_bekalan<-ggplot(highest_lowest_rows, aes(x = NEGERI, y = BEKALAN, fill = NEGERI)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  labs(x = "NEGERI", y = "BEKALAN", title = "Highest and Lowest BEKALAN") +
  theme_minimal() +   theme(legend.position = "none") # Remove the legend

#======= URIN

# Find the row with the highest and lowest URIN
highest_urin_row <- data_cleaned[which.max(data_cleaned$URIN), ]
lowest_urin_row <- data_cleaned[which.min(data_cleaned$URIN), ]

# Get the names from column 1 for highest and lowest rows
highest_name_urin <- highest_urin_row$NEGERI
lowest_name_urin <- lowest_urin_row$NEGERI

# Create a dataframe for both highest and lowest rows with names
highest_lowest_rows_urin <- data.frame(
  NEGERI = c(highest_name_urin, lowest_name_urin),
  URIN = c(highest_urin_row$URIN, lowest_urin_row$URIN)
)
print(highest_lowest_rows_urin)

# Create a bar plot
plot_urin<-ggplot(highest_lowest_rows_urin, aes(x = NEGERI, y = URIN, fill = NEGERI)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  labs(x = "NEGERI", y = "URIN", title = "Highest and Lowest URIN") +
  theme_minimal() + theme(legend.position = "none") # Remove the legend

#======= MEMILIKI

# Find the row with the highest and lowest POSES
highest_poses_row <- data_cleaned[which.max(data_cleaned$MEMILIKI), ]
lowest_poses_row <- data_cleaned[which.min(data_cleaned$MEMILIKI), ]

# Get the names from column 1 for highest and lowest rows
highest_name_poses <- highest_poses_row$NEGERI
lowest_name_poses <- lowest_poses_row$NEGERI

# Create a dataframe for both highest and lowest rows with names
highest_lowest_rows_poses <- data.frame(
  NEGERI = c(highest_name_poses, lowest_name_poses),
  MEMILIKI = c(highest_urin_row$MEMILIKI, lowest_urin_row$MEMILIKI)
)
print(highest_lowest_rows_poses)

# Create a bar plot
plot_memiliki<-ggplot(highest_lowest_rows_poses, aes(x = NEGERI, y = MEMILIKI, fill = NEGERI)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  labs(x = "NEGERI", y = "MEMILIKI", title = "Highest and Lowest MEMILIKI") +
  theme_minimal() + theme(legend.position = "none") # Remove the legend

#==== JUMLAH KES

# Clean the data by removing commas from the JUMLAH KES column and converting it to numeric
data$`JUMLAH KES` <- as.numeric(gsub(",", "", data$`JUMLAH KES`))

# Rename the column `JUMLAH KES` to `JUMLAH`
data <- data %>%
  rename(JUMLAH = `JUMLAH KES`)

data_cleaned <- data %>%
  select(-c(6:9))
print(data_cleaned)

# Find the row with the highest and lowest JUMLAH KES
highest_jumlah_row <- data_cleaned[which.max(data_cleaned$JUMLAH), ]
lowest_jumlah_row <- data_cleaned[which.min(data_cleaned$JUMLAH), ]

# Get the names from column 1 for highest and lowest rows
highest_name_jumlah <- highest_jumlah_row$NEGERI
lowest_name_jumlah <- lowest_jumlah_row$NEGERI

# Create a dataframe for both highest and lowest rows with names
highest_lowest_rows_jumlah <- data.frame(
  NEGERI = c(highest_name_jumlah, lowest_name_jumlah),
  JUMLAH = c(highest_jumlah_row$JUMLAH, lowest_jumlah_row$JUMLAH)
)
print(highest_lowest_rows_jumlah)

# Create a bar plot
plot_jumlah<- ggplot(highest_lowest_rows_jumlah, aes(x = NEGERI, y = JUMLAH, fill = NEGERI)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  labs(x = "NEGERI", y = "JUMLAH", title = "Highest and Lowest JUMLAH") +
  theme_minimal() + theme(legend.position = "none") # Remove the legend

#combine plots
# Combine all plots into one canvas
final_plot <- plot_bekalan + plot_urin + plot_memiliki + plot_jumlah

# Display the final plot
final_plot
