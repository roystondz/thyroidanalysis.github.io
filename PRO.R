###########

# Importing libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(mice)

#Reading the dataset
df <- read.csv('thyroidDF.csv')
# Displaying the first few rows of the dataframe
head(df)

# Getting the dimensions of the dataframe
df_dimensions <- dim(df)

# Displaying the dimensions
print(df_dimensions)

# Getting the number of rows
num_rows <- df_dimensions[1]

# Getting the number of columns
num_columns <- df_dimensions[2]

# Displaying the number of rows and columns
cat("Number of rows:", num_rows, "\n")
cat("Number of columns:", num_columns, "\n")

# Using summary() to get descriptive statistics
summary_df <- summary(df)

# Transposing the summary dataframe for better readability
summary_df <- t(summary_df)

# Displaying the transposed summary dataframe
print(summary_df)

# Display the summary per column
library(skimr)
skim(df)

#to check if there are any duplicate columns
sum(duplicated(df))


library(dplyr)
library(ggplot2)

# Drop specified columns
columns_to_drop <- df %>% c('TSH_measured', 'T3_measured', 'TT4_measured', 'T4U_measured', 'FTI_measured', 'TBG_measured',
                            'referral_source', 'patient_id')


df <- df[, setdiff(names(df), columns_to_drop)]
# Plot distribution of target variable

target_counts <- table(df$target)
target_df <- data.frame(Target = names(target_counts), Count = as.numeric(target_counts))
print(df$target)
ggplot(target_df, aes(x = Target, y = Count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Target", y = "Count", title = "Distribution of Target")

#Finding the unique targets
unique_targets <- unique(df$target)
print(unique_targets)
unique_targets_df <- data.frame(Target = unique_targets)

#The target values to keep
target_values_to_keep <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'AK', 'C|I', 'H|K', 'GK', 'FK', 'GI', 'GKJ', 'D|R', '-')
df <- df[df$target %in% target_values_to_keep, ]

# Print the dimensions of the filtered data frame
print(dim(df))

library(dplyr)

#dictionary
mapping <- c('-' = 'Negative',
             'A' = 'Hyperthyroid', 'AK' = 'Hyperthyroid', 'B' = 'Hyperthyroid', 'C' = 'Hyperthyroid', 'C|I' = 'Hyperthyroid', 'D' = 'Hyperthyroid', 'D|R' = 'Hyperthyroid',
             'E' = 'Hypothyroid', 'F' = 'Hypothyroid', 'FK' = 'Hypothyroid', 'G' = 'Hypothyroid', 'GK' = 'Hypothyroid', 'GI' = 'Hypothyroid', 'GKJ' = 'Hypothyroid', 'H|K' = 'Hypothyroid'
)

# Apply mapping to the 'target' column
df <- df %>%
  mutate(target = ifelse(target %in% names(mapping), mapping[target], target))

# Print the unique values of the 'target' column
unique_targets <- unique(df$target)
print(unique_targets)

target_distribution <- data.frame(Target = names(table(df$target)), Count = as.numeric(table(df$target)))

# Print the target distribution data frame
print(target_distribution)


library(plotly)

# Create a data frame for target distribution
target_distribution <- data.frame(Target = names(table(df$target)), Count = as.numeric(table(df$target)))

# Create a pie chart
fig <- plot_ly(target_distribution, labels = ~Target, values = ~Count, type = "pie", 
               marker = list(colors = c("#BEE9E9", "#6C7B8B", "#3A506B")))

fig
#We notice that there are rows with age the reaches 6000
#since this is unrealistic i'll remove rows above 100

subset_df <- subset(df, age < 100)

# Create a matrix indicating missing values
missing_matrix <- matrix(as.numeric(is.na(subset_df)), ncol = ncol(subset_df))

# Set up row and column names
row_names <- rownames(subset_df)
col_names <- colnames(subset_df)

# Create a heatmap based on missing values
heatmap(missing_matrix, col = c("white", "black"), Rowv = NA, Colv = NA, 
        xlab = "Variables", ylab = "Observations", main = "Missing Values Heatmap",
        labRow = row_names, labCol = col_names)

missing_percentages <- colSums(is.na(df)) / nrow(df) * 100
print(missing_percentages)
nulls_df <- data.frame("Missing Values %" = missing_percentages)

# Print the resulting data frame
print(nulls_df)

library(plotly)

# Drop 'TBG' column
df <- df[, !(colnames(df) %in% c('TBG'))]

# Original target distribution
origin <- table(df$target)
origin_df <- data.frame(Target = names(origin), Count = as.numeric(origin))
print(origin_df)
# Target distribution after dropping rows with at least 21 non-missing values
after <- table(df[complete.cases(df), ]$target)
after_df <- data.frame(Target = names(after), Count = as.numeric(after))

#creating a 3Dpie for the filtered data 
library(plotrix)

slices <- after_df$Count
lbls <- after_df$Target
colors <- c("#BEE9E9", "#6C7B8B", "#3A506B")

# Calculate percentages
percentages <- round(slices / sum(slices) * 100, 1)
label_text <- paste( percentages, "%", sep = "")

# Create a 3D pie chart with percentages as labels
pie3D(slices, labels = label_text, explode = 0.1, col = colors, main = "After Dropping")

# Add legend manually
legend("topright", legend = lbls, fill = colors, title = "Targets", cex = 0.5)

df <- na.omit(df)
# Print the dimensions of the resulting data frame
print(dim(df))
##############
#bar graph for gender distribution

library(ggplot2)
library(dplyr)

# Filter out rows with empty values in the 'sex' column
df_filtered <- df %>% 
  filter(!is.na(sex) & sex != "")

# Plotting gender distribution after filtering empty rows
ggplot(df_filtered, aes(x = sex, fill = sex)) +
  geom_bar() +
  labs(title = "Gender Distribution in Thyroid Disease Dataset",
       x = "Gender",
       y = "Count") +
  theme_minimal()

##############
# Create a line chart for age
ggplot(df, aes(x = age)) +
  geom_line(stat = "count") +
  labs(title = "Line Chart of Age in Thyroid Disease Dataset (Age <= 100)",
       x = "Age",
       y = "Count") +
  theme_minimal()
###########
#Seeing the distribution of the data
library(plotly)
analysis <- function(df, GoGraph = "histogram") {
  p1 <- plot_ly(x = ~df$TSH, type = GoGraph, name = 'TSH')
  p2 <- plot_ly(x = ~df$T3, type = GoGraph, name = 'T3')
  p3 <- plot_ly(x = ~df$TT4, type = GoGraph, name = 'TT4')
  p4 <- plot_ly(x = ~df$T4U, type = GoGraph, name = 'T4U')
  p5 <- plot_ly(x = ~df$FTI, type = GoGraph, name = 'FTI')
  
  subplot <- subplot(
    p1, p2, p3,
    p4, p5,
    nrows = 6, margin = 0.01
  )
  
  subplot
}

analysis(df)
##############
#displaying counts in presence of TSH 
fig <- plot_ly(df, x = ~TSH, color = ~target, type = "histogram", 
               histnorm = "percent",
               colors = c('#4b9546', '#F65366', '#3498db'),
               opacity = 0.75) %>%
  layout(title = "TSH Level by Diagnosis", yaxis = list(title = "Count"),
         xaxis = list(range = c(0, 11)))

# Show the plot
fig

#seeing the relations betweeen columns 
input <- df[,c('TSH','T3','TT4','T4U','FTI','target')]
png(file = "Scatterplot.png")
plot(x=input$TSH ,y=input$T4U,xlim = c(100,500),ylim=c(1.0,2.0),main = "TSH vs T4U")
dev.off()


png(file = "SCATTERPLOT_2.png")
plot(x=input$TSH ,y=input$FTI,xlim = c(0,200),ylim=c(0,60),main = "TSH vs FTI")
dev.off()

#creating a scatterplot matrix
png(file = "Scatterplot_Matrix.png")
pairs(~TSH+T3+TT4+T4U+FTI,data = input,main = "Scatterplot Matrix")
dev.off()

#creating a boxplot
png(file = "Boxplot.png")
boxplot(TSH~FTI,data = input,xlab = "FTI",ylab = "TSH",xlim = c(20,40),ylim=c(0,250),main= "FTI vs TSH")
dev.off()


# Selecting columns
columns <- c( 'TT4', 'T3', 'T4U', 'FTI', 'TSH','target')
model_df <- df[, columns]

# Mapping 'target' column to numeric values
target_to_num <- c('Negative' = 0, 'Hypothyroid' = 1, 'Hyperthyroid' = 2)
model_df$target <- target_to_num[as.character(model_df$target)]
print(model_df)

#prediction model

model <- lm(target~T3+TT4+T4U+FTI,data = model_df)
print("Coefficients")
print(model)

a <- data.frame(T3=2,TT4=100,T4U=1.15,FTI=100)

value <- predict(model,a)
print(value)

if(value <=0){
  print("Negative")
}else if(value <= 1){
  print("Hypothyroid")
}else{
  print("Hyperthyroid")
}

