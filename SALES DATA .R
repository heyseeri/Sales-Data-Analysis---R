# 📊 Sales Data Analysis Project (in R)
# Author: Shrirang Wadikhaye

# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

# Step 1: Generate Synthetic Sales Data
set.seed(42)

dates <- seq.Date(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")
products <- c("Shoes", "T-Shirts", "Bags", "Jackets", "Accessories")
regions <- c("North", "South", "East", "West")

n <- 1000

df <- data.frame(
  Date = sample(dates, n, replace = TRUE),
  Product = sample(products, n, replace = TRUE),
  Region = sample(regions, n, replace = TRUE),
  Units_Sold = sample(1:20, n, replace = TRUE),
  Unit_Price = round(runif(n, min = 10, max = 100), 2)
)

df <- df %>%
  mutate(
    Revenue = round(Units_Sold * Unit_Price, 2),
    Month = format(Date, "%Y-%m")
  )

# Step 2: Preview the Data
print("Sample Data:")
head(df)

# Step 3: Monthly Revenue Trend
monthly_revenue <- df %>%
  group_by(Month) %>%
  summarise(Revenue = sum(Revenue))

ggplot(monthly_revenue, aes(x = Month, y = Revenue)) +
  geom_line(color = "blue", group = 1) +
  geom_point(color = "blue") +
  labs(title = "📈 Monthly Revenue Trend", x = "Month", y = "Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 4: Top-Selling Products (by revenue)
product_revenue <- df %>%
  group_by(Product) %>%
  summarise(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue))

ggplot(product_revenue, aes(x = reorder(Product, -Revenue), y = Revenue, fill = Product)) +
  geom_bar(stat = "identity") +
  labs(title = "🛍️ Top-Selling Products by Revenue", x = "Product", y = "Revenue") +
  theme_minimal()

# Step 5: Regional Performance
region_revenue <- df %>%
  group_by(Region) %>%
  summarise(Revenue = sum(Revenue)) %>%
  arrange(desc(Revenue))

ggplot(region_revenue, aes(x = reorder(Region, -Revenue), y = Revenue, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "📍 Regional Revenue Performance", x = "Region", y = "Revenue") +
  theme_minimal()

# Step 6: Export data
write_csv(df, "sales_data.csv")
print("✅ Analysis complete. Dataset saved as sales_data.csv")
