# Load necessary library
library(ggplot2)

# Define price range (X-axis)
price <- seq(30000, 70000, by = 1000)  # Varying price values

# Define profit calculations (Y-axis)
profit1 <- 5.3 * price - 8 * 12000
profit2 <- 3.23 * price
profit3 <- 3.75 * price

# Create a dataframe for plotting
df <- data.frame(
		   Price = rep(price, 3),
		     Profit = c(profit1, profit2, profit3),
		     Model = rep(c("Bazooka", "Local Seed", "Recycling"), each = length(price))
		     )

# Plot the three profit lines
p <- ggplot(df, aes(x = Price, y = Profit, color = Model)) +
	  geom_line(size = 1.2) +
	    labs(x = "Price",
			       y = "Profit",
			       color = "Farmer type") +
  theme_minimal() +
    theme(text = element_text(size = 14),
     plot.title = element_text(hjust = 0.5)  # Center the title
     )

  p

  ggsave("profit_plot.png", plot = p, width = 8, height = 6, dpi = 300) 
