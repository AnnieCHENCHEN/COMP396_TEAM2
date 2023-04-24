library(ggplot2)
library(reshape2)

plotTotalReturnsComparison <- function(results_part1, results_part2, results_part3) {
  pfolioPnL_part1 <- results_part1$pfolioPnL
  pfolioPnL_part2 <- results_part2$pfolioPnL
  pfolioPnL_part3 <- results_part3$pfolioPnL
  
  daily_toplot_part1 <- data.frame(Date = index(pfolioPnL_part1), Part1 = pfolioPnL_part1$CumPnL)
  daily_toplot_part2 <- data.frame(Date = index(pfolioPnL_part2), Part2 = pfolioPnL_part2$CumPnL)
  daily_toplot_part3 <- data.frame(Date = index(pfolioPnL_part3), Part3 = pfolioPnL_part3$CumPnL)
  
  daily_toplot_df <- merge(daily_toplot_part1, daily_toplot_part2, by = "Date", all = TRUE)
  daily_toplot_df <- merge(daily_toplot_df, daily_toplot_part3, by = "Date", all = TRUE)
  
  daily_toplot_df <- daily_toplot_df %>% 
    tidyr::fill(Part1, .direction = "down") %>%
    tidyr::fill(Part2, .direction = "down") %>%
    tidyr::fill(Part3, .direction = "down") %>%
    gather("Part", "CumPnL", -Date)
  
  total_returns_plot <- ggplot(daily_toplot_df, aes(x = Date, y = CumPnL, color = Part)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Total Returns Comparison",
         x = "Date",
         y = "Cumulative PnL",
         color = "Data Part")
  
  return(total_returns_plot)
}
