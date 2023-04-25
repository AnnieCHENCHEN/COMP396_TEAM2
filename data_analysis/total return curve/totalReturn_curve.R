plotAggregateResults <- function(dataList, results, titleString=NULL) {
  STARTING_CAPITAL <- 1000000
  
  pfolioPnL <- aggregatePnL(results$pnlList)
  pfolioNetWorth <- results$netWorthList
  
  # The plot for the aggregate performance
  p1 <- ggplot() +
    geom_line(aes(x = index(pfolioNetWorth), y = pfolioNetWorth), color = 'black') +
    geom_hline(yintercept = STARTING_CAPITAL, colour = "red", linetype = "longdash") +
    labs(x = "Date", y = "Cumulative Returns", title = titleString) +
    theme_minimal()
  
  print(p1)
}
