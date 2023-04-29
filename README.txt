COMP396 --- TEAM2
We have four strategies of TMA-Momentum strategy, MACD-Momentum strategy, Turtle trading strategy and Combination strategy.
In "ass2Strategis" folder, we stored: MACD.R, Momentum-TMA.R, turtle_trade.R and TEAM2.R.

# How to run three strategies and the combination strategy:
In "main.R", change the name of the strategy (MACD, Momentum-TMA, turtle_trade, TEAM2) and run it, it will show the graph of PD ratio of each series.
The parameters combinations of four strategies have been wrote in the "example_strategies.R".

# Data analysis part
In "data_analysis" folder, we stored: volatility.R, trend_movingAverage.R, volume_trading.R and totalReturn_curve.R.
1. Run "volatility.R" can get the graph of 30-historical volatility changing of each series. We can see and make the data analysis through graphs of 10 series.
2. Run "trend_movingAverage.R" can get the graph of 10-day moving average line, 30-day moving average line, 100-day moving average line and 200-day moving average line. We can see and make the data analysis through graphs of 10 series.
3. Run "volume_trading.R" can get the graph of daily volume, 30-day moving average of the volume and 100-day moving average of the volume. We can see and make the data analysis through graphs of 10 series.
4. In order to obtain the total return curve of "TEAM2.R" (combination strategy), we have wrote "main_compare.R" to get the graph of total return curve. Run "main_compare.R" (in the "main_compare.R", we have sourced the "data_analysis/totalReturn_curve.R") and change the data source (PART1, PART2, PART3), we can get the total return curve of the combination strategy based on the difference dataset which can help us to compare the difference of the total return changing between three dataset.

# Optimization part
In "optim" folder, we have 6 files which named "optimise_MACD.R", "optimise_MACD2.R".
1. For MACD-Momentum strategy, run "optimise_MACD.R" can obtain the results of optimisation of MACD-Momentum strategy with random 4 series group. Run "optimise_MACD2.R" can obtain the results of optimisation of MACD-Momentum strategy with series 3 and 9 (with specific parameter range).
2. For TMA-Momentum strategy, run "Optimise_withSeries-MomentumTMA.R" can obtain the results of optimisation of TMA-Momentum strategy with randon 4 series group. Run "Optimise_withoutSeries-MomentumTMA.R" can obtain the results of TMA-Momentum strategy with seties 1, 5, 10 (with specific parameter range).
3. For turetle trading strategy, run "optimise_turtle.R" can obatin the results of optimisation of turetle trading strategy with random 5 series group. Run "optimise_turtle2.R" can obtain the results of optimisation of turetle trading strategy with series 2, 4, 8 (with specific parameter range).

# Source code
Our three strategies all three of our strategies refer to three codes that have all been uploaded to the “source_code" folder: "source_code_MACD.R", "source_code_turtle.R" and "source_code_TMA.R".

# Framework
The "backtester.R", "data.R", "processResults.R" and "utilities.R" stored in the "framework" folder. We used "backtester.R" to backtest our strategies check their stability and reliability. And "processResults.R" and "utilities.R" shown the performaces of our strategies using graphs. We sourced "data.R" in the "main.R" to read the data files (we have three data files: part 1 data, part 2, data and part 3 data).
