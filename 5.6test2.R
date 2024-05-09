simulate_monty_hall <- function(n) {
  switch_strategy_success <- numeric(n)
  stay_strategy_success <- numeric(n)
  for (i in 1:n) {
    doors <- sample(c(0, 0, 1), 3, replace = FALSE)
    first_choice <- sample(1:3, 1)
    show_goat <- sample(setdiff(1:3, c(first_choice, which(doors == 1))), 1)
    switch_choice <- setdiff(1:3, c(first_choice, show_goat))
    stay_strategy_success[i] <- doors[first_choice]
    switch_strategy_success[i] <- doors[switch_choice]
  }
  stay_cumulative <- cumsum(stay_strategy_success) / (1:n)
  switch_cumulative <- cumsum(switch_strategy_success) / (1:n)
  
  list(stay = stay_cumulative, switch = switch_cumulative)
}
result <- simulate_monty_hall(100000)
plot(result$stay, type = "l", col = "red", ylim = c(0, 1), xlab = "模拟次数", ylab = "成功率", 
     main = "成功率随模拟次数增加的累积变化图")
lines(result$switch, col = "blue")
legend("bottomright", legend = c("更换策略", "保持策略"), col = c("blue", "red"), lty = 1)
fit_stay <- lm(result$stay ~ I(1:100000))
fit_switch <- lm(result$switch ~ I(1:100000))
abline(fit_stay, col = "red", lty = 2)
abline(fit_switch, col = "blue", lty = 2)
