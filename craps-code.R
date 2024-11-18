roll_dice <- function() {
  die1 <- sample(1:6, 1) 
  die2 <- sample(1:6, 1)  
  total <- die1 + die2
  return(total)
}

roll_dice()  

eval_first_roll <- function(roll) {
  if (roll %in% c(7, 11)) {
    return(list(outcome = "win", point = NA))
  } else if (roll %in% c(2, 3, 12)) {
    return(list(outcome = "loss", point = NA))
  } else {
    return(list(outcome = "continue", point = roll))
  }
}

eval_first_roll(roll_dice())

eval_point_roll <- function(point, roll) {
  
  
  if (roll == point) {
    return(list(outcome = "win"))
  } else if (roll == 7) {
    return(list(outcome = "loss"))
  } else {
    return(list(outcome = "continue"))
  }
}


eval_point_roll(point = 5, roll = roll_dice())  


simulate_craps_game <- function() {
  
  roll_count <- 0
  point <- NA
  game_active <- TRUE
  game_data <- data.frame(id = integer(), roll = integer(), outcome = character(), point = integer(), stringsAsFactors = FALSE)
  
  while (game_active) {
    roll_count <- roll_count + 1
    roll <- roll_dice()
    
    if (roll_count == 1) {
      result <- eval_first_roll(roll)
      game_data <- rbind(game_data, data.frame(id = roll_count, roll = roll, outcome = result$outcome, point = result$point))
      if (result$outcome %in% c("win", "loss")) {
        game_active <- FALSE
      } else {
        point <- result$point
      }
    } else {
      result <- eval_point_roll(point, roll)
      game_data <- rbind(game_data, data.frame(id = roll_count, roll = roll, outcome = result$outcome, point = point))
      if (result$outcome %in% c("win", "loss")) {
        game_active <- FALSE
      }
    }
  }
  return(game_data)
}


simulate_craps_game()


summarize_craps_game <- function(game_data) {
  n_rolls <- nrow(game_data)
  outcome <- tail(game_data$outcome, n = 1)  # Last outcome of the game
  point <- if (!is.na(game_data$point[1]) && game_data$outcome[1] == "continue") game_data$point[1] else NA
  
  summary <- data.frame(
    n_rolls = n_rolls,
    outcome = outcome,
    point = point,
    stringsAsFactors = FALSE
  )
  return(summary)
}

library(dplyr)
simulate_craps_game() %>% summarize_craps_game()

run_craps_simulation <- function(N) {
  
  all_summaries <- data.frame(n_rolls = integer(), outcome = character(), point = integer(), stringsAsFactors = FALSE)
  
  for (i in 1:N) {
    game_data <- simulate_craps_game()
    game_summary <- summarize_craps_game(game_data)
    all_summaries <- rbind(all_summaries, game_summary)
  }
  return(all_summaries)
}

result <- run_craps_simulation(N=5) # demonstrate result
result

