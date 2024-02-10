### Hospital finder Functions

## Best function
best <- function(state, outcome) {
  # Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check validity of state and outcome
  valid_states <- unique(outcome_data$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!state %in% valid_states) {
    stop("invalid state")
  }
  
  if (!tolower(outcome) %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  # Filter data for the specified state and outcome
  state_outcome_data <- outcome_data[outcome_data$State == state, ]
  outcome_column <- switch(outcome,
                           "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                           "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                           "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  state_outcome_data <- state_outcome_data[complete.cases(state_outcome_data[, outcome_column]), ]
  
  # Find hospital with lowest 30-day mortality
  lowest_mortality_index <- which.min(as.numeric(state_outcome_data[, outcome_column]))
  best_hospital <- state_outcome_data$Hospital.Name[lowest_mortality_index]
  
  # If there are ties, select alphabetically first
  best_hospital <- min(best_hospital)
  
  return(best_hospital)
}

# Use the function
best("AK", "pneumonia")

## Rank Hospital function
rankhospital <- function(state, outcome, num = "best") {
  # Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check validity of state and outcome
  valid_states <- unique(outcome_data$State)
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!state %in% valid_states) {
    stop("invalid state")
  }
  
  if (!tolower(outcome) %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  # Filter data for the specified state and outcome
  state_outcome_data <- outcome_data[outcome_data$State == state, ]
  outcome_column <- switch(outcome,
                           "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                           "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                           "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  state_outcome_data <- state_outcome_data[complete.cases(state_outcome_data[, outcome_column]), ]
  
  # Handle different cases of num
  if (is.numeric(num)) {
    num <- as.integer(num)
    if (num > nrow(state_outcome_data)) {
      return(NA)
    } else {
      ranked_hospitals <- state_outcome_data[order(as.numeric(state_outcome_data[, outcome_column]), state_outcome_data$Hospital.Name), ]
      hospital_name <- ranked_hospitals$Hospital.Name[num]
      return(hospital_name)
    }
  } else if (tolower(num) == "best") {
    ranked_hospitals <- state_outcome_data[order(as.numeric(state_outcome_data[, outcome_column]), state_outcome_data$Hospital.Name), ]
    hospital_name <- ranked_hospitals$Hospital.Name[1]
    return(hospital_name)
  } else if (tolower(num) == "worst") {
    ranked_hospitals <- state_outcome_data[order(-as.numeric(state_outcome_data[, outcome_column]), state_outcome_data$Hospital.Name), ]
    hospital_name <- ranked_hospitals$Hospital.Name[1]
    return(hospital_name)
  } else {
    stop("Invalid num value")
  }
}

# Use the function
rankhospital("NY", "heart attack", 7)

## Rank All function
rankall <- function(outcome, num = "best") {
  # Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Check validity of outcome
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!tolower(outcome) %in% valid_outcomes) {
    stop("invalid outcome")
  }
  
  # Filter data for the specified outcome
  outcome_column <- switch(outcome,
                           "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                           "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                           "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  
  outcome_data <- outcome_data[complete.cases(outcome_data[, outcome_column]), ]
  
  # Initialize a data frame to store results
  result_df <- data.frame(hospital = character(), state = character(), stringsAsFactors = FALSE)
  
  # Iterate through each state
  for (state in unique(outcome_data$State)) {
    state_data <- outcome_data[outcome_data$State == state, ]
    num_hospitals <- nrow(state_data)
    
    if (num_hospitals == 0) {
      result_df <- rbind(result_df, data.frame(hospital = NA, state = state))
      next
    }
    
    # Handle different cases of num
    if (is.numeric(num)) {
      num <- as.integer(num)
      if (num > num_hospitals) {
        result_df <- rbind(result_df, data.frame(hospital = NA, state = state))
        next
      } else {
        ranked_hospitals <- state_data[order(as.numeric(state_data[, outcome_column]), state_data$Hospital.Name), ]
        hospital_name <- ranked_hospitals$Hospital.Name[num]
        result_df <- rbind(result_df, data.frame(hospital = hospital_name, state = state))
      }
    } else if (tolower(num) == "best") {
      ranked_hospitals <- state_data[order(as.numeric(state_data[, outcome_column]), state_data$Hospital.Name), ]
      hospital_name <- ranked_hospitals$Hospital.Name[1]
      result_df <- rbind(result_df, data.frame(hospital = hospital_name, state = state))
    } else if (tolower(num) == "worst") {
      ranked_hospitals <- state_data[order(-as.numeric(state_data[, outcome_column]), state_data$Hospital.Name), ]
      hospital_name <- ranked_hospitals$Hospital.Name[1]
      result_df <- rbind(result_df, data.frame(hospital = hospital_name, state = state))
    } else {
      stop("Invalid num value")
    }
  }
  
  return(result_df)
}

# Use the function
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
