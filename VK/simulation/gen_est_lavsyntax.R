# Load necessary libraries
library(lavaan)
library(Matrix)

# Source the other files containing the functions gen_mat and gen_pop_model_syntax
source("gen_mat.R")
source("gen_pop_lavsyntax.R")

# Define the model types
model_types <- c("1.1", "1.2", "1.3", "1.4", "2.1", "2.2_exo", "2.2_endo", "2.2_both")


# Function to generate model syntaxes
gen_model_syntax <- function(model_types) {
  syntaxes <- list()
  
  for (model_type in model_types) {
    # Generate the matrices for the current model type
    model_matrices <- gen_mat(model_type)
    
    # Generate the lavaan model syntax from the matrices
    model_syntax <- gen_pop_model_syntax(model_matrices)
    
    # Store the syntax in the list
    syntaxes[[model_type]] <- model_syntax
  }
  
  return(syntaxes)
}

# Function to generate estimation model syntaxes
generate_estimation_model_syntax <- function(pop_model_syntax) {
  # Remove header and split the syntax into lines
  lines <- strsplit(pop_model_syntax, "\n")[[1]]
  
  # Remove header comment line
  lines <- lines[-1]
  
  # Initialize lists to hold different parts of the model syntax
  latent_vars <- list()
  variances <- list()
  regressions <- list()
  
  for (line in lines) {
    # If the line defines a latent variable (e.g., f1 =~ y1 + y2 + y3), modify it accordingly
    if (grepl("=~", line)) {
      parts <- strsplit(line, "=~")[[1]]
      lhs <- trimws(parts[1])
      rhs <- gsub("[0-9.]*\\*", "", trimws(parts[2]))
      if (lhs %in% names(latent_vars)) {
        latent_vars[[lhs]] <- c(latent_vars[[lhs]], rhs)
      } else {
        latent_vars[[lhs]] <- c(rhs)
      }
    }
    # If the line defines a variance (e.g., y1 ~~ 0.25*y1), modify it accordingly
    else if (grepl("~~", line)) {
      line <- gsub("[0-9.]*\\*", "", line)
      variances <- c(variances, line)
    }
    # If the line defines a regression path (e.g., f3 ~ 0.1*f1), modify it accordingly
    else if (grepl("~", line)) {
      parts <- strsplit(line, "~")[[1]]
      lhs <- trimws(parts[1])
      rhs <- gsub("[0-9.]*\\*", "", trimws(parts[2]))
      if (lhs %in% names(regressions)) {
        regressions[[lhs]] <- c(regressions[[lhs]], rhs)
      } else {
        regressions[[lhs]] <- c(rhs)
      }
    }
  }
  
  # Convert lists to strings
  latent_vars_str <- sapply(names(latent_vars), function(lhs) {
    paste0(lhs, " =~ ", paste(latent_vars[[lhs]], collapse = " + "))
  })
  
  variances_str <- variances
  
  regressions_str <- sapply(names(regressions), function(lhs) {
    paste0(lhs, " ~ ", paste(regressions[[lhs]], collapse = " + "))
  })
  
  # Combine all parts into a single string
  est_model_syntax <- paste(c(latent_vars_str, variances_str, regressions_str), collapse = "\n")
  
  return(est_model_syntax)
}

# Generate the model syntaxes for all model types
model_syntaxes <- gen_model_syntax(model_types)

# Generate estimation model syntaxes for all population models
est_model_syntaxes <- list()
for (model_type in names(model_syntaxes)) {
  est_model_syntaxes[[model_type]] <- generate_estimation_model_syntax(model_syntaxes[[model_type]])
}

# Print the generated estimation model syntaxes
# for (model_type in names(est_model_syntaxes)) {
#   cat(paste0("Estimation Model ", model_type, " Syntax:\n"))
#   cat(est_model_syntaxes[[model_type]], "\n\n")
# }
