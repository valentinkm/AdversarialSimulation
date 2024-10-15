library(dplyr)
library(stringr)

source("utils.R")
source("process_study1.R")
source("process_study2.R")
source("process_study3.R")


get_latest_file <- function(directory, pattern) {
  # Normalize the directory path
  directory <- normalizePath(directory, mustWork = TRUE)
  
  # List all files in the directory
  all_files <- list.files(directory, full.names = TRUE)
  cat("All files in directory:\n", paste(all_files, collapse = "\n"), "\n\n")
  
  # Filter files based on the pattern
  files <- list.files(directory, pattern = pattern, full.names = TRUE)
  cat("Matching files:\n", paste(files, collapse = "\n"), "\n\n")
  
  # Check if there are any matching files
  if (length(files) == 0) {
    stop(paste("No files found matching pattern:", pattern))
  }
  
  # Extract the timestamps from the file names and sort them
  timestamps <- str_extract(files, "[0-9]{12,}")
  latest_file <- files[which.max(timestamps)]
  
  return(latest_file)
}

main_processing <- function() {
  all_warnings <- list()
  
  simulation_directory <- "../simulation/results_replic/"

  study1_path <- get_latest_file(simulation_directory, "simulation_results_study1r[0-9]{12,}\\.rda$")
  study2_path <- get_latest_file(simulation_directory, "simulation_results_study2r[0-9]{12,}\\.rda$")
  study3_path <- get_latest_file(simulation_directory, "simulation_results_study3r[0-9]{12,}\\.rda$")
  
  study1 <- readRDS(study1_path)
  study2 <- readRDS(study2_path)
  study3 <- readRDS(study3_path)

  # Process Study 1
  print("Processing Study 1...")
  all_warnings[[1]] <- process_study1(study1_path)
  gc()
  
  # Process Study 2
  print("Processing Study 2...")
  all_warnings[[2]] <- process_study2(study2_path)
  gc()
  
  # Process Study 3
  print("Processing Study 3 (joint study)...")
  all_warnings[[3]] <- process_study3(study3_path)
  gc()
  
  # Combine all unique messages
  all_unique_messages <- unique(unlist(lapply(all_warnings, function(x) x$Message)))
  
  combined_messages_list <- tibble(
    ID = seq_along(all_unique_messages),
    Message = all_unique_messages
  )
  
  # Create id_map
  id_map <- setNames(combined_messages_list$ID, all_unique_messages)
  
  # Combine all summaries
  combined_messages_summary <- bind_rows(all_warnings) %>%
    mutate(MessageID = id_map[Message]) %>%
    select(-Message) %>%
    rename(
      `Model Type` = model_type,
      `Message ID` = MessageID,
      `Reliability` = reliability,
      `Method` = method,
      `Count` = count
    )
  
  # Save combined_messages_list
  saveRDS(combined_messages_list, file = "../simulation/results_replic/list_messages.rds")
  
  # Save combined_messages_summary
  saveRDS(combined_messages_summary, file = "../simulation/results_replic/summary_messages.rds")
  
  rm(all_warnings, all_unique_messages, combined_messages_list, combined_messages_summary, id_map)
  gc()
  
  print("All processing steps completed. Results saved.")
}

# Run the main processing function
main_processing()