library(dplyr)
library(stringr)

source("utils.R")
source("process_study1.R")
source("process_study2.R")
source("process_study3.R")

main_processing <- function() {
  all_warnings <- list()
  
  # # File paths for each study full
  study1_path <- "../simulation/results/simulation_results_study1.rda"
  study2_path <- "../simulation/results/simulation_results_study2.rda"
  study3_path <- "../simulation/results/simulation_results_study3.rda"

  # File paths for each study test
  # study1_path <- "../simulation/results/simulation_results_study1r20240814170807.rda"
  # study2_path <- "../simulation/results/simulation_results_study2r20240814172211.rda"
  # study3_path <- "../simulation/results/simulation_results_study3r20240814172328.rda"
  
  
  # Process Study 1
  print("Processing Study 1...")
  all_warnings[[1]] <- process_study1(study1_path)
  gc()
  
  # Process Study 3
  print("Processing Study 3...")
  all_warnings[[3]] <- process_study3(study3_path)
  gc()
  
  # Process Study 2
  print("Processing Study 2...")
  all_warnings[[2]] <- process_study2(study2_path)
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
  saveRDS(combined_messages_list, file = "../simulation/results/list_messages.rds")
  
  # Save combined_messages_summary
  saveRDS(combined_messages_summary, file = "../simulation/results/summary_messages.rds")
  
  rm(all_warnings, all_unique_messages, combined_messages_list, combined_messages_summary, id_map)
  gc()
  
  print("All processing steps completed. Results saved.")
}

# Run the main processing function
main_processing()