debug_patient_state <- function(env, patient_name = NULL) {
  if (is.null(patient_name)) {
    # Show all patients
    cat("=== ALL PATIENT STATES ===\n")
    arrivals <- get_mon_arrivals(env, per_resource = TRUE)
    attributes <- get_mon_attributes(env)
    
    # Group by patient
    patients <- unique(arrivals$name)
    for (p in patients) {
      cat("\n", p, ":\n")
      p_arrivals <- subset(arrivals, name == p)
      p_attrs <- subset(attributes, name == p)
      
      cat("  Resources:\n")
      for (i in 1:nrow(p_arrivals)) {
        cat("    ", p_arrivals$resource[i], ": ", 
            round(p_arrivals$start_time[i], 3), " to ", 
            round(p_arrivals$end_time[i], 3), "\n")
      }
      
      if (nrow(p_attrs) > 0) {
        cat("  Final attributes:\n")
        final_attrs <- p_attrs %>% 
          group_by(key) %>% 
          slice_tail(n = 1) %>%
          ungroup()
        
        for (i in 1:nrow(final_attrs)) {
          cat("    ", final_attrs$key[i], ":", final_attrs$value[i], "\n")
        }
      }
    }
  } else {
    # Show specific patient
    cat("=== PATIENT", patient_name, "===\n")
    arrivals <- get_mon_arrivals(env, per_resource = TRUE)
    attributes <- get_mon_attributes(env)
    
    p_arrivals <- subset(arrivals, name == patient_name)
    p_attrs <- subset(attributes, name == patient_name)
    
    cat("Resources seized/released:\n")
    for (i in 1:nrow(p_arrivals)) {
      cat("  ", p_arrivals$resource[i], ": ", 
          round(p_arrivals$start_time[i], 3), " to ", 
          round(p_arrivals$end_time[i], 3), "\n")
    }
    
    if (nrow(p_attrs) > 0) {
      cat("\nAttribute changes:\n")
      for (i in 1:nrow(p_attrs)) {
        cat("  ", round(p_attrs$time[i], 3), ":", 
            p_attrs$key[i], "=", p_attrs$value[i], "\n")
      }
    }
  }
}

des_run_debug <- function(inputs,pp="patient0",model = "10") {
  cat("Starting simulation with debugging...\n")
  
  env <<- simmer("DiseaseModelWithCloning")
  traj <- des(env, inputs)
  
  if (model!="10") {
    env %>%
      create_counters(counters) %>%
      add_resource("treatment_center", capacity = inputs$treatment_capacity, queue_size = Inf) %>%
      add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) %>%
      run(inputs$horizon + 1/365) %>%
      wrap()
  } else if (model == "10") {
    env |> 
      create_counters(counters) |>
      add_generator("patient", traj, at(rep(0, inputs$N)), mon=2) |>
      run(inputs$horizon+1/365) |> # Simulate just past horizon (in years)
      wrap()
  }

  
  # Debug specific patient
  
  arrivals = get_mon_arrivals(env, per_resource = TRUE)
  
  list(
    patient = debug_patient_state(env, pp),
    arrivals = arrivals,
    resources = get_mon_resources(env),
    attributes = get_mon_attributes(env)
  )
}
