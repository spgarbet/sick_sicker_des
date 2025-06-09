#############################################################################
# DISEASE MODEL WITH CLONING - LONG UNCERTAIN TREATMENT WAITS
#############################################################################
#
# MODEL DESCRIPTION:
# This model simulates the same disease progression but with severely limited
# treatment capacity, creating long and uncertain wait times for treatment.
# Patients' disease continues to evolve while they wait for care.
#
# KEY ASSUMPTIONS:
# - Treatment capacity is severely limited (e.g., 1 slot for 50 patients)
# - Treatment wait times are long (months to years) and uncertain
# - Disease progression continues normally while waiting for treatment
# - Patients may give up waiting, die, or become ineligible while in queue
#
# WHY CLONING IS ESSENTIAL:
# 1. PARALLEL COMPETING PROCESSES: Two simultaneous processes must be modeled
#    Clone 1: Treatment-seeking pathway (waits in queue for uncertain duration)
#    Clone 2: Natural disease progression (continues independently of queue status)
#
# 2. UNCERTAIN QUEUE DYNAMICS: Wait time is not deterministic
#    - Patient may wait months/years before getting treatment
#    - During this time, disease progression can't be "paused"
#    - Need to model interaction between wait time and disease evolution
#
# 3. COMPLEX SYNCHRONIZATION: Multiple exit conditions from waiting state
#    - Treatment becomes available → start treatment (Clone 1 wins)
#    - Disease progresses to ineligible state → leave queue (Clone 2 wins)
#    - Patient dies while waiting → leave queue (Clone 2 wins)
#    - Patient gives up waiting → continue natural progression (Clone 2 wins)
#
# 4. REALISTIC HEALTHCARE ACCESS: Models real-world resource constraints
#    - Queue position doesn't guarantee eventual treatment
#    - Disease severity may change eligibility while waiting
#    - Patient preferences and patience affect treatment seeking
#
# TECHNICAL APPROACH:
# - Clone 1: seize("treatment_center") with trap() to listen for Clone 2 events
# - Clone 2: continues event-driven progression, sends signals to Clone 1
# - synchronize() determines which pathway continues based on what happened
# - Extensive use of trap()/send() for inter-clone communication
#
# USE CASES:
# - Resource-constrained healthcare systems
# - Expensive/specialized treatments with limited capacity
# - Modeling healthcare access disparities and wait time impacts
# - Policy analysis of treatment capacity vs. population health outcomes
# - Studying patient behavior under uncertainty (giving up, seeking alternatives)
#
# KEY INSIGHTS THIS MODEL REVEALS:
# - Impact of wait times on clinical outcomes
# - Treatment access rates vs. clinical need
# - Disease progression while waiting for care
# - Resource utilization efficiency under constraints
# - Equity implications of limited treatment capacity
#
#############################################################################

library(tidyverse)
library(simmer)

# Parameters - modeling a scenario with very limited treatment capacity
inputs <- list(
  N = 50,                     # Number of patients
  horizon = 100,               # Time horizon (years)
  d.r = 0.03,                # Discount Rate
  
  # Disease progression rates (per year)
  r.HS = 0.2,                # Healthy to Sick rate (higher for demonstration)
  r.SH = 0.05,               # Sick to Healthy (natural recovery) rate  
  r.SS2 = 0.3,               # Sick to Sicker rate (higher - disease progresses while waiting)
  r.HD = 0.005,              # Healthy to Death rate
  r.SD = 0.03,               # Sick to Death rate (higher than healthy)
  r.S2D = 0.15,              # Sicker to Death rate (much higher)
  
  # Treatment parameters - VERY LIMITED CAPACITY
  treatment_duration = 3,     # Treatment duration (years) - long treatment
  treatment_capacity = 10,     # Only 1 treatment slot for 50 patients!
  treatment_success_rate = 0.95, # High success rate when you can get it
  treatment_protection_factor = 0.1, # Treatment reduces progression by 90%
  
  # Queue eligibility - patients may become too sick for treatment
  max_wait_time = 5,          # Maximum time willing to wait (years)
  sicker_eligible_for_treatment = FALSE # Sicker patients can't get treatment
)

#############################################################################
# EVENT DEFINITIONS WITH CLONING
#############################################################################

# Event: Healthy to Sick progression
years_till_sick <- function(inputs) {
  state <- get_attribute(env, "State")
  if(state == 0) { # 0 = Healthy
    rexp(1, inputs$r.HS)
  } else {
    inputs$horizon + 1 # Past simulation time
  }
}

become_sick <- function(traj, inputs) {
  traj %>%
    log_("Starting become_sick transition") %>%
    set_attribute("State", 1) %>% # 1 = Sick
    release('healthy') %>%
    seize('sick') %>%
    log_("Became sick - creating clones for treatment vs natural progression") %>%
    
    # Create patient-specific signal names
    set_attribute("patient_id", function() {
      name <- get_name(env)
      as.numeric(gsub("patient", "", name))
    }) %>%
    
    # Initialize treatment tracking
    set_attribute("treatment_started", 0) %>%
    
    # KEY: Create clones for parallel processes
    clone(
      n = 2,
      
      # Clone 1: Treatment-seeking pathway
      trajectory("Treatment Seeking") %>%
        log_("Clone 1: Seeking treatment") %>%
        set_attribute("seeking_treatment", 1) %>%
        
        # Try to get treatment - this might take a VERY long time
        log_("Attempting to seize treatment center") %>%
        seize("treatment_center", 1) %>%
        log_("FINALLY got treatment slot!") %>%
        
        # Signal that treatment started
        set_attribute("treatment_started", 1) %>%
        
        # Undergo treatment with reduced progression risk
        set_attribute("on_treatment", 1) %>%
        
        timeout(inputs$treatment_duration) %>%
        
        # Treatment completion
        log_("Treatment completed!") %>%
        branch(
          function() if(runif(1) < inputs$treatment_success_rate) 1 else 2,
          continue = c(TRUE, TRUE),
          
          # Successful treatment
          trajectory() %>%
            log_("Treatment successful - returning to healthy!") %>%
            set_attribute("State", 0) %>%
            release("sick") %>%
            seize("healthy"),
          
          # Treatment failed
          trajectory() %>%
            log_("Treatment failed - remaining sick")
        ) %>%
        
        set_attribute("on_treatment", 0) %>%
        set_attribute("seeking_treatment", 0) %>%
        release("treatment_center"),
      
      # Clone 2: Natural disease progression while waiting
      trajectory("Natural History While Waiting") %>%
        log_("Clone 2: Natural progression while waiting for treatment") %>%
        
        # Wait for max_wait_time OR until treatment starts
        renege_if("treatment_started",
                  out = trajectory() %>%
                    log_("Treatment started - Clone 2 terminating")) %>%
        
        timeout(inputs$max_wait_time) %>%
        log_("Got tired of waiting - giving up on treatment") %>%
        renege_abort()
    ) %>%
    
    # Synchronize clones - continue based on what happened
    synchronize(wait = TRUE) %>%
    log_("Treatment pathway resolved - continuing with final state") %>%
    
    # CORRECT FIX: Just reschedule events, don't restart event loop
    # The main trajectory's rollback will continue the simulation
    assign_events(inputs)
}
# Modified progression events that interact with clones
years_till_healthy <- function(inputs) {
  state <- get_attribute(env, "State")
  on_treatment <- get_attribute(env, "on_treatment")
  seeking_treatment <- get_attribute(env, "seeking_treatment")
  
  # Can only recover naturally if sick, not on treatment, and either:
  # 1. Not seeking treatment, OR
  # 2. Gave up waiting for treatment
  if(state == 1 && on_treatment == 0) {
    rexp(1, inputs$r.SH)
  } else {
    inputs$horizon + 1
  }
}
# Fixed recover_naturally function that continues event processing
recover_naturally <- function(traj, inputs) {
  traj %>%
    set_attribute("State", 0) %>% # 0 = Healthy
    release('sick') %>%
    seize('healthy') %>%
    log_("Recovered naturally to healthy state") %>%
    
    # Send patient-specific signal
    send(function() paste0("patient", get_attribute(env, "patient_id"), "_recovered")) %>%
    
    # CORRECT FIX: Just reschedule events, don't restart event loop
    assign_events(inputs)
}

years_till_sicker <- function(inputs) {
  state <- get_attribute(env, "State")
  on_treatment <- get_attribute(env, "on_treatment")
  
  if(state == 1) { # Patient is sick
    if(on_treatment == 1) {
      # Reduced progression rate during treatment
      reduced_rate <- inputs$r.SS2 * inputs$treatment_protection_factor
      rexp(1, reduced_rate)
    } else {
      # Normal progression rate
      rexp(1, inputs$r.SS2)
    }
  } else {
    inputs$horizon + 1
  }
}

# Fixed progress_to_sicker function that continues event processing
progress_to_sicker <- function(traj, inputs) {
  traj %>%
    set_attribute("State", 2) %>% # 2 = Sicker
    release('sick') %>%
    seize('sicker') %>%
    log_("Progressed to sicker state") %>%
    
    # Send patient-specific signals based on treatment status
    branch(
      function() get_attribute(env, "on_treatment"),
      continue = rep(TRUE, 2),
      
      trajectory() %>%
        log_("Progressed to sicker during treatment") %>%
        set_attribute("on_treatment", 0) %>%
        send(function() paste0("patient", get_attribute(env, "patient_id"), "_progressed_during_tx")),
      
      trajectory() %>%
        log_("Progressed to sicker while waiting or not seeking treatment") %>%
        send(function() paste0("patient", get_attribute(env, "patient_id"), "_progressed"))
    ) %>%
    
    # CORRECT FIX: Just reschedule events, don't restart event loop
    assign_events(inputs)
}

years_till_death <- function(inputs) {
  state <- get_attribute(env, "State")
  patient_name <- get_name(env)
  
  rate <- switch(state + 1,
                 inputs$r.HD,   # Healthy (state 0)
                 inputs$r.SD,   # Sick (state 1)
                 inputs$r.S2D)  # Sicker (state 2)
  
  time_to_death <- rexp(1, rate)
  
  # Debug output
  cat("Patient", patient_name, "in state", state, 
      "- death rate:", rate, "- time to death:", time_to_death, "\n")
  
  return(time_to_death)
}

death <- function(traj, inputs) {
  traj %>%
    log_("Death event triggered") %>%
    
    # Send death signals while patient is still active
    branch(
      function() get_attribute(env, "on_treatment"),
      continue = rep(TRUE, 2),
      
      trajectory() %>%
        send(function() paste0("patient", get_attribute(env, "patient_id"), "_died_during_tx")),
      
      trajectory() %>%
        send(function() paste0("patient", get_attribute(env, "patient_id"), "_died"))
    ) %>%
    
    # CRITICAL FIX: Release time_in_model BEFORE any termination
    release("time_in_model") %>%
    log_("Released time_in_model for death") %>%
    
    # FIXED: Only release the resource that matches current state
    branch(
      function() get_attribute(env, "State") + 1,
      continue = rep(TRUE, 4),
      
      # State 0: Healthy - only release healthy
      trajectory() %>% 
        log_("Death in healthy state - releasing healthy") %>%
        release("healthy"),
      
      # State 1: Sick - only release sick  
      trajectory() %>% 
        log_("Death in sick state - releasing sick") %>%
        release("sick"),
      
      # State 2: Sicker - only release sicker
      trajectory() %>% 
        log_("Death in sicker state - releasing sicker") %>%
        release("sicker"),
      
      # State 3: Already dead - no state resource to release
      trajectory() %>% 
        log_("Death in dead state - no state resource to release")
    ) %>%
    
    # Release treatment center if on treatment
    branch(
      function() get_attribute(env, "on_treatment") + 1,
      continue = rep(TRUE, 2),
      trajectory(),
      trajectory() %>% release("treatment_center") %>% log_("Released treatment center")
    ) %>%
    
    # Mark death and set state
    mark("death") %>%
    set_attribute("State", 3) %>%
    log_("Patient marked as dead") %>%
    
    # TERMINATE - all cleanup is complete
    branch(
      function() 1,
      continue = FALSE,
      trajectory()
    )
}
#############################################################################
# INFRASTRUCTURE FUNCTIONS (same as before)
#############################################################################

create_counters <- function(env, counters) {
  sapply(counters, FUN=function(counter) {
    env <- add_resource(env, counter, Inf, 0)
  })
  env
}

mark <- function(traj, counter) {
  traj %>%
    seize(counter, 1) %>%
    timeout(0) %>%
    release(counter, 1)
}

assign_events <- function(traj, inputs) {
  sapply(event_registry, FUN=function(event) {
    traj <- set_attribute(traj, event$attr, function() {
      event$time_to_event(inputs)
    })
  })
  traj
}

next_event <- function() {
  event_time <- Inf
  event <- NA
  id <- 0
  
  for(i in 1:length(event_registry)) {
    e <- event_registry[[i]]
    tmp_time <- get_attribute(env, e$attr)
    if(tmp_time < event_time) {
      event <- e
      event_time <- tmp_time
      id <- i
    }
  }
  
  return(list(event=event, event_time=event_time, id=id))
}

process_events <- function(traj, env, inputs) {
  traj <- timeout(traj, function() {
    ne <- next_event()
    event_time <- ne[['event_time']]
    max(0, event_time - now(env))
  })
  
  args <- lapply(event_registry, FUN=function(e) {
    trajectory(e$name) %>%
      e$func(inputs) %>%
      set_attribute(e$attr, function() {now(env) + e$time_to_event(inputs)})
  })
  
  args$".trj" <- traj
  args$option <- function() next_event()$id
  args$continue <- rep(TRUE, length(event_registry))
  traj <- do.call(branch, args)
  
  lapply(event_registry[sapply(event_registry, function(x) x$reactive)], 
         FUN=function(e) {
           traj <- set_attribute(traj, e$attr, function() {now(env) + e$time_to_event(inputs)})
         })
  
  traj
}

des <- function(env, inputs) {
  trajectory("Patient") %>%
    initialize_patient(inputs) %>%
    assign_events(inputs) %>%
    branch(
      function() 1,
      continue = TRUE,
      trajectory("main_loop") %>% process_events(env, inputs)
    ) %>%
    rollback(1, 100)
}

initialize_patient <- function(traj, inputs) {
  traj %>%
    seize("time_in_model") %>%
    set_attribute("AgeInitial", function() sample(20:30, 1)) %>%
    set_attribute("State", 0) %>%           # 0 = Healthy
    set_attribute("on_treatment", 0) %>%
    set_attribute("seeking_treatment", 0) %>%
    set_attribute("patient_id", function() {
      name <- get_name(env)
      as.numeric(gsub("patient", "", name))
    }) %>%
    # ENSURE patient actually enters healthy state in the monitoring system
    seize("healthy") %>%
    log_("Patient initialized in healthy state")
}
cleanup_on_termination <- function(traj, inputs) {
  traj %>%
    log_("Starting cleanup on termination (non-death)") %>%
    
    # Only handle non-death terminations
    # Death cleanup is handled in the death() function itself
    branch(
      function() {
        state <- get_attribute(env, "State")
        if (state == 3) {
          # Already dead - cleanup already done
          return(1)
        } else {
          # Normal termination - need cleanup
          return(2)
        }
      },
      continue = rep(TRUE, 2),
      
      # Patient already dead - no cleanup needed
      trajectory("already_dead_cleanup") %>%
        log_("Patient already dead - no additional cleanup"),
      
      # Normal termination cleanup
      trajectory("normal_termination_cleanup") %>%
        log_("Normal termination - cleaning up resources") %>%
        # NOTE: time_in_model already released in terminate_simulation()
        
        # Clean up state-based resources
        branch(
          function() {
            state <- get_attribute(env, "State")
            cat("Normal termination cleanup for patient in state:", state, "\n")
            state + 1  # Convert to 1-based indexing for branch
          },
          continue = rep(TRUE, 4),
          
          # State 0: Healthy
          trajectory("normal_cleanup_healthy") %>% 
            log_("Releasing healthy resource") %>%
            release("healthy"),
          
          # State 1: Sick  
          trajectory("normal_cleanup_sick") %>%
            log_("Releasing sick resource") %>%
            release("sick"),
          
          # State 2: Sicker
          trajectory("normal_cleanup_sicker") %>%
            log_("Releasing sicker resource") %>%
            release("sicker"),
          
          # State 3: Dead (shouldn't happen here)
          trajectory("normal_cleanup_dead") %>%
            log_("Dead state in normal cleanup - unexpected")
        ) %>%
        
        # Clean up treatment resources if applicable
        branch(
          function() get_attribute(env, "on_treatment") + 1,
          continue = rep(TRUE, 2),
          
          # Not on treatment
          trajectory("normal_cleanup_no_treatment"),
          
          # On treatment - release treatment center
          trajectory("normal_cleanup_treatment") %>%
            log_("Releasing treatment center") %>%
            release("treatment_center")
        )
    ) %>%
    
    log_("Cleanup completed")
}

terminate_simulation <- function(traj, inputs) {
  traj %>%
    log_("Terminating simulation at horizon") %>%
    release("time_in_model") %>%  # Release time_in_model for living patients
    branch(
      function() 1,
      continue = FALSE,
      trajectory("Horizon Termination") %>% 
        cleanup_on_termination(inputs) %>%
        log_("Horizon termination complete")
    )
}
#############################################################################
# EVENT REGISTRY
#############################################################################

event_registry <- list(
  list(name = "Terminate at horizon",
       attr = "aTerminate", 
       time_to_event = function(inputs) inputs$horizon - now(env),
       func = terminate_simulation,
       reactive = FALSE),
  
  list(name = "Death",
       attr = "aDeath",
       time_to_event = years_till_death,
       func = death, 
       reactive = TRUE),
  
  list(name = "Become Sick",
       attr = "aSick",
       time_to_event = years_till_sick,
       func = become_sick,
       reactive = TRUE),
  
  list(name = "Recover to Healthy", 
       attr = "aHealthy",
       time_to_event = years_till_healthy,
       func = recover_naturally,
       reactive = TRUE),
  
  list(name = "Progress to Sicker",
       attr = "aSicker", 
       time_to_event = years_till_sicker,
       func = progress_to_sicker,
       reactive = TRUE)
)

#############################################################################
# SIMULATION SETUP AND EXECUTION
#############################################################################

counters <- c(
  "time_in_model",
  "death", 
  "healthy",
  "sick",
  "sicker"
)

des_run <- function(inputs) {
  env <<- simmer("DiseaseModelWithCloning")
  traj <- des(env, inputs)
  
  env %>%
    create_counters(counters) %>%
    add_resource("treatment_center", capacity = inputs$treatment_capacity, queue_size = Inf) %>%
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) %>%
    run(inputs$horizon + 1/365) %>%
    wrap()
  
  arrivals = get_mon_arrivals(env, per_resource = TRUE)
  
  # outcomes = arrivals |> group_by(name) %>% 
  #   filter(resource != "time_in_model") %>% 
  #   nest() %>% 
  #   mutate(data = map2(data,name,~(.x %>% mutate(name = .y)))) %>% 
  #   mutate(qaly_i = map(data,~(split_arrivals(.x) %>% qaly_arrivals(inputs)))) %>% 
  #   mutate(cost_i = map(data,~(split_arrivals(.x) %>% cost_arrivals(inputs)))) %>% 
  #   mutate(qaly = map_dbl(qaly_i,~(.x %>% summarise(qaly = sum(qaly)) %>% pull(qaly)))) %>% 
  #   mutate(dqaly = map_dbl(qaly_i,~(.x %>% summarise(dqaly = sum(dqaly)) %>% pull(dqaly)))) %>% 
  #   mutate(cost = map_dbl(cost_i,~(.x %>% summarise(cost = sum(cost)) %>% pull(cost)))) %>% 
  #   mutate(dcost = map_dbl(cost_i,~(.x %>% summarise(dcost = sum(dcost)) %>% pull(dcost)))) %>% 
  #   ungroup()
  # 
  list(
    arrivals = arrivals,
    #outcomes = outcomes,
    resources = get_mon_resources(env),
    attributes = get_mon_attributes(env)
  )
}

#############
# DEBUG
#############
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

des_run_debug <- function(inputs,pp="patient0") {
  cat("Starting simulation with debugging...\n")
  
  env <<- simmer("DiseaseModelWithCloning")
  traj <- des(env, inputs)
  
  env %>%
    create_counters(counters) %>%
    add_resource("treatment_center", capacity = inputs$treatment_capacity, queue_size = Inf) %>%
    add_generator("patient", traj, at(rep(0, inputs$N)), mon = 2) %>%
    run(inputs$horizon + 1/365) %>%
    wrap()
  
  # Debug specific patient
  debug_patient_state(env, pp)
  
  arrivals = get_mon_arrivals(env, per_resource = TRUE)
  
  list(
    arrivals = arrivals,
    resources = get_mon_resources(env),
    attributes = get_mon_attributes(env)
  )
}

# Test with a smaller example first
test_inputs <- inputs
#test_inputs$N <- 5  # Just 5 patients for debugging
#test_inputs$horizon <- 100  # Shorter simulation

cat("Running debug simulation...\n")
set.seed(123)
debug_results <- des_run_debug(test_inputs, pp = "patient42")
#############################################################################
# RUN SIMULATION AND ANALYZE RESULTS
#############################################################################

cat("Running disease model with cloning (long uncertain treatment waits)...\n")
cat("Treatment duration:", inputs$treatment_duration, "years\n")
cat("Treatment capacity:", inputs$treatment_capacity, "slots for", inputs$N, "patients\n")
cat("Max waiting time:", inputs$max_wait_time, "years\n")
cat("Disease progression rate:", inputs$r.SS2, "per year\n\n")

set.seed(123)
results <- des_run(inputs)

cat("=== Simulation Results ===\n")

# Show final states
final_states <- results$attributes %>%
  subset(key == "State") %>%
  aggregate(value ~ name, data = ., FUN = tail, n = 1)

state_names <- c("Healthy", "Sick", "Sicker", "Dead")
final_states$state_name <- state_names[final_states$value + 1]

cat("Final patient states:\n")
print(table(final_states$state_name))

# Analyze treatment center usage
treatment_usage <- subset(results$resources, resource == "treatment_center")
if(nrow(treatment_usage) > 0) {
  cat("\nTreatment Center Analysis:\n")
  cat("Max server count:", max(treatment_usage$server), "\n")
  cat("Max queue length:", max(treatment_usage$queue), "\n")
  cat("Final queue length:", tail(treatment_usage$queue, 1), "\n")
  cat("Average queue length:", round(mean(treatment_usage$queue), 2), "\n")
  
  # Calculate queue waiting times
  if(max(treatment_usage$queue) > 0) {
    cat("Queue was non-empty", 
        round(sum(treatment_usage$queue > 0) / nrow(treatment_usage) * 100, 1), 
        "% of simulation time\n")
  }
}

# Analyze treatment outcomes
treatment_events <- subset(results$arrivals, resource == "treatment_center")
if(nrow(treatment_events) > 0) {
  cat("\nTreatment Outcomes:\n")
  cat("Total patients who got treatment:", nrow(treatment_events), "\n")
  
  # Identify completed vs interrupted treatments
  completed_treatments <- subset(treatment_events, 
                                 abs((end_time - start_time) - inputs$treatment_duration) < 0.1)
  
  cat("Treatments completed:", nrow(completed_treatments), "\n")
  cat("Treatments interrupted:", nrow(treatment_events) - nrow(completed_treatments), "\n")
  
  if(nrow(treatment_events) > nrow(completed_treatments)) {
    interrupted <- subset(treatment_events, 
                          (end_time - start_time) < inputs$treatment_duration * 0.9)
    cat("Average time before interruption:", 
        round(mean(interrupted$end_time - interrupted$start_time), 2), "years\n")
  }
  
  # Show waiting times for those who got treatment
  cat("Average wait time for treatment:", 
      round(mean(treatment_events$start_time), 2), "years\n")
}

# Count patients who likely gave up waiting
sick_at_end <- subset(final_states, state_name == "Sick")
sicker_at_end <- subset(final_states, state_name == "Sicker")
patients_who_needed_treatment <- nrow(sick_at_end) + nrow(sicker_at_end)

cat("\nAccess to Care Analysis:\n")
cat("Patients who got treatment:", nrow(treatment_events), "out of", inputs$N, "\n")
cat("Patients still sick/sicker at end:", patients_who_needed_treatment, "\n")
cat("Treatment access rate:", 
    round(nrow(treatment_events) / inputs$N * 100, 1), "%\n")

# Show some example patient trajectories
cat("\nSample Patient Journeys:\n")
sample_patients <- head(unique(results$arrivals$name), 5)
for(patient in sample_patients) {
  cat("\n", patient, ":\n")
  patient_events <- subset(results$arrivals, name == patient)
  for(i in 1:nrow(patient_events)) {
    cat("  ", patient_events$resource[i], ": ", 
        round(patient_events$start_time[i], 2), " to ", 
        round(patient_events$end_time[i], 2), " years\n")
  }
}

# Analyze Wait Times

analyze_wait_times <- function(results, model_type = "queue") {
  
  if (model_type == "direct") {
    # For direct scheduling - wait times are predetermined
    return("Wait times are deterministic = wait_days/365")
    
  } else if (model_type == "queue") {
    # For queue-based models
    treatment_events <- results$arrivals %>%
      filter(resource == "treatment_center")
    
    if (nrow(treatment_events) == 0) {
      return("No patients received treatment")
    }
    
    # Assuming arrival_time tracks when patient joined queue
    wait_times <- treatment_events$start_time
    
  } else if (model_type == "cloning") {
    # For cloning models - more complex analysis needed
    sick_times <- results$attributes %>%
      filter(key == "State", value == 1) %>%
      group_by(name) %>%
      dplyr::slice(1) %>%
      dplyr::select(name, became_sick_time = time)
    
    treatment_times <- results$arrivals %>%
      filter(resource == "treatment_center") %>%
      dplyr::select(name, treatment_start_time = start_time)
    
    wait_data <- sick_times %>%
      left_join(treatment_times, by = "name") %>%
      mutate(wait_time = treatment_start_time - became_sick_time) %>%
      filter(!is.na(wait_time))
    
    wait_times <- wait_data$wait_time
  }
  
  # Calculate summary statistics
  list(
    n_observations = length(wait_times),
    mean = mean(wait_times),
    median = median(wait_times),
    sd = sd(wait_times),
    min = min(wait_times),
    max = max(wait_times),
    quantiles = quantile(wait_times, c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)),
    # Additional metrics
    cv = sd(wait_times) / mean(wait_times),  # Coefficient of variation
    iqr = quantile(wait_times, 0.75) - quantile(wait_times, 0.25)
  )
}

# Usage
wait_stats <- analyze_wait_times(results, model_type = "cloning")
print(wait_stats)
