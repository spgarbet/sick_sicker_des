##############################################################################
# DES with {simmer}: queue-limited treatment vs. progression — FIXED audit
# (ensures “event” column is always character, even when a block has 0 rows)
##############################################################################

library(simmer)          # ≥ 4.4
library(simmer.bricks)
library(dplyr)
select <- dplyr::select          # avoid simmer::select clash

set.seed(20250606)

# --------------------------- parameters -----------------------------------
N            <- 100
rate_dis     <- 0.02
rate_prog    <- 0.03
rate_death_H <- 0.01
rate_death_S <- rate_death_H * 3
tx_capacity  <- 1
tx_duration  <- 0.5      # years on treatment
horizon      <- 100

# --------------------------- environment ----------------------------------
env <- simmer("minimal") %>%
  add_resource("tx", capacity = tx_capacity)

# --------------------------- patient trajectory ---------------------------
patient <- trajectory() %>%
  set_attribute("progressed", 0) %>%
  set_attribute(c("t_dis", "t_dH"),
                function() c(rexp(1, rate_dis),
                             rexp(1, rate_death_H))) %>%
  timeout(function() min(get_attribute(env, "t_dis"),
                         get_attribute(env, "t_dH"))) %>%
  branch(
    function() if (get_attribute(env, "t_dis") < get_attribute(env, "t_dH")) 1 else 2,
    continue = c(TRUE, FALSE),
    trajectory("sick") %>%                                       # diseased
      set_attribute("disease_onset", 1) %>%
      set_attribute("dead", 0) %>%
      clone(
        3,
        trajectory() %>%                                          # treatment
          set_attribute("t_queue_enter", function() now(env)) %>%
          seize("tx", 1) %>%
          set_attribute("wait_time",
                        function() now(env) - get_attribute(env, "t_queue_enter")) %>%
          set_attribute("treated", 1) %>%
          timeout(tx_duration) %>%
          release("tx", 1) %>% 
          synchronize(wait = TRUE), # allows for secular death or progression while in treatment
        trajectory() %>%                                          # death
          timeout(function() rexp(1, rate_death_S)) %>%
          set_attribute("dead", 1) %>%
          synchronize(wait = FALSE),
        trajectory() %>%                                          # progression
          timeout(function() rexp(1, rate_prog)) %>%
          set_attribute("progressed", 1) %>%
          synchronize(wait = FALSE)
      ) %>%
      branch(
        function() {
          if      (isTRUE(get_attribute(env,"dead") == 1))        1
          else if (isTRUE(get_attribute(env,"progressed") == 1))  2
          else                                                    3 },
        continue = c(FALSE, TRUE, TRUE),
        trajectory(),                         # 1: died while waiting
        trajectory("post-prog") %>%
          timeout(function() rexp(1, rate_death_S)) %>%
          set_attribute("dead", 1),           # 2: progressed
        trajectory("post-tx") %>%             # 3: treated
          timeout(function() rexp(1, rate_death_H)) %>%
          set_attribute("dead", 1)
      ),
    trajectory("healthy-death") %>%           # died while healthy
      set_attribute("dead", 1)
  ) %>%
  timeout(function() max(0, horizon - now(env)))       # horizon exit

# --------------------------- run simulation -------------------------------
env %>%
  add_generator("patient", patient, at(rep(0, N)), mon = 2) %>%
  run(until = horizon)

# --------------------------- audit helper ---------------------------------
audit_patient <- function(env, id = 1) {
  nm <- paste0("patient", id)
  
  # helper to guarantee character type even for 0-row tibbles
  char_df <- function(df) mutate(df, event = as.character(event))
  
  seize_tx <- get_mon_arrivals(env, per_resource = TRUE) %>%
    filter(name == nm, resource == "tx") %>%
    transmute(time = start_time, event = "seize_tx") %>% char_df()
  
  attrs <- get_mon_attributes(env) %>%
    filter(name == nm) %>%
    arrange(time)
  
  onset <- attrs %>%
    filter(key == "disease_onset") %>%
    transmute(time, event = "disease_onset") %>% char_df()
  
  queue <- attrs %>%
    filter(key %in% c("t_queue_enter", "wait_time")) %>%
    mutate(event = ifelse(key == "t_queue_enter",
                          "queue_enter", "treatment_start")) %>%
    select(time, event) %>% char_df()
  
  progression <- attrs %>%
    filter(key == "progressed", value == 1) %>%
    slice_head(n = 1) %>%
    transmute(time, event = "progression") %>% char_df()
  
  death <- attrs %>%
    filter(key == "dead", value == 1) %>%
    slice_head(n = 1) %>%
    transmute(time, event = "death") %>% char_df()
  
  exit <- get_mon_arrivals(env) %>%
    filter(name == nm) %>%
    transmute(time = end_time, event = "model_exit") %>% char_df()
  
  events <- bind_rows(seize_tx, onset, queue, progression, death, exit) %>%
    arrange(time)
  
  # truncate after first death or exit
  cut <- which(events$event %in% c("death", "model_exit"))
  if (length(cut)) events <- events[seq_len(min(cut)), ]
  events
}

# --------------------------- example audits -------------------------------
print(audit_patient(env, sample(1:100,1)), row.names = FALSE)

