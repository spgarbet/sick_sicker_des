


sick1 <- function(traj, inputs)
{
  traj                      |> 
    set_attribute("State", 1) |> # 1 => Sick 1 (S1)
    release('healthy')        |> # Track state change for tally later
    seize('sick1')            |>
    branch( 
      function() get_attribute(env, "Treat")+1,
      continue = rep(TRUE, 2),
      trajectory(),  # No Treatment
      trajectory() |> 
        clone(2,
              trajectory() %>%  # Treatment Clone
                set_attribute("t_queue_enter", function() now(env)) %>%
                seize("treated", 1) %>% 
                set_attribute("wait_time",
                              function() now(env) - get_attribute(env, "t_queue_enter")) %>% 
                set_attribute("treated", 1) %>%
                timeout(inputs$tx_duration) %>% 
                release("treated", 1) %>% 
                synchronize(wait = TRUE), # allows for secular death or progression while in treatment
              
              trajectory() %>% 
                # seize("foo") %>% 
                # timeout(.1) %>% 
                # release("foo") %>% 
                synchronize(wait = FALSE)
        )
    )
}

