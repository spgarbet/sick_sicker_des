set.seed(123)
run.test <- des_run(modifyList(inputs, list(N = 2, treatA = TRUE, treatB = TRUE)))
foo <- run.test %>% 
  group_by(name) %>% 
  filter(resource != "time_in_model") %>% 
  nest() %>% 
  mutate(data = map2(data,name,~(.x %>% mutate(name = .y)))) %>% 
  mutate(qaly = map(data,~(split_arrivals(.x) ))) 

arrivals = foo[1,]$qaly[[1]] ; arrivals

cost_arrivals <- function(arrivals, inputs)
{
    
  arrivals$cost  <- 0  # No costs yet
  arrivals$dcost <- 0  # No discounted costs either
  
  selector = arrivals$resource == 'healthy'
  arrivals$cost[selector] <- inputs$c.H *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- discount_value(inputs$c.H,
                                             arrivals$period_start[selector], arrivals$period_end[selector])
  selector = grepl('sick1',arrivals$active_resources) 
  arrivals$cost[selector] <- arrivals$cost[selector] + inputs$c.S1 *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector]  + discount_value(inputs$c.S1,
                                             arrivals$period_start[selector], arrivals$period_end[selector])
  
  selector = grepl('sick2',arrivals$active_resources) 
  arrivals$cost[selector] <- arrivals$cost[selector] + inputs$c.S2 *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.S2,
                                             arrivals$period_start[selector], arrivals$period_end[selector])
  
  selector = arrivals$A_active
  arrivals$cost[selector] <- arrivals$cost[selector] + inputs$c.TrtA *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector] + discount_value(inputs$c.TrtA,
                                             arrivals$period_start[selector], arrivals$period_end[selector])
  
  selector = arrivals$B_active
  arrivals$cost[selector] <- arrivals$cost[selector]  + inputs$c.TrtB *
    (arrivals$period_end[selector] - arrivals$period_start[selector])
  arrivals$dcost[selector] <- arrivals$dcost[selector]  + discount_value(inputs$c.TrtB,
                                             arrivals$period_start[selector], arrivals$period_end[selector])
  
  arrivals
}