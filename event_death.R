#


# Given attributes of a patient (trajectory), it returns in years 
# how long till the patient would die a secular death.
#
# This is a terrible mortality model, but is the reference
# from sicker as a simple exponential draw
years_till_death <- function(inputs)
{
  rexp(1, inputs$r.HD)
}

# Given a trajectory, modify as needed when a secular
# death occurs.
#
# In this case, it marks a counter and terminates 
# the trajectory. A branch is required, even though
# it doesn't branch to force the termination.
death <- function(traj, inputs)
{
  traj |> branch(
    function() 1,
    continue=c(FALSE), # False is patient death, had to use a branch to force termination
    trajectory("Death") |>
      mark("death")     |> # Must be in 'counters'
      terminate_simulation()
  )
}