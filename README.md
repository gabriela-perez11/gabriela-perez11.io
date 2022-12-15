# making-amsterdam-greener-report
This repository hold all of the data and Prioritizr script used in our research paper titled "Making Amsterdam Greener: An Urban Planning Approach" submitted for the course Modeling for Sustainability at UvA


# Prioritizr Script
 #load packages
 library(sp)
 library(rgdal)
 library(raster)
 library(prioritizr)
 library(prioritizrdata)
 library(gurobi)

  #Load the data into Rstudio
  amsterdam <- readOGR(dsn = "C:/Users/krast/Documents/UVA/MFS/WGS/WGS_files/Amsterdam_Urban_Planning.shp") 
  fs <- list.files(path="C:/Users/krast/Documents/UVA/MFS/WGS/WGS_files/", pattern = "tif$", full.names = TRUE)
  features <- raster::stack(fs)

  #Plot data
  plot(amsterdam)
  plot(features)
  
  
  #Transform data to numbers and logics
  amsterdam$cost <- as.numeric(amsterdam$grid_code)
  amsterdam$locked_in <- as.logical(as.numeric(amsterdam$PARK))
    
  #Base problem
  problem(ams_map, features, cost_column = "cost") %>%
    add_max_utility_objective(38500) %>%
    add_binary_decisions() %>%
    add_default_solver()

  #Scenario 1 
  #budget 38.5 million
  #maximize NO2 and distance
  #minimize population that has to move
  #added manual targets
  
  p1 <- problem(ams_map, features, cost_column = "cost") %>%
    add_min_shortfall_objective(38500) %>%
    add_manual_targets(data.frame(        feature = 
       names(features)[1:3], type = "relative",
       sense = c(">=", ">=", "<="), target = c(0.02, 0.02, 0.01))) %>%
    add_binary_decisions() %>%
    add_default_solver()
  
  s1 <- solve(p1)
  
  print(eval_cost_summary(p1, s1[, "solution_1"]), width = Inf)
  print(eval_target_coverage_summary(p1, s1[, "solution_1"]), width = Inf)
  
  spplot(s1, "solution_1", main = "Solution Scenario 1", col.regions = c("white", "darkgreen"))
  
  #Scenario 2 - values the size of green spaces, that should be at least 2ha = 2 cells
  #adding neighbor constraints
  #every green space cell should have at least 1 neighbor in order to complete the 2ha size suggestion
  
  p2 <- p1 %>%
    add_neighbor_constraints(1)
    

  s2 <- solve(p2)
  
  print(eval_cost_summary(p2, s2[, "solution_1"]), width = Inf)
  print(eval_target_coverage_summary(p2, s2[, "solution_1"]), width = Inf)
  
spplot(s2, "solution_1", main = "Solution Scenario 2", col.regions = c("white", "darkgreen"))


#Scenario 3 - see how much of the city can be converted into green spaces by meeting specific targets, without a set budget
#meet all the targets and minimise the cost of the solution
  p3 <- problem (ams_map, features, cost_column = "cost") %>%
    add_min_set_objective() %>%
    add_manual_targets(data.frame(        feature = 
        names(features)[1:3], type = "relative",
        sense = c(">=", ">=", "<="), target = c(0.1, 0.1, 0.5))) %>%
    add_binary_decisions() %>%
    add_default_solver()
  
  s3 <- solve(p3)
  
  print(eval_cost_summary(p3, s3[, "solution_1"]), width = Inf)
  print(eval_target_coverage_summary(p3, s3[, "solution_1"]), width = Inf)
  
  spplot(s3, "solution_1", main = "Solution Scenario 3", col.regions = c("white", "darkgreen"))

# Data Used
[WGS_files.zip](https://github.com/gabriela-perez11/gabriela-perez11.io/files/10239908/WGS_files.zip)
