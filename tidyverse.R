
library(dplyr)

# need curly brakcets bc otherwise, R can't find the var in the pipe
var_summary <- function(data, var) {
   data |>
      summarise(
         minvar = min({{var}}),
         maxvar = max({{var}})
      )
}

mtcars |>
   group_by(cyl) |>
   var_summary()

# for arugemnts passed as characters

mtcars |>
   summarise(min_var = min(.data[["mpg"]]))

var_summary <- function(data, var1, var2) {
   data |>
      summarise(
         min_var = min(.data[[var1]]),
         max_var = max(.data[[var2]])
      )
}


mtcars |>
   group_by(cyl) |>
   var_summary("mpg", "disp")

# with dynamic names
var_summary1 <- function(data, var, var_name) {
   data |>
      summarise(
         "{var_name}" := min({{var}}),
      )
}

mtcars |>
   group_by(mpg) |>
   var_summary1(mpg, "min_mpg")

# with dynamic names that grab column name
var_summary2 <- function(data, var) {
   data |>
      summarise(
         "{{var}}_name" := min({{var}}),
      )
}

mtcars |>
   group_by(cyl) |>
   var_summary2(mpg)


# create function to filter and summarize by group  - choose variable
big_cars_summary <- function(var){
   mtcars |>
      filter(.data$cyl >= 6) |>
      group_by(.data$cyl) |>
      summarise(
         n = n(),
         mean = mean({{ var }})
      )
}
big_cars_summary(mpg)

# dynamically sum variable with named variables
big_cars_summary1 <- function(data, var_sum, var){
   data |>
      filter({{var_sum}} >= 100) |>
      group_by({{var_sum}}) |>
      summarise(
         n = n(),
         mean = mean({{ var }})
      )
}
big_cars_summary1(starwars, height, birth_year)


# dynamically sum variable with 2 named variables
big_cars_summary2 <- function(data, grp_var1, grp_var2, var){
   data |>
      group_by({{grp_var1}},{{grp_var2}}) |>
      summarise(
         n = n(),
         mean = mean({{ var }})
      )
}
big_cars_summary2(starwars, sex,  eye_color, height)


# dynamically sum variables with ellipsis
dynamic_sum <- function(data, var, ...) {
   data |>
      group_by(...) |>
      summarise(
         n = n(),
         mean = mean({{ var }})
      )
}
dynamic_sum(starwars, height, sex,  eye_color)


# dynamically sum and name variable
dynamic_sum_name <- function(data, var, ...) {
   data |>
      group_by(...) |>
      summarise(
         "{{var}}_n" := n(),
         "{{var}}_mean" := mean({{ var }})
      )
}
dynamic_sum_name(starwars, height, sex,  eye_color)



# OR ..... # note how curly brakets change
homework1 <- function(var){
   starwars |>
      summarise("mean_{var}" := mean(.data[[var]], na.rm = T),
                "max_{var}" := max(.data[[var]], na.rm = T),
                count = n())
}

homework1("height")

# tidy selection ----
## across() inside data-masking variables ()

summy <- function(df, group_var, cols){
   df |>
      group_by(({{group_var}})) |>
      summarise(
         across({{ cols }}, .fns = list(min=min, max=max))
      )
}

mtcars |>
   summy(cyl, c(mpg, disp))


mtcars |>
   summy(cyl, starts_with("mp"))

mtcars |>
   summy(cyl, where(is.numeric))




# nick brown ----

#Paul van Dam-Bates 3:42 PM
#@Nick Brown (DFO) I learned new tidyverse thing from your solution. Here is #my generalization from yours...

group_summary <- function(data, grpvar, sumvar, fns=list()){
  data |>
    group_by(across({{grpvar}})) |>
    summarise(
      across({{sumvar}},
        .fns = fns,
        .names = "{.col}_{.fn}"
      )
    )
}
group_summary(starwars, grpvar = c(hair_color, eye_color),
  c(height, mass), fns = list(mean = mean, med = median, min = min, max = max))
