library(tidyverse)

## Jupyter notebooks use the repr package to create viewable representations
## of R objects (https://github.com/IRkernel/repr). I am updating the default
## plot dimensions to 12 x 6.
options(repr.plot.width = 12, repr.plot.height = 6)

## We will use ggplot2 for all plots. I am defining a custom theme here
## that mainly updates the backgrounds and legend position. We set this
## custom theme as the default, and also update the default for line size.
theme_custom <- function(base_size, ...){
   ggplot2::theme_gray(base_size = base_size, ...) +
      ggplot2::theme(
         plot.title = element_text(face = 'bold'),
         plot.subtitle = element_text(color = '#333333'),
         panel.background = element_rect(fill = "#EBF4F7"),
         strip.background = element_rect(fill = "#33AACC"),
         legend.position = "bottom"
      )
}
ggplot2::theme_set(theme_custom(base_size = 20))
ggplot2::update_geom_defaults("line", list(size = 1.5))

# Utility functions

## We will use a utility function to display the head of dataframes.
## Note that we need this hack mainly to add the class 'dataframe' to
## the tables that are printed. This should ideally be handled
## by the `repr` package, and I will be sending a PR.
display_df <- function(x){
   d <- as.character(
      knitr::kable(x, format = 'html', table.attr = "class='dataframe'")
   )
   #IRdisplay::display_html(d)
}

display_head <- function(x, n = 6){
   head(x, n)
}

display_random <- function(x, n = 6){
   display_df(dplyr::sample_n(x, n))
}


#######################################################################################
## poisson distribution
#######################################################################################
# Number of new cases observed in a day
k = 0:69

# Arrival rate of new infections per day
lambda = c(10, 20, 30, 40)

poisson_densities = crossing(lambda = lambda, k = k) %>%
   mutate(p = dpois(k, lambda))

display_head(poisson_densities)

poisson_densities %>%
   # We convert lambda to a factor so that each line gets a discrete color
   mutate(lambda = factor(lambda)) %>%
   ggplot(aes(x = k, y = p, color = lambda)) +
   geom_line() +
   labs(
      title = expression(paste("Probability of k new cases P(k|", lambda, ")")),
      x = 'Number of new cases',
      y = NULL,
      color = expression(lambda)
   )


#######################################################################################
## likelihood
#######################################################################################
