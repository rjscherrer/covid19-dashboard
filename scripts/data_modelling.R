library(smoother)
library(zoo)
library(HDInterval)
library(tidyverse)
library(ggforce)

# load cleaned data into main memory
load("./data/data_cleaned.RData")


#######################################################################################
#> Chance Plot Theme
#> 
#> Changing ggplot theme to create nicer plots.
#######################################################################################
theme_custom <- function(base_size, ...) {
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


#######################################################################################
#> Remove Reporting Lags
#> 
#> The removing of reporting lags has to be done as some countries (e.g. Switzerland) 
#> did not report any data about new cases over the weekends. Therefore, the number of
#> new cases is 0 on weekends and too high on Mondays.
#######################################################################################
# plot number of new cases before removing reporting lags
data_cases$Switzerland %>%
   ggplot(aes(x = Date, y = Cases.New)) +
   geom_line(aes(y = Cases.New), color = "#14243e") +
   labs(
      title = "New Cases Per Day (With Reporting Lags)",
      subtitle = "Switzerland",
      x = NULL, y = NULL
   )

# remove reporting lags
for (i in c(1:length(data_cases))) {
   cases_new <- data_cases[[i]]$Cases.New
   cases_new_nolag <- c()
   
   for (ii in c(length(cases_new):2)) {
      if (cases_new[ii-1] > 0) {
         cases_new_nolag <- c(cases_new_nolag, cases_new[ii])
      } else {
         iii <- ii
         while (cases_new[iii-1] <= 0 & (iii-1) >= 2) {
            iii <- iii-1
         }
         
         replace_value <- floor(cases_new[ii] / (ii-iii+1))
         
         if (replace_value > 0) {
            cases_new_nolag <- c(cases_new_nolag, replace_value)
            for (iv in ii:iii) {
               cases_new[iv] <- replace_value
            }
         } else {
            cases_new_nolag <- c(cases_new_nolag, max(0, replace_value))
         }
      }
   }
   cases_new_nolag <- c(cases_new_nolag, cases_new[1])
   
   data_cases[[i]] <- add_column(data_cases[[i]], Cases.New.NoLag=rev(cases_new_nolag))
}

data_cases$Switzerland %>%
   ggplot(aes(x = Date, y = Cases.New.NoLag)) +
   geom_line(aes(y = Cases.New.NoLag), color = "#14243e") +
   labs(
      title = "New Cases Per Day (With Reporting Lags)",
      subtitle = "Switzerland",
      x = NULL, y = NULL
   )


#######################################################################################
#> Smoothing New Cases
#> 
#> The following function is used for smoothing the time series of new cases. To smooth 
#> the time series, a Gaussian smoother with a 7 day rolling window is used.
#######################################################################################
ts_smoothed <- function(time_series) {
   ts_smoothed <- round(smoother::smth(time_series, 
                                       # weighting values according to a gaussian.
                                       # near points are weighted higher than farther
                                       # points
                                       method = "gaussian",
                                       # average over seven data points
                                       window = 7, 
                                       # include tails of the gaussian
                                       tails = TRUE), digits = 0)
   
   return(ts_smoothed)
}

# add column containing smoothed new cases
for (i in c(1:length(data_cases))) {
   cases_new_smoothed <- ts_smoothed(time_series = data_cases[[i]]$Cases.New.NoLag)
   data_cases[[i]] <- add_column(data_cases[[i]], 
                                 Cases.New.Smoothed=cases_new_smoothed)
}

# plot example for sense making
data_cases$Switzerland %>%
   ggplot(aes(x = Date, y = Cases.New)) +
   geom_line(linetype = 'dotted', color = 'gray40') +
   geom_line(aes(y = Cases.New.Smoothed), color = "#14243e") +
   labs(
      title = "New cases per day",
      subtitle = "Switzerland",
      x = NULL, y = NULL
   )


#######################################################################################
#> Computing Likelihoods
#> 
#> In the following step the log-likelihoods are computed. The log was chosen to make
#> it easier to smooth over a rolling window of only using the latest m intervals to 
#> compute Rt.
#######################################################################################
R_T_MAX <- 12
# genearting sequence of possible r values
R_T_RANGE <-  seq(0, R_T_MAX, length = R_T_MAX*100 + 1)
GAMMA <- 1/4

add_likelihood <- function(cases) {
   likelihood <- cases %>%
      mutate(
         r_t = list(R_T_RANGE),
         # see equation slide 5, bullet point 3
         lambda = map(lag(Cases.New.Smoothed, 1), ~ .x * exp(GAMMA * (R_T_RANGE - 1))),
         # see equation slide 5, bullet point 3
         # computing log likelihoods instead of likelihoods for easier application of
         # the rolling window approach proposed by (Systrom, 2020)
         likelihood_r_t = map2(Cases.New.Smoothed, lambda, dpois, log = TRUE)
      ) %>%
      slice(-1) %>%
      select(-lambda) %>%
      unnest(c(likelihood_r_t, r_t))
}

# add columns related to likelihood
for (i in c(1:length(data_cases))) {
   data_cases[[i]] <- add_likelihood(data_cases[[i]])
}


#######################################################################################
#> Computing Posteriors
#> 
#> To calculate the posterior probabilities, a rolling 7 day sum of the log
#> likelihoods is computed which is then exponentiated. Finally, the posteriors are 
#> getting normalised to 1
#######################################################################################
add_posterior <- function(cases) {
   cases %>%
      arrange(Date) %>%
      group_by(r_t) %>%
      mutate(posterior = exp(
         # see equation on slide 6, bullet point 6 with m=7
         zoo::rollapplyr(likelihood_r_t, 7, sum, partial = TRUE)
      )) %>%
      group_by(Date) %>%
      mutate(posterior = posterior / sum(posterior, na.rm = TRUE)) %>%
      # remove NA to prevent problems later on
      mutate(posterior = ifelse(is.nan(posterior), 0, posterior)) %>%
      ungroup() %>%
      select(-likelihood_r_t)
}

# add column with posterior probability
for (i in c(1:length(data_cases))) {
   data_cases[[i]] <- add_posterior(data_cases[[i]])
}


#######################################################################################
#> Estimate Rt
#> 
#> In this final step, the the Rt values and the 95% highest density intervals around 
#> them are estimated.
#######################################################################################
add_estimated_rt <- function(cases) {
   cases %>%
      group_by(Country.Region, Date, Cases.Total, Cases.New, 
               Cases.New.NoLag, Cases.New.Smoothed) %>%
      summarize(
         # see equation on slide 6, bullet point 4
         r_t_simulated = list(sample(R_T_RANGE, 
                                     10000, 
                                     replace = TRUE, 
                                     prob = if (max(posterior)==0) 
                                        rep(.Machine$double.xmin, 
                                            length(posterior)) else posterior)),
         Rt.Most.Likely = R_T_RANGE[which.max(posterior)]
      ) %>%
      mutate(
         Rt.Low = map_dbl(r_t_simulated, ~ hdi(.x)[1]),
         Rt.High = map_dbl(r_t_simulated, ~ hdi(.x)[2])
      ) %>%
      select(-r_t_simulated)
}


# add column with estimated rt
for (i in c(1:length(data_cases))) {
   data_cases[[i]] <- add_estimated_rt(data_cases[[i]])
}


#######################################################################################
#> Visualise Estimated Rt
#> 
#> Now, the estimated Rt can be visualised.
#######################################################################################
# plot single example
data_cases$Switzerland %>%
   ggplot(aes(x = Date, y = Rt.Most.Likely)) +
   geom_line(color = "#14243e") +
   geom_hline(yintercept = 1, linetype = 'dashed') +
   geom_ribbon(
      aes(ymin = Rt.Low, ymax = Rt.High),
      fill = 'darkred',
      alpha = 0.2
   ) +
   labs(
      title = expression('Real time R'[t]), x = '', y = '',
      subtitle = "Switzerland"
   ) +
   coord_cartesian(ylim = c(0, 4))

# concateante list
data_plot <- map_dfr(data_cases, bind_rows)    

# create facet plot
data_plot %>%
   ggplot(aes(x = Date, y = Rt.Most.Likely)) +
   geom_line(color = "#14243e") +
   geom_hline(yintercept = 1, linetype = 'dashed') +
   geom_ribbon(
      aes(ymin = Rt.Low, ymax = Rt.High),
      fill = 'darkred',
      alpha = 0.2
   ) +
   labs(
      title = expression('Real time R'[t]), x = '', y = ''
   ) +
   coord_cartesian(ylim = c(0, 4)) +
   facet_wrap_paginate(~Country.Region, nrow=3, ncol=3, page = 5)


#######################################################################################
#> Add Doubeling Time of New Cases
#> 
#> The time until the number of new cases doubles is estimated based on on the Rt
#> value that estimated before.
#######################################################################################
for (i in c(1:length(data_cases))) {
   doubling_time <- log(2) / log(data_cases[[i]]$Rt.Most.Likely)
   doubling_time[doubling_time <= 0 | doubling_time == Inf] <- NA
      
   
   data_cases[[i]] <- add_column(data_cases[[i]], Doubling.Time = doubling_time)
}

# save output to disk
saveRDS(data_countries, file = "./data/data_countries.RDS")
saveRDS(data_cases, file = "./data/data_cases.RDS")
