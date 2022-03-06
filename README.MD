---
title: "Time series of COVID-19 Case Fatality Ratios (CFR) in Canada"
output: github_document
---
                                                                                                         
   Written by: Jessica Yu, 2021.                                                                         
               j.yu@ubc.ca                                                                               
                                                                                                         
   Data from: COVID-19 Canada Open Data Working Group: https://opencovid.ca/api/                         
                                                                                                         
   CFR definition from: Dictionary of Epidemiology                                                       
                                                                                                         
   **Outline**                                                                                               
             (A) Load and keep data of interest                                                          
             (B) Format and clean data                                                                   
             (C) Compute CFRs                                                                            
             (D) Visualise CFRs with interactive time series                                             
             (E) Visualise CFRs with testing and vaccinations                                            
                                                                                                       

### Install necessary packages if they are not already installed

```{r install packages}
list.of.packages <- c("httr","rjson","dplyr", "dygraphs", "xts", "lubridate", "ggplot2", "hrbrthemes", "knitr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
```


```{r load packages, include=FALSE}
lapply(list.of.packages, require, character.only=T)
```


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Load data from COVID-19 Canada Open Data Working Group dataset

```{r collect data}
data_api <- GET("http://api.opencovid.ca/timeseries?loc=canada")
data <- fromJSON(rawToChar(data_api$content)) # pull data
```

### Extract data of interest and compute tCFR (total case fatality ratio), cCFR (complete case fatality ratio), and mCFR (mean case fatality ratio)

```{r prepare data, message=FALSE}

data_active <- lapply(data$active, function(x) { # dataset with cases, mortality, recovered
  data.frame(x) %>%     
    mutate(
         tCFR=cumulative_deaths/cumulative_cases, cCFR=cumulative_deaths/(cumulative_deaths+cumulative_recovered)) %>%
    mutate(mCFR=(tCFR+cCFR)/2) %>%
    rename(date=date_active) %>%
    replace(is.na(.), 0) %>% # replace NA values with 0 for cCFR early in the pandemic 
    select(date, tCFR, cCFR, mCFR) # keep variables of interest
})
data_active<-as.data.frame(do.call(rbind, data_active)) 

data_vaccine <- lapply(data$avaccine, function(x) { # dataset with administered vaccinations
  data.frame(x) %>%
    select(date_vaccine_administered, cumulative_avaccine) %>%
    rename(date=date_vaccine_administered)
})
data_vaccine<-as.data.frame(do.call(rbind, data_vaccine)) 

data_testing <- lapply(data$testing, function(x) { #dataset with cumulative tests
  data.frame(x) %>%
    select(date_testing, cumulative_testing) %>%
    rename(date=date_testing)
})
data_testing<-as.data.frame(do.call(rbind, data_testing)) 

final_data <- left_join(data_active, data_vaccine) %>% # merge all data and replace NA with 0
  left_join(data_testing) %>%
  replace(is.na(.), 0)

final_data$date<- dmy(final_data$date) # convert date format for plotting
```


### Plot simple time series of tCFR, cCFR, and mCFR with key dates highlighted

```{r time series plot, echo=FALSE}

p_data <- xts(x=final_data[,2:4], order.by = final_data$date) # only plot CFRs

# Include some key dates for context

p <- dygraph(p_data, main = "Case Fatality Ratios in Canada (January 2020-July 2021)") %>% 
  dyEvent("2020-01-27", "First confirmed case in Canada") %>%
  dyEvent("2020-03-08", "Canada records first death from COVID-19") %>%
  dyEvent("2020-03-11", "WHO declares COVID-19 a pandemic") %>%
  dyEvent("2020-03-25", "Mandatory quarantine for all travelers arriving in the country") %>%
  dyEvent("2020-05-04", "Restrictions begin to lift in several provinces") %>%
  dyEvent("2020-05-20", "Masks are recommended in public spaces") %>%
  dyEvent("2020-09-23", "Ottawa declares second wave is underway") %>%
  dyEvent("2020-12-14", "First covid vaccines are administered") %>%
  dyRangeSelector()
p
```

### Plot CFRs and COVID-19 testing with dual Y axis
Be careful of interpretation of cumulative covid testing given that people can be tested multiple times.

```{r ggplot, echo=FALSE}

# Restricting data for only variables of interest
p_data2 <- final_data %>%
  select(date,mCFR,cumulative_avaccine,cumulative_testing) # Used mCFR but other CFRs can be used too

pop <- 38008005		 # Factor by Q2 population estimate from Statistics Canada to allow visual comparability with CFR

ggplot(p_data2, aes(x=date)) +
  
  geom_bar( aes(y=cumulative_testing / pop), stat="identity", size=.1, fill="#69b3a2", color="black", alpha=.4) + 
  geom_line( aes(y=mCFR), size=2, color="#3399E6") +
  
  scale_y_continuous(
    
    # Features of the line plot
    name = "Mean Case Fatality Ratio",
    
    # Add a second axis for testing
    sec.axis = sec_axis(~., name="Ratio of Cumulative Tests per Total Population")
  ) + 
  
  theme_ipsum() +

  theme(
    axis.title.y = element_text(color = "#3399E6", size=13),
    axis.title.y.right = element_text(color = "#69b3a2", size=13)
  ) +

  ggtitle("Case fatality ratios and cumulative COVID-19 testing")

```


### Plot CFRs and COVID-19 administered vaccines with dual Y axis (log scale)

```{r ggplot logged, echo=FALSE}

ggplot(p_data2, aes(x=date)) +
  
  geom_line( aes(y=log10(mCFR)), color="#3399E6") + 
  geom_line( aes(y=log10(cumulative_avaccine)), color="#69b3a2") + 
  
  scale_y_continuous(
    
    # First axis
    name = "Mean Case Fatality Ratio (log10)",
    
    # Second axis
    sec.axis = sec_axis(~., name="Cumulative Administered Vaccines (log10)")
  ) +
  
  theme_ipsum() +

  theme(
    axis.title.y = element_text(color = "#3399E6", size=13),
    axis.title.y.right = element_text(color = "#69b3a2", size=13)
  ) +

  ggtitle("Case fatality ratios and COVID-19 administered vaccines")
```
