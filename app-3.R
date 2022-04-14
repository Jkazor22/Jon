library(shiny)
library(tidyverse)
library(dplyr)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>% 
  separate(state, into = c("dot","state"), extra = "merge") %>% 
  select(-dot) %>% 
  mutate(state = str_to_lower(state))

daily_case <- covid19 %>% 
  group_by(state, date) %>% 
  summarise(case_count_total = sum(cases)) %>% 
  mutate(casecount_1day = lag(case_count_total, n=1)) %>% 
  replace_na(list(casecount_1day = 0)) %>% 
  mutate(newcases_day = (case_count_total - casecount_1day))

covid_by_pop <- daily_case %>% 
  mutate(state = str_to_lower(state)) %>% 
  left_join(census_pop_est_2018,
            by = "state") %>% 
  mutate(per100000 = (newcases_day/est_pop_2018)*100000)


ui <- fluidPage (selectInput(inputId = "state",
                           label = "state",
                           choices = unique(covid_by_pop$state),
                           multiple = TRUE
                           ),
                 sliderInput(inputId = "date",
                             label = "date",
                             min = as.Date(min(covid_by_pop$date)),
                             max = as.Date(max(covid_by_pop$date)),
                             value = c(as.Date(min(covid_by_pop$date)), as.Date(max(covid_by_pop$date))),
                             ),
                 submitButton(text = "submit",
                              icon = ""),
                 plotOutput(outputId = "timeplot"))
server <- function(input, output) {
  output$timeplot <- renderPlot(
    covid_by_pop %>% 
      filter(state %in% input$state) %>% 
      ggplot(aes(x = date, 
                 y = per100000,
                 color = state)) +
      geom_line() +
      labs(title = "Daily number of COVID cases per 100,000 people, by state, over time",
           x = "month, year; year, month",
           y = "Daily number of Covid cases per 100,000 people") + 
      scale_x_date(limits = input$date)
  )
}
shinyApp(ui = ui, server = server)