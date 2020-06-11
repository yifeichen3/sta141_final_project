library(shiny)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(maps)
library(usmap)
library(openintro)
library(shinyWidgets)

# load the world map
world <- map_data("world")
# load the data
summary_data <- fromJSON("https://api.covid19api.com/summary")
countries <- summary_data$Countries$Country
top_10_countries <- summary_data$Countries %>%
  arrange(desc(TotalConfirmed)) %>%
  select(Country, TotalConfirmed) %>%
  top_n(10)
top_10_countries <- top_10_countries$Country
states_data <- fromJSON("https://covidtracking.com/api/states")
states_data$state <- abbr2state(states_data$state)
all_data <- read.csv("data1.csv")
all_data <- all_data %>% separate(Date, c("Date", "Unused"), sep = "T00:00:00Z")
all_data$Date <- as.Date(all_data$Date)
top_10_states <- states_data %>%
  select(state, positive) %>%
  arrange(desc(positive)) %>%
  top_n(10)

ui <- fluidPage(
  # three tabs
  tabsetPanel(
    # create a global/US map
    tabPanel(
      "Map",
      sidebarLayout(
        sidebarPanel(
          pickerInput("level3", "Level:", choices = c("Global", "US"), selected = "Global", multiple = FALSE)
        ),
        mainPanel(
          textOutput("text4"),
          tableOutput("summary"),
          plotOutput("plot3"),
          textOutput("text1"),
          verbatimTextOutput("summary1")
        )
      )
    ),
    # create a US/Country/Global hist
    tabPanel(
      "Histogram",
      sidebarLayout(
        sidebarPanel(
          pickerInput("level2", "Level:", choices = c("-", "Country", "Global", "US"), selected = "-", multiple = FALSE),
          pickerInput("state2", "State/Country:", choices = c("-"), selected = "-", multiple = TRUE, options = list("max-options" = 10, "max-options-text" = "No more than 10"))
        ),
        mainPanel(
          plotOutput("plot2"),
          textOutput("text2"),
          verbatimTextOutput("summary2"),
          tags$head(tags$style("#summary2{font-size:12px;; overflow-y:scroll; max-height: 200px; background: ghostwhite;}"))
        )
      )
    ),
    # create a US/Country/Global scatterplot
    tabPanel(
      "Scatterplot",
      sidebarLayout(
        sidebarPanel(
          pickerInput("level", "Level:", choices = c("-", "Country", "Global", "US"), selected = "-", multiple = FALSE),
          pickerInput("state", "State/Country:", choices = c("-"), selected = "-", multiple = TRUE, options = list("max-options" = 10, "max-options-text" = "No more than 10")),
          pickerInput("type", "Type:", choices = c("-"), selected = "-", multiple = FALSE),
          dateRangeInput("dateRange", label = "Date Range:", format = "yyyy/mm/dd", start = "2020-01-22", end = as.Date("2020-06-09"), min = "2020-01-22", max = as.Date("2020-06-09"), startview = "year", separator = " - ")
        ),
        mainPanel(
          plotOutput("plot1")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # update the option
  observeEvent(input$level, {
    if (input$level == "Country") {
      updatePickerInput(session, "state", "State/Country:(default top 10)", choices = c(summary_data$Countries$Country), selected = top_10_countries)
      updatePickerInput(session, "type", "Type", choices = c("Death", "Confirmed"), selected = "Confirmed")
    } else if (input$level == "US") {
      all_states <- all_data %>%
        filter(Country == "United States of America") %>%
        filter(Date == as.Date("2020-06-09")) %>%
        group_by(Province) %>%
        summarize(total_confirmed = sum(Confirmed)) %>%
        filter(Province != "")
      updatePickerInput(session, "state", "State/Country:(default top 10)", choices = c(as.character(all_states$Province)), selected = top_10_states$state)
      updatePickerInput(session, "type", "Type", choices = c("Death", "Confirmed"), selected = "Confirmed")
    } else {
      updatePickerInput(session, "state", "State/Country:", choices = c("-"), selected = "-")
      updatePickerInput(session, "type", "Type:", choices = c("-"), selected = "-")
    }
  })
  observeEvent(input$level2, {
    if (input$level2 == "Country") {
      updatePickerInput(session, "state2", "State/Country:(default top 10)", choices = c(summary_data$Countries$Country), selected = top_10_countries)
    } else if (input$level2 == "US") {
      all_states <- all_data %>%
        filter(Country == "United States of America") %>%
        filter(Date == as.Date("2020-06-09")) %>%
        group_by(Province) %>%
        summarize(total_confirmed = sum(Confirmed)) %>%
        filter(Province != "")
      updatePickerInput(session, "state2", "State/Country:(default top 10)", choices = c(as.character(all_states$Province)), selected = top_10_states$state)
    } else {
      updatePickerInput(session, "state2", "State/Country:", choices = c("-"), selected = "-")
    }
  })

  # generate a table to record: death, recover, and confirmed cases
  output$summary <- renderTable({
    level1 <- input$level3
    if (level1 == "Global") {
      total_confirmed <- summary_data$Global$TotalConfirmed
      total_deaths <- summary_data$Global$TotalDeaths
      total_recovered <- summary_data$Global$TotalRecovered
      tibble("Confirmed Cases" = total_confirmed, "Deaths" = total_deaths, "Recovered" = total_recovered)
    } else if (level1 == "US") {
      df <- data_frame(states_data) %>%
        summarize(positives = sum(positive), deaths = sum(death), recovereds = sum(recovered))
      total_confirmed <- df$positives
      total_deaths <- df$deaths
      total_recovered <- df$recovereds
      tibble("Confirmed Cases" = total_confirmed, "Deaths" = total_deaths, "Recovered" = total_recovered)
    }
  })

  # generate the scatterplot
  output$plot1 <- renderPlot({
    level1 <- input$level
    state1 <- input$state
    type1 <- input$type
    if (level1 == "Global") {
      data_without_US <- all_data %>%
        filter(Date >= as.Date(input$dateRange[1])) %>%
        filter(Date <= as.Date(input$dateRange[2])) %>%
        filter(Country != "United States of America") %>%
        group_by(Date) %>%
        summarize(total_confirmed = sum(Confirmed), total_deaths = sum(Deaths), total_recovered = sum(Recovered))
      data_US <- all_data %>%
        filter(Date >= as.Date(input$dateRange[1])) %>%
        filter(Date <= as.Date(input$dateRange[2])) %>%
        filter(Country == "United States of America") %>%
        group_by(Date) %>%
        summarize(total_confirmed = sum(Confirmed) / 2, total_deaths = sum(Deaths) / 2, total_recovered = sum(Recovered) / 2)
      data <- cbind(data_without_US[1], data_without_US[-1] + data_US[-1])
      new_data <- reshape2::melt(data, id.var = "Date")
      ggplot(new_data, aes(x = Date, y = value, col = variable)) +
        geom_point(size = 1) +
        ggtitle("Number of cases")
    } else if (level1 == "Country") {
      data_without_US <- all_data %>%
        filter(Date >= as.Date(input$dateRange[1])) %>%
        filter(Date <= as.Date(input$dateRange[2])) %>%
        filter(Country != "United States of America") %>%
        group_by(Date, Country) %>%
        summarize(total_confirmed = sum(Confirmed), total_deaths = sum(Deaths), total_recovered = sum(Recovered))
      data_US <- all_data %>%
        filter(Date >= as.Date(input$dateRange[1])) %>%
        filter(Date <= as.Date(input$dateRange[2])) %>%
        filter(Country == "United States of America") %>%
        group_by(Date, Country) %>%
        summarize(total_confirmed = sum(Confirmed) / 2, total_deaths = sum(Deaths) / 2, total_recovered = sum(Recovered) / 2)
      data <- rbind(data_US, data_without_US)
      data <- data[order(as.Date(data$Date)), ] %>% filter(Country %in% state1)
      if (type1 == "Confirmed") {
        ggplot() +
          geom_point(data = data, aes(x = Date, y = total_confirmed, color = as.character(Country)), size = 1) +
          theme(legend.title = element_blank()) +
          ylab("Confirmed cases") +
          ggtitle("Number of cases")
      } else if (type1 == "Death") {
        ggplot() +
          geom_point(data = data, aes(x = Date, y = total_deaths, color = as.character(Country)), size = 1) +
          theme(legend.title = element_blank()) +
          ylab("Deaths cases") +
          ggtitle("Number of cases")
      }
    } else if (level1 == "US") {
      data <- all_data %>%
        filter(Date >= as.Date(input$dateRange[1])) %>%
        filter(Date <= as.Date(input$dateRange[2])) %>%
        filter(Country == "United States of America") %>%
        group_by(Province, Date) %>%
        filter(Province != "") %>%
        summarize(total_confirmed = sum(Confirmed), total_deaths = sum(Deaths), total_recovered = sum(Recovered))
      data <- data[order(as.Date(data$Date)), ] %>% filter(Province %in% state1)
      if (type1 == "Confirmed") {
        ggplot() +
          geom_point(data = data, aes(x = Date, y = total_confirmed, color = as.character(Province)), size = 1) +
          theme(legend.title = element_blank()) +
          ylab("Confirmed cases") +
          ggtitle("Number of cases")
      } else if (type1 == "Death") {
        ggplot() +
          geom_point(data = data, aes(x = Date, y = total_deaths, color = as.character(Province)), size = 1) +
          theme(legend.title = element_blank()) +
          ylab("Deaths cases") +
          ggtitle("Number of cases")
      }
    }
  })

  # generate the histogram
  output$plot2 <- renderPlot({
    level1 <- input$level2
    state1 <- input$state2
    if (level1 == "Global") {
      data <- data.frame(
        name = c("Total Confirmed", "Total Deaths", "Total Recovered"),
        value = c(summary_data$Global$TotalConfirmed, summary_data$Global$TotalDeaths, summary_data$Global$TotalRecovered)
      )
      ggplot(data, aes(x = name, y = value, fill = name)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        ggtitle("Number of cases")
    } else if (level1 == "Country") {
      data_without_US <- all_data %>%
        filter(Country != "United States of America") %>%
        filter(Date == as.Date("2020-06-09")) %>%
        group_by(Country) %>%
        summarize(total_confirmed = sum(Confirmed), total_deaths = sum(Deaths), total_recovered = sum(Recovered))
      data_US <- all_data %>%
        filter(Country == "United States of America") %>%
        filter(Date == as.Date("2020-06-09")) %>%
        group_by(Country) %>%
        summarize(total_confirmed = sum(Confirmed) / 2, total_deaths = sum(Deaths) / 2, total_recovered = sum(Recovered) / 2)
      data <- rbind(data_US, data_without_US)
      data <- data %>% filter(Country %in% state1)
      new_data <- reshape2::melt(data, id.var = "Country")
      ggplot(new_data, aes(fill = variable, y = value, x = Country)) +
        geom_bar(position = "fill", stat = "identity") +
        ylab("Percentage") +
        xlab("Countries") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle("Percentage of cases")
    } else if (level1 == "US") {
      data <- states_data %>%
        select(state, positive, recovered, death)
      data <- data %>% filter(state %in% state1)
      new_data <- reshape2::melt(data, id.var = "state")
      ggplot(new_data, aes(fill = variable, y = value, x = state)) +
        geom_bar(position = "fill", stat = "identity") +
        ylab("Percentage") +
        xlab("States") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        ggtitle("Percentage of cases")
    }
  })

  # generate the map
  output$plot3 <- renderPlot({
    level1 <- input$level3
    if (level1 == "Global") {
      data <- all_data %>%
        filter(Date == as.Date("2020-06-09"))
      ggplot() +
        geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.5) +
        geom_point(data = data, aes(x = Lon, y = Lat, color = Confirmed, size = Confirmed), alpha = 0.7) +
        scale_size_continuous(name = "Cases", trans = "log", range = c(0.4, 5), breaks = c(1, 20, 100, 1000, 50000), labels = c("1-19", "20-99", "100-999", "1,000-49,999", "50,000+")) +
        scale_color_viridis_c(option = "cividis", direction = -1, name = "Cases", trans = "log", breaks = c(1, 20, 100, 1000, 50000), labels = c("1-19", "20-99", "100-999", "1,000-49,999", "50,000+")) +
        theme_void() +
        guides(colour = guide_legend()) +
        theme(legend.position = "bottom") +
        ggtitle("Number of confirmed cases")
    } else if (level1 == "US") {
      data <- fromJSON("https://covidtracking.com/api/states")
      plot_usmap(data = states_data, values = "positive", color = "red") +
        scale_fill_continuous(
          low = "white", high = "red", name = "Confirmed cases", label = scales::comma
        ) + theme(legend.position = "right") + ggtitle("Number of confirmed cases")
    }
  })

  output$text1 <- renderText({
    level1 <- input$level3
    if (level1 != "-") {
      c <- "Summary Statistics"
    }
  })
  
  output$text4 <- renderText({
    c <- "Note: Due to memory issue, I saved one of the JSON file to csv, so the last day will be 2020-06-09"
  })

  output$text2 <- renderText({
    level1 <- input$level2
    if (level1 != "-") {
      c <- "Note: Some of the recovered cases are missing, which will be indicated to 0"
    }
  })

  # summary data
  output$summary1 <- renderPrint({
    level1 <- input$level3
    if (level1 == "Global") {
      data_without_US <- all_data %>%
        filter(Country != "United States of America") %>%
        group_by(Date) %>%
        summarize(.groups = "drop", total_confirmed = sum(Confirmed), total_deaths = sum(Deaths), total_recovered = sum(Recovered))
      data_US <- all_data %>%
        filter(Country == "United States of America") %>%
        group_by(Date) %>%
        summarize(.groups = "drop", total_confirmed = sum(Confirmed) / 2, total_deaths = sum(Deaths) / 2, total_recovered = sum(Recovered) / 2)
      data <- cbind(data_without_US[1], data_without_US[-1] + data_US[-1])
      cols <- c("total_confirmed", "total_deaths", "total_recovered")
      summary(data[cols])
    } else if (level1 == "US") {
      options(warn = -1)
      data <- all_data %>%
        filter(Country == "United States of America") %>%
        group_by(Date) %>%
        summarize(.groups = "drop", total_confirmed = sum(Confirmed) / 2, total_deaths = sum(Deaths) / 2, total_recovered = sum(Recovered) / 2)
      cols <- c("total_confirmed", "total_deaths", "total_recovered")
      summary(data[cols])
    }
  })

  # summary data
  output$summary2 <- renderPrint({
    level1 <- input$level2
    state1 <- input$state2
    if (level1 == "Global") {
      data_without_US <- all_data %>%
        filter(Country != "United States of America") %>%
        group_by(Date) %>%
        summarize(total_confirmed = sum(Confirmed), total_deaths = sum(Deaths), total_recovered = sum(Recovered))
      data_US <- all_data %>%
        filter(Country == "United States of America") %>%
        group_by(Date) %>%
        summarize(.groups = "drop", total_confirmed = sum(Confirmed) / 2, total_deaths = sum(Deaths) / 2, total_recovered = sum(Recovered) / 2)
      data <- cbind(data_without_US[1], data_without_US[-1] + data_US[-1])
      cols <- c("total_confirmed", "total_deaths", "total_recovered")
      summary(data[cols])
    } else if (level1 == "US") {
      List <- list()
      for (val in state1) {
        data <- all_data %>%
          filter(Country == "United States of America") %>%
          filter(Province == val) %>%
          group_by(Date) %>%
          summarize(.groups = "drop", total_confirmed = sum(Confirmed), total_deaths = sum(Deaths), total_recovered = sum(Recovered))
        cols <- c("total_confirmed", "total_deaths", "total_recovered")
        List[[val]] <- summary(data[cols])
      }
      print(List)
    } else if (level1 == "Country") {
      List <- list()
      for (val in state1) {
        data <- all_data %>%
          filter(Country == val) %>%
          group_by(Date) %>%
          summarize(.groups = "drop", total_confirmed = sum(Confirmed), total_deaths = sum(Deaths), total_recovered = sum(Recovered))
        cols <- c("total_confirmed", "total_deaths", "total_recovered")
        List[[val]] <- summary(data[cols])
      }
      print(List)
    }
  })
}


shinyApp(ui, server)
