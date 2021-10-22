## Dev Survey Shiny App ##

# DEPLOY
# rsconnect::deployApp('dashboard.R') # nolint
# RUN
# shiny::runApp('app/dashboard.R') # nolint

library(shiny)
library(shinydashboard)
library(treemapify)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(highcharter)
library(ggrepel)
library(scales)
library(plotly)
library(cowplot)
library(knitr)
library(ggalluvial)

dev <- read.csv("cleaned_dev_survey.csv")

ui <- dashboardPage(
  dashboardHeader(
    title = "Dev Survey Shiny App",
    titleWidth = 260
  ),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      menuItem("Gender / Sexual Orientation by Country", tabName = "gender_sex_country"),
      menuItem("Employment / Age by Country", tabName = "gender_edu_age"),
      menuItem("Student Analysis by Country", tabName = "student_analysis_country"),
      menuItem("Salary by Gender", tabName = "salary_gender"),
      menuItem("Salary by Gender Country Comparison", tabName = "salary_gender_country_comparison"),
      menuItem("Coding Years vs Satisfaction", tabName = "code_year_satisfy"),
      menuItem("Satisfied Long Term Coders", tabName = "long_term_coders"),
      menuItem("Salary Considerations", tabName = "salary_considerations")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "gender_sex_country",
        fluidPage( # Page 1
          fluidRow(
            h3("Gender / Sexual Orientation by Country", align = "center"),
            br(),
            column(
              width = 4,
              offset = 4,
              align = "center",
              box(
                width = 12,
                title = "Country",
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                  inputId = "choose_country",
                  label = "",
                  choices = unique(dev$country),
                  selected = "United States"
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              box(
                width = 12,
                plotOutput("gender")
              )
            ),
            column(
              width = 6,
              box(
                width = 12,
                plotOutput("sex_orient")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "gender_edu_age",
        fluidPage( # Page 2
          fluidRow(
            column(
              width = 3,
              align = "center",
              box(
                width = 12,
                title = "Country",
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                  inputId = "user_choice",
                  label = "",
                  choices = unique(dev$country),
                  selected = "United States"
                )
              )
            ),
            column(
              width = 4,
              offset = 1,
              align = "center",
              h2("Employment / Age by Country")
            ),
            column(
              width = 3,
              offset = 1,
              align = "center",
              box(
                width = 12,
                title = "Category",
                status = "primary",
                solidHeader = TRUE,
                radioButtons(
                  inputId = "rb_choice",
                  label = "",
                  choices = c(
                    "Employment Status",
                    "Student Status",
                    "Age Groups"
                  )
                )
              )
            )
          ),
          fluidRow(
            plotOutput("plot1", height = 350)
          ),
          br(),
          fluidRow(
            column(
              width = 8,
              offset = 2,
              plotOutput("plot4", height = 300)
            )
          )
        )
      ),
      tabItem(
        tabName = "student_analysis_country",
        fluidPage( # Page 3
          fluidRow(
            column(
              width = 4,
              offset = 4,
              align = "center",
              box(
                width = 12,
                title = "Student Analysis by Country",
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                  inputId = "choice2",
                  label = "",
                  choices = unique(dev$country),
                  selected = "United States"
                )
              )
            )
          ),
          fluidRow(
            plotOutput("plot2", height = 300)
          ),
          br(),
          fluidRow(
            plotOutput("undergraduate", height = 350)
          )
        )
      ),
      tabItem(
        tabName = "salary_gender",
        fluidPage( # Page 4
          fluidRow(
            plotOutput("salary_top_5")
          ),
          fluidRow(
            plotOutput("salary_equal_1")
          )
        )
      ),
      tabItem(
        tabName = "salary_gender_country_comparison",
        fluidPage( # Page 5
          fluidRow(
            h3("Salary by Gender Across Countries", align = "center")
          ),
          fluidRow(
            column(
              width = 2,
              box(
                width = 12,
                status = "primary",
                checkboxGroupInput(
                  inputId = "salary_country_mf_choice_1",
                  label = "Gender",
                  choices = list("Male", "Female"),
                  selected = list("Male", "Female")
                )
              )
            ),
            column(
              width = 3,
              box(
                width = 12,
                title = "Country for Comparison",
                status = "primary",
                selectInput(
                  inputId = "salary_country_choice_1",
                  label = "",
                  choices = unique(dev$country),
                  selected = "United States"
                )
              )
            ),
            column(
              width = 3,
              offset = 2,
              box(
                width = 12,
                title = "Country for Comparison",
                status = "primary",
                selectInput(
                  inputId = "salary_country_choice_2",
                  label = "",
                  choices = unique(dev$country),
                  selected = "United Kingdom"
                )
              )
            ),
            column(
              width = 2,
              box(
                width = 12,
                status = "primary",
                checkboxGroupInput(
                  inputId = "salary_country_mf_choice_2",
                  label = "Gender",
                  choices = list("Male", "Female"),
                  selected = list("Male", "Female")
                )
              )
            )
          ),
          fluidRow(
            plotOutput("salary_country_compare_1", height = 300),
            br(),
            plotOutput("salary_country_compare_2", height = 300)
          )
        )
      ),
      tabItem(
        tabName = "code_year_satisfy",
        fluidPage( # Page 6
          fluidRow(
            h3("Years Coding vs Job and Career Satisfaction", align = "center")
          ),
          br(),
          fluidRow(
            column(
              width = 2,
              offset = 1,
              box(
                width = 12,
                status = "primary",
                radioButtons(
                  inputId = "job_color_choice",
                  label = "Job Chart Color",
                  choices = c("Gray", "Red", "Blue", "Gold"),
                  selected = "Gray"
                )
              )
            ),
            column(
              width = 4,
              offset = 1,
              box(
                width = 12,
                title = "Years Coding Professionally",
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                  inputId = "coding_prof",
                  label = "",
                  choices = c(
                    "0-2 years", "3-5 years", "6-8 years",
                    "9-11 years", "12-14 years", "15-17 years",
                    "18-20 years", "21-23 years", "24-26 years",
                    "27-29 years", "30 or more years"
                  )
                )
              )
            ),
            column(
              width = 2,
              offset = 1,
              box(
                width = 12,
                status = "primary",
                radioButtons(
                  inputId = "career_color_choice",
                  label = "Career Chart Color",
                  choices = c("Gray", "Red", "Blue", "Gold"),
                  selected = "Red"
                )
              )
            )
          ),
          fluidRow(
            plotOutput("job_satisfy"),
            br(),
            plotOutput("career_satisfy")
          )
        )
      ),
      tabItem(
        tabName = "long_term_coders",
        fluidPage( # Page 7
          fluidRow(
            h3("Satisfaction Level by Years Coding", align = "center")
          ),
          br(),
          fluidRow(
            column(
              width = 4,
              box(
                width = 12,
                align = "center",
                sliderInput(
                  inputId = "job_opacity_input",
                  label = "Job Satisfaction Opacity (Blue)",
                  min = 0, max = 1, value = .6
                )
              )
            ),
            column(
              width = 4,
              box(
                width = 12,
                title = "Satisfaction Level",
                align = "center",
                status = "primary",
                solidHeader = TRUE,
                selectInput(
                  inputId = "satisfaction_level_input",
                  label = "",
                  choices = c(
                    "Extremely dissatisfied",
                    "Moderately dissatisfied",
                    "Slightly dissatisfied",
                    "Neither satisfied nor dissatisfied",
                    "Slightly satisfied",
                    "Moderately satisfied",
                    "Extremely satisfied"
                  ),
                  selected = "Neither satisfied nor dissatisfied"
                )
              )
            ),
            column(
              width = 4,
              box(
                width = 12,
                align = "center",
                sliderInput(
                  inputId = "career_opacity_input",
                  label = "Career Satisfaction Opacity (Gold)",
                  min = 0, max = 1, value = .6
                )
              )
            )
          ),
          br(),
          fluidRow(
            plotOutput("satisfaction_level_plot", height = 600)
          )
        )
      ),
      tabItem(
        tabName = "salary_considerations",
        fluidPage( # Page 8
          fluidRow(
            h3("Annual Salary by Years of Professional Coding Experience", align = "center")
          ),
          fluidRow(
            plotOutput("coding_vs_salary")
          ),
          br(),
          fluidRow(
            h3("Average Salaries per Satisfaction Level", align = "center")
          ),
          fluidRow(
            plotOutput("salary_satisfaction")
          )
        )
      ) # tabItem
    ) # tabItems
  ) # dashboardBody
) # dashboardPage

server <- function(input, output, session) {

  # Theme

  my_theme_sizes <- theme_igray() +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14)
    )

  gender_fill_colors <- scale_fill_manual(
    values = c(
      "Male" = "royalblue4",
      "Female" = "darkred",
      "Non-binary, genderqueer, or gender non-conforming" = "gray35",
      "Transgender" = "orange2"
    )
  )

  mf_fill_colors <- scale_fill_manual(
    values = c(
      "Male" = "royalblue4",
      "Female" = "darkred"
    )
  )

  sex_fill_colors <- scale_fill_manual(
    values = c(
      "Straight or heterosexual" = "gray35",
      "Bisexual or Queer" = "darkred",
      "Gay or Lesbian" = "royalblue4",
      "Asexual" = "orange2"
    )
  )

  student_fill_colors <- scale_fill_manual(
    values = c(
      "No" = "royalblue4",
      "Yes, part-time" = "darkred",
      "Yes, full-time" = "orange2"
    )
  )

  # Page 1 - Gender / Sexual Orientation by Country

  country_choice_gender <- reactive({
    dev %>%
      filter(!is.na(gender)) %>%
      filter(!is.na(country)) %>%
      select(gender, country) %>%
      mutate(gender = str_split(gender, pattern = ";")) %>%
      unnest(gender) %>%
      group_by(country, gender) %>%
      summarize(Count = n()) %>%
      arrange(desc(Count)) %>%
      ungroup() %>%
      mutate(gender = reorder(gender, Count)) %>%
      filter(country == input$choose_country)
  })

  country_choice_sex <- reactive({
    dev %>%
      filter(!is.na(sexual_orientation)) %>%
      filter(!is.na(country)) %>%
      select(sexual_orientation, country) %>%
      mutate(sexual_orientation = str_split(sexual_orientation, pattern = ";")) %>%
      unnest(sexual_orientation) %>%
      group_by(country, sexual_orientation) %>%
      summarize(Count = n()) %>%
      arrange(desc(Count)) %>%
      ungroup() %>%
      mutate(sexual_orientation = reorder(sexual_orientation, Count)) %>%
      filter(country == input$choose_country)
  })

  output$gender <- renderPlot({
    ggplot(
      data = country_choice_gender(),
      mapping = aes(
        x = gender,
        y = Count,
        fill = gender
      )
    ) +
      geom_bar(stat = "identity", color = "gray50") +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
      labs(title = "Gender", x = "", y = "Respondants") +
      my_theme_sizes +
      gender_fill_colors +
      theme(legend.position = "none") +
      coord_flip()
  })

  output$sex_orient <- renderPlot({
    ggplot(
      data = country_choice_sex(),
      mapping = aes(
        x = sexual_orientation,
        y = Count,
        fill = sexual_orientation
      )
    ) +
      geom_bar(stat = "identity", color = "gray50") +
      labs(title = "Sexual Orientation", x = "", y = "Respondants") +
      my_theme_sizes +
      sex_fill_colors +
      theme(legend.position = "none") +
      coord_flip()
  })

  # Page 2 - Employment / Age by Country

  category_choice <- reactive({
    input$rb_choice
  })

  country_choice <- reactive({
    input$user_choice
  })

  rando <- reactive({
    list(
      input$user_choice,
      input$rb_choice
    )
  })

  observeEvent(rando(), {
    dev_fil <- dev %>%
      filter(
        !is.na(country),
        !is.na(gender),
        !is.na(employment),
        !is.na(student)
      )

    public_data_filtered <- reactive({
      dev_fil %>%
        filter(
          country == input$user_choice,
          !is.na(gender),
          !is.na(employment),
          !is.na(student),
          !is.na(age)
        ) %>%
        mutate(gender = str_split(gender, pattern = ";")) %>%
        unnest(gender)
    })

    var_data_filtered <- reactive({
      dev_fil %>%
        select(country, !!sym(typer1), gender) %>%
        group_by(country, !!sym(typer1), gender) %>%
        count(!!sym(typer1), name = "count") %>%
        filter(
          !is.na(country == input$user_choice),
          !is.na(!!sym(typer1)),
          !is.na(gender)
        ) %>%
        filter(country == input$user_choice) %>%
        mutate(gender = str_split(gender, pattern = ";")) %>%
        unnest(gender)
    })

    typer1 <- case_when(
      category_choice() == "Employment Status" ~ "employment",
      category_choice() == "Student Status" ~ "student",
      category_choice() == "Age Groups" ~ "age"
    )

    titleboi <- case_when(
      category_choice() == "Employment Status" ~ "Count of each employemnt status by Country",
      category_choice() == "Student Status" ~ "Count of each student status by Country",
      category_choice() == "Age Groups" ~ "Count of each age group by Country"
    )

    titleboi2 <- case_when(
      category_choice() == "Employment Status" ~ "Total count of employemnt status of each gender",
      category_choice() == "Student Status" ~ "Total count of student status of each gender",
      category_choice() == "Age Groups" ~ "Total count of age group of each gender"
    )

    xboi <- case_when(
      category_choice() == "Employment Status" ~ "input$user_choice",
      category_choice() == "Student Status" ~ "Count of Student Status",
      category_choice() == "Age Groups" ~ "Count of Age Groups"
    )

    yboi <- case_when(
      category_choice() == "Employment Status" ~ "Count of Employemnt Status",
      category_choice() == "Student Status" ~ "Count of Student Status",
      category_choice() == "Age Groups" ~ "Count of Age Groups"
    )

    output$plot1 <- renderPlot({
      ggplot(data = var_data_filtered(), aes_string(x = typer1, y = "count", fill = "gender")) +
        geom_bar(stat = "identity") +
        labs(
          x = "",
          y = yboi,
          title = titleboi,
          subtitle = paste0(country_choice())
        ) +
        my_theme_sizes +
        gender_fill_colors +
        theme(axis.text.x = element_text(angle = 5, vjust = -0.01))
    })

    get_count <- count(dev, country, gender, name = "count") %>%
      filter(!is.na(gender)) %>%
      filter(country == input$user_choice) %>%
      mutate(gender = str_split(gender, pattern = ";")) %>%
      unnest(gender) %>%
      group_by(country, gender) %>%
      summarise(count = sum(count))

    output$plot4 <- renderPlot({
      ggplot(data = get_count) +
        geom_treemap(aes_string(area = "count", fill = "gender")) +
        geom_treemap_text(
          mapping = aes(label = count, area = count),
          colour = "white", reflow = TRUE, place = "centre", size = 15
        ) +
        labs(x = "", title = titleboi2) +
        my_theme_sizes +
        gender_fill_colors
    })
  })

  # Page 3 - Student Analysis by Country

  public_data_filtered2 <- reactive({
    dev %>%
      filter(
        country == input$choice2, !is.na(formal_education),
        !is.na(undergrad_major), !is.na(student)
      )
  })

  output$plot2 <- renderPlot({
    ggplot(data = public_data_filtered2()) +
      geom_histogram(
        mapping = aes(x = formal_education, fill = student),
        stat = "count"
      ) +
      labs(
        title = paste0(country_choice()),
        x = "Formal Education",
        y = "Count of Formal Education Degrees"
      ) +
      my_theme_sizes +
      student_fill_colors
  })

  output$undergraduate <- renderPlot({
    ggplot(data = public_data_filtered2()) +
      geom_histogram(
        mapping = aes(x = undergrad_major, fill = student),
        stat = "count"
      ) +
      labs(
        title = paste0(country_choice()),
        x = "Undergraduate Majors",
        y = "Count of Education Majors"
      ) +
      my_theme_sizes +
      student_fill_colors +
      coord_flip()
  })

  # Page 4 - Salary by Gender

  dist_sal <- dev %>%
    filter(employment %in% "Employed full-time") %>%
    filter(gender == "Male" | gender == "Female") %>%
    group_by(country) %>%
    mutate(count = n()) %>%
    filter(count > 1500) %>%
    filter(!is.na(converted_salary)) %>%
    ungroup(country)

  output$salary_top_5 <- renderPlot({
    ggplot(dist_sal) +
      geom_boxplot(aes(x = country, converted_salary, fill = gender)) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      coord_flip(ylim = c(0, 250000)) +
      labs(
        x = "Countries",
        y = "Converted Salary",
        title = "Salary of top 5 countries"
      ) +
      my_theme_sizes +
      mf_fill_colors
  })

  dist_sal2 <- dev %>%
    filter(employment %in% "Employed full-time") %>%
    filter(gender == "Male" | gender == "Female") %>%
    group_by(country) %>%
    mutate(count = n()) %>%
    filter(count == 1) %>%
    filter(!is.na(converted_salary)) %>%
    ungroup(country)

  output$salary_equal_1 <- renderPlot({
    ggplot(dist_sal2) +
      geom_boxplot(aes(x = country, converted_salary, fill = gender)) +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      coord_flip(ylim = c(0, 250000)) +
      labs(
        x = "Countries",
        y = "Converted Salary",
        title = "Salary of countries with only one reported salary"
      ) +
      my_theme_sizes +
      mf_fill_colors
  })


  # Page 5 - Salary Distribution by Gender and Country

  salary_data <- dev %>%
    filter(employment == "Employed full-time") %>%
    filter(!is.na(converted_salary)) %>%
    filter(converted_salary > 1000) %>%
    select(country, gender, converted_salary)

  country_1_male <- reactive({
    salary_data %>%
      filter(country == input$salary_country_choice_1) %>%
      filter(gender == "Male")
  })

  country_1_female <- reactive({
    salary_data %>%
      filter(country == input$salary_country_choice_1) %>%
      filter(gender == "Female")
  })

  output$salary_country_compare_1 <- renderPlot({
    ggplot() +
      {
        if ("Male" %in% input$salary_country_mf_choice_1) {
          list(
            geom_density(
              data = country_1_male(),
              aes(x = converted_salary, color = "Male", fill = "Male"),
              alpha = .2, adjust = 1.5, size = 1
            ),
            geom_vline(
              data = country_1_male(),
              aes(
                xintercept = median(converted_salary),
                color = "Male", linetype = "Median Salary"
              ), size = 2
            ),
            geom_vline(
              data = country_1_male(),
              aes(
                xintercept = mean(converted_salary),
                color = "Male", linetype = "Mean Salary"
              ), size = 1
            )
          )
        }
      } +
      {
        if ("Female" %in% input$salary_country_mf_choice_1) {
          list(
            geom_density(
              data = country_1_female(),
              aes(x = converted_salary, color = "Female", fill = "Female"),
              alpha = .2, adjust = 1.5, size = 1
            ),
            geom_vline(
              data = country_1_female(),
              aes(
                xintercept = median(converted_salary),
                color = "Female", linetype = "Median Salary"
              ), size = 2
            ),
            geom_vline(
              data = country_1_female(),
              aes(
                xintercept = mean(converted_salary),
                color = "Female", linetype = "Mean Salary"
              ), size = 1
            )
          )
        }
      } +
      coord_cartesian(xlim = c(0, 200000)) +
      my_theme_sizes +
      mf_fill_colors +
      theme(
        axis.text.y = element_blank(),
        legend.title = element_blank()
      ) +
      guides(
        fill = guide_legend(
          override.aes = list(alpha = 1),
          label.position = "bottom"
        ),
        linetype = guide_legend(
          keywidth = 1, keyheight = 4,
          label.position = "bottom"
        ),
        color = "none"
      ) +
      labs(
        title = input$salary_country_choice_1,
        x = "Salary (Annual USD)",
        y = ""
      ) +
      scale_color_manual(values = c(
        "Male" = "royalblue4",
        "Female" = "darkred"
      )) +
      scale_linetype_manual(
        labels = c("Median\nSalary", "Mean\nSalary"),
        values = c(
          "Median Salary" = "dotted",
          "Mean Salary" = "dashed"
        ),
      )
  })

  country_2_male <- reactive({
    salary_data %>%
      filter(country == input$salary_country_choice_2) %>%
      filter(gender == "Male")
  })

  country_2_female <- reactive({
    salary_data %>%
      filter(country == input$salary_country_choice_2) %>%
      filter(gender == "Female")
  })

  output$salary_country_compare_2 <- renderPlot({
    ggplot() +
      {
        if ("Male" %in% input$salary_country_mf_choice_2) {
          list(
            geom_density(
              data = country_2_male(),
              aes(x = converted_salary, color = "Male", fill = "Male"),
              alpha = .2, adjust = 1.5, size = 1
            ),
            geom_vline(
              data = country_2_male(),
              aes(
                xintercept = median(converted_salary),
                color = "Male", linetype = "Median Salary"
              ), size = 2
            ),
            geom_vline(
              data = country_2_male(),
              aes(
                xintercept = mean(converted_salary),
                color = "Male", linetype = "Mean Salary"
              ), size = 1
            )
          )
        }
      } +
      {
        if ("Female" %in% input$salary_country_mf_choice_2) {
          list(
            geom_density(
              data = country_2_female(),
              aes(x = converted_salary, color = "Female", fill = "Female"),
              alpha = .2, adjust = 1.5, size = 1
            ),
            geom_vline(
              data = country_2_female(),
              aes(
                xintercept = median(converted_salary),
                color = "Female", linetype = "Median Salary"
              ), size = 2
            ),
            geom_vline(
              data = country_2_female(),
              aes(
                xintercept = mean(converted_salary),
                color = "Female", linetype = "Mean Salary"
              ), size = 1
            )
          )
        }
      } +
      coord_cartesian(xlim = c(0, 200000)) +
      my_theme_sizes +
      mf_fill_colors +
      theme(
        axis.text.y = element_blank(),
        legend.title = element_blank()
      ) +
      guides(
        fill = guide_legend(
          override.aes = list(alpha = 1),
          label.position = "bottom"
        ),
        linetype = guide_legend(
          keywidth = 1, keyheight = 4,
          label.position = "bottom"
        ),
        color = "none"
      ) +
      labs(
        title = input$salary_country_choice_2,
        x = "Salary (Annual USD)",
        y = ""
      ) +
      scale_color_manual(values = c(
        "Male" = "royalblue4",
        "Female" = "darkred"
      )) +
      scale_linetype_manual(
        labels = c("Median\nSalary", "Mean\nSalary"),
        values = c(
          "Median Salary" = "dotted",
          "Mean Salary" = "dashed"
        ),
      )
  })

  # Page 6 - Coding Years vs Satisfaction

  coding_prof_vs_job_filter <- reactive({
    dev %>%
      filter(years_coding_prof == input$coding_prof) %>%
      filter(!is.na(job_satisfaction)) %>%
      filter(!is.na(years_coding_prof)) %>%
      mutate(job_satisfaction = factor(job_satisfaction,
        levels = c(
          "Extremely dissatisfied",
          "Moderately dissatisfied",
          "Slightly dissatisfied",
          "Neither satisfied nor dissatisfied",
          "Slightly satisfied",
          "Moderately satisfied",
          "Extremely satisfied"
        )
      ))
  })

  coding_prof_vs_career_filter <- reactive({
    dev %>%
      filter(years_coding_prof == input$coding_prof) %>%
      filter(!is.na(career_satisfaction)) %>%
      filter(!is.na(years_coding_prof)) %>%
      mutate(career_satisfaction = factor(career_satisfaction,
        levels = c(
          "Extremely dissatisfied",
          "Moderately dissatisfied",
          "Slightly dissatisfied",
          "Neither satisfied nor dissatisfied",
          "Slightly satisfied",
          "Moderately satisfied",
          "Extremely satisfied"
        )
      ))
  })

  job_color <- reactive({
    input$job_color_choice
  })

  career_color <- reactive({
    input$career_color_choice
  })


  observeEvent(input$job_color_choice, {
    job_color_fill <- case_when(
      job_color() == "Gray" ~ "gray35",
      job_color() == "Red" ~ "darkred",
      job_color() == "Blue" ~ "royalblue4",
      job_color() == "Gold" ~ "orange2"
    )

    job_color_border <- case_when(
      job_color() == "Gray" ~ "black",
      job_color() == "Red" ~ "gray20",
      job_color() == "Blue" ~ "darkblue",
      job_color() == "Gold" ~ "gray35"
    )

    output$job_satisfy <- renderPlot({
      ggplot(data = coding_prof_vs_job_filter()) +
        geom_histogram(
          mapping = aes(job_satisfaction),
          stat = "count",
          color = job_color_border,
          size = 1,
          fill = job_color_fill
        ) +
        labs(
          title = "Job Satisfaction vs Years of Professional Coding",
          x = "",
          y = "Respondants"
        ) +
        my_theme_sizes
    })
  })

  observeEvent(input$career_color_choice, {
    career_color_fill <- case_when(
      career_color() == "Gray" ~ "gray35",
      career_color() == "Red" ~ "darkred",
      career_color() == "Blue" ~ "royalblue4",
      career_color() == "Gold" ~ "orange2"
    )

    career_color_border <- case_when(
      career_color() == "Gray" ~ "black",
      career_color() == "Red" ~ "gray20",
      career_color() == "Blue" ~ "darkblue",
      career_color() == "Gold" ~ "gray35"
    )

    output$career_satisfy <- renderPlot({
      ggplot(data = coding_prof_vs_career_filter()) +
        geom_histogram(
          mapping = aes(career_satisfaction),
          stat = "count",
          color = career_color_border,
          size = 1,
          fill = career_color_fill
        ) +
        labs(
          title = "Career Satisfaction vs Years of Professional Coding",
          x = "",
          y = "Respondants"
        ) +
        my_theme_sizes
    })
  })

  # Page 7 - Satisfied Long Term Coders

  job_opacity <- reactive({
    input$job_opacity_input
  })

  career_opacity <- reactive({
    input$career_opacity_input
  })

  satisfaction_choice <- reactive({
    input$satisfaction_level_input
  })

  dev_code <- dev %>%
    mutate(years_coding_prof = factor(years_coding_prof,
      levels = c(
        "0-2 years", "3-5 years", "6-8 years",
        "9-11 years", "12-14 years", "15-17 years",
        "18-20 years", "21-23 years", "24-26 years",
        "27-29 years", "30 or more years"
      )
    ))

  job_across_experience <- dev_code %>%
    filter(!is.na(years_coding_prof)) %>%
    filter(!is.na(job_satisfaction)) %>%
    group_by(years_coding_prof) %>%
    summarize(job_total = n())

  job_data <- reactive({
    dev_code %>%
      filter(!is.na(years_coding_prof)) %>%
      filter(!is.na(job_satisfaction)) %>%
      filter(job_satisfaction == satisfaction_choice()) %>%
      group_by(years_coding_prof) %>%
      summarize(job_n = n()) %>%
      mutate(percentage = (job_n / job_across_experience$job_total) * 100)
  })

  career_across_experience <- dev_code %>%
    filter(!is.na(years_coding_prof)) %>%
    filter(!is.na(career_satisfaction)) %>%
    group_by(years_coding_prof) %>%
    summarize(career_total = n())

  career_data <- reactive({
    dev_code %>%
      filter(!is.na(years_coding_prof)) %>%
      filter(!is.na(career_satisfaction)) %>%
      filter(career_satisfaction == satisfaction_choice()) %>%
      group_by(years_coding_prof) %>%
      summarize(career_n = n()) %>%
      mutate(percentage = (career_n / career_across_experience$career_total) * 100)
  })

  output$satisfaction_level_plot <- renderPlot({
    ggplot() +
      geom_bar(
        data = job_data(),
        aes(x = years_coding_prof, y = percentage),
        stat = "identity", fill = "royalblue4", alpha = job_opacity()
      ) +
      geom_bar(
        data = career_data(),
        aes(x = years_coding_prof, y = percentage),
        stat = "identity", fill = "orange2", alpha = career_opacity()
      ) +
      labs(
        title = "Job and Career Satisfaction vs Years of Professional Coding",
        x = "",
        y = "% of Respondants"
      ) +
      my_theme_sizes
  })

  # Page 8 - Salary Considerations

  coding_salary_data <- dev %>%
    filter(employment %in% "Employed full-time") %>%
    filter(!is.na(converted_salary)) %>%
    filter(!is.na(years_coding_prof)) %>%
    select(years_coding_prof, converted_salary) %>%
    mutate(years_coding_prof = factor(years_coding_prof,
      levels = c(
        "0-2 years", "3-5 years", "6-8 years",
        "9-11 years", "12-14 years", "15-17 years",
        "18-20 years", "21-23 years", "24-26 years",
        "27-29 years", "30 or more years"
      )
    )) %>%
    group_by(years_coding_prof)

  output$coding_vs_salary <- renderPlot({
    ggplot(data = coding_salary_data) +
      geom_boxplot(
        mapping = aes(x = years_coding_prof, y = converted_salary),
        fill = "orange2",
        notch = TRUE
      ) +
      labs(x = "", y = "Salary (Annual USD)") +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
      my_theme_sizes +
      coord_flip(ylim = c(0, 300000))
  })

  summ_career_data <- dev %>%
    filter(employment %in% "Employed full-time") %>%
    filter(!is.na(converted_salary)) %>%
    filter(!is.na(career_satisfaction)) %>%
    select(career_satisfaction, converted_salary) %>%
    mutate(career_satisfaction = factor(career_satisfaction,
      levels = c(
        "Extremely dissatisfied",
        "Moderately dissatisfied",
        "Slightly dissatisfied",
        "Neither satisfied nor dissatisfied",
        "Slightly satisfied",
        "Moderately satisfied",
        "Extremely satisfied"
      )
    )) %>%
    group_by(career_satisfaction) %>%
    summarize(
      mean_salary = mean(converted_salary),
      med_salary = median(converted_salary)
    )

  reshaped_career <- melt(summ_career_data, id = "career_satisfaction")

  output$salary_satisfaction <- renderPlot({
    ggplot(
      data = reshaped_career,
      aes(x = career_satisfaction, y = value, color = variable)
    ) +
      geom_point(size = 3) +
      geom_point(size = 15, shape = 1, stroke = 3) +
      labs(x = "", y = "Salary (Annual USD)") +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE), limits = c(0, 150000)) +
      scale_color_manual(
        labels = c("Mean Salary", "Median Salary"),
        values = c("darkred", "royalblue4")
      ) +
      my_theme_sizes +
      theme(
        legend.background = element_rect(fill = "white", size = 0.5, color = "gray"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)
      )
  })
} # server

shinyApp(ui, server)