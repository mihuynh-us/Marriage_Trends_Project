library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(viridisLite)
# Setup
library(shiny)
library(bslib)
library(ggplot2)
library(tidyverse)
library(epidatr)
library(censusapi)
library(cdlTools)
library(plotly)
library(maps)
cs_key <- read_file("census_api.txt")
# Data
# Census 2000
census_2000 <- getCensus(name = "dec/sf2profile",
                         vintage = 2000, 
                         vars = c("NAME",
                                  "DP1_C82", # Percent Married Couple Family
                                  "DP1_C84", # Percent Married Couple Family with children under 18
                                  "DP1_C86", # Percent Female household, no spouse
                                  "DP1_C87", # Percent Female household, no spouse with children under 18
                                  "DP1_C90", # Percent Nonfamily household ?What that mean
                                  "DP1_C68" # Percent Unmarried partner
                         ), 
                         region = "state", 
                         key = cs_key)
census_2000_sf3 <- getCensus(name = "dec/sf3profile",
                             vintage = 2000, 
                             vars = c("NAME",
                                      "DP2_C13", # Education
                                      "DP3_C112", # Median Income
                                      "DP3_C129", # Families
                                      "DP3_C155", # Below Poverty
                                      "DP3_C157", # Below Poverty children under 18
                                      "DP2_C19", # HS or higher
                                      "DP2_C11"), # College or grad school
                             region = "state",
                             key = cs_key) 
census_2000 <-
  census_2000 %>%
  rename(Married_Households = DP1_C82, 
         Married_Households_children = DP1_C84, 
         Female_householder_no_spouse = DP1_C86, 
         Female_householder_no_spouse_children = DP1_C87,
         Unmarried_couple = DP1_C68) 
# Unmarried couple = Unmarried partner in this case
us_states <- map_data("state")
census_2000$region <- tolower(census_2000$NAME)
census_2000_state_1 <- inner_join(us_states, census_2000)
census_2000_sf3 <-
  census_2000_sf3 %>%
  rename(Education = DP2_C13, 
         Median_income = DP3_C112,
         Families = DP3_C129,
         Below_poverty = DP3_C155,
         Below_poverty_children = DP3_C157,
         HS = DP2_C19,
         College_Grad = DP2_C11) 
census_2000_sf3$region <- tolower(census_2000_sf3$NAME)
census_2000_state_2 <- inner_join(us_states, census_2000_sf3)
data_2000_states <- inner_join(census_2000_state_2, census_2000_state_1)

# ACS 2010
library(tidycensus)
acs_2010 <- get_acs(
  year = 2010,
  survey = "acs5",
  variables = c("DP02_0002PE", # Percent Family Household
                "DP02_0004PE", # Percent Married-couple household
                "DP02_0005PE", # Percent Married-couple household!!With children of the householder under 18 years
                "DP02_0006PE", # Percent Male householder, no wife present, family
                "DP02_0007PE", # Percent Male householder, no wife present, family!!With children of the householder under 18 years
                "DP02_0008PE", # Percent Female householder, no husband present, family
                "DP02_0009PE", # Percent Female householder, no husband present, family!!With children of the householder under 18 years
                "DP02_0023PE", # Percent Unmarried couple
                "DP03_0136PE", # Below Poverty ~ implied HH
                "DP03_0120PE", # Below Poverty Children under 18
                "DP02_0058PE", # Education
                "DP03_0062E", # Median income ~ implied HH
                "DP02_0066PE", # HS Diploma or higher
                "DP02_0067PE"), # College or graduate
  geography = "state",
  output = "wide",
  show_call = TRUE
)
# One interesting thing is that the data changes from wife/husband -> spouse
acs_2010 <- acs_2010 %>%
  rename(Family_Household = DP02_0002PE,
         Married_Households = DP02_0004PE,
         Married_Households_children = DP02_0005PE,
         Male_householder_no_spouse = DP02_0006PE,
         Male_householder_no_spouse_children = DP02_0007PE,
         Female_householder_no_spouse = DP02_0008PE,
         Female_householder_no_spouse_children = DP02_0009PE,
         Unmarried_couple = DP02_0023PE,
         Below_poverty = DP03_0136PE,
         Below_poverty_children = DP03_0120PE,
         Education = DP02_0058PE,
         Median_income = DP03_0062E,
         HS = DP02_0066PE,
         College_Grad = DP02_0067PE)
us_states <- map_data("state")
acs_2010$region <- tolower(acs_2010$NAME)
data_2010_states <- inner_join(us_states, acs_2010)


# ACS 2020
acs_2020 <- get_acs(
  year = 2020,
  survey = "acs5",
  cache_table = TRUE,
  variables = c("DP02_0002P", # Percent Married-couple household
                "DP02_0006P", # Percent Male householder, no spouse present
                "DP02_0010P", # Percent Female householder, no spouse present
                "DP02_0004P", # Percent Cohabiting couple
                "DP02_0021P", # Percent Unmarried couple
                "DP03_0119PE", # Below Poverty
                "DP03_0120PE", # Below Poverty Children under 18
                "DP03_0062E", # Median income
                "DP02_0059PE", # Education percent
                "DP02_0058E", # Education estimate
                "DP02_0067P", # HS Diploma or higher
                "DP02_0058P"), # College or graduate
  geography = "state",
  output = "wide",
  key = cs_key,
  show_call = TRUE
)
# Terminology changed from wife/husband -> spouse in 2020
acs_2020 <- acs_2020 %>%
  rename(Married_Households = DP02_0002PE,
         Male_householder_no_spouse = DP02_0006PE,
         Female_householder_no_spouse = DP02_0010PE,
         Unmarried_couple = DP02_0021PE,
         Cohabiting_couple = DP02_0004PE,
         Below_poverty = DP03_0119PE,
         Below_poverty_children = DP03_0120PE,
         Median_income = DP03_0062E,
         Education_percent = DP02_0059PE,
         Education = DP02_0058E,
         HS = DP02_0067PE,
         College_Grad = DP02_0058PE,
         state = GEOID)
# Unmarried ~ roommates
# Cohabiting ~ relationship
us_states <- map_data("state")
acs_2020$region <- tolower(acs_2020$NAME)
acs_2020_state_1 <- inner_join(us_states, acs_2020)
# Census 2020
# For missing children variables because they changed the variables
census_2020 <- getCensus(name = "dec/dp",
                         vintage = 2020, 
                         vars = c("NAME",
                                  "DP1_0135P", # Cohabitating couple
                                  "DP1_0136P", # Cohabitating couple children
                                  "DP1_0134P", # Married couple children
                                  "DP1_0140P", # Male, no spouse, children
                                  "DP1_0144P"), # Female, no spouse, children
                         region = "state", 
                         key = cs_key)
census_2020 <-
  census_2020 %>%
  rename(Cohabiting_couple_census = DP1_0135P, 
         Cohabiting_couple_children = DP1_0136P, 
         Married_Households_children = DP1_0134P, 
         Male_householder_no_spouse_children = DP1_0140P,
         Female_householder_no_spouse_children = DP1_0144P)
us_states <- map_data("state")
census_2020$region <- tolower(census_2020$NAME)
census_2020_state_1 <- inner_join(us_states, census_2020)
data_2020_states <- inner_join(acs_2020_state_1, census_2020_state_1)

data_2000_states$year <- "2000"
data_2010_states$year <- "2010"
data_2020_states$year <- "2020"

# Keep only common columns
common_cols <- Reduce(intersect, list(
  names(data_2000_states),
  names(data_2010_states),
  names(data_2020_states)
))
fin_data_2000_states <- data_2000_states[, common_cols]
fin_data_2010_states <- data_2010_states[, common_cols]
fin_data_2020_states <- data_2020_states[, common_cols]

# Combine all years
all_df <- rbind(fin_data_2000_states, fin_data_2010_states, fin_data_2020_states)
all_df$year <- as.numeric(all_df$year)
model <- lm(Married_Households ~ factor(year) + Median_income + HS + College_Grad, data = all_df)
s <- summary(model)
coefs <- as.data.frame(s$coefficients)

#######################################################
#######################################################
#######################################################
#######################################################

# UI
ui <- fluidPage(
  titlePanel("Marriage and Childbearing Trends Between U.S. States from 2000 to 2020"),
  fluidRow(
    column(width = 12,
           div(style = "font-size: 24px;", "By: Mi Huynh", br(),
               "Date: 12/10/2025")
    )), br(), hr(),
  tags$style(HTML("
    .nav-list > li {
      font-size: 10px;
      padding: 0px 0px;
    }
  ")),
  # Table of Contents on the right
  navlistPanel(widths = c(3,9),
    
    # ----------------- Sections -----------------
    "Description of Project",
    tabPanel("Introduction",
             fluidRow(
               column(width = 12,
                      div(style = "font-size: 24px; font-weight: bold;", "Introduction"))
             ), hr(),
             # Image after title
             img(src = "family.png", width = "50%"), br(),
             fluidRow(
               column(width = 12,
                      div(style = "font-size: 16px;", 
                          "• The average family consisted of 3.15 persons in 2021, down from 3.7 in the 1960s.", tags$a("Source: Statista", 
                                                                                                                        href = "https://www.statista.com/statistics/183657/average-size-of-a-family-in-the-us/#:~:text=As%20of%202023%2C%20the%20U.S.,about%2040%20percent%20in%202020", target = "_blank"), br(),
                          "• This project observes how trends about marriage and childbearing have changed from 2000 to 2020."))), br(), hr(),
             fluidRow(
               column(width = 12,
                      div(style = "font-size:16px; font-weight:bold; text-decoration: underline;", "Motivation:")),
               column(width = 12, div(style = "font-size:16px;",
                                      "• Not many visualizations currently exist to show differences between U.S. states.", br(),
                                      "• This project aims to bridge that gap by using interactive visualizations to show stark differences between states, their demographics, and the decade."))), br(), hr(),
             fluidRow(
               column(width = 12,
                      div(style = "font-size: 16px; font-weight: bold; text-decoration: underline;",
                          "Research Question:")),
               column(width = 12, div(style = "font-size: 16px;", "How did marriage and childbearing trends change from 2000 to 2020, and how did these shifts vary across U.S. states?")),
                      br(), hr(), br()
    )),
    tabPanel("Methodology",
             fluidRow(
               column(width = 12,
                      div(style = "font-size: 24px; font-weight: bold;", "Methodology"))), hr(),
             # Image after title
             img(src = "census.jpg", width = "50%"), br(), hr(),
             fluidRow(
               column(width = 12,
                      div(style = "font-size: 16px; font-weight: bold;",
                          "Decenniel Census API was used for:"))), br(),
             fluidRow(
               column(width = 12, div(style = "font-size:16px;", "• 2000", br(),
                          "• 2020"))), hr(),
             fluidRow(
               column(width = 12,
                      div(style = "font-size: 16px; font-weight: bold;",
                      "American Community Survey (ACS) was used for:"))), br(),
             fluidRow(
               column(width = 12, div(style = "font-size:16px;", "• 2010", br(),"• 2020"))), hr(),
             fluidRow(
               column(width = 12, div(style = "font-side:16px; font-weight: bold;", "Variables include:"))), br(),
             fluidRow(
               column(width = 12, div(style = "font-size:16px;",
                                      "• Married households %", br(),
                                      "• Households with children %", br(),
                                      "• Education level (% higher than high school or college)", br(),
                                      "• Median income $"))), br()),
    tabPanel("Github Link", 
             img(src = "github.png", width = "40%"), br(),
             tags$a("Visit my Github Repository",
                    href = "https://github.com/mihuynh-us/Marriage_Trends_Project", target = "_blank", style = "font-size:16px;")),
    "Plots & Tables",
    tabPanel("Interactive Plots",
             sidebarLayout(
               sidebarPanel(width = 3,
                 tags$div("• Select between states and variables.", style="color:red"),
                 tags$div("• Hover over states to get more information.", style="color:red"),
                 # 1. State selector
                 selectInput("state", "State:",
                             choices = c("All States", sort(unique(all_df$NAME))),
                             selected = "All States"),
                 
                 # 2. Fill-variable selector
                 selectInput(
                   "fill",
                   "Fill Variable:",
                   choices = c("Married Households" = "Married_Households",
                               "Married Households with Children" = "Married_Households_children",
                               "Median Household Income" = "Median_income",
                               "Below Poverty" = "Below_poverty",
                               "College Graduate or Higher" = "College_Grad",
                               "High School Graduate or Higher" = "HS"),
                   selected = "Married_Households")
               ),
               mainPanel(width = 9,
                 plotlyOutput("plot", width = "600px", height = "500px"), br(), htmlOutput("StatesText"),
                 br(), hr(), br(),
             ))),
    tabPanel("Multiple Linear Regression Model",
             sidebarLayout(
               tags$h3(
                 "Multiple Linear Regression Model with Married Households as the Outcome Variable",
                 style = "border-bottom: 2px solid #ccc; padding-left: 8px;"
               ),
               mainPanel(tableOutput("summaryTable"), br(), htmlOutput("TableText")
               ))
             ),
    tabPanel("Discussion & Future Research",
             fluidRow(
               column(width = 12, div(style = "font-size: 24px; font-weight: bold", "Discussion"))), br(), hr(),
             fluidRow(
               column(width = 12, div(style = "font-size: 16px;", 
                                      "• There is a clear declining of households getting married and having children.", br(),
                                      "• This trend is consistent throughout the United States, 
                                      even states with high marriage percentages (like Utah) are decreasing."))), br(), br(),
             fluidRow(
               column(width = 12, div(style = "font-size: 24px; font-weight: bold", "Future Research"))), br(), hr(),
             fluidRow(
               column(width = 12, div(style = "font-size: 16px;", 
               "• Future research could look into sentiment analysis of behaviors that cause this trend."))), br(),
             fluidRow(
               column(width = 12, div(style = "font-size: 16px; margin-left: 20px;", "• Sentiment analysis of dating, marriage, and loneliness topics on social media.",
                                      br(), "• Examples: Tiktok, Instagram, Reddit comments and discussions.", br(),
                                      "• Potential Topics: #SAHM (Stay at Home Mom), Black Pill (Nihilistic views on relationships), Male Lonelinesss Epidemic.")))
             )))


## Add future directions, see if COVID had something to do with it

server <- function(input, output, session) {
  
  # Filter state first
  filtered_df <- reactive({
    if (input$state == "All States") {
      all_df
    } else {
      all_df %>% filter(NAME == input$state)
    }
  })
  sig_stars <- function(p) {
    ifelse(p < 0.001, "***",
           ifelse(p < 0.01, "**",
                  ifelse(p < 0.05, "*", "")))
  }
  output$plot <- renderPlotly({
    
    df <- filtered_df()
    # Safety check
    if (nrow(df) == 0) return(plotly_empty())
    clean_label <- function(x) gsub("_", " ", x)
    
    df <- df %>% 
      mutate(
        hover_text = if (input$fill == "Median_income") {
          paste0(
            "State: ", NAME, "<br>",
            clean_label(input$fill), ": $",
            .data[[input$fill]]
          )
        } else {
          paste0(
            "State: ", NAME, "<br>",
            clean_label(input$fill), ": ",
            .data[[input$fill]], "%"
          )
        },
        hover_text = as.character(hover_text)   
      )
    fill_legend <- if (input$fill == "Median_income") "Dollars" else "Percent"
    p <- ggplot(df) +
      geom_polygon(
        aes(
          x = long,
          y = lat,
          group = group,
          fill = .data[[input$fill]],     # Gets fill from each variable input
          frame = year,
          text = hover_text),
        color = "white",
        linewidth = 0.1
      ) +
      scale_fill_viridis_c(option = "C") +
      coord_fixed(1.3) +
      labs(
        fill = fill_legend,
        title = paste("Map colored by", clean_label(input$fill))) +
      theme_void()
    
    ggplotly(p, tooltip = "text") %>%
      animation_opts(frame = 0, transition = 0, redraw = TRUE) %>%
      animation_slider(currentvalue = list(prefix = "Year: ")) %>%
      animation_button(x = 1, y = 0, visible = FALSE) %>%
      layout(margin = list(t = 60, b = 40, l = 20, r = 20))
  })
  coefs$Significance <- sig_stars(coefs[,4]) # 4th column = p-value
  
  output$summaryTable <- renderTable({
    coefs %>%
      dplyr::rename(
        Estimate = Estimate,
        StdError = `Std. Error`,
        tValue = `t value`,
        pValue = `Pr(>|t|)`
      )
  }, rownames = TRUE)
  
  output$TableText <- renderUI({
    HTML("
    <p style=font-size:14px;><b>Interpretation:</b> The model summary above shows the estimated effect of each variable
    on <i>Married Households</i>. All the p-values are statistically significant.</p>
    <p> • The <i>intercept</i> estimate of 48.89 is the percentage of married households at the year 2000.</p>
    <p> • The <i>factor(year)2010</i> estimate of -12.71 shows that there is a decrease of married households from 2000 to 2010 of 12.71%.</p>
    <p> • The <i>factor(year)2020</i> estimate of -16.61 shows that there is a decrease of married households from 2000 to 2020 of 16.61%.</p>
    <p> • The <i>Median_income</i> estimate of 0 shows there's no differences between married households and household median income.</p>
    <p> • The <i>HS</i> estimate of 0.19 shows that there's a increase of married household percentage by 0.19% if someone has at least high school education.</p>
    <p> • The <i>College_Grad</i> estimate of -0.32 shows that there's a decrease of married household percentage by 0.32% if someone has at least college education.</p>
    <p>       People with at least college education might be more focused on their career and personal development, opposed to marriage and children.</p>
  ")
  })
  output$StatesText <- renderUI({
    HTML("
    <p style=font-size:14px;> Toggling through the map, you can see that marriage and childbearing trends decreases every decade.</p>
    <p style=font-size:14px;> As people are getting more educated, it seems like marriage trends are declining. </p>
    <p style=font-size:14px;> These changes are prominent throughout states, which you can filter out. </p>

  ")
  })
}

shinyApp(ui, server)
