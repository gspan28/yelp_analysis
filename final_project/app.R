library(shiny)
library(leaflet)
library(readr)
library(plotly)
library(tidyverse)
library(shinythemes)
library(gt)


# Reads in data

lm_stars_tidy <- read_rds("lm_stars_tidy.RDS")
lm_date_tidy <- read_rds("lm_date_tidy.RDS")




# Define UI for application 

ui <- navbarPage(theme = shinytheme("united"),
                 "Need A Good Date Spot?",
                 tabPanel("Effect of Restaurant Type on User Ratings",
                          h1("Regression"),
                          
                          sidebarLayout(
                              
                              sidebarPanel(
                                  helpText(
                                      "Select up to three cuisines to compare their 
                                      likelihood of receiving positive ratings on Yelp:"),
                                  
                                  selectInput("cuisine1", 
                                              "Cuisine 1:", 
                                              choices = list(
                                                  "American" = "american",
                                                  "Asian Fusion" = "asian_fusion",
                                                  "Bakeries" = "bakeries",
                                                  "Barbeque" = "barbeque",
                                                  "Bars" = "bars",
                                                  "Breakfast / Brunch" = "breakfast_brunch",
                                                  "Burgers" = "burgers",
                                                  "Cafes" = "cafes",
                                                  "Canadian" = "canadian",
                                                  "Chinese" = "chinese",
                                                  "Chicken Wings" = "chicken_wings",
                                                  "Delis" = "delis",
                                                  "Desserts" = "desserts",
                                                  "Fast Food" = "fast_food",
                                                  "French" = "french",
                                                  "Gastropubs" = "gastropubs",
                                                  "Ice Cream / Frozen Yogurt" = "ice_cream_frozen_yogurt",
                                                  "Indian" = "indian",
                                                  "Japanese" = "japanese",
                                                  "Italian" = "italian",
                                                  "Juice Bars / Smoothies" = "juice_bars_smoothies",
                                                  "Korean" = "korean",
                                                  "Mediterranean" = "mediterranean",
                                                  "Mexican" = "mexican",
                                                  "Pizza" = "pizza",
                                                  "Sandwiches" = "sandwiches",
                                                  "Salad" = "salad",
                                                  "Seafood" = "seafood",
                                                  "Soup" = "soup",
                                                  "Specialty Food" = "specialty_food",
                                                  "Steakhouses" = "steakhouses",
                                                  "Thai" = "thai",
                                                  "Vegetarian" = "vegetarian",
                                                  "Vietnamese" = "vietnamese",
                                                  "Wine Bars" = "wine_bars"),
                                              selected = "steakhouses"),
                                  selectInput("cuisine2", 
                                              "Cuisine 2:", 
                                              choices = list(
                                                  "American" = "american",
                                                  "Asian Fusion" = "asian_fusion",
                                                  "Bakeries" = "bakeries",
                                                  "Barbeque" = "barbeque",
                                                  "Bars" = "bars",
                                                  "Breakfast / Brunch" = "breakfast_brunch",
                                                  "Burgers" = "burgers",
                                                  "Cafes" = "cafes",
                                                  "Canadian" = "canadian",
                                                  "Chinese" = "chinese",
                                                  "Chicken Wings" = "chicken_wings",
                                                  "Delis" = "delis",
                                                  "Desserts" = "desserts",
                                                  "Fast Food" = "fast_food",
                                                  "French" = "french",
                                                  "Gastropubs" = "gastropubs",
                                                  "Ice Cream / Frozen Yogurt" = "ice_cream_frozen_yogurt",
                                                  "Indian" = "indian",
                                                  "Japanese" = "japanese",
                                                  "Italian" = "italian",
                                                  "Juice Bars / Smoothies" = "juice_bars_smoothies",
                                                  "Korean" = "korean",
                                                  "Mediterranean" = "mediterranean",
                                                  "Mexican" = "mexican",
                                                  "Pizza" = "pizza",
                                                  "Sandwiches" = "sandwiches",
                                                  "Salad" = "salad",
                                                  "Seafood" = "seafood",
                                                  "Soup" = "soup",
                                                  "Specialty Food" = "specialty_food",
                                                  "Steakhouses" = "steakhouses",
                                                  "Thai" = "thai",
                                                  "Vegetarian" = "vegetarian",
                                                  "Vietnamese" = "vietnamese",
                                                  "Wine Bars" = "wine_bars"),
                                              selected = "thai"),
                                  selectInput("cuisine3", 
                                              "Cuisine 3:", 
                                              choices = list(
                                                "American" = "american",
                                                "Asian Fusion" = "asian_fusion",
                                                "Bakeries" = "bakeries",
                                                "Barbeque" = "barbeque",
                                                "Bars" = "bars",
                                                "Breakfast / Brunch" = "breakfast_brunch",
                                                "Burgers" = "burgers",
                                                "Cafes" = "cafes",
                                                "Canadian" = "canadian",
                                                "Chinese" = "chinese",
                                                "Chicken Wings" = "chicken_wings",
                                                "Delis" = "delis",
                                                "Desserts" = "desserts",
                                                "Fast Food" = "fast_food",
                                                "French" = "french",
                                                "Gastropubs" = "gastropubs",
                                                "Ice Cream / Frozen Yogurt" = "ice_cream_frozen_yogurt",
                                                "Indian" = "indian",
                                                "Japanese" = "japanese",
                                                "Italian" = "italian",
                                                "Juice Bars / Smoothies" = "juice_bars_smoothies",
                                                "Korean" = "korean",
                                                "Mediterranean" = "mediterranean",
                                                "Mexican" = "mexican",
                                                "Pizza" = "pizza",
                                                "Sandwiches" = "sandwiches",
                                                "Salad" = "salad",
                                                "Seafood" = "seafood",
                                                "Soup" = "soup",
                                                "Specialty Food" = "specialty_food",
                                                "Steakhouses" = "steakhouses",
                                                "Thai" = "thai",
                                                "Vegetarian" = "vegetarian",
                                                "Vietnamese" = "vietnamese",
                                                "Wine Bars" = "wine_bars"),
                                              selected = "french")),
                              
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Select 3",plotlyOutput("stars_plotly")),
                                      tabPanel("All", plotlyOutput("stars_all"))
                                  )
                              )
                 )),
                          
                 tabPanel("Effect of Restaurant Type on Date Worthiness",
                          h1("Regression"),
                          
                          sidebarLayout(
                              
                              sidebarPanel(
                                  helpText(
                                      "Select up to three cuisines to compare their likelihood of being good date spots on Yelp:"),
                                  
                                  selectInput("cuisinex", 
                                              "Cuisine 1:", 
                                              choices = list(
                                                "American" = "american",
                                                "Asian Fusion" = "asian_fusion",
                                                "Bakeries" = "bakeries",
                                                "Barbeque" = "barbeque",
                                                "Bars" = "bars",
                                                "Breakfast / Brunch" = "breakfast_brunch",
                                                "Burgers" = "burgers",
                                                "Cafes" = "cafes",
                                                "Canadian" = "canadian",
                                                "Chinese" = "chinese",
                                                "Chicken Wings" = "chicken_wings",
                                                "Delis" = "delis",
                                                "Desserts" = "desserts",
                                                "Fast Food" = "fast_food",
                                                "French" = "french",
                                                "Gastropubs" = "gastropubs",
                                                "Ice Cream / Frozen Yogurt" = "ice_cream_frozen_yogurt",
                                                "Indian" = "indian",
                                                "Japanese" = "japanese",
                                                "Italian" = "italian",
                                                "Juice Bars / Smoothies" = "juice_bars_smoothies",
                                                "Korean" = "korean",
                                                "Mediterranean" = "mediterranean",
                                                "Mexican" = "mexican",
                                                "Pizza" = "pizza",
                                                "Sandwiches" = "sandwiches",
                                                "Salad" = "salad",
                                                "Seafood" = "seafood",
                                                "Soup" = "soup",
                                                "Specialty Food" = "specialty_food",
                                                "Steakhouses" = "steakhouses",
                                                "Thai" = "thai",
                                                "Vegetarian" = "vegetarian",
                                                "Vietnamese" = "vietnamese",
                                                "Wine Bars" = "wine_bars"),
                                              selected = "chinese"),
                                  selectInput("cuisiney",
                                              "Cuisine 2:", 
                                              choices = list(
                                                  "American" = "american",
                                                  "Asian Fusion" = "asian_fusion",
                                                  "Bakeries" = "bakeries",
                                                  "Barbeque" = "barbeque",
                                                  "Bars" = "bars",
                                                  "Breakfast / Brunch" = "breakfast_brunch",
                                                  "Burgers" = "burgers",
                                                  "Cafes" = "cafes",
                                                  "Canadian" = "canadian",
                                                  "Chinese" = "chinese",
                                                  "Chicken Wings" = "chicken_wings",
                                                  "Delis" = "delis",
                                                  "Desserts" = "desserts",
                                                  "Fast Food" = "fast_food",
                                                  "French" = "french",
                                                  "Gastropubs" = "gastropubs",
                                                  "Ice Cream / Frozen Yogurt" = "ice_cream_frozen_yogurt",
                                                  "Indian" = "indian",
                                                  "Japanese" = "japanese",
                                                  "Italian" = "italian",
                                                  "Juice Bars / Smoothies" = "juice_bars_smoothies",
                                                  "Korean" = "korean",
                                                  "Mediterranean" = "mediterranean",
                                                  "Mexican" = "mexican",
                                                  "Pizza" = "pizza",
                                                  "Sandwiches" = "sandwiches",
                                                  "Salad" = "salad",
                                                  "Seafood" = "seafood",
                                                  "Soup" = "soup",
                                                  "Specialty Food" = "specialty_food",
                                                  "Steakhouses" = "steakhouses",
                                                  "Thai" = "thai",
                                                  "Vegetarian" = "vegetarian",
                                                  "Vietnamese" = "vietnamese",
                                                  "Wine Bars" = "wine_bars"),
                                              selected = "american"),
                                  selectInput("cuisinez", 
                                              "Cuisine 3:", 
                                              choices = list(
                                                "American" = "american",
                                                "Asian Fusion" = "asian_fusion",
                                                "Bakeries" = "bakeries",
                                                "Barbeque" = "barbeque",
                                                "Bars" = "bars",
                                                "Breakfast / Brunch" = "breakfast_brunch",
                                                "Burgers" = "burgers",
                                                "Cafes" = "cafes",
                                                "Canadian" = "canadian",
                                                "Chinese" = "chinese",
                                                "Chicken Wings" = "chicken_wings",
                                                "Delis" = "delis",
                                                "Desserts" = "desserts",
                                                "Fast Food" = "fast_food",
                                                "French" = "french",
                                                "Gastropubs" = "gastropubs",
                                                "Ice Cream / Frozen Yogurt" = "ice_cream_frozen_yogurt",
                                                "Indian" = "indian",
                                                "Japanese" = "japanese",
                                                "Italian" = "italian",
                                                "Juice Bars / Smoothies" = "juice_bars_smoothies",
                                                "Korean" = "korean",
                                                "Mediterranean" = "mediterranean",
                                                "Mexican" = "mexican",
                                                "Pizza" = "pizza",
                                                "Sandwiches" = "sandwiches",
                                                "Salad" = "salad",
                                                "Seafood" = "seafood",
                                                "Soup" = "soup",
                                                "Specialty Food" = "specialty_food",
                                                "Steakhouses" = "steakhouses",
                                                "Thai" = "thai",
                                                "Vegetarian" = "vegetarian",
                                                "Vietnamese" = "vietnamese",
                                                "Wine Bars" = "wine_bars"),
                                              selected = "italian")),
                              
                              mainPanel(
                                      tabsetPanel(
                                          tabPanel("Select 3",plotlyOutput("date_plotly")),
                                          tabPanel("All", plotlyOutput("date_all"))
                                      )
                          ))),
                        
                 
                 tabPanel("Yelp Business Types",
                          h1("10 Most Popular Business Types"),
                          column(2, imageOutput("types", height = "100%"))),
                 
                 tabPanel("Methods",
                          h1("Models"),
                          p("I created two models: one showing the regression of Yelp user ratings 
                and restaurant types, another showing the regression of date-worthiness
                and restaurant types. Below are the regression tables:"),
                          
                          gt_output("lm_date_gt"),
                          hr(),
                          gt_output("lm_stars_gt"),
                          
                          h1("Data Wrangling"),
                          p("In order to transform the raw data into a format valuable to my analysis, I 
                carried out four steps. First, I subsetted the data to only include restaurants
                and bars. Second, in order to determine the date-worthiness of these establishments, 
                I scraped Yelp user reviews for keywords relating to dating, such as 'girl/boyfriend', 
                'anniversary', and 'romantic. Third, I exluded the cuisines without at least ten reviews. 
                Once I completed these four steps, I was ready to do my regressions."),
                          h1("Regressions"),
                          p("In this project, we are modeling for explanation. What is the relationship between 
                type of cuisine and the likelihood for high Yelp ratings? What is of
                interest here is the coefficient of each cuisine: a positive coefficient suggests a cuisine
                is associated with a positive change of Yelp ratings, and vice versa. The same logic applies 
                for the effect of cuisine on date-worthiness."),
                          
                          p("Observing the confidence intervals, one should look for whether the confidence interval
                for a particular point estimate includes zero. If a 95% confidence interval includes zero, then there
                is no statistically meaningful difference between the groups.The Bayesian interpretation of the 
                confidence intervals suggests that we are 95% certain that the true value is within our upper 
                and lower bounds of our estimate. The Frequentist interpretation of the confidence intervals suggests that 95% 
                of the time that we perform this model, the intervals constructed will contain 
                the true but unknown population parameter.")),

                 tabPanel("About",
                          column(7,
                                 h1("The Project"),
                                 p("What makes a restaurant a good date spot? Will my date like 
                          steak or sandwiches? This project uses a geographically diverse subset 
                          of Yelp data across North America to analyze what type of 
                          restaurants make good date spots. I scrape keywords 
                          relating to dating in Yelp user comments as a proxy to determine 
                          whether a restaurant is a good date spot. The results are useful 
                          not only for individuals trying to impress their dates, but also 
                          for professionals in the restaurant businesses looking to attract more customers."),
                                 
                                 h1("The Data"),
                                 p("The analysis and visualizations from this project are based off of a " , 
                                   a("subset ", href = "https://www.kaggle.com/yelp-dataset/yelp-dataset/version/6#yelp_review.csv"), "of 
                          Yelp's businesses, reviews, and user data. It was originally put together for the Yelp Dataset Challenge 
                          which is a chance for students to conduct research or analysis on Yelp's data and 
                          share their discoveries. In the dataset you'll find information about 
                          businesses across 11 metropolitan areas in four countries: Montreal, QC; Madison, WI; 
                          Pittsburgh, PA; Toronto, ON; Cleveland, OH; Las Vegas, NV; and Charlotte, NC. 
                          The geographic diversity of the cities serve to randomize our Yelp sample."),
                                 
                                 p("You can find the code for this project on my ", 
                                   a("Github.", href = "https://github.com/gspan28")),
                                 
                                 h1("About Me"),
                                 p("My name is Grace Pan, and I am a graduating senior at Harvard 
                          studying Social Studies with an interest in data science."),
                                 p("You can reach me at ",
                                   a("pangrace00@gmail.com",
                                     href = "mailto: pangrace00@gmail.com"),
                                   "or ",
                                   a("LinkedIn.",
                                     href = "https://www.linkedin.com/in/grace-pan-3b0b4513b"))))
                 
                 )
                 
                 
                


# 
#       tabPanel("Map",
#                h1("Las Vegas"),
#                column(2, imageOutput("las_vegas_map", height = "100%")))
#                leafletOutput("leaflet"))




server <- function(input, output) {
    
    # Renders categories head plot
    
    output$types<- renderImage({
        filename  <- normalizePath(file.path('./categories_head_plot.png'))
        
        list(src = filename,
             alt = "Categories Plot", width= "700")
        
    }, deleteFile = FALSE)

    
    # Renders Las Vegas map
    
    # output$leaflet <- renderLeaflet({
    #     LasvegasCoords = business %>% filter(city == "Las Vegas")
    #     
    #     center_lon = median(LasvegasCoords$longitude,na.rm = TRUE)
    #     center_lat = median(LasvegasCoords$latitude,na.rm = TRUE)
    #     
    #     
    #     # Layered coordinates onto National Geographic World Map. Formatted settings of map.
    #     las_vegas_map <- leaflet(LasvegasCoords) %>%
    #         addProviderTiles("Esri.NatGeoWorldMap") %>%
    #         addCircles(lng = ~longitude, lat = ~latitude,radius = ~sqrt(review_count))  %>%
    #         setView(lng=center_lon, lat=center_lat,zoom = 13)
    #     
    #     las_vegas_map
    # })
    
    # Renders stars plotly
    
    output$stars_plotly <- renderPlotly({
        
       stars <- lm_stars_tidy %>% 
            filter(term %in% c(input$cuisine1, input$cuisine2, input$cuisine3)) %>%
            ggplot(aes(x = reorder(term, -estimate), y = estimate, group = 1, 
                       text = paste("Cuisine: ", term,
                                    "<br>Coefficient: ", estimate
                       ))) +
           geom_pointrange(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
           geom_bar(stat = "identity", fill = "light blue" ) +
            labs(title = "Effect of Restaurant Type on User Ratings", subtitle = "Ratings on a 1-5 scale",
                 x = "Type of Food", y = "Coefficient",caption = "Source: Yelp") +
            theme_classic()

        
        ggplotly(stars, tooltip = "text") %>%
            layout(title = list(text = paste0('Effect of Restaurant Type on User Ratings',
                                              '<br>',
                                              '<sup>',
                                              'Ratings on a 1-5 scale',
                                              '</sup>')),
                   annotations = 
                       list(x = 1, y = -.08, text = "Source: Yelp", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=12)))
        })
    
    # Renders stars_all
    
    output$stars_all<- renderPlotly({
        
        stars_all <- lm_stars_tidy %>% 
            ggplot(aes(x = reorder(term, -estimate), y = estimate, group = 1, 
                       text = paste("Cuisine: ", term,
                                    "<br>Coefficient: ", estimate))) +
             geom_pointrange(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
            geom_bar(stat = "identity", fill = "light blue" ) +
            labs(title = "Effect of Restaurant Type on User Ratings", subtitle = "Ratings on a 1-5 scale",
                 x = "Type of Food", y = "Coefficient",caption = "Source: Yelp") +
            theme_classic() +
        theme(axis.text.x= element_blank(),
              axis.ticks.x=element_blank())
        
        
        ggplotly(stars_all, tooltip = "text") %>%
            layout(title = list(text = paste0('Effect of Restaurant Type on User Ratings',
                                              '<br>',
                                              '<sup>',
                                              'Ratings on a 1-5 scale',
                                              '</sup>')),
                   annotations = 
                       list(x = 1, y = -.08, text = "Source: Yelp", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=12)))
    })
    
    # Renders date plotly
    
    output$date_plotly <- renderPlotly({
        date <- lm_date_tidy %>% 
        filter(term %in% c(input$cuisinex, input$cuisiney, input$cuisinez)) %>%
        ggplot(aes(x = reorder(term, -estimate), y = estimate, group = 1, 
                   text = paste("Cuisine: ", term,
                                "<br>Coefficient: ", estimate))) +
        geom_pointrange(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
        geom_bar(stat = "identity", fill = "dark green") +
        labs(title = "Effect of Restaurant Type on Date Appropriateness", subtitle = "Date appropriateness determined by scraping user reviews for date-related keywords",
             x = "Type of Food", y = "Coefficient",caption = "Source: Yelp") +
        theme_classic() 
        
        ggplotly(date, tooltip = "text") %>%
            layout(title = list(text = paste0('Effect of Restaurant Type on Date Appropriateness',
                                              '<br>',
                                              '<sup>',
                                              'Date appropriateness determined by scraping user reviews for date-related keywords',
                                              '</sup>')),
                   annotations = 
                       list(x = 1, y = -.08, text = "Source: Yelp", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=12)))
        })
    
    # Renders date_all
    
    output$date_all<- renderPlotly({
        date_all <- lm_date_tidy %>%
    
        ggplot(aes(x = reorder(term, -estimate), y = estimate, group = 1, 
                   text = paste("Cuisine: ", term,
                                "<br>Coefficient: ", estimate))) +
            geom_bar(stat = "identity", fill = "dark green" ) +
            geom_pointrange(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
            labs(title = "Effect of Restaurant Type on Date Appropriateness", subtitle = "Date appropriateness determined by scraping user reviews for date-related keywords",
                 x = "Type of Food", y = "Coefficient",caption = "Source: Yelp") +
            theme_classic() +
            theme(axis.text.x= element_blank(),
                  axis.ticks.x=element_blank())
        
        
        ggplotly(date_all, tooltip = "text") %>%
            layout(title = list(text = paste0('Effect of Restaurant Type on Date Appropriateness',
                                              '<br>',
                                              '<sup>',
                                              'Date appropriateness determined by scraping user reviews for date-related keywords',
                                              '</sup>')),
                   annotations = 
                       list(x = 1, y = -.10, text = "Source: Yelp", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=12)
                 )
              )
    
        
    })
    
    # Render lm_stars_gt
    
    output$lm_stars_gt <- render_gt({
    
    lm_stars_gt <- lm_stars_tidy %>%
      gt() %>%
      fmt_number(columns = vars(estimate, conf.low), decimals = 3) %>%
      fmt_number(columns = vars(conf.high), decimals = 4) %>%
      tab_header(
        title = "Effect of Type of Restaurant on Restaurant Ratings",
        subtitle = "Data from Yelp") %>%
      cols_label(
        "term" = "Variable",
        "estimate" = "Estimate",
        "conf.low" = "Lower bound",
        "conf.high" = "Upper bound") 
    
    })
  
    
    # Render lm_date_gt
  
    
    output$lm_date_gt <- render_gt({
        
    lm_date_gt <- lm_date_tidy %>%
        gt() %>%
        fmt_number(columns = vars(estimate, conf.low), decimals = 3) %>%
        fmt_number(columns = vars(conf.high), decimals = 4) %>%
        tab_header(
            title = "Effect of Type of Restaurant on Likelihood of Being a Date Spot",
            subtitle = "Data from Yelp") %>%
        cols_label(
            "term" = "Variable",
            "estimate" = "Estimate",
            "conf.low" = "Lower bound",
            "conf.high" = "Upper bound")
    
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
