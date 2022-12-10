#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
intro_page = tabPanel(
  'Introduction',
  titlePanel('Hello')
)

summary_page = tabPanel(
  "Overview",
  titlePanel('Current State of AV Testing'),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = 'disengage_factor',
        label = 'Initiator/Location',
        choices = c('Initiator', 'Location')
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("disengagePlot")
      )
    )
  )


chart_3 = tabPanel(
  'Autonomous Vehicle (AV)',
  p('How has AV testing advanced over the years?'),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = 'av_year',
        label = 'Filter by year:',
        choices = unique(combined_df$Year),
        selected = 2019
      ),
      sliderInput(
        inputId = 'Mpd',
        label = 'Filter by Miles per Disengagement (MPD):',
        min = 0,
        max = 55000,
        value = c(0, 55000)
      ),
      selectInput(
        inputId = 'av_company',
        label = 'Filter by Company:',
        choices = c('All', unique(combined_df$Company))
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3(HTML("<b>AV Testing & Safety Reports, California DMV</b>")),
      plotlyOutput("yearPlot")
    )
  )
)

  
av_testing_page = tabPanel(
  'AV Testing Results',
  tabsetPanel(
  type='tabs',
  summary_page,
  #filter_by_company_page,
  chart_3
  )
)

ui <- navbarPage(
    'Traffic Safety & Collision Analysis',
    intro_page,
    av_testing_page
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$yearPlot = renderPlotly({
      if (input$av_company == 'All') {
        filtered_df = combined_df %>% 
          filter(Year == input$av_year,
                 avg_mpd >= input$Mpd[1],
                 avg_mpd <= input$Mpd[2])
      } else {
        filtered_df = combined_df %>% 
          filter(Year == input$av_year,
                 Company == input$av_company,
                 avg_mpd >= input$Mpd[1],
                 avg_mpd <= input$Mpd[2])
      }
      overview_plot = filtered_df %>% 
        ggplot(aes(x=Total_Mileage, y=Total_Disengagements,
                   size=avg_mpd,
                   color=Company, text=interactive_text)) +
        geom_point(alpha=0.7) +
        theme_bw() +
        xlim(0, 2500000) +
        ylim(0, 3000) +
        labs(x = 'Total Mileage Tested', y = '# of Disengagements',
             title = paste('*Bubble size represents MPD (Hover for details)')) +
        theme(plot.title = element_text(size=10),
              axis.title = element_text(size=10),
              legend.title = element_text(size=10)
        ) +
        guides(size=FALSE)
      overview_plot %>% ggplotly(tooltip = 'interactive_text') %>% 
        layout(width=900, height=400)
                                    
    })

    
  }
    

    

# Run the application 
shinyApp(ui = ui, server = server)
