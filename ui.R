# Aggro druid progress shiny app.
# UI file using the navbar style

library(shiny)
library(shinythemes)

# Fluid page
fluidPage(
  title = "Druid Play Analysis",
  theme = shinytheme("flatly"),
  
  titlePanel(
    title = "Druid Play Analysis"
  ),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      
      width = 3,
      
      # Rank selector
      numericInput(
        "rank_sel",
        "Start analysis from rank",
        value = 25,
        min = 5, max = 25, step = 5
      ),
      
      # Class selector
      selectInput(
        "class_sel",
        "Select the class/classes to analyze:",
        choices = list(
          'Druid',
          'Warrior',
          'Hunter',
          'Mage',
          'Warlock',
          'Priest',
          'Shaman',
          'Rogue',
          'Paladin'
        ),
        multiple = TRUE,
        selected = c('Druid', 'Warrior', 'Hunter', 'Mage', 'Warlock',
                     'Priest', 'Shaman', 'Rogue', 'Paladin')
      )
    ),
    
    # Main panel
    mainPanel(
      
      # Tabset Panel
      tabsetPanel(
        # Tab panel for rank progress
        tabPanel(
          "Rank progression",
          icon = icon("line-chart"),
          
          # row
          fluidRow(
            # empty column
            # column(4),
            # rank plot column
            column(
              8,
              plotOutput("rank_plot", height = '600px')
            )
          )
        ),
        
        # Tab planel for Archetypes statistics
        tabPanel(
          "Archetypes & Classes",
          icon = icon("users"),
          
          # row
          fluidRow(
            # Ratio plot
            column(
              12,
              plotOutput("ratios", height = '700px')
            )
          )
        ),
        
        # Tab Panel for Cards
        tabPanel(
          "Cards statistics",
          icon = icon("id-card"),
          
          # row
          fluidRow(
            # empty column
            # column(4),
            # rank plot column
            column(
              8,
              plotOutput("cards_plot", height = '600px')
            )
          )
        ),
        
        # Tab panel for probabilistic plots
        tabPanel(
          "Probabilities",
          icon = icon("calculator"),
          
          # row
          fluidRow(
            # empty column
            # column(4),
            # rank plot column
            column(
              7,
              plotOutput("prob_plot", height = '600px')
            ),
            column(
              5,
              plotOutput("turn_hist", height = '600px')
            )
          )
        ),
        
        # Tab panel for probabilistic plots 2
        tabPanel(
          "Probabilities II",
          icon = icon("calculator"),
          
          # row
          fluidRow(
            # empty column
            # column(4),
            # rank plot column
            column(
              12,
              plotOutput("prob_per_arch", height = '700px')
            )
          )
        )
      )
    )
  )
)





##### OLD ######################################################################
# # Navbar
# navbarPage("Aggro Druid Analysis", id = 'aggro',
#            theme = shinytheme("flatly"),
#            
#   # Tab panel for achievements, as value boxes
#   # I need flexdashboards to add valueBoxes, I dunno if is worthy :(
#   
#   # Tab panel for rank progress
#   tabPanel(
#     "Rank progression",
#     icon = icon("line-chart"),
# 
#     # row
#     fluidRow(
#       # empty column
#       # column(4),
#       # rank plot column
#       column(
#         8,
#         plotOutput("rank_plot", height = '600px')
#       )
#     )
#   ),
# 
#   # Tab planel for Archetypes statistics
#   tabPanel(
#     "Archetypes & Classes",
#     icon = icon("users"),
# 
#     # row
#     fluidRow(
#       # Ratio plot
#       column(
#         12,
#         plotOutput("ratios", height = '700px')
#       )
#     )
#   ),
# 
#   # Tab Panel for Cards
#   tabPanel(
#     "Cards statistics",
#     icon = icon("id-card"),
# 
#     # row
#     fluidRow(
#       # empty column
#       # column(4),
#       # rank plot column
#       column(
#         8,
#         plotOutput("cards_plot", height = '600px')
#       )
#     )
#   ),
# 
#   # Tab panel for probabilistic plots
#   tabPanel(
#     "Probabilities",
#     icon = icon("calculator"),
# 
#     # row
#     fluidRow(
#       # empty column
#       # column(4),
#       # rank plot column
#       column(
#         7,
#         plotOutput("prob_plot", height = '600px')
#       ),
#       column(
#         5,
#         plotOutput("turn_hist", height = '600px')
#       )
#     )
#   ),
# 
#   # Tab panel for probabilistic plots 2
#   tabPanel(
#     "Probabilities II",
#     icon = icon("calculator"),
# 
#     # row
#     fluidRow(
#       # empty column
#       # column(4),
#       # rank plot column
#       column(
#         12,
#         plotOutput("prob_per_arch", height = '700px')
#       )
#     )
#   )
# )
