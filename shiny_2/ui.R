fluidPage(
  navbarPage(
    title="Welcome!",
    id = 'index',
    tabPanel("New_user", icon=icon("home")),
    tabPanel("Who_are_you"),
    tabPanel("Douban_Guess")
  ),
  ###main block
  
  ###Douban_Guess block
  conditionalPanel(
    condition = 'input.index == "Douban_Guess"',
    numericInput("n", "score",min = 1, value = 5),
    #textOutput('otx', container=pre),
    uiOutput(outputId = "image"),
    #actionButton('btn', 'button', icon=icon('play-circle')),
    actionButton("refresh", "Refresh movie"),
    actionButton('save_inputs', 'Save inputs'),
    tabsetPanel(
      id='YOU_MIGHT_LIKE', type='pills',
      tabPanel('Close YML'),
      tabPanel('Open YML')
    ),
    
    ###You_might_light_block(connect with server!)
    conditionalPanel(
      condition = 'input.YOU_MIGHT_LIKE == "Open YML"',
      tabsetPanel(
        id='mainpan', type='pills',
        tabPanel('BY MOVIE'),
        tabPanel('BY USER')
      ),
      ###BY_MOVIE_block
      conditionalPanel(
        condition = 'input.mainpan == "BY MOVIE"',
        uiOutput(outputId = "movie_might1"),
        actionButton('rate_movie1', 'score this!'),
        
        conditionalPanel(
          condition = 'input.rate_movie1===1',
          numericInput("n_might1", "score",min = 1, value = 5)), ###small block!
  
        uiOutput(outputId = "movie_might2"),
        uiOutput(outputId = "movie_might3"),
        uiOutput(outputId = "movie_might4"),
        uiOutput(outputId = "movie_might5")
      )
    )
  ),
  
  ###user_analyse block(connect with server!)
  conditionalPanel(
    condition = 'input.index == "Who_are_you"',
    plotOutput('hist'),
    tableOutput('table')
  )
)