
library(shiny)
library(bslib)
library(ggplot2)
library(plotly)

chess <- read.csv("chess_tournament_clean.csv")

chess$Rating_Difference <-
  chess$Avg_Opponent_Rating - chess$Pre_Rating

total_players <- nrow(chess)

rating_cor <- cor(
  chess$Pre_Rating,
  chess$Avg_Opponent_Rating
)

performance_cor <- cor(
  chess$Rating_Difference,
  chess$Total_Points
)

most_common_score <- as.numeric(
  names(which.max(table(chess$Total_Points)))
)

state_stats <- aggregate(
  Total_Points ~ State,
  data = chess,
  FUN = function(x) {
    c(
      Players = length(x),
      Average = mean(x),
      Total = sum(x)
    )
  }
)

state_rankings <- data.frame(
  State = state_stats$State,
  Players = state_stats$Total_Points[, "Players"],
  Average_Points = round(
    state_stats$Total_Points[, "Average"],
    2
  ),
  Total_Points = state_stats$Total_Points[, "Total"]
)

state_rankings <- state_rankings[
  order(
    -state_rankings$Average_Points,
    -state_rankings$Players,
    state_rankings$State
  ),
]

player_rankings <- chess[
  order(
    chess$State,
    -chess$Total_Points,
    -chess$Pre_Rating
  ),
]

player_rankings$State_Rank <- ave(
  -player_rankings$Total_Points,
  player_rankings$State,
  FUN = function(x) {
    match(x, sort(unique(x)))
  }
)

total_states <- length(unique(chess$State))

ui <- page_fluid(
  
  theme = bs_theme(
    version = 5,
    bg = "#0D1728",
    fg = "#F1F5F9",
    primary = "#49D8BC",
    base_font = font_google("Inter")
  ),
  
  
  tags$head(
    tags$style(HTML("
    body {
      padding: 25px;
    }

    h1 {
      font-weight: 800;
    }

    .card {
      background: #17273A;
      border: 1px solid #2B4055;
      border-radius: 15px;
      margin-bottom: 20px;
      overflow: hidden;
    }

    .card-header {
      background: transparent;
      color: #49D8BC;
      font-weight: bold;
      padding: 15px;
    }

    .form-control,
    .selectize-input {
      background: #FFFFFF !important;
      color: #17273A !important;
      border: 2px solid #49D8BC !important;
      border-radius: 8px !important;
    }

    .selectize-input input,
    .selectize-input .item {
      color: #17273A !important;
    }

    .selectize-dropdown {
      background: #FFFFFF !important;
      color: #17273A !important;
      border: 1px solid #49D8BC !important;
    }

    .selectize-dropdown .option {
      background: #FFFFFF !important;
      color: #17273A !important;
      padding: 10px 15px;
    }

    .selectize-dropdown .option.active,
    .selectize-dropdown .option.selected {
      background: #49D8BC !important;
      color: #17273A !important;
      font-weight: bold;
    }

    .table {
      --bs-table-color: #F1F5F9;
      --bs-table-bg: #17273A;
      --bs-table-striped-color: #FFFFFF;
      --bs-table-striped-bg: #263E53;
      --bs-table-border-color: #40566C;
    }

    .table thead th {
      background: #304D63;
      color: #49D8BC;
    }

    .dashboard-footer {
      text-align: center;
      padding: 30px 15px;
      margin-top: 35px;
      border-top: 1px solid #40566C;
      color: #A6B5C9;
      font-size: 13px;
    }

    .dashboard-footer p {
      margin-bottom: 8px;
    }

    .dashboard-footer strong {
      color: #49D8BC;
    }
  "))
  ),
  
  
  div(
    style = "margin-bottom: 30px;",
    
    h5(
      "TOURNAMENT INTELLIGENCE",
      style = "color: #49D8BC;"
    ),
    
    h1("Beyond the Chessboard"),
    
    p(
      "Interactive Chess Tournament Analytics",
      style = "color: #A6B5C9;"
    )
  ),
  
  layout_columns(
    
    value_box(
      "Total Players",
      total_players,
      theme = "primary"
    ),
    
    value_box(
      "Most Common Score",
      most_common_score,
      theme = "primary"
    ),
    
    value_box(
      "Rating Correlation",
      round(rating_cor, 3),
      theme = "primary"
    ),
    
    value_box(
      "Performance Correlation",
      round(performance_cor, 3),
      theme = "primary"
    ),
    
    col_widths = c(3, 3, 3, 3)
  ),
  
  layout_columns(
    
    card(
      card_header(
        "Player Rating vs. Opponent Strength"
      ),
      
      plotlyOutput(
        "rating_scatter",
        height = "350px"
      )
    ),
    
    card(
      card_header(
        "Tournament Points Distribution"
      ),
      
      plotlyOutput(
        "points_chart",
        height = "350px"
      )
    ),
    
    col_widths = c(6, 6)
  ),
  
  card(
    
    card_header("Player Explorer"),
    
    layout_sidebar(
      
      sidebar = sidebar(
        selectInput(
          "player",
          "Select a player",
          choices = chess$Player_Name,
          selected = chess$Player_Name[1]
        )
      ),
      
      h3(
        textOutput("selected_name")
      ),
      
      tableOutput("player_info"),
      
      plotlyOutput(
        "player_chart",
        height = "350px"
      )
    )
  ),
  
  card(
    
    card_header(
      "State & Regional Rankings"
    ),
    
    p(
      "Compare average tournament performance ",
      "across states and regions. Explore player ",
      "rankings within each location."
    ),
    
    layout_columns(
      
      value_box(
        "States / Regions Represented",
        total_states,
        theme = "primary"
      ),
      
      value_box(
        "Tournament Players",
        total_players,
        theme = "primary"
      ),
      
      col_widths = c(6, 6)
    ),
    
    layout_columns(
      
      card(
        card_header(
          "Average Tournament Points by State"
        ),
        
        plotlyOutput(
          "state_chart",
          height = "450px"
        )
      ),
      
      card(
        card_header(
          "State Leaderboard"
        ),
        
        p(
          "Ranked by average tournament points. ",
          "Participant counts are included for context."
        ),
        
        tableOutput("state_table")
      ),
      
      col_widths = c(7, 5)
    ),
    
    hr(),
    
    h4("Player Rankings by State"),
    
    p(
      "Select a state or region to view its ",
      "players ranked by tournament points. ",
      "Players with equal points share a rank."
    ),
    
    selectInput(
      "selected_state",
      "Select a state or region:",
      choices = sort(unique(chess$State)),
      selected = if ("MI" %in% chess$State) {
        "MI"
      } else {
        sort(unique(chess$State))[1]
      }
    ),
    
    tableOutput("state_players")
  ),
  
  tags$footer(
    
    class = "dashboard-footer",
    
    tags$p(
      tags$strong("Beyond the Chessboard")
    ),
    
    tags$p(
      "Developed by Mohd Afwan Shaikh | DATA 607"
    ),
    
    tags$p(
      tags$strong("AI Acknowledgment: "),
      "ChatGPT (OpenAI) was used to
     assist with debugging this interactive dashboard."
    ),
    
    tags$p(
      "Data analysis and project implementation: ",
      "Mohd Afwan Shaikh"
    )
  )
)

server <- function(input, output, session) {
  
  selected_player <- reactive({
    chess[
      chess$Player_Name == input$player,
    ]
  })
  
  output$rating_scatter <- renderPlotly({
    
    p <- ggplot(
      chess,
      aes(
        x = Pre_Rating,
        y = Avg_Opponent_Rating,
        text = paste0(
          "Player: ", Player_Name,
          "<br>Rating: ", Pre_Rating,
          "<br>Opponent Average: ",
          Avg_Opponent_Rating
        )
      )
    ) +
      
      geom_point(
        color = "#49D8BC",
        size = 2.5,
        alpha = 0.8
      ) +
      
      geom_smooth(
        aes(
          x = Pre_Rating,
          y = Avg_Opponent_Rating
        ),
        method = "lm",
        se = FALSE,
        color = "#E8A75E"
      ) +
      
      labs(
        x = "Player Pre-Tournament Rating",
        y = "Average Opponent Rating"
      ) +
      
      theme_minimal() +
      
      theme(
        text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")
      )
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      layout(
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        
        font = list(
          color = "black"
        ),
        
        margin = list(
          l = 75,
          r = 25,
          b = 75,
          t = 35
        )
      )
  })
  
  output$points_chart <- renderPlotly({
    
    counts <- as.data.frame(
      table(chess$Total_Points)
    )
    
    names(counts) <- c(
      "Points",
      "Players"
    )
    
    p <- ggplot(
      counts,
      aes(
        x = Points,
        y = Players,
        text = paste0(
          "Points: ", Points,
          "<br>Players: ", Players
        )
      )
    ) +
      
      geom_col(
        fill = "#49D8BC",
        width = 0.7
      ) +
      
      geom_text(
        aes(label = Players),
        vjust = -0.5,
        color = "black",
        size = 4
      ) +
      
      scale_y_continuous(
        expand = expansion(
          mult = c(0, 0.15)
        )
      ) +
      
      labs(
        x = "Tournament Points",
        y = "Number of Players"
      ) +
      
      theme_minimal() +
      
      theme(
        text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")
      )
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      layout(
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        
        font = list(
          color = "black"
        ),
        
        margin = list(
          l = 65,
          r = 25,
          b = 75,
          t = 35
        )
      )
  })
  
  output$selected_name <- renderText({
    selected_player()$Player_Name
  })
  
  output$player_info <- renderTable({
    
    player <- selected_player()
    
    data.frame(
      State = player$State,
      Points = player$Total_Points,
      Pre_Rating = player$Pre_Rating,
      Opponent_Average =
        player$Avg_Opponent_Rating,
      Rating_Difference =
        player$Rating_Difference
    )
    
  },
  striped = TRUE,
  bordered = FALSE
  )
  
  output$player_chart <- renderPlotly({
    
    player <- selected_player()
    
    ratings <- data.frame(
      Category = c(
        "Player Rating",
        "Opponent Average"
      ),
      
      Rating = c(
        player$Pre_Rating,
        player$Avg_Opponent_Rating
      )
    )
    
    p <- ggplot(
      ratings,
      aes(
        x = Category,
        y = Rating,
        fill = Category
      )
    ) +
      
      geom_col(
        width = 0.5
      ) +
      
      geom_text(
        aes(label = Rating),
        vjust = 1.5,
        color = "black",
        size = 5,
        fontface = "bold"
      ) +
      
      scale_fill_manual(
        values = c(
          "Opponent Average" = "#49D8BC",
          "Player Rating" = "#E8A75E"
        )
      ) +
      
      scale_y_continuous(
        limits = c(
          0,
          max(ratings$Rating) * 1.15
        )
      ) +
      
      labs(
        x = NULL,
        y = "Rating"
      ) +
      
      theme_minimal() +
      
      theme(
        legend.position = "none",
        
        text = element_text(
          color = "black"
        ),
        
        axis.text = element_text(
          color = "black"
        ),
        
        axis.title = element_text(
          color = "black"
        )
      )
    
    ggplotly(p) %>%
      
      layout(
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        
        font = list(
          color = "black",
          size = 13
        ),
        
        margin = list(
          l = 75,
          r = 25,
          b = 90,
          t = 35
        )
      )
  })
  
  output$state_chart <- renderPlotly({
    
    p <- ggplot(
      state_rankings,
      aes(
        x = reorder(
          State,
          Average_Points
        ),
        
        y = Average_Points,
        
        text = paste0(
          "State / Region: ", State,
          "<br>Players: ", Players,
          "<br>Average Points: ", Average_Points,
          "<br>Total Points: ", Total_Points
        )
      )
    ) +
      
      geom_col(
        fill = "#49D8BC",
        width = 0.65
      ) +
      
      coord_flip() +
      
      labs(
        x = "State / Region",
        y = "Average Tournament Points"
      ) +
      
      theme_minimal() +
      
      theme(
        text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black")
      )
    
    ggplotly(
      p,
      tooltip = "text"
    ) %>%
      
      layout(
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        
        font = list(
          color = "black"
        ),
        
        margin = list(
          l = 65,
          r = 25,
          b = 65,
          t = 35
        )
      )
  })
  
  output$state_table <- renderTable({
    
    state_rankings
    
  },
  digits = 2,
  striped = TRUE,
  bordered = FALSE
  )
  
  output$state_players <- renderTable({
    
    req(input$selected_state)
    
    selected <- player_rankings[
      player_rankings$State == input$selected_state,
    ]
    
    data.frame(
      Rank = as.integer(selected$State_Rank),
      Player = selected$Player_Name,
      Points = selected$Total_Points,
      Pre_Rating = selected$Pre_Rating,
      Opponent_Average =
        selected$Avg_Opponent_Rating
    )
    
  },
  striped = TRUE,
  bordered = FALSE
  )
}

shinyApp(
  ui = ui,
  server = server
)
