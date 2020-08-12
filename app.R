# Imports and config

library(tidyverse)
library(dplyr)
library(vroom)
library(scales)
library(lubridate)
library(ggthemes)
library(ggtext)
library(glue)
library(gt)
library(paletteer)
library(jsonlite)

library(shiny)
library(shinydashboard)
library(shinycssloaders)

options(dplyr.summarise.inform=F)

theme_set(theme_minimal() +
              theme(text = element_text(family = 'Helvetica', size = 16),
                    axis.text = element_text(face = 'bold', size = 14),
                    strip.background = element_rect(fill = '#d9d9d950'),
                    strip.text = element_text(face = 'bold', size = 12),
                    plot.title = element_markdown(),
                    plot.subtitle = element_markdown(),
                    plot.caption = element_markdown()))

# Processing code
STEAM_API_KEY <- "86493034E4F067BDDF2B880936C1CDE8"

rank_list <- c('Silver 1', 'Silver 2', 'Silver 3', 'Silver 4', 'Silver Elite', 'Silver Elite Master',
               'Gold Nova 1', 'Gold Nova 2', 'Gold Nova 3', 'Gold Nova Master', 
               'Master Guardian 1', 'Master Guardian 2', 'Master Guardian Elite', 'Distinguished Master Guardian',
               'Legendary Eagle', 'Legendary Eagle Master',
               'Supreme Master First Class', 'The Global Elite')

names(rank_list) <- c(1:18)

## Plotting functions

# 1. Map distribution
get_map_dist <- function(match_list, nickname, map_labels) {
    
    map_count <- match_list %>%
        group_by(Map) %>%
        count()
    
    map_dist <- match_list %>%
        group_by(Map, Win_Loss) %>%
        count() %>%
        ggplot(aes(x = Map, y = n)) +
        geom_col(aes(fill = Win_Loss), position = 'fill', alpha = 0.95) +
        geom_text(data = map_count, aes(x = Map, label = n, y = 1), 
                  colour = 'white', family = 'Helvetica', fontface = 'bold', size = 5, vjust = 1) +
        scale_y_continuous(labels = percent_format()) +
        scale_x_discrete(name = NULL, labels = map_labels) +
        scale_fill_tableau(palette = 'Traffic') +
        labs(x = 'Map', y = 'Percentage', title = glue('Map distribution of <span style="color:#3270ab">**{nickname}**</span>'), 
             subtitle = '**Number** indicates *total times map was played*', caption = 'Graphic by *@lakshyaag*')+
        theme(axis.text.x = element_markdown(color = "black", size = 11),
              legend.position = 'none',
              plot.title = element_markdown(),
              plot.subtitle = element_markdown(), 
              plot.caption = element_markdown(),
              panel.grid.major.x = element_blank(),
              text = element_text(family = "Helvetica", size = 16))
    
    return(map_dist)
}

# 2. Rank Distribution
get_rank_dist <- function(match_list, nickname, rank_labels) {
    rank_dist <- match_list %>% 
        group_by(Rank, RankName, Map) %>%
        count() %>%
        mutate(Rank = as.factor(Rank)) %>%
        ggplot(aes(x = Rank, y = n, label = n, fill = Map)) +
        geom_col(width = 0.5, position = position_dodge(width = 0.5)) +
        geom_hline(yintercept = 0, color = '#000000', size = 1) +
        #geom_text(vjust = -0.5, family = 'Helvetica', fontface = 'bold', color = 'black', size = 4, position = position_dodge(0.75)) +
        scale_y_continuous(breaks = pretty_breaks(10)) +
        scale_x_discrete(name = NULL, labels = rank_labels) +
        scale_fill_tableau(palette = 'Tableau 10') +
        labs(x = 'Rank', y = 'Number of matches played', title = glue('Rank distribution of <span style="color:#3270ab">**{nickname}**</span>'),
             caption = 'Graphic by *@lakshyaag*', fill = "Map") +
        theme(axis.text.x = element_markdown(color = "black", size = 11),
              legend.position = 'top',
              plot.title = element_markdown(),
              plot.subtitle = element_markdown(), 
              plot.caption = element_markdown(),
              panel.grid.major.x = element_blank(),
              text = element_text(family = "Helvetica", size = 16))

    return(rank_dist)
}

# 3. Kill distribution
get_kill_dist <- function(match_list, nickname, map_labels) {
    
    map_count <- match_list %>%
        group_by(Map) %>%
        count()
    
    kill_dist <- match_list %>%
        ggplot(aes(x = Map, y = K, fill = Map)) +
        geom_boxplot(outlier.shape = NA) +
        geom_text(data = map_count, aes(y = 0, label = n), 
                  vjust = -0.5, family = 'Helvetica', fontface = 'bold', color = 'black', size = 4) +
        geom_hline(yintercept = 0, color = '#000000', size = 1) +
        scale_y_continuous(breaks = pretty_breaks(10)) +
        scale_x_discrete(name = NULL, labels = map_labels) +
        scale_fill_tableau() +
        labs(x = 'Map', y = 'Kills', title = glue('Kill distribution of <span style="color:#3270ab">**{nickname}**</span> by *map*'),
             caption = 'Graphic by *@lakshyaag*', subtitle = '**Number** indicates total times map was played') +
        theme(axis.text.x = element_markdown(color = "black", size = 11),
              legend.position = 'none',
              panel.grid.major.x = element_blank(),
              text = element_text(family = "Helvetica", size = 16))
    
    return(kill_dist)
}

# 4. Clutches by map
get_clutch_map <- function(match_list, nickname, map_labels) {
    
    clutch_map_dist <- match_list %>%
        pivot_longer(cols = '1v5':'1v1', names_to = 'clutches', values_to = 'number_of_clutches') %>%
        select(Map, clutches, number_of_clutches, `Match Links`, Rank, RankName) %>%
        group_by(Map, clutches) %>%
        summarise(n = sum(number_of_clutches)) %>%
        ggplot(aes(x = Map, y = n, fill = clutches)) +
        geom_col(position = position_stack()) +
        scale_y_continuous(breaks = pretty_breaks(10)) +
        scale_x_discrete(name = NULL, labels = map_labels) +
        scale_fill_tableau() +
        labs(x = 'Map', y = 'Number of clutches', title = glue('Clutch distribution of <span style="color:#3270ab">**{nickname}**</span> by *map*'),
             caption = 'Graphic by *@lakshyaag*', fill = "Clutch Type") +
        theme(axis.text.x = element_markdown(color = "black", size = 11),
              legend.position = 'top',
              panel.grid.major.x = element_blank(),
              text = element_text(family = "Helvetica", size = 16))
    
    return(clutch_map_dist)
}

# 5. Clutches by rank
get_clutch_rank <- function(match_list, nickname, rank_labels) {
    
    clutch_rank_dist <- match_list %>%
        pivot_longer(cols = '1v5':'1v1', names_to = 'clutches', values_to = 'number_of_clutches') %>%
        select(Map, clutches, number_of_clutches, `Match Links`, Rank, RankName) %>%
        group_by(Rank, RankName, clutches) %>%
        mutate(Rank = as.factor(Rank)) %>%
        summarise(n = sum(number_of_clutches)) %>%
        ggplot(aes(x = Rank, y = n, fill = clutches)) +
        geom_col(position = position_stack()) +
        scale_y_continuous(breaks = pretty_breaks(10)) +
        scale_x_discrete(name = NULL, labels = rank_labels) +
        scale_fill_tableau() +
        labs(x = 'Rank', y = 'Number of clutches', 
             title = glue('Clutch distribution of <span style="color:#3270ab">**{nickname}**</span> by *rank*'),
             caption = 'Graphic by *@lakshyaag*', fill = "Clutch Type") +
        theme(axis.text.x = element_markdown(color = "black", size = 11),
              legend.position = 'top',
              panel.grid.major.x = element_blank(),
              text = element_text(family = "Helvetica", size = 16))
    
    return(clutch_rank_dist)
}

# 6. First K/D by map
get_fkd_map <- function(match_list, scoreboard, nickname, map_labels) {
    
    fkd_map <- scoreboard %>%
        pivot_longer(cols = c(FK_T, FK_CT, FD_T, FD_CT), names_to = c('KorD', 'Side'), names_sep = "_", values_to = "KillsorDeaths")%>%
        select(Name, KorD, Side, KillsorDeaths, `Match ID`) %>%
        left_join(y = match_list %>% select(Map, `Match Links`), by = c("Match ID" = "Match Links")) %>%
        mutate(KorD = case_when(KorD == "FK" ~ "First Kills",
                                KorD == "FD" ~ "First Deaths",
                                TRUE ~ KorD)) %>%
        mutate(KorD = factor(KorD, levels = c("First Kills", "First Deaths"))) %>%
        group_by(Map, KorD, Side) %>%
        summarise(KillsorDeaths = sum(KillsorDeaths)) %>%
        ggplot(aes(x = Map, y = KillsorDeaths, fill = Side)) +
        geom_col(position = position_dodge2(10)) +
        scale_x_discrete(name = NULL, labels = map_labels) +
        scale_y_continuous(breaks = pretty_breaks()) +
        scale_fill_tableau() +
        labs(y = "", title = glue('First kills/deaths of <span style="color:#3270ab">**{nickname}**</span> by map'), 
             subtitle = "Data from last 10 matches", caption = glue("Graphic by *@lakshyaag*")) +
        facet_wrap(~KorD, nrow = 2) +
        theme(axis.text.x = element_markdown(color = "black", size = 11),
              legend.position = 'top',
              panel.grid.major.x = element_blank(),
              strip.background = element_rect(fill = '#d9d9d950', linetype = 'blank'))
    
    return(fkd_map)
}

# 7. Kill combos
get_kill_combo <- function(match_list, scoreboard, nickname, map_labels) {
    
    kill_combo <- scoreboard %>%
        pivot_longer(cols = c('5k':'1k'), names_to = "kill_combo", values_to = "count") %>%
        select(Name, kill_combo, count, `Match ID`) %>%
        left_join(y = match_list %>% select(Map, `Match Links`), by = c("Match ID" = "Match Links")) %>%
        mutate(kill_combo = case_when(kill_combo == "5k" ~ "Ace",
                                      kill_combo == "4k" ~ "Quad Kill",
                                      kill_combo == "3k" ~ "Triple Kill",
                                      kill_combo == "2k" ~ "Double Kill",
                                      kill_combo == "1k" ~ "Single Kill")) %>%
        mutate(kill_combo = factor(kill_combo, levels = c("Single Kill", "Double Kill", "Triple Kill", "Quad Kill", "Ace"))) %>%
        group_by(Map, kill_combo) %>%
        summarise(count = sum(count)) %>%
        ggplot(aes(x = Map, y = count, fill = kill_combo)) +
        geom_col(position = position_dodge2(5)) +
        scale_x_discrete(name = NULL, labels = map_labels) +
        scale_y_continuous(breaks = pretty_breaks(5)) +
        scale_fill_tableau() +
        labs(y = "Count", fill = "", title = glue('Count of *kill combos* of <span style="color:#3270ab">**{nickname}**</span> by map'), 
             subtitle = "Data from last 10 matches", caption = glue("Graphic by *@lakshyaag*")) +
        theme(axis.text.x = element_markdown(color = "black", size = 11),
              legend.position = 'top',
              panel.grid.major.x = element_blank())
    
    return(kill_combo) 
}

# 8. Performance Summary Table
get_performance_table <- function(match_list, nickname) {
    summary_table_data <- match_list %>% 
        mutate(RankName = fct_reorder(RankName, Rank)) %>%
        group_by(Map, RankName) %>%
        summarise(across(.cols = c(K:ADR, -`HS%`), .fns = ~round(mean(.))), 
                  across(.cols = c(`1v5`:`1v1`), .fns = sum), 
                  Rating = mean(Rating),
                  `HS%` = mean(`HS%`),
                  `Number of Matches` = n()) %>%
        mutate('K/D Ratio' = K/D) %>%
        relocate('K/D Ratio', .after = RankName) %>%
        relocate('Number of Matches', .before = 'K/D Ratio') %>%
        select(-K, -D) %>%
        rename('Assists' = A, 'K/D Difference' = `+/-`, 'HLTV Rating 1.0' = Rating)
    
        summary_table <- summary_table_data %>%
            gt(
                groupname_col = 'RankName',
                rowname_col = 'Map'
            ) %>%
            tab_header(
                title = md(glue("CS:GO Performance of ***{nickname}***")),
                subtitle = md(glue('**{last(match_list$Date) %>% str_remove_all(pattern = "(th)|(st)|(nd)|(rd)") %>% as_date(format = "%a, %d %b") %>% format("%d %B")}** to **{first(match_list$Date)}**'))
            ) %>%
            fmt_number(
                columns = vars('K/D Ratio', 'HLTV Rating 1.0'),
                decimals = 2
            ) %>%
            fmt_percent(
                columns = vars('HS%'), 
                scale_values = FALSE
            ) %>%
            tab_spanner(
                label = "Number of clutches",
                columns = vars(`1v5`, `1v4`, `1v3`, `1v2`, `1v1`)
            ) %>%
            tab_spanner(
                label = "Average Performance",
                columns = vars('K/D Ratio', 'Assists', 'K/D Difference', 'ADR', 'HS%')
            ) %>%
            cols_align(align = 'right', columns = TRUE) %>%
            tab_footnote(
                footnote = md('Calcuated as **Kills - Deaths**'),
                locations = cells_column_labels(
                    columns = vars('K/D Difference')
                )
            ) %>%
            tab_footnote(
                footnote = md('Total clutches for given ***map-rank*** combination'),
                locations = cells_column_spanners(
                    spanners = 'Number of clutches'
                )
            ) %>%
            tab_footnote(
                footnote = md('Average of *Rating 1.0* in difference matches'),
                locations = cells_column_labels(
                    columns = vars('HLTV Rating 1.0')
                )
            ) %>%
            tab_source_note(
                source_note = md('Table by ***@lakshyaag***')
            ) %>%
            summary_rows(
                groups = TRUE,
                columns = vars(`Number of Matches`, `1v5`, `1v4`, `1v3`, `1v2`, `1v1`),
                fns = list(TOTAL = 'sum'),
                formatter = fmt_number,
                decimals = 0,
                use_seps = TRUE
            ) %>%
            grand_summary_rows(
                columns = vars(`Number of Matches`, `1v5`, `1v4`, `1v3`, `1v2`, `1v1`),
                fns = list(`GRAND TOTAL` = 'sum'),
                formatter = fmt_number,
                decimals = 0,
                use_seps = TRUE
            ) %>%
            row_group_order(
                groups = levels(summary_table_data$RankName)
            ) %>%
            data_color(
                columns = vars('K/D Ratio', 'ADR', 'HLTV Rating 1.0'),
                colors = col_numeric(
                    palette = paletteer_d(
                        palette = "ggsci::teal_material"
                    ) %>% as.character(),
                    domain = NULL
                ),
                alpha = 0.8
            ) %>%
            tab_style(
                style = list(
                    cell_borders(sides = c('bottom'), color = '#787878'),
                    cell_text(weight = 'bold')
                ),
                locations = cells_column_spanners(spanners = c('Number of clutches', 'Average Performance'))
            ) %>%
            opt_table_font(font = "Product Sans") %>%
            opt_all_caps(locations = c('column_labels', 'row_group')) %>%
            tab_options(
                summary_row.background.color = "#9cd6c4",
                grand_summary_row.background.color = "#00877a",
                row_group.background.color = "#ffb42d",
                heading.background.color = "#ffdbba",
                column_labels.background.color = "#ffdbba",
                stub.background.color = "#ffdbba",
                table.font.color = "#1a1a1a",
                table_body.hlines.color = "#989898",
                table_body.border.top.color = "#989898",
                heading.border.bottom.color = "#989898",
                row_group.border.top.color = "#989898",
                row_group.border.bottom.style = "none",
                stub.border.style = "dashed",
                stub.border.color = "#989898",
                stub.border.width = "2px",
                stub.font.weight = 'bold',
                summary_row.border.color = "#989898",
                table.width = '100%'
            )
        
        return(summary_table)
}

# ui.R

header <- dashboardHeader(
    title = 'CSGO Stats'
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Player Analysis", tabName = 'player_analysis', icon = icon('info')),
        menuItem(""),
        menuItem('Created by Lakshya Agarwal', href = 'https://github.com/lakshyaag/', newtab = T, 
                 icon = icon('magic')),
        menuItem('Check out the code!', href = 'https://github.com/lakshyaag/csgostats', newtab = T, 
                 icon = icon('github'), badgeLabel = 'New', badgeColor = 'aqua')
    )
)

body <- dashboardBody(
    tabItems(
        tabItem('player_analysis',
                fluidRow(
                    box(width = 12, status = 'success',
                        selectInput('player_id', 'Player ID',
                                    choices = c('76561198236436482', '76561198064711336'), 
                                    selected = '76561198236436482')
                        # selectInput('nickname', 'Nickname',
                        #             choices = c('lazyG', 'recker'), 
                        #             selected = 'lazyG'),
                        
                    )
                ),
                
                fluidRow(
                    tabBox(width = 12,
                           tabPanel("Map Distribution",
                                    plotOutput('map_dist') %>% withSpinner()),
                           tabPanel("Rank Distribution",
                                    plotOutput('rank_dist') %>% withSpinner()),
                           tabPanel("Kill Distribution",
                                    plotOutput('kill_dist') %>% withSpinner()),
                           tabPanel("Clutches by map",
                                    plotOutput('clutch_map_dist') %>% withSpinner()),
                           tabPanel("Clutches by rank",
                                    plotOutput('clutch_rank_dist') %>% withSpinner()),
                           tabPanel("First K/D by map",
                                    plotOutput('fkd_map') %>% withSpinner()),
                           tabPanel("Kill Combos",
                                    plotOutput('kill_combo') %>% withSpinner()),
                           tabPanel("Overall Performance Summary",
                                    gt_output('performance_table') %>% withSpinner())
                    )
                )
        )
    )
)

ui <- dashboardPage(header, sidebar, body, skin = 'red')

# server.R

server <- function(input, output, session) {
    
    # Update on Player ID change
    
    nickname <- reactive({
        nickname_data <- fromJSON( 
            glue("http://api.steampowered.com/ISteamUser/GetPlayerSummaries/v0002/?key={STEAM_API_KEY}&steamids={input$player_id}")
        )$response$players$personaname
    })
    
    match_list <- reactive({
        vroom(glue('data/{input$player_id}.csv', delim = ',')) %>% 
        mutate(RankName = rank_list[Rank], pid = as.character(PlayerID)) %>%
        mutate(Date = case_when(Date == 'Yesterday' ~ ((Sys.Date() - 1) %>% format('%d %B')),
                                Date == '2 days ago' ~ ((Sys.Date() - 2) %>% format('%d %B')),
                                Date == '3 days ago' ~ ((Sys.Date() - 3) %>% format('%d %B')),
                                Date == '4 days ago' ~ ((Sys.Date() - 4) %>% format('%d %B')),
                                Date == '5 days ago' ~ ((Sys.Date() - 5) %>% format('%d %B')),
                                Date == '6 days ago' ~ ((Sys.Date() - 6) %>% format('%d %B')),
                                TRUE ~ Date)) %>%
        separate(Score, c("S1", "S2"), sep = ":", remove = FALSE, convert = TRUE) %>%
        mutate("Win_Loss" = case_when(S1 > S2 ~ "Win",
                                      S1 < S2 ~ "Loss",
                                      TRUE ~ "Tie")) %>%
        select(-S1, -S2) %>%
        relocate("Win_Loss", .before = Score) %>%
        relocate("RankName", .after = Rank)
    })
    
    scoreboard <- reactive({
        vroom(glue('data/scores_recent10_{input$player_id}.csv'), delim = ',') %>%
        mutate(RankName = rank_list[Rank], pid = as.character(`Player ID`)) %>%
        filter(pid == (match_list() %>% select(pid) %>% unique() %>% as.character()))
    })

    map_list <- reactive({
        match_list() %>%
        select(Map) %>%
        unique()
    })

    map_labels <- reactive({
        setNames(
        paste0("<img src = 'assets/map_icons/", map_list()$Map, ".png' width = '30'/><br>", map_list()$Map),
        map_list()$Map)
    })

    rank_list_unique <- reactive({
        match_list() %>%
        select(Rank,RankName) %>%
        unique()
    })

    rank_labels <- reactive({
        setNames(
        paste0("<img src = 'assets/rank_icons/", rank_list_unique()$Rank, ".png' width = '60'/><br>", rank_list_unique()$RankName),
        rank_list_unique()$Rank)
    })
    
    # Plot outputs

    output$map_dist <- renderPlot({get_map_dist(match_list(), nickname(), map_labels())}) # 1
    
    output$rank_dist <- renderPlot({get_rank_dist(match_list(), nickname(), rank_labels())}) # 2
    
    output$kill_dist <- renderPlot({get_kill_dist(match_list(), nickname(), map_labels())}) # 3
    
    output$clutch_map_dist <- renderPlot({get_clutch_map(match_list(), nickname(), map_labels())}) # 4
    
    output$clutch_rank_dist <- renderPlot({get_clutch_rank(match_list(), nickname(), rank_labels())}) # 5
    
    output$fkd_map <- renderPlot({get_fkd_map(match_list(), scoreboard(), nickname(), map_labels())}) # 6
    
    output$kill_combo <- renderPlot({get_kill_combo(match_list(), scoreboard(), nickname(), map_labels())}) # 7
    
    output$performance_table <- render_gt({get_performance_table(match_list(), nickname())}) # 8
    
}

shinyApp(ui = ui, server = server)
