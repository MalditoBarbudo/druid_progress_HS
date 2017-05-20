# Aggro druid progress shiny app.
# Server file

# libraries
library(shiny)
library(tidyverse)
library(forcats)
library(cowplot)
library(ggthemes)
library(googlesheets)
library(viridis)
library(stringr)
library(extrafont)

# Source helper functions from global.R
source('global.R')

# Data
# We need to read the file every five minutes, this way while the app is up we
# always have "real time" data.

function(input, output, session) {
  
  # data timestamp to check for data updates in the reactivePoll
  data_timestamp <- (gs_ls() %>% filter(sheet_title == 'aggro_druid'))$updated
  
  # Load reactive
  games_data <- reactivePoll(
    10000,
    session = session,
    checkFunc = check_function,
    valueFunc = read_gs_data
  )
  
  # Example data to debug
  # games_data <- reactiveFileReader(
  #   60000,
  #   session = session,
  #   filePath = 'Old_Data/aggro_druid.csv',
  #   readFunc = read_csv2
  # )
  
  #### Rank plot ####
  output$rank_plot <- renderPlot({
    games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      ggplot(aes(x = Id_game, y = Rank)) +
      ylim(25,0) +
      geom_line(color = 'firebrick') +
      geom_point(aes(colour = Result), size = 2) +
      scale_colour_manual(values = c('#D35400', '#2ECC71')) +
      labs(x = 'Game',
           title = 'Rank progression') +
      theme_hs
  },
  res = 100)
  
  #### Ratios per Archetype and Class####
  output$ratios <- renderPlot({
    overall <- games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      mutate(win_ratio = if_else(Result == 'Win', 1, 0,
                                 NA_real_)) %>%
      summarise(Ratio = mean(win_ratio)) %>%
      mutate(Opponent = 'Overall',
             Class = 'Overall')
    
    ratio_per_arch <- games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      mutate(win_ratio = if_else(Result == 'Win', 1, 0,
                                 NA_real_)) %>%
      group_by(Opponent) %>%
      summarise(Ratio = mean(win_ratio),
                Count = n()) %>%
      filter(Count > 10) %>%
      bind_rows(overall) %>%
      ggplot(aes(y = Ratio, x = fct_relevel(Opponent, "Overall", after = Inf),
                 fill = Ratio)) +
      geom_hline(yintercept = 0.5, color = 'red') +
      geom_bar(stat = 'identity') +
      geom_text(aes(label = round(Ratio, 2)), hjust = 1.2, size = 3) +
      scale_y_continuous(breaks = c(0, 0.33, 0.5, 0.66, 1), limits = c(0,1)) +
      scale_fill_viridis(option = 'viridis') +
      labs(x = 'Opponent',
           title = 'Win Ratio per Archetype',
           subtitle = '(only archetypes with > 10 games)') +
      coord_flip() +
      theme_hs +
      theme(legend.position = 'none')
    
    ratio_per_class <- games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      mutate(win_ratio = if_else(Result == 'Win', 1, 0,
                                 NA_real_)) %>%
      group_by(Class) %>%
      summarise(Ratio = mean(win_ratio),
                Count = n()) %>%
      filter(Count > 10) %>%
      bind_rows(overall) %>%
      ggplot(aes(y = Ratio, x = fct_relevel(Class, "Overall", after = Inf),
                 fill = Ratio)) +
      geom_hline(yintercept = 0.5, color = 'red') +
      geom_bar(stat = 'identity') +
      geom_text(aes(label = round(Ratio, 2)), hjust = 1.2, size = 3) +
      scale_y_continuous(breaks = c(0, 0.33, 0.5, 0.66, 1), limits = c(0,1)) +
      scale_fill_viridis(option = 'viridis') +
      labs(x = 'Class',
           title = 'Win Ratio per Class',
           subtitle = '(only classes with > 10 games)') +
      coord_flip() +
      theme_hs +
      theme(legend.position = 'none')
    
    count_per_arch <- games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      ggplot(aes(x = Opponent, fill = Result)) +
      geom_bar(position = 'stack', alpha = 0.5) +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count', x = '',
           title = 'Games per Archetype') +
      coord_flip() +
      theme_hs
    
    count_per_class <- games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      ggplot(aes(x = Class, fill = Result)) +
      geom_bar(position = 'stack', alpha = 0.5) +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count', x = '',
           title = 'Games per Class') +
      coord_flip() +
      theme_hs
    
    plot_grid(ratio_per_arch, count_per_arch,
              ratio_per_class, count_per_class,
              ncol = 2, labels = 'auto', align = 'h')
  },
  res = 100)
  
  #### Cards statistics ####
  output$cards_plot <- renderPlot({
    LivingMana_total <- games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      ggplot(aes(x = LivingMana, fill = Result)) +
      geom_bar(position = 'identity', alpha = 0.5) +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count', x = '',
           title = 'Living Mana') +
      theme_hs
    
    Finja_total <- games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      ggplot(aes(x = Finja, fill = Result)) +
      geom_bar(position = 'identity', alpha = 0.5) +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count', x = '',
           title = 'Finja') +
      theme_hs
    
    Combinations <- games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      mutate(Card_combo = fct_recode(as_factor(paste0(LivingMana, Finja)),
                                     None = 'NoNo', LivingMana = 'YesNo',
                                     Finja = 'NoYes', Both = 'YesYes')) %>%
      ggplot(aes(x = Card_combo, fill = Result)) +
      geom_bar(position = 'identity', alpha = 0.5) +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count', x = '',
           title = 'Detailed') +
      theme_hs
    
    upper_grid <- plot_grid(LivingMana_total, Finja_total, labels = 'auto')
    plot_grid(upper_grid, Combinations, labels = c('', 'c'),
              ncol = 1)
  },
  res = 100)
  
  #### Probabilities ####
  output$prob_plot <- renderPlot({
    overall_win_prob <- games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      mutate(win_ratio = if_else(Result == 'Win', 1, 0,
                                 NA_real_)) %>%
      ggplot(aes(x = Turn, y = win_ratio)) +
      geom_hline(yintercept = .5, color = 'red', size = 1) +
      geom_jitter(alpha = .5, width = 0.07, height = 0.01) +
      stat_smooth(method = 'glm',
                  method.args = list(family = "binomial"), se = FALSE,
                  colour = 'darkblue') +
      scale_x_continuous(breaks = c(2:10)) +
      labs(y = 'Ratio',
           title = 'Overall probabilistic smooth',
           subtitle = '(dots are jittered for better visualization)') +
      theme_hs
    
    card_win_prob <- games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      mutate(Card_combo = fct_recode(as_factor(paste0(LivingMana, Finja)),
                                     None = 'NoNo', LivingMana = 'YesNo',
                                     Finja = 'NoYes', Both = 'YesYes'),
             win_ratio = if_else(Result == 'Win', 1, 0,
                                 NA_real_)) %>%
      ggplot(aes(x = Turn, y = win_ratio, color = Card_combo)) +
      geom_hline(yintercept = .5, color = 'red', size = 1) +
      geom_jitter(alpha = .5, width = 0.07, height = 0.01) +
      stat_smooth(method = 'glm',
                  method.args = list(family = "binomial"), se = FALSE) +
      scale_x_continuous(breaks = c(2:10)) +
      labs(y = 'Ratio',
           title = 'By played card probabilistic smooth',
           subtitle = '(dots are jittered for better visualization)') +
      theme_hs +
      theme(legend.position = 'top')
    
    plot_grid(overall_win_prob, card_win_prob, ncol = 1, labels = 'auto',
              align = 'v', rel_heights = c(1,1.3))
  },
  res = 100)
  
  #### Games Histogram ####
  output$turn_hist <- renderPlot({
    games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      filter(Disconnect == 'No') %>%
      ggplot(aes(x = Turn, fill = Result)) +
      geom_histogram(binwidth = 1, color = 'black',
                     alpha = 0.5, position = 'identity') +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count',
           title = 'Games finished by turn') +
      scale_x_continuous(breaks = 2:10) +
      theme_hs
  },
  res = 100)
  
  #### Probabilities per Archetype ####
  output$prob_per_arch <- renderPlot({
    games_data() %>%
      mutate(Class = get_class_from_archetype(Opponent)) %>%
      filter(Rank <= input$rank_sel) %>%
      filter(Class %in% input$class_sel) %>%
      mutate(Card_combo = fct_recode(as_factor(paste0(LivingMana, Finja)),
                                     None = 'NoNo', LivingMana = 'YesNo',
                                     Finja = 'NoYes', Both = 'YesYes'),
             win_ratio = if_else(Result == 'Win', 1, 0,
                                 NA_real_)) %>%
      ggplot(aes(x = Turn, y = win_ratio, color = Card_combo)) +
      geom_hline(yintercept = .5, color = 'red', size = 1) +
      geom_jitter(alpha = .5, width = 0.07, height = 0.01) +
      stat_smooth(method = 'glm',
                  method.args = list(family = "binomial"), se = FALSE) +
      facet_wrap(~Opponent, ncol = 3) +
      scale_x_continuous(breaks = c(2:10)) +
      labs(y = 'Ratio',
           title = 'By played card and archetype probabilistic smooth',
           subtitle = '(dots are jittered for better visualization)') +
      theme_hs +
      theme(legend.position = 'top',
            strip.background = element_blank(),
            strip.text = element_text(face = 'bold'))
  },
  res = 100)
}
