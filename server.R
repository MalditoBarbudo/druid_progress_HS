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

# Source helper functions from global.R
source('global.R')

# Data
# We need to read the file every five minutes, this way while the app is up we
# always have "real time" data.

function(input, output, session) {
  # Load reactive
  games_data <- reactiveFileReader(
    60000,
    session = session,
    filePath = getwd(),
    readFunc = read_gs_data
  )
  
  #### Rank plot ####
  output$rank_plot <- renderPlot({
    games_data() %>%
      # filter(Disconnect == 'No') %>%
      # mutate(Id_game = 1:length(Date)) %>%
      ggplot(aes(x = Id_game, y = Rank)) +
      ylim(25,0) +
      geom_line(color = 'firebrick') +
      geom_point(aes(colour = Result), size = 2) +
      scale_colour_manual(values = c('#D35400', '#2ECC71')) +
      labs(x = 'Game',
           title = 'Rank progression') +
      theme_minimal()
  },
  res = 100)
  
  #### Ratios per Archetype and Class####
  output$ratios <- renderPlot({
    overall <- games_data() %>%
      filter(Disconnect == 'No') %>%
      # as_factor(Result) %>%
      mutate(win_ratio = if_else(Result == 'Win', 1, 0,
                                 NA_real_)) %>%
      summarise(Ratio = mean(win_ratio)) %>%
      mutate(Opponent = 'Overall',
             Class = 'Overall')
    
    ratio_per_arch <- games_data() %>%
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
      geom_text(aes(label = round(Ratio, 2)), vjust = -1.2, size = 3) +
      scale_y_continuous(breaks = c(0, 0.33, 0.5, 0.66, 1), limits = c(0,1)) +
      scale_fill_viridis(option = 'viridis') +
      labs(x = 'Opponent',
           title = 'Win Ratio per Archetype',
           subtitle = '(only archetypes with > 10 games)') +
      # coord_flip() +
      theme_minimal() +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 90))
    
    ratio_per_class <- games_data() %>%
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
      geom_text(aes(label = round(Ratio, 2)), vjust = -1.2, size = 3) +
      scale_y_continuous(breaks = c(0, 0.33, 0.5, 0.66, 1), limits = c(0,1)) +
      scale_fill_viridis(option = 'viridis') +
      labs(x = 'Class',
           title = 'Win Ratio per Class',
           subtitle = '(only classes with > 10 games)') +
      # coord_flip() +
      theme_minimal() +
      theme(legend.position = 'none') +
      theme(axis.text.x = element_text(angle = 90))
    
    count_per_arch <- games_data() %>%
      filter(Disconnect == 'No') %>%
      ggplot(aes(x = Opponent, fill = Result)) +
      geom_bar(position = 'stack', alpha = 0.5) +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count', x = '',
           title = 'Games per Archetype') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))
    
    count_per_class <- games_data() %>%
      filter(Disconnect == 'No') %>%
      ggplot(aes(x = Class, fill = Result)) +
      geom_bar(position = 'stack', alpha = 0.5) +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count', x = '',
           title = 'Games per Class') +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))
    
    plot_grid(ratio_per_arch, count_per_arch,
              ratio_per_class, count_per_class,
              ncol = 2, labels = 'auto', align = 'h')
  },
  res = 100)
  
  #### Cards statistics ####
  output$cards_plot <- renderPlot({
    LivingMana_total <- games_data() %>%
      filter(Disconnect == 'No') %>%
      ggplot(aes(x = LivingMana, fill = Result)) +
      geom_bar(position = 'identity', alpha = 0.5) +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count', x = '',
           title = 'Living Mana') +
      theme_minimal()
    
    Finja_total <- games_data() %>%
      filter(Disconnect == 'No') %>%
      ggplot(aes(x = Finja, fill = Result)) +
      geom_bar(position = 'identity', alpha = 0.5) +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count', x = '',
           title = 'Finja') +
      theme_minimal()
    
    Combinations <- games_data() %>%
      filter(Disconnect == 'No') %>%
      mutate(Card_combo = fct_recode(as_factor(paste0(LivingMana, Finja)),
                                     None = 'NoNo', LivingMana = 'YesNo',
                                     Finja = 'NoYes', Both = 'YesYes')) %>%
      ggplot(aes(x = Card_combo, fill = Result)) +
      geom_bar(position = 'identity', alpha = 0.5) +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count', x = '',
           title = 'Detailed') +
      theme_minimal()
    
    upper_grid <- plot_grid(LivingMana_total, Finja_total, labels = 'auto')
    plot_grid(upper_grid, Combinations, labels = c('', 'c'),
              ncol = 1)
  },
  res = 100)
  
  #### Probabilities ####
  output$prob_plot <- renderPlot({
    overall_win_prob <- games_data() %>%
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
      theme_minimal()
    
    card_win_prob <- games_data() %>%
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
      theme_minimal() +
      theme(legend.position = 'top')
    
    plot_grid(overall_win_prob, card_win_prob, ncol = 1, labels = 'auto',
              align = 'v', rel_heights = c(1,1.3))
  },
  res = 100)
  
  #### Games Histogram ####
  output$turn_hist <- renderPlot({
    games_data() %>%
      filter(Disconnect == 'No') %>%
      ggplot(aes(x = Turn, fill = Result)) +
      geom_histogram(binwidth = 1, color = 'black',
                     alpha = 0.5, position = 'identity') +
      scale_fill_manual(values = c('#D35400', '#2ECC71')) +
      labs(y = 'Count',
           title = 'Games finished by turn') +
      scale_x_continuous(breaks = 2:10) +
      theme_minimal()
  },
  res = 100)
  
  #### Probabilities per Archetype ####
  output$prob_per_arch <- renderPlot({
    games_data() %>%
      mutate(Card_combo = fct_recode(as_factor(paste0(LivingMana, Finja)),
                                     None = 'NoNo', LivingMana = 'YesNo',
                                     Finja = 'NoYes', Both = 'YesYes'),
             win_ratio = if_else(Result == 'Win', 1, 0,
                                 NA_real_)) %>%
      ggplot(aes(x = Turn, y = win_ratio, color = Card_combo)) +
      # geom_ribbon(ymin = 0.33, ymax = 0.5, fill = 'orange', alpha = 0.3) +
      # geom_ribbon(ymin = 0, ymax = 0.33, fill = 'red', alpha = 0.3) +
      # geom_ribbon(ymin = 0.5, ymax = 0.66, fill = 'darkgreen', alpha = 0.3) +
      # geom_ribbon(ymin = 0.66, ymax = 1, fill = 'green', alpha = 0.3) +
      geom_hline(yintercept = .5, color = 'red', size = 1) +
      geom_jitter(alpha = .5, width = 0.07, height = 0.01) +
      stat_smooth(method = 'glm',
                  method.args = list(family = "binomial"), se = FALSE) +
      facet_wrap(~Opponent, ncol = 3) +
      scale_x_continuous(breaks = c(2:10)) +
      labs(y = 'Ratio',
           title = 'By played card and archetype probabilistic smooth',
           subtitle = '(dots are jittered for better visualization)') +
      theme_minimal() +
      theme(legend.position = 'top')
  },
  res = 100)
}
