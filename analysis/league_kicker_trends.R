# Loading Dependencies ----------------------------------------------------

packages = c('tidyverse', 'lubridate', 'here', 'nflreadr', 'plotly',
             'DescTools')

check_packages = function(packages, repos = 'https://cloud.r-project.org') {
  invisible(
    lapply(packages, function(p) {
      #' Checking if the package is installed
      if (!requireNamespace(p, quietly = TRUE)) {
        message(paste('Package', p, 'not found. Attempting to install...'))
        
        #' Installing the missing package
        install.packages(p, repos = repos)
      }
      
      #' Loading the package
      suppressPackageStartupMessages(
        library(p, character.only = TRUE)
      )
    })
  )
}
check_packages(packages)

# -------------------------------------------------------------------------
# Loading Helper Functions ------------------------------------------------

helpers_file = here('helpers', 'helpers.R')
source(helpers_file)

# -------------------------------------------------------------------------
# Loading Data ------------------------------------------------------------

pbp_file = here('data', 'pp', 'pbp', 'pbp_pp.rdata')
load(pbp_file)

stats_file = here('data', 'pp', 'stats', 'stats_pp.rdata')
load(stats_file)

# -------------------------------------------------------------------------
# Creating the Analytical Datasets ----------------------------------------

kicker_stats = pbp_pp %>%
  group_by(season, posteam, kicker_player_id, kicker_player_name) %>%
  summarise(
    age = last(age),
    height = last(height),
    weight = last(weight),
    
    fg_att = last(season_fg_attempt),
    fg_pct = last(season_fg_pct),
    
    outdoor_kicks = sum(roof_clean == 'outdoors', na.rm = TRUE),
    indoor_kicks = sum(roof_clean == 'indoors', na.rm = TRUE),
    
    good_weather_kicks = sum(
      weather_clean == 'good' | weather_clean == 'decent',
      na.rm = TRUE
    ),
    bad_weather_kicks = sum(weather_clean == 'bad', na.rm = TRUE),
    
    mean_kick_dst = mean(kick_distance, na.rm = TRUE),
    med_kick_dst = median(kick_distance, na.rm = TRUE),
    kick_dst_diff = abs(mean_kick_dst - med_kick_dst),
    
    sd_kick_dst = sd(kick_distance, na.rm = TRUE),
    q25_kick_dst = quantile(kick_distance, prob = .25, na.rm = TRUE),
    q75_kick_dst = quantile(kick_distance, prob = .75, na.rm = TRUE),
    
    mean_wind = mean(wind_clean, na.rm = TRUE),
    med_wind = median(wind_clean, na.rm = TRUE),
    wind_diff = abs(mean_wind - med_wind),
    
    sd_wind = sd(wind_clean, na.rm = TRUE),
    q25_wind = quantile(wind_clean, prob = .25, na.rm = TRUE),
    q75_wind = quantile(wind_clean, prob = .75, na.rm = TRUE),
    
    mean_temp = mean(temp_clean, na.rm = TRUE),
    med_temp = median(temp_clean, na.rm = TRUE),
    temp_diff = abs(mean_temp - med_temp),
    
    sd_temp = sd(temp_clean, na.rm = TRUE),
    q25_temp = quantile(temp_clean, prob = .25, na.rm = TRUE),
    q75_temp = quantile(temp_clean, prob = .75, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  rename(team = 'posteam')

team_stats = kicker_stats %>%
  group_by(season, team) %>%
  summarise(
    team_fg_att = sum(fg_att, na.rm = TRUE),
    
    n_kickers = n_distinct(kicker_player_id),
    
    outdoor_kicks = sum(outdoor_kicks, na.rm = TRUE),
    indoor_kicks = sum(indoor_kicks, na.rm = TRUE),
    
    good_weather_kicks = sum(good_weather_kicks, na.rm = TRUE),
    bad_weather_kicks = sum(bad_weather_kicks, na.rm = TRUE),
    
    mean_age = mean(age, na.rm = TRUE),
    mean_weight = mean(weight, na.rm = TRUE),
    mean_height = mean(height, na.rm = TRUE),
    
    .groups = 'drop'
  )

league_stats = team_stats %>%
  group_by(season) %>%
  summarise(
    mean_fg_att = mean(team_fg_att, na.rm = TRUE),
    med_fg_att = median(team_fg_att, na.rm = TRUE),
    
    mean_k_count = mean(n_kickers, na.rm = TRUE),
    med_k_count = median(n_kickers, na.rm = TRUE),
    
    mean_outdoor_kicks = mean(outdoor_kicks, na.rm = TRUE),
    med_outdoor_kicks = median(outdoor_kicks, na.rm = TRUE),
    
    mean_indoor_kicks = mean(indoor_kicks, na.rm = TRUE),
    med_indoor_kicks = median(indoor_kicks, na.rm = TRUE),
    
    mean_good_weather_kicks = mean(good_weather_kicks, na.rm = TRUE),
    med_good_weather_kicks = median(good_weather_kicks, na.rm = TRUE),
    
    mean_bad_weather_kicks = sum(bad_weather_kicks, na.rm = TRUE),
    med_bad_weather_kicks = median(bad_weather_kicks, na.rm = TRUE),
    
    .groups = 'drop'
  )

# -------------------------------------------------------------------------
# Creating Visualizations -------------------------------------------------

team_info = load_teams() %>%
  dplyr::select(team_abbr, team_color, team_color2)
nfl_colors = team_info$team_color
names(nfl_colors) = team_info$team_abbr

plot_ly(
  data = team_stats,
  x = ~season,
  y = ~n_kickers,
  color = ~team,
  colors = nfl_colors,
  customdata = ~team,
  type = 'scatter',
  mode = 'lines+markers',
  hovertemplate = paste0(
    'NFL Team: %{customdata}<br>',
    'Number of Kickers: %{y}<extra></extra>'
  )
) %>%
  layout(
    title = 'NFL Team Kicker Count',
    xaxis = list(title = 'Season', dtick = 1),
    yaxis = list(title = 'Number of Kickers', rangemode = 'tozero'),
    showlegend = FALSE
  )

x_cols = c('season')
y_cols = c(
  'mean_fg_att', 'med_fg_att',
  'mean_k_count', 'med_k_count',
  'mean_outdoor_kicks', 'med_outdoor_kicks',
  'mean_indoor_kicks', 'med_indoor_kicks',
  'mean_good_weather_kicks', 'med_good_weather_kicks',
  'mean_bad_weather_kicks', 'med_bad_weather_kicks'
  )
dynamic_line_plot(league_stats, x_cols, y_cols)

# -------------------------------------------------------------------------