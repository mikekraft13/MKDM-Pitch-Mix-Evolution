# Mike's working script


# Overall (2014-2024) analysis of individual pitch usage w/ respect to velocity
# FF = 4-seam fastball; SI = sinker; FC = cutter
# CH = changeup; FS = split-finger; FO = forkball; SC = screwball
# CU = curveball; KC = knuckle-curve; CS = slow curve
# SL = slider; ST = sweeper; SV = slurve
# KN = knuckleball
# EP = eephus; FA = other; IN = intentional ball; PO = pitchout


filtered_dfs_binded <- read_rds(here::here('data', 'filtered_dfs_binded.rds'))

filter(!pitch_type %in% c('AB', 'EP', 'FO', 'IN', 'KN', 'FA', 'PO', 'SC', 'CS', 'SV'))

# Get & calculate hitting metrics ####
## Data frame for Whiff Rate & Slugging Against figures ####
whiff_slugging_df <- filtered_dfs_binded |>
  filter(!is.na(pitch_type)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str_sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS') ~ 'Breaking-Curve',
                                 pitch_type %in% c('SL', 'ST', 'SV') ~ 'Breaking-Slider',
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out, though always good to have a catch-all
                                 TRUE ~ NA_character_),
         relevant.pitches = case_when(pitch_type == 'FF' ~ '4-Seam Fastball',
                                      pitch_type == 'CH' ~ 'Changeup',
                                      pitch_type == 'CU' ~ 'Curveball',
                                      pitch_type == 'FC' ~ 'Cutter',
                                      pitch_type == 'KC' ~ 'Knuckle Curve',
                                      pitch_type == 'SI' ~ 'Sinker',
                                      pitch_type == 'SL' ~ 'Slider',
                                      pitch_type == 'FS' ~ 'Split-Finger',
                                      pitch_type == 'ST' ~ 'Sweeper',
                                      TRUE ~ 'Other'),
         p_throws = case_when(p_throws == 'L' ~ 'LHP',
                              p_throws == 'R' ~ 'RHP',
                              TRUE ~ NA_character_),
         stand = case_when(stand == 'L' ~ 'LHH',
                           stand == 'R' ~ 'RHH',
                           TRUE ~ NA_character_),
         total_bases = case_when(
           events == "single" ~ 1,
           events == "double" ~ 2,
           events == "triple" ~ 3,
           events == "home_run" ~ 4,
           TRUE ~ 0  # Other events like strikeouts, walks, etc.
                                  )) |>
  group_by(year.of.game, p_throws, stand, relevant.pitches) |>
  summarise(whiff.pct = (sum(description %in% c('swinging_strike', 'swinging_strike_blocked', 'swinging_pitchout'))
                        /
                        sum(description %in% c('swinging_strike', 'hit_into_play', 'foul',
                                     'foul_tip', 'swinging_strike_blocked', 'swinging_pitchout',
                                     'foul_pitchout'))), ## Whiff Percentage ####
            avg.pitch.velo = mean(release_speed, na.rm = T), ## Mean Pitch Velo ####
            total_bases = sum(total_bases, na.rm = T),
            at_bats = sum(!is.na(events) & events %in% c('single', 'double', 'triple', 'home_run',
                                                                  'field_error', 'grounded_into_double_play',
                                                                  'strikeout', 'fielders_choice', 'field_error',
                                                                  'force_out', 'fielders_choice_out', 'double_play',
                                                                  'strikeout_double_play', 'triple_play')),
            # not including the 'truncated_pa' event as an at-bat since the plate appearance doesn't conclude
            avg.slugging.against = (total_bases/at_bats)) |> ## Mean Slugging Against (total # of bases / AB) #### 
  ungroup()
  
## Data frame for Exit Velocity Against figure ####
# This data frame needs to be separate since we cannot filter out null exit velo values for whiff & slugging calculations
exit_velo_df <- filtered_dfs_binded |>
  filter(!is.na(pitch_type), !is.na(launch_speed)) |>
  # extracting the year from each row, and making it a new field
  mutate(year.of.game = str_sub(game_date, 1, 4) |> as.numeric(),
         # putting the individual pitches in groups to analyze
         pitch.group = case_when(pitch_type %in% c('FF', 'SI', 'FC') ~ 'Fastball',
                                 pitch_type %in% c('CH', 'FS', 'FO', 'SC') ~ 'Offspeed',
                                 pitch_type %in% c('CU', 'KC', 'CS') ~ 'Breaking-Curve',
                                 pitch_type %in% c('SL', 'ST', 'SV') ~ 'Breaking-Slider',
                                 pitch_type == 'KN' ~ 'Breaking',
                                 pitch_type %in% c('EP', 'FA', 'IN', 'PO') ~ 'Other',
                                 # Should not get to here since NA's are being filtered out, though always good to have a catch-all
                                 TRUE ~ NA_character_),
         relevant.pitches = case_when(pitch_type == 'FF' ~ '4-Seam Fastball',
                                      pitch_type == 'CH' ~ 'Changeup',
                                      pitch_type == 'CU' ~ 'Curveball',
                                      pitch_type == 'FC' ~ 'Cutter',
                                      pitch_type == 'KC' ~ 'Knuckle Curve',
                                      pitch_type == 'SI' ~ 'Sinker',
                                      pitch_type == 'SL' ~ 'Slider',
                                      pitch_type == 'FS' ~ 'Split-Finger',
                                      pitch_type == 'ST' ~ 'Sweeper',
                                      TRUE ~ 'Other'),
         p_throws = case_when(p_throws == 'L' ~ 'LHP',
                              p_throws == 'R' ~ 'RHP',
                              TRUE ~ NA_character_),
         stand = case_when(stand == 'L' ~ 'LHH',
                           stand == 'R' ~ 'RHH',
                           TRUE ~ NA_character_)) |>
  group_by(year.of.game, p_throws, stand, relevant.pitches) |>
  summarise(avg.pitch.velo = mean(release_speed, na.rm = T), ## Mean Pitch Velo ####
            avg.exit.velo = mean(launch_speed, na.rm = T)) |> ## Mean Exit Velo ####
  ungroup()  
  
  
# Plots ####

## Whiff Rate Plot ####
whiff_slugging_df |>
  filter(relevant.pitches != 'Other') |>
  ggplot() + 
  geom_point(aes(x = as.factor(year.of.game), y = whiff.pct, colour = relevant.pitches)) +
  #scale_size_continuous(limits = c(70,100), range = c(1,6)) +
  facet_grid(stand ~ p_throws) +
  scale_color_manual(
    values = c(
      '4-Seam Fastball' = '#FF1A1C',
      'Changeup' = '#0DA01A',
      'Curveball' = '#377EB8',
      'Cutter' = '#B63653',
      'Knuckle Curve' = '#5B0FD8',
      'Sinker' = '#8A4513',
      'Slider' = '#9840E2',
      'Split-Finger' = '#9FF04D',
      'Sweeper' = '#A91FA5'
    )
  ) +
  labs(x = 'Year', 
       y = 'Whiff Rate',
       title = 'Whiff Rate by Pitch and Platoon',
       color = 'Pitches') +
  # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 0.8, by = 0.1)) +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.background = element_rect(fill = "gray75", color = NA)) + # Adjust panel background
  ggsave('Whiff-Rate-Trends.png', path = here::here('plots'))

        #plot.background = element_rect(fill = "gray15", color = NA))   # Adjust entire plot background
  #geom_jitter(width = 0.05, height = 0.05)
  #geom_text(aes(label = some_label), vjust = -1, size = 4)
  #theme_dark()
# theme(
# panel.background = element_rect(fill = "lightgray"),
# panel.grid.major = element_line(color = "white"),
# axis.text = element_text(size = 12, face = "bold"),
# axis.title = element_text(size = 14, face = "bold")
# )


## Mean Exit Velo Against Plot ####
exit_velo_df |>
  filter(relevant.pitches != 'Other') |>
  ggplot() +
  geom_violin(aes(x = relevant.pitches, y = avg.exit.velo)) +
  facet_grid(stand ~ p_throws) +
  labs(x = 'Pitch', 
       y = 'Exit Velo',
       title = 'Average Exit Velocity by Pitch and Platoon') +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.background = element_rect(fill = "gray75", color = NA)) +
  ggsave('Exit-Velocity-Trends.png', path = here::here('plots'))
  

# exit_velo_df |>
#   filter(relevant.pitches != 'Other') |>
#   ggplot() +
#   geom_point(aes(x = as.factor(year.of.game), y = avg.exit.velo, colour = relevant.pitches)) +
#   facet_grid(stand ~ p_throws) +
#   scale_color_manual(
#     values = c(
#       '4-Seam Fastball' = '#FF1A1C',
#       'Changeup' = '#0DA01A',
#       'Curveball' = '#377EB8',
#       'Cutter' = '#B63653',
#       'Knuckle Curve' = '#5B0FD8',
#       'Sinker' = '#8A4513',
#       'Slider' = '#9840E2',
#       'Split-Finger' = '#9FF04D',
#       'Sweeper' = '#A91FA5'
#     )
#   ) +
#   labs(x = 'Year', 
#        y = 'Exit Velo',
#        title = 'Average Exit Velocity by Pitch and Platoon',
#        color = 'Pitches') +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


## Mean Slugging Against Plot ####
whiff_slugging_df |>
  filter(relevant.pitches != 'Other') |>
  ggplot() +
  geom_point(aes(x = as.factor(year.of.game), y = avg.slugging.against, colour = relevant.pitches)) +
  facet_grid(stand ~ p_throws) +
  scale_color_manual(
    values = c(
      '4-Seam Fastball' = '#FF1A1C',
      'Changeup' = '#0DA01A',
      'Curveball' = '#377EB8',
      'Cutter' = '#B63653',
      'Knuckle Curve' = '#5B0FD8',
      'Sinker' = '#8A4513',
      'Slider' = '#9840E2',
      'Split-Finger' = '#9FF04D',
      'Sweeper' = '#A91FA5'
    )
  ) +
  labs(x = 'Year', 
       y = 'Slugging',
       title = 'Average Slugging Against by Pitch and Platoon',
       color = 'Pitches') +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        panel.background = element_rect(fill = "gray75", color = NA)) +
  ggsave('Slugging-Percentage-Trends.png', path = here::here('plots'))

