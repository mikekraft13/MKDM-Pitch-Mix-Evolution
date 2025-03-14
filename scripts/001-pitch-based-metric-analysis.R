# Get pitch metrics by pitch ####
yearly_pitch_metrics <- filtered_dfs_binded %>%
  mutate(year = str_sub(game_date, 1, 4)) %>%
  group_by(year, p_throws, stand) %>%
  mutate(
    n_pitches = n(),
    n_pitchers = n_distinct(pitcher)
  ) %>%
  group_by(pitch_name, p_throws, stand, year) %>%
  summarise(
    pitch_type = first(pitch_type),
    usage = 100 * n() / first(n_pitches),
    n_pitches = n(),
    pct_of_pitchers = 100 * n_distinct(pitcher) / first(n_pitchers),
    n_pitchers = n_distinct(pitcher),
    across(
      c(
        release_speed, release_pos_x, release_pos_z, pfx_x, pfx_z, plate_x, plate_z, hc_x, hc_y, hit_distance_sc, launch_speed, launch_angle, 
        release_spin_rate, release_extension, release_pos_y, estimated_ba_using_speedangle, estimated_woba_using_speedangle, estimated_slg_using_speedangle,
        babip_value, iso_value, launch_speed_angle, delta_run_exp, api_break_z_with_gravity, api_break_x_arm, api_break_x_batter_in,
        sz_top, sz_bot
      ),
      ~ mean(., na.rm = T)
    )
  ) %>%
  ungroup() %>%
  filter(!pitch_type %in% c('AB', 'EP', 'FO', 'IN', 'KN', 'FA', 'PO', 'SC', 'CS', 'SV'))

# Create plot for usage ####
usage_plot <- yearly_pitch_metrics %>%
  mutate(
    p_throws = ifelse(p_throws == 'L', 'LHP', 'RHP'),
    stand = ifelse(stand == 'L', 'LHH', 'RHH'),
  ) %>%
  ggplot(aes(x = year, y = usage, color = pitch_name, group = pitch_name)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(
    title = paste0('Pitch Usage Trends'),
    x = 'Year',
    y = 'Usage %',
    color = 'Pitch'
  ) +
  facet_grid(
    cols = vars(p_throws),
    rows = vars(stand)
  ) +
  theme_dark() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = 'gray75', color = NA)
  ) +
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
  )

# Save plot for usage ####
ggsave('plots/Usage-Trends.png', plot = usage_plot, width = 8)

# Create plot for pitch movement ####
movement_plot <- yearly_pitch_metrics %>%
  mutate(
    p_throws = ifelse(p_throws == 'L', 'LHP', 'RHP'),
    stand = ifelse(stand == 'L', 'LHH', 'RHH'),
  ) %>%
  ggplot(aes(x = -pfx_x, y = pfx_z)) +
  geom_point(aes(size = year, fill = pitch_name), shape = 21, colour = 'black', stroke = 1) +
  labs(
    title = paste0('Pitch Movement Evolution'),
    x = 'Horizontal Movement (Feet)',
    y = 'Vertical Movement (Feet)',
    fill = 'Pitch',
    size = 'Year'
  ) +
  xlim(-2, 2) +
  ylim(-2, 2) +
  coord_fixed() +
  theme_dark() +
  theme(
    panel.background = element_rect(fill = 'gray75', color = NA)
  ) +
  annotate('path',
           x = 2 * cos(seq(0, 2 * pi, length.out = 100)),
           y = 2 * sin(seq(0, 2 * pi, length.out = 100)),
           color = 'black',
           linewidth = 1) +
  geom_vline(xintercept = 0, linetype = 'dotted', color = 'black') +
  geom_hline(yintercept = 0, linetype = 'dotted', color = 'black') +
  facet_grid(
    cols = vars(p_throws),
    rows = vars(stand)
  ) +
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
  )

# Save plot for movement ####
ggsave('plots/Movement-Trends.png', plot = movement_plot, width = 8)

