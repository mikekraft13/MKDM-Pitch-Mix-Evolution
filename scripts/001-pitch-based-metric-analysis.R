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

plot <- yearly_pitch_metrics %>%
  mutate(
    p_throws = ifelse(p_throws == 'L', 'LHP', 'RHP'),
    stand = ifelse(stand == 'L', 'LHH', 'RHH'),
  ) %>%
  ggplot(aes(x = year, y = usage, color = pitch_name, group = pitch_name)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  labs(
    title = paste0('Pitch Usage and Velocity Trends'),
    x = 'Year',
    y = 'Usage %',
    color = 'Velocity (MPH)',
    shape = 'Pitch'
  ) +
  facet_grid(
    cols = vars(p_throws),
    rows = vars(stand)
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
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

ggsave('plots/Usage-Velocity-Trends.png', plot = plot, width = 8)

# Loop through platoons and save plots ####
## Usage Evolution ####
lapply(unique(yearly_pitch_metrics$p_throws), function(throw) {
  lapply(unique(yearly_pitch_metrics$stand), function(bat) {
    plot <- yearly_pitch_metrics %>%
      filter(p_throws == throw & stand == bat) %>%
      ggplot(aes(x = year, y = usage, color = pitch_name, group = pitch_name)) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      labs(
        title = paste0(throw, 'HP vs. ', bat, 'HH Pitch Usage'),
        x = 'Year',
        y = 'Usage %',
        color = 'Pitch Type'
      )
    
    ggsave(paste0('plots/Usage-Trends-', throw, 'HP-vs-', bat, 'HH', '.png'), plot = plot)
  })
})

## Strike Zone Visualization with heat map ####
lapply(unique(yearly_pitch_metrics$p_throws), function(throw) {
  lapply(unique(yearly_pitch_metrics$stand), function(bat) {
    lapply(unique(yearly_pitch_metrics$pitch_name), function(pitch) {
      df <- yearly_pitch_metrics %>%
        filter(p_throws == throw & stand == bat & pitch_name == pitch) %>%
        mutate(year = as.numeric(year))
      
      szone_top <- mean(df$sz_top, na.rm = T)
      szone_bottom <- mean(df$sz_bot, na.rm = T)
      
      mid1 <- szone_bottom + (szone_top - szone_bottom) / 3
      mid2 <- szone_bottom + 2 * (szone_top - szone_bottom) / 3
      
      line_data <- data.frame(
        y = c(szone_top, mid2, mid1, szone_bottom),
        line_type = c("Top", "Mid 2", "Mid 1", "Bottom")
      )
      
      sz_left <- -8.5 / 12
      sz_right <- 8.5 / 12
      mid_h1 <- sz_left + (sz_right - sz_left) / 3
      mid_h2 <- sz_left + 2 * (sz_right - sz_left) / 3
      
      plot <- ggplot(df, aes(x = plate_x, y = plate_z, color = year)) +
        geom_point(size = 3) +
        geom_segment(aes(x = sz_left, xend = sz_right, y = szone_top, yend = szone_top), color = 'black') +
        geom_segment(aes(x = sz_left, xend = sz_right, y = mid1, yend = mid1), color = 'black') +
        geom_segment(aes(x = sz_left, xend = sz_right, y = mid2, yend = mid2), color = 'black') +
        geom_segment(aes(x = sz_left, xend = sz_right, y = szone_bottom, yend = szone_bottom), color = 'black') +
        geom_segment(aes(x = sz_left, xend = sz_left, y = szone_top, yend = szone_bottom), color = 'black') +
        geom_segment(aes(x = mid_h1, xend = mid_h1, y = szone_top, yend = szone_bottom), color = 'black') +
        geom_segment(aes(x = mid_h2, xend = mid_h2, y = szone_top, yend = szone_bottom), color = 'black') +
        geom_segment(aes(x = sz_right, xend = sz_right, y = szone_top, yend = szone_bottom), color = 'black') +
        scale_color_viridis_c(option = 'plasma') +
        labs(
          title = paste0(pitch, ' Location Evolution ', throw, 'HP-vs-', bat, 'HH'),
          x = 'Horizontal Location', 
          y = 'Vertical Location'
        ) +
        xlim(-1, 1) +
        ylim(3 * szone_bottom / 4, (szone_bottom / 4) + szone_top)
      
      ggsave(paste0('plots/Location-Trends-', pitch, '-', throw, 'HP-vs-', bat, 'HH', '.png'), plot = plot)
    })
  })
})

## Movement Plot (Similar to Baseball Savant) (Work on this one?) ####
lapply(unique(yearly_pitch_metrics$p_throws), function(throw) {
  lapply(unique(yearly_pitch_metrics$stand), function(bat) {
    lapply(unique(yearly_pitch_metrics$pitch_name), function(pitch) {
      df <- yearly_pitch_metrics %>%
        filter(p_throws == throw & stand == bat & pitch_name == pitch) %>%
        mutate(year = as.numeric(year))
      
      plot <- df %>%
        ggplot(aes(pfx_x, pfx_z, color = year)) +
        geom_point(size = 3) +
        scale_color_viridis_c(option = 'plasma') +
        labs(
          title = paste0(pitch, ' Movement Evolution ', throw, 'HP-vs-', bat, 'HH'),
          x = 'Horizontal Movement', 
          y = 'Vertical Movement'
        ) +
        xlim(-2, 2) +
        ylim(-2, 2) +
        coord_fixed() +
        theme_minimal() +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid = element_blank(),
          plot.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank()
        ) +
        annotate("path",
                 x = 2 * cos(seq(0, 2 * pi, length.out = 100)),
                 y = 2 * sin(seq(0, 2 * pi, length.out = 100)),
                 color = "black",
                 linewidth = 1) +
        geom_vline(xintercept = 0, linetype = "dotted", color = "black") + 
        geom_hline(yintercept = 0, linetype = "dotted", color = "black") 
      
      ggsave(paste0('plots/Movement-Trends-', pitch, '-', throw, 'HP-vs-', bat, 'HH', '.png'), plot = plot)
    })
  })
})

## Exit Velo Evolution ####
lapply(unique(yearly_pitch_metrics$p_throws), function(throw) {
  lapply(unique(yearly_pitch_metrics$stand), function(bat) {
    plot <- yearly_pitch_metrics %>%
      filter(p_throws == throw & stand == bat & !is.na(launch_speed)) %>%
      ggplot(aes(x = year, y = launch_speed, color = pitch_name, group = pitch_name)) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      labs(
        title = paste0(throw, 'HP vs. ', bat, 'HH Exit Velocity'),
        x = 'Year',
        y = 'Exit Velocity',
        color = 'Pitch Type'
      )
    
    ggsave(paste0('plots/Exit-Velocity-Trends-', throw, 'HP-vs-', bat, 'HH', '.png'), plot = plot)
  })
})
