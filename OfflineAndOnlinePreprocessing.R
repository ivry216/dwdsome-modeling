# COlumns we are interesed in
columns_to_process <- c("temperature", "specific_conductance", "dissolved_oxygen", "orp", "ph")

# Initialize data
offline_data_list_plt <- offline_data_list
online_data_list_plt <- data_to_postporcess

# Make online data suitable
for (i in seq(online_data_list_plt)) {
  online_data_list_plt[[i]] <- online_data_list_plt[[i]] %>%
    mutate(Line = i) %>%
    select(one_of(c("timestamp", columns_to_process, "Line"))) %>%
    mutate(Line = as.factor(Line))
}

# Merge all
online_data_plt <- do.call("rbind", online_data_list_plt)
offline_data_plt <- do.call("rbind", offline_data_list_plt)

# Add callibration points
callibration_points <- list(
  as.POSIXct("16.06.2017", format = "%d.%m.%Y", tz = "UTC"),
  as.POSIXct("07.07.2017", format = "%d.%m.%Y", tz = "UTC"),
  as.POSIXct("21.07.2017", format = "%d.%m.%Y", tz = "UTC"),
  as.POSIXct("04.08.2017", format = "%d.%m.%Y", tz = "UTC"),
  as.POSIXct("11.08.2017", format = "%d.%m.%Y", tz = "UTC"),
  as.POSIXct("18.08.2017", format = "%d.%m.%Y", tz = "UTC"),
  as.POSIXct("31.08.2017", format = "%d.%m.%Y", tz = "UTC"),
  as.POSIXct("19.09.2017", format = "%d.%m.%Y", tz = "UTC"),
  as.POSIXct("09.10.2017", format = "%d.%m.%Y", tz = "UTC")
)

if (FALSE) {
  p <- ggplot(NULL, mapping = aes_string(x = "timestamp", y = "ph", color = "Line")) +
    geom_point(data = online_data_plt, alpha = 0.5, size = 1) + 
    geom_line(data = sis_clean, color = "black") + 
    geom_point(data = sis_clean, color = "black", fill = "grey", size = 2, shape = 21) +
    geom_line(data = sis_amiini, color = "cyan") + 
    geom_point(data = sis_amiini, color = "black", fill = "cyan", size = 2, shape = 21) +
    geom_line(data = sis_hypo, color = "magenta") + 
    geom_point(data = sis_hypo, color = "black", fill = "magenta", size = 2, shape = 21) +
    geom_line(data = offline_data_plt, color = "black") + 
    geom_point(data = offline_data_plt, color = "black", shape = 15) +
    scale_color_manual(values=c("red3", "yellow3", "limegreen", "royalblue3")) +
    scale_x_datetime(date_labels = "%d/%m") +
    facet_grid(Line ~ .) +
    labs(x = "Time", y = "pH")
  
  print(p)
  
  p <- ggplot(NULL, mapping = aes_string(x = "timestamp", y = "ph", color = "Line")) +
    geom_point(data = online_data_plt[day(online_data_plt$timestamp) == 30 & month(online_data_plt$timestamp) == 7,]) +
    facet_wrap(~Line) + 
    scale_x_datetime(date_breaks = "1 day", date_labels =  "%d/%m") +
    scale_color_manual(values=c("red3", "yellow3", "limegreen", "royalblue3")) +
    labs(x = "Time", y = "pH")
  
  print(p)
  
  
  p <- ggplot(NULL, mapping = aes_string(x = "timestamp", y = "dissolved_oxygen", color = "Line")) +
    geom_point(data = online_data_plt, alpha = 0.3, size = 1)
  
  for (vline_data in callibration_points) {
    p <- p + geom_vline(xintercept = vline_data, color = "slategray", linetype= 4, size = 1)
  }
  
  p <- p + scale_color_manual(values=c("red3", "yellow3", "limegreen", "royalblue3")) +
    scale_x_datetime(date_labels = "%d/%m") +
    labs(x = "Time", y = "Dissolved Oxygen")
  
  print(p)
  
}

