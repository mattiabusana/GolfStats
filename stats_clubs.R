
library(tidyverse)
library(googlesheets4)
library(plotly)
library(webshot)



sheet_id <- "1GM_VmaCJ_A_WqsjLrdAgjMnV48RXqDBimVIzismitqE"

df <- read_sheet(sheet_id, sheet = 2)



df <- df %>% mutate(date = as.Date(date, format = "%d.%m.%y"))
df$club_type <- factor(df$club_type, levels = c("lw", "sw", "gw", "pw", "9i", 
                                                   "8i", "7i", "6i", "5i", "4i", "3h", "3w","d"),
                       labels = c("LW", "SW", "GW", "PW", "9i", 
                                    "8i", "7i", "6i", "5i", "4i", "3H", "3W","D"))


df <- df %>% mutate(oblique_carry = sqrt(carry^2 - side_carry^2))


parabolic_launch <- function(speed, angle, apex, carry) {
  
  vector_points <- c()
  
  
  return(vector_points)
}




date_selector <- c("all", "last_month", "last_3_months", "last_6_months", "last_year")
club_selector <- c("LW", "SW", "GW", "PW", "9i", 
                     "8i", "7i", "6i", "5i", "4i", "3H", "3W", "D")


option_selected <- date_selector[1]

date_selected <- switch(option_selected,
                 all = {
                   df$date[length(df$date)] - df$date[1]
                 },
                 last_month = {
                   as.difftime(30, units = "days")
                 },
                 last_3_months = {
                   as.difftime(90, units = "days")
                 },
                 last_6_months = {
                   as.difftime(180, units = "days")
                 },
                 last_year = {
                   as.difftime(365, units = "days")
                 })


summary_club <- df %>% filter(date > (date - date_selected)) %>%
  group_by(club_type) %>% summarise(
    median_club = round(median(carry, na.rm = TRUE),0),
    iqr_club = round(IQR(carry, na.rm = TRUE),0)
  )


summary_driver <- df %>% 
  filter(club_type=="D") %>% summarise(
    median_carry = round(median(carry, na.rm = TRUE),0),
    iqr_carry = round(IQR(carry, na.rm = TRUE),0),
    
    median_side_carry = round(median(side_carry, na.rm = TRUE),0),
    iqr_side_carry = round(IQR(side_carry, na.rm = TRUE),0),
    
    median_club_speed = round(median(club_speed, na.rm = TRUE),1),

    median_ball_speed = round(median(ball_speed, na.rm = TRUE),1),
  )



df %>% filter(club_type %in% club_selector) %>%
ggplot(aes(x = date,y = carry, color = club_type))+
  theme_minimal()+
  stat_summary(fun.data = mean_se, geom = "line", aes(group = club_type)) +
  stat_summary(fun.data = mean_se, geom = "point", aes(group = club_type), size = 3) +
  labs(x ="Date", y = "Carry (m)", title = "Average Carry by Club Type", color = "Club")+
  scale_y_continuous(breaks = seq(40, 220, 10), limits = c(40, 220))



df %>% filter(club_type %in% club_selector) %>%
ggplot(aes(x = side_carry, y = oblique_carry, color = club_type))+
  theme_minimal()+
  geom_point(size = 3, alpha = 0.6)+
  #stat_density_2d(aes(alpha = ..level..), geom = "polygon") +
  geom_vline(xintercept = 0, linetype = "dashed")+
  scale_y_continuous(breaks = seq(40, 220, 10), limits = c(40, 220))+
  scale_x_continuous(breaks = seq(-60, 60, 10), limits = c(-60, 60))+
  labs(x ="Reference (m)", y = "Carry (m)", title = "Shot location", color = "Club")
  
  

list_medians <- df %>% 
  filter(date > (date - date_selected)) %>%
  group_by(club_type) %>% summarise(
    median_carry = round(median(carry, na.rm = TRUE),0),
  )
  

filtered_df <- df %>% filter(date > (date - date_selected))

club_levels <- sort(unique(filtered_df$club_type))
colors <- c(
  "#DDFFE7", "#98D7C2", "#29A0B1", "#167D7F",
  "#FABEC0", "#F85C70", "#F37970", "#E43D40",
  "#E7625F", "#C85250", "#BFD7ED", "#60A3D9", "#0074B7"
)
names(colors) <- club_levels

p <- plot_ly()

  
for (club in club_levels) {
  club_data <- filtered_df %>% filter(club_type == club)
  p <- add_trace(
    p,
    y = club_data$club_type,
    x = club_data$carry,
    type = "box",
    name = club,
    orientation = 'h',
    boxpoints = FALSE,
    line = list(color = "black", width = 1),  # Border around boxes
    fillcolor = colors[club],
    marker = list(color = colors[club]),
    showlegend = FALSE
  )
}

# Add median labels to the left
median_text_x <- 25

p <- add_trace(
  p,
  type = "scatter",
  mode = "text",
  x = rep(median_text_x, nrow(list_medians)),
  y = list_medians$club_type,
  text = list_medians$median_carry,
  textposition = "middle right",
  textfont = list(size = 15, color = "black"),
  showlegend = FALSE,
  hoverinfo = "none"
)

# Layout with stronger axes
p <- layout(
  p,
  xaxis = list(
    title = "Carry (m)",
    tickvals = seq(30, 230, 10),
    range = c(20, 230),
    showline = TRUE,
    linewidth = 1,
    linecolor = "black"
  ),
  yaxis = list(
    title = "Club",
    tickfont = list(size = 14),
    showline = TRUE,
    linewidth = 1,
    linecolor = "black"
  ),
  margin = list(l = 120, r = 50),
  template = "plotly_white"
)

p

htmlwidgets::saveWidget(p, "plot.html")

# Now use webshot to save as JPEG
webshot::webshot("plot.html", 
                 file = "high_res_boxplot_with_medians.jpg", 
                 vwidth = 800,     # Increase width
                 vheight = 600,    # Increase height
                 zoom = 4           # Increase zoom for better resolution
)





