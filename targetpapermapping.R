#This code is for generating the scatterplot of target papers for the KO thematic review journal paper.

#For 2D plot
library(readxl)

df <- read_excel("KO review target paper mapping.xlsx")

library(dplyr)

df_jittered <- df %>%
  group_by(Function, Degree_of_automation) %>%
  mutate(
    overlap_count = n(),
    offset = row_number() - (overlap_count + 1) / 2,
    Function_jit = Function + offset * 0.05,              
    Degree_jit = Degree_of_automation + offset * 0.05     
  ) %>%
  ungroup()

library(ggplot2)
library(ggrepel)

ggplot(df_jittered, aes(
  x = Function_jit,
  y = Degree_jit,
  size = Reasoning_cap,
  label = Cite
)) +
  geom_point(color = "tomato", alpha = 0.7) +
  geom_text_repel(size = 3, box.padding = 0.5, point.padding = 0.3) +
  scale_size(range = c(4, 10), name = "Reasoning Capability") +
  guides(size = "none") +
  labs(
    x = "Functionality Sophistication",
    y = "Degree of Automation"
  ) +
  scale_x_continuous(breaks = seq(0, 5, 1)) +  
  scale_y_continuous(breaks = seq(0, 5, 1)) +
  theme_minimal(base_size = 14)+
  theme(
    axis.line=element_line(color = "black"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )


#For interactive 3D plot----------------------------------------------
library(readxl)
library(dplyr)
library(plotly)
library(scales)

df <- read_excel("KO review target paper mapping.xlsx")

df_jittered <- df %>%
  group_by(Function, Degree_of_automation) %>%
  mutate(
    overlap_count = n(),
    offset = row_number() - (overlap_count + 1) / 2,
    Function_jit = Function + offset * 0.05,
    Degree_jit   = Degree_of_automation + offset * 0.05
  ) %>%
  ungroup() %>%
  mutate(size_px = rescale(Reasoning_cap, to = c(6, 18)))

p <- plot_ly(
  df_jittered,
  x = ~Function_jit,
  y = ~Degree_jit,
  z = ~Reasoning_cap,           
  type = "scatter3d",
  mode = "markers",
  text = ~Cite,                  
  hovertemplate = paste(
    "<b>%{text}</b><br>",
    "Functionality: %{x}<br>",
    "Automation: %{y}<br>",
    "Reasoning cap.: %{z}<extra></extra>"
  ),
  marker = list(
    size = ~size_px,
    color = "tomato",
    opacity = 0.7,
    line = list(width = 0.5, color = "rgba(0,0,0,0.25)")
  ),
  showlegend = FALSE
) %>%
  layout(
    scene = list(
      xaxis = list(title = "Functionality Sophistication", zeroline = FALSE),
      yaxis = list(title = "Degree of Automation", zeroline = FALSE),
      zaxis = list(title = "Reasoning Capability", zeroline = FALSE),
      camera = list(eye = list(x = 1.6, y = 1.6, z = 0.9))  # starting angle
    ),
    margin = list(l = 0, r = 0, b = 0, t = 20)
  )

p


#For static 3D plot---------------------------------------------
library(readxl)
library(dplyr)
library(plotly)
library(scales)

df <- read_excel("KO review target paper mapping.xlsx")
df <- df %>%
  mutate(
    Cite = gsub(",.*", "", Cite),       
    Cite = gsub("\\s+\\d{4}$", "", Cite), 
    Cite = ifelse(
      grepl("Gene Ontology Consortium", Cite),
      sub("^The\\s+", "", Cite) |> sub("\\s+et al\\.$", "", x = _),
      Cite
    )
  )

dup_radius   <- 0.22
group_round  <- 1
label_base_y <- 0.22
label_step_y <- 0.12

phi <- pi * (3 - sqrt(5))
df_xyz <- df %>%
  group_by(Function, Degree_of_automation, Reasoning_cap) %>%
  mutate(
    m = n(), k = row_number(),
    y0    = ifelse(m > 1, 1 - (2*k - 1)/m, 0),
    r0    = sqrt(pmax(0, 1 - y0^2)),
    theta = k * phi,
    dx    = ifelse(m > 1, dup_radius * r0 * cos(theta), 0),
    dz    = ifelse(m > 1, dup_radius * r0 * sin(theta), 0),
    dy    = ifelse(m > 1, dup_radius * y0,                0),
    X = Function + dx,
    Y = Degree_of_automation + dy,
    Z = Reasoning_cap + dz,
    size_px = rescale(Reasoning_cap, to = c(6, 18))
  ) %>%
  ungroup()

df_lab <- df_xyz %>%
  mutate(
    gx = round(X, group_round),
    gz = round(Z, group_round)
  ) %>%
  group_by(gx, gz) %>%
  arrange(Y, .by_group = TRUE) %>%
  mutate(
    label_x = X,
    label_z = Z,
    label_y = Y + label_base_y + (row_number() - 1) * label_step_y
  ) %>%
  ungroup()

fit <- lm(Y ~ X + Z, data = df_lab)

grid <- expand.grid(
  X = seq(min(df_lab$X), max(df_lab$X), length.out = 20),
  Z = seq(min(df_lab$Z), max(df_lab$Z), length.out = 20)
)
grid$Y <- predict(fit, newdata = grid)

p2 <- plot_ly(
  df_lab,
  x = ~X, y = ~Y, z = ~Z,
  type = "scatter3d", mode = "markers",
  marker = list(
    size = ~size_px,
    color = "tomato",
    opacity = 0.7,
    line = list(width = 0.5, color = "rgba(0,0,0,0.25)")
  ),
  hovertext = ~Cite, hoverinfo = "text",
  showlegend = FALSE
) %>%
  add_trace(
    x = ~label_x, y = ~label_y, z = ~label_z,
    type = "scatter3d", mode = "text",
    text = ~Cite,
    textfont = list(size = 10, color = "black"),
    inherit = FALSE, showlegend = FALSE
  ) %>%
  #add_trace(
  #  x = grid$X, y = grid$Y, z = grid$Z,
  #  type = "mesh3d",
  #  opacity = 0.3,
  #  color = "lightblue",
  #  inherit = FALSE,
  #  showlegend = FALSE
  #) %>%
  layout(
    scene = list(
      xaxis = list(title = "Functionality Sophistication", zeroline = FALSE),
      yaxis = list(title = "Degree of Automation", zeroline = FALSE),
      zaxis = list(title = "Reasoning Capability", zeroline = FALSE),
      aspectmode = "cube"
    ),
    margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0)
  ) %>%
  config(scrollZoom = TRUE)

p2
library(htmlwidgets)
saveWidget(p2, file = "targetpaper3D.html", selfcontained = TRUE)
