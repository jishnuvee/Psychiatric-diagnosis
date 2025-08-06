#
#
#
#
#
#
#
#
#
#
#
#
#| include: false
# Load libraries quietly
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(shiny)
})

# Load dataset
data <- read.csv("EEG.machinelearing_data_BRMH.csv")
x <- data

# Ensure Healthy control appears first in factor levels
x$specific.disorder <- factor(x$specific.disorder,
                              levels = c("Healthy control",
                                       setdiff(unique(x$specific.disorder), "Healthy control")))
#
#
#
#
#
#
#
#
#
#| context: server

output$interactivePlot <- renderPlotly({
  feature_col <- sym(input$feature)
  
  plot_data <- x %>%
    select(specific.disorder, !!feature_col) %>%
    filter(!is.na(specific.disorder), !is.na(!!feature_col)) %>%
    mutate(
      clean_labels = gsub(" disorder", "", specific.disorder, ignore.case = TRUE),  # Clean labels for x-axis
      legend_labels = specific.disorder  # Keep original labels for legend
    ) %>%
    # Ensure Healthy control appears first in both clean and original labels
    mutate(
      clean_labels = factor(clean_labels, levels = c("Healthy control", setdiff(unique(clean_labels), "Healthy control"))),
      legend_labels = factor(legend_labels, levels = c("Healthy control", setdiff(unique(legend_labels), "Healthy control")))
    )
  
  plot_ly(
    plot_data,
    x = ~clean_labels,  # Use clean labels for x-axis ticks
    y = as.formula(paste0("~`", input$feature, "`")),
    type = 'box',
    color = ~legend_labels,  # Use original labels for legend
    boxpoints = 'all',
    jitter = 0.3,
    pointpos = -1.8
  ) %>%
    layout(
      title = list(
        text = paste("EEG Power at", gsub(".*\\.", "", gsub("AB\\.A\\.[^.]*\\.[^.]*\\.", "", input$feature)), "Electrode"),
        x = 0.5,
        xanchor = 'center'
      ),
      xaxis = list(title = "Specific Disorder"),
      yaxis = list(
        title = "EEG Power",
        titlefont = list(size = 14)
      ),
      margin = list(b = 150, t = 80),
      annotations = list(
        list(
          text = "EEG power at chosen electrodes across disorders versus healthy controls",
          x = 0.5,
          y = 1.05,
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          xanchor = "center",
          font = list(size = 10, color = "gray")
        )
      )
    )
})
#
#
#
selectInput("feature", "Choose EEG Feature:",
           choices = names(x)[grep("AB.A", names(x))],
           selected = "AB.A.delta.a.FP1")

plotlyOutput("interactivePlot")
#
#
#
#
#
# FP1 and FP2 columns
fp1_col <- "AB.A.delta.a.FP1"
fp2_col <- "AB.A.delta.b.FP2"

# Filter only rows with FP1 & FP2 values
clean_data <- data %>%
  filter(!is.na(.data[[fp1_col]]) & !is.na(.data[[fp2_col]])) %>%
  select(specific.disorder,
         FP1 = all_of(fp1_col),
         FP2 = all_of(fp2_col))

# Short label for axis (remove 'disorder' but keep full in legend)
clean_data <- clean_data %>%
  mutate(short_label = gsub(" disorder", "", specific.disorder))

# Long format
plot_data <- clean_data %>%
  pivot_longer(cols = c(FP1, FP2),
               names_to = "Electrode",
               values_to = "DeltaPower")

# Create interactive violin plot with enhanced features
p_violin <- plot_ly(
  data = plot_data,
  x = ~short_label,
  y = ~DeltaPower,
  color = ~specific.disorder,
  type = 'violin',
  split = ~Electrode,
  box = list(visible = TRUE),
  meanline = list(visible = TRUE),
  hovertemplate = paste(
    "<b>Disorder:</b> %{fullData.name}<br>",
    "<b>Electrode:</b> %{text}<br>",
    "<b>Delta Power:</b> %{y:.3f}<br>",
    "<extra></extra>"
  ),
  text = ~Electrode
) %>%
  layout(
    title = list(
      text = "Delta Power at FP1 and FP2 Across Psychiatric Disorders",
      x = 0.5,
      xanchor = 'center'
    ),
    yaxis = list(title = "Delta Power"),
    xaxis = list(title = "Specific Disorder"),
    legend = list(orientation = "v", x = 1.02, y = 1),
    margin = list(r = 150),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 0.5,
        y = -0.15,
        showactive = TRUE,
        buttons = list(
          list(
            label = "Show All",
            method = "restyle",
            args = list("visible", rep(TRUE, length(unique(plot_data$specific.disorder))))
          ),
          list(
            label = "Alcohol & Schizophrenia Only",
            method = "restyle", 
            args = list("visible", ifelse(unique(plot_data$specific.disorder) %in% c("Alcohol use disorder", "Schizophrenia"), TRUE, FALSE))
          )
        )
      )
    )
  )

p_violin

# Create a focused view for Alcohol and Schizophrenia
alcohol_schizo_data <- plot_data %>%
  filter(specific.disorder %in% c("Alcohol use disorder", "Schizophrenia"))

p_focused <- plot_ly(
  data = alcohol_schizo_data,
  x = ~short_label,
  y = ~DeltaPower,
  color = ~specific.disorder,
  type = 'violin',
  split = ~Electrode,
  box = list(visible = TRUE),
  meanline = list(visible = TRUE),
  hovertemplate = paste(
    "<b>Disorder:</b> %{fullData.name}<br>",
    "<b>Electrode:</b> %{text}<br>",
    "<b>Delta Power:</b> %{y:.3f}<br>",
    "<extra></extra>"
  ),
  text = ~Electrode
) %>%
  layout(
    title = list(
      text = "Focus: Delta Power in Alcohol Use Disorder vs Schizophrenia",
      x = 0.5,
      xanchor = 'center'
    ),
    yaxis = list(title = "Delta Power"),
    xaxis = list(title = "Specific Disorder"),
    showlegend = TRUE
  )

p_focused
#
#
#
#
#
# Identify key delta electrodes by exact match
key_delta_cols <- c("AB.A.delta.a.FP1",
                    "AB.A.delta.d.F3",
                    "AB.A.delta.i.C3",
                    "AB.A.delta.o.Pz",
                    "AB.A.delta.r.O1")

# Pivot longer for plotting
data_long <- data %>%
  select(specific.disorder, all_of(key_delta_cols)) %>%
  pivot_longer(cols = -specific.disorder,
               names_to = "Position",
               values_to = "DeltaPower") %>%
  filter(!is.na(DeltaPower), !is.na(specific.disorder))

# Clean position names (remove prefix)
data_long$Position <- sub(".*\\.", "", data_long$Position)

# Create static ggplot and convert to interactive plotly
g <- ggplot(data_long, aes(x = specific.disorder, y = DeltaPower, fill = specific.disorder)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.size = 0.5, alpha = 0.5) +
  facet_wrap(~ Position, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  labs(title = "Delta Power at Key Electrode Positions by Specific Disorder",
       x = "Specific Disorder",
       y = "Delta Power") +
  scale_fill_viridis_d()

ggplotly(g, tooltip = c("x", "y")) %>%
  layout(height = 600)
#
#
#
#
#
# Create interactive scatter plot
scatter_data <- x %>%
  filter(!is.na(AB.A.delta.a.FP1), !is.na(AB.A.delta.b.FP2), !is.na(specific.disorder))

p_scatter <- plot_ly(
  scatter_data,
  x = ~AB.A.delta.a.FP1,
  y = ~AB.A.delta.b.FP2,
  color = ~specific.disorder,
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 8, opacity = 0.7),
  hovertemplate = paste(
    "<b>Disorder:</b> %{fullData.name}<br>",
    "<b>FP1 Delta Power:</b> %{x:.3f}<br>",
    "<b>FP2 Delta Power:</b> %{y:.3f}<br>",
    "<extra></extra>"
  )
) %>%
  layout(
    title = list(
      text = "FP1 vs FP2 Delta Power by Psychiatric Disorder",
      x = 0.5,
      xanchor = 'center'
    ),
    xaxis = list(title = "Absolute Delta Power (FP1)"),
    yaxis = list(title = "Absolute Delta Power (FP2)")
  )

p_scatter
#
#
#
#
#
summary_stats <- x %>%
  group_by(specific.disorder) %>%
  summarise(
    mean_fp1 = mean(AB.A.delta.a.FP1, na.rm = TRUE),
    mean_fp2 = mean(AB.A.delta.b.FP2, na.rm = TRUE),
    se_fp1 = sd(AB.A.delta.a.FP1, na.rm = TRUE)/sqrt(sum(!is.na(AB.A.delta.a.FP1))),
    se_fp2 = sd(AB.A.delta.b.FP2, na.rm = TRUE)/sqrt(sum(!is.na(AB.A.delta.b.FP2))),
    n_fp1 = sum(!is.na(AB.A.delta.a.FP1)),
    n_fp2 = sum(!is.na(AB.A.delta.b.FP2)),
    .groups = "drop"
  )

# Create interactive bar chart with enhanced hover info
p_bar <- plot_ly() %>%
  add_trace(
    data = summary_stats,
    x = ~specific.disorder,
    y = ~mean_fp1,
    type = 'bar',
    name = 'FP1',
    error_y = list(
      type = 'data',
      array = ~1.96 * se_fp1,
      visible = TRUE
    ),
    marker = list(color = '#2E86AB'),
    hovertemplate = paste(
      "<b>Electrode:</b> FP1<br>",
      "<b>Disorder:</b> %{x}<br>",
      "<b>Mean Delta Power:</b> %{y:.3f}<br>",
      "<b>Sample Size:</b> %{customdata}<br>",
      "<extra></extra>"
    ),
    customdata = ~n_fp1
  ) %>%
  add_trace(
    data = summary_stats,
    x = ~specific.disorder,
    y = ~mean_fp2,
    type = 'bar',
    name = 'FP2',
    error_y = list(
      type = 'data',
      array = ~1.96 * se_fp2,
      visible = TRUE
    ),
    marker = list(color = '#A23B72'),
    hovertemplate = paste(
      "<b>Electrode:</b> FP2<br>",
      "<b>Disorder:</b> %{x}<br>",
      "<b>Mean Delta Power:</b> %{y:.3f}<br>",
      "<b>Sample Size:</b> %{customdata}<br>",
      "<extra></extra>"
    ),
    customdata = ~n_fp2
  ) %>%
  layout(
    title = list(
      text = "Mean Delta Power (±95% CI) by Disorder",
      x = 0.5,
      xanchor = 'center'
    ),
    xaxis = list(title = "Specific Disorder"),
    yaxis = list(title = "Mean Absolute Delta Power"),
    barmode = 'group'
  )

p_bar
#
#
#
#
#
# Create a comprehensive summary table
summary_table <- summary_stats %>%
  select(specific.disorder, mean_fp1, mean_fp2, se_fp1, se_fp2, n_fp1, n_fp2) %>%
  mutate(
    across(c(mean_fp1, mean_fp2, se_fp1, se_fp2), ~round(.x, 3))
  ) %>%
  arrange(desc(mean_fp1))  # Sort by FP1 mean values

knitr::kable(
  summary_table,
  col.names = c("Disorder", "Mean FP1", "Mean FP2", "SE FP1", "SE FP2", "N FP1", "N FP2"),
  caption = "Summary Statistics for Delta Power at FP1 and FP2 Electrodes (sorted by FP1 mean)"
)
#
#
#
#
#
# Calculate correlations by disorder
correlation_stats <- x %>%
  filter(!is.na(AB.A.delta.a.FP1), !is.na(AB.A.delta.b.FP2)) %>%
  group_by(specific.disorder) %>%
  summarise(
    correlation = cor(AB.A.delta.a.FP1, AB.A.delta.b.FP2, use = "complete.obs"),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(correlation))

knitr::kable(
  correlation_stats %>% mutate(correlation = round(correlation, 3)),
  col.names = c("Disorder", "FP1-FP2 Correlation", "N Observations"),
  caption = "Correlation between FP1 and FP2 Delta Power by Disorder"
)
#
#
#
#
#
