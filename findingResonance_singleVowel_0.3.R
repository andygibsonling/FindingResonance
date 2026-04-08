# -------------------------------------------------------------------------
# Finding Resonance - single vowel mode 
# Andy Gibson — 2025-11-11
# -------------------------------------------------------------------------

#### NB: CHAT GPT WAS USED IN CODE DESIGN AND EDITING


# if you wanna clean up your environment:
#rm(list = ls())

library(tidyverse)
library(patchwork)

# set wd to script location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# get current time
datestamp <- format(Sys.time(), "%Y%m%d_%H%M")

# --- Read CSV and set up output folder ---
results_path <- readLines("latest_results.txt")
df <- read.csv(results_path)
output_dir <- readLines("latest_run.txt")


# Only keep rows where f0 != 1
df <- df %>% filter(f0 != 1)

df <- df %>% mutate(token_id = paste(Token,trimws(Vowel),sep="_"))

token <- unique(df$token_id)

nearest_dist_cents <- function(F, harmonics) {
  min(abs(1200 * log2(F / harmonics)))
}

# --- Helper: cents to Hz ---
cents_to_hz <- function(freq, cents){
  lo <- freq * 2^(-cents/1200)
  hi <- freq * 2^( cents/1200)
  c(lo, hi)
}

# Build rectangles for harmonics----
# --- Parameters ---
k_list <- 1:12
F1_limits <- c(0, 1000)
F2_limits <- c(500, 3000)



tokens <- unique(df$token_id)

for(token in tokens){
  df_token <- df %>% filter(token == token_id)
  f0 <- unique(df_token$f0)
  F1 <- unique(df_token$F1)
  F2 <- unique(df_token$F2)
  F3 <- unique(df_token$F3)
  BW1 <- unique(df_token$BW1)
  BW2 <- unique(df_token$BW2)
  
  harmonics <- f0 * (1:50)
  d1 <- nearest_dist_cents(F1, harmonics)
  d2 <- nearest_dist_cents(F2, harmonics)
  d3 <- nearest_dist_cents(F3, harmonics)
  
  show_f3 <- (d3 < d1) & (d3 < d2)
  closestFormant <- min(d1,d2,d3)
  
  x_max <- (F2 + BW2/2) * 1.1
  if(show_f3){x_max <- F3 * 1.1}
  y_max <- (F1 + BW1/2) * 1.1
  
  # --- Rebuild harmonic rectangles ---
  rects <- tibble()
  for(k in 1:20){
    f <- f0 * k
    band_inner <- cents_to_hz(f, 25)
    band_outer <- cents_to_hz(f, 50)
    
    rects <- bind_rows(
      rects,
      tibble(xmin = F2_limits[2], xmax = F2_limits[1],
             ymin = band_inner[1], ymax = band_inner[2], fill = "green", k = k),
      tibble(xmin = band_inner[1], xmax = band_inner[2],
             ymin = F1_limits[1], ymax = F1_limits[2], fill = "green", k = k))
  }
  
  # --- Determine most prominent harmonic ---
  max_prom_row <- df_token %>% filter(TargetProm == max(TargetProm))
  most_prom_harmonic <- max_prom_row$Harmonic
  most_prom_value <- max_prom_row$TargetProm
  
  # --- Map prominence to color (hardwired limits: 5 dB to +15 dB) ---
  prom_value_clamped <- pmin(pmax(most_prom_value, -15), 15)
  
  prom_color <- scales::col_numeric(
    palette = c("blue", "red"),
    domain = c(8, 15)
  )(prom_value_clamped)
  
  
  # --- Sweet spot plot with harmonic label ---
  sweet_plot <- ggplot() +
    # Harmonic rectangles
    geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill),
              alpha = 0.15, color = NA) +
    scale_fill_identity() +
    # Vowel bandwidth
    geom_rect(aes(xmin = F2 - BW2/2, xmax = F2 + BW2/2,
                  ymin = F1 - BW1/2, ymax = F1 + BW1/2),
              fill = "black", alpha = 0.3) +
    # Vowel label in black (above harmonic label)
    geom_text(aes(x = F2, y = F1, label = token),
              color = "black", size = 3) +
    # Most prominent harmonic label in color
    #geom_text(aes(x = F2, y = F1 + 30, label = paste0("H", most_prom_harmonic)),
    #          color = prom_color, size = 2, vjust = 1, hjust = 0.5) +
    # Harmonic numbers in grey for reference
    geom_text(
      data = rects %>%
        filter(fill == "green", xmin == F2_limits[2]) %>%  # horizontal bands
        mutate(y = (ymin + ymax)/2),
      aes(x = F2*0.75, y = y, label = paste0("H",k)),  # push slightly inside visible area
      size = 5,
      color = "darkblue",
      hjust = 0
    ) +
    geom_text(
      data = rects %>%
        filter(fill == "green", xmin != F2_limits[2]) %>%  # vertical bands
        mutate(x = (xmin + xmax)/2),
      aes(x = x, y = F1*0.75, label = paste0("H",k)),  # push slightly inside
      size = 5,
      color = "darkgrey",
      vjust = 1
    ) +
    scale_x_reverse(name = "F2 (Hz)",expand = c(0,0)) +
    scale_y_reverse(name = "F1 (Hz)",expand = c(0,0)) +
    coord_cartesian(xlim = c(x_max, F2*0.5), ylim = c(y_max, F1*0.5)) +
    ggtitle(paste0("F0 for token ", token, ": ", round(f0,1), " Hz)")) +
    theme_bw(base_size = 13)

sweet_plot  
  
  # If F3 is closest, Add arrow from F2 to F3 on F2 axis
  if(show_f3){sweet_plot <- sweet_plot +    
    geom_segment(aes(x = F2 + 100, y = F1, xend = F3, yend = F1), 
                 arrow = arrow(length = unit(0.2, "cm")), colour = "purple")
  }
    
  
  # --- Prominence plot ---
  prom_plot <- ggplot(df_token, aes(x = Harmonic, y = TargetProm)) +
    geom_line(aes(group = 1), size = 1, color = "#1b9e77") +
    geom_point(size = 3, color = "#1b9e77") +
    geom_text(aes(label = ifelse(TargetProm == max(TargetProm), paste0("H", Harmonic), "")),
              vjust = 2, size = 3.5) +
    scale_x_continuous(limits = c(2,13), breaks = c(2:13)) +
    coord_cartesian(ylim = c(min(df_token$TargetProm)*1.2,
        max(df_token$TargetProm)*1.2)) +    
    labs(title = paste0("Harmonic Prominence – ", token),
         x = "Harmonic Number",
         y = "Prominence (target H dB - mean \n of neighbour H dBs)") +
    theme_bw(base_size = 14) +
    theme(panel.grid.minor.x = element_blank())
  
  # --- Shorten/prom plot ---
  prom_plot_short <- prom_plot + theme(
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )
  
  # --- Combine and save ---
  combined_plot <- sweet_plot / prom_plot_short + plot_layout(heights = c(3,1))
  
  ggsave(filename = file.path(output_dir, paste0(token, "_combined_", datestamp, ".png")),
         plot = combined_plot, width = 8, height = 7, dpi = 300)
}

