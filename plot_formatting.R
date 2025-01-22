### NASTE Style Guide ###

library(tidyverse)

# Plot Theme - use this at the top of your script and all following plots will follow this theme
theme_set(
  theme_minimal(base_size = 15) +                             # Set base theme
    theme(
      plot.title = element_text(size = 18, face = "bold"),    # Customize title
      axis.title = element_text(size = 15),                   # Customize axis titles
      axis.text = element_text(size = 15),                    # Customize axis text
      legend.title = element_text(size = 15),                 # Customize legend title
      legend.text = element_text(size = 15)                  # Customize legend text
    )
)


# Define colors for the treatments - change treatment names if needed (ie "C" instead of "Control")
naste_colors <- c("Control" = "#08b5d3",  
                  "Effluent" = "#e12618",   
                  "Guano" = "#01ad74",   
                  "Inorganic" = "#d9a33a")  

