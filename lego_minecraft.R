# TidyTuesday | September 6, 2022 | Week 36 Lego
# Data sources are rebrickable.com and brickeconomy.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(scales)

# add font ----------------------------------------------------------------
font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()

# get data ----------------------------------------------------------------
inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
pricing <- readr::read_csv('Pricing.csv')

# combine data sets and filter for minecraft ------------------------------
all_df <- left_join(inventories, inventory_sets, by = "set_num") |>
  left_join(sets, by = "set_num") %>% 
  filter(theme_id == 577) %>% 
  filter(!version == 2) %>% 
  filter(!name == "Minecraft Bundle")

# combine price data from brickeconomy and wrangle data -------------------
df <- left_join(all_df, pricing, by = "set_num") %>% 
  unique() %>% # an issue with my join creating duplicates...using unique() as a hack to deal with issue
  select(id, set_num, name, year, num_parts, retail_price, current_price, Retired) %>% 
  na.omit() %>% 
  mutate(growth_rate = round(((current_price - retail_price) / retail_price) * 100, 1)) %>% 
  mutate(color = ifelse(growth_rate >= 200, "Above", "Below"))
  
# create plot -------------------------------------------------------------
df %>%
  ggplot(aes(x = growth_rate, y = reorder(name, growth_rate, sum), fill = color)) +
  geom_col() +
  geom_text(aes(label = ifelse(growth_rate >= 200, paste0(growth_rate,"%"), "")), color = "#000000", size = 3, family = font, position = position_dodge2(width = 0.5), hjust = 1.2) +
  scale_x_continuous(expand = c(0, 0), limits = c(NA, 600), labels = percent_format(scale = 1)) +
  scale_fill_manual(values = c("#FFAA00", "#AAAAAA")) +
  annotate(geom = "curve", x = 450, y = "The Skeleton Attack", xend = 500, yend = "The Ice Spikes",curvature = -0.2, size = 0.5,  arrow = arrow(length = unit(2, "mm")), color = "#000000") +
  annotate(geom = "text", y = "The Jungle Tree House", x = 450, label = "Released in 2017 for $249.99,\nThe Mountain Cave has a current\nestimated value of $1,746.79.", hjust = "center", family = font, size = 3, color = "#000000") +
  theme_minimal() +
  theme(text = element_text(size = 9, family = font, color = "#000000"),
        plot.title = element_text(family = font, size = 28, hjust = 0, face = "bold"),
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.subtitle = element_text(family = font, size = 12, hjust = 0),
        plot.caption = element_text(family = font, size = 10, hjust = 0.5),
        axis.title.x = element_text(size = 9, family = font, color = "#000000"),
        axis.text = element_text(size = 9, family = font, color = "#000000"),
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#000000", size = 0.25, linetype = "blank"),
        axis.line.x.bottom = element_line(color = "#000000", size = 0.4),
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm"),
        plot.background = element_rect(color = "#F2F2F2", fill = "#F2F2F2")) +
  labs(title = "LEGO MINECRAFT",
       subtitle = "Since Lego introduced the Minecraft theme in 2013, 55 Lego sets have been retired,\ncreating a massive demand on secondary markets like Amazon and eBay. Collectors,\nhobbyists, and resellers are mining for gold, with nine of the retired sets having an\nestimated value greater than 200% over the original retail price.\n",
       caption = "\n\n#TidyTuesday | Data: rebrickable.com & brickeconomy.com | Design: Ryan Hart",
       x = "\nGrowth rate based on estimated market prices (as of 9.6.2022)")

# save plot ---------------------------------------------------------------
ggsave(paste0("lego_minecraft_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 8, height = 12)

