library(tidyverse)
library(patchwork)
phases = c("Early exponential", "Late exponential", "Stationary/death")
phases_flt = c("Early exponential", "Late exponential", "Stationary/death")

# Toggle line 7 and 8 to plot absolute number and proportion :)

dat <- read_csv("fluxSampling_subsystems_for_ggplot2_km.csv") %>% 
  mutate(down = -down) %>% 
  pivot_longer(c(down, up), names_to = "direction") %>% 
  filter(abs(value) > 1) %>% 
  pivot_wider(names_from = "phase", values_fill = NA) %>% 
  pivot_longer(any_of(phases), names_to = "phase") %>%
  pivot_wider(names_from = direction, values_fill = NA) %>% 
  arrange(sumValues) %>% 
  mutate(order = 1:nrow(.),
         phase = factor(phase, levels = phases, ordered = T))

#dat <- read_csv("fluxSampling_subsystems_proportions_for_ggplot2.csv") %>% 

g_up <- ggplot(dat, aes(x = up, y = reorder(ss, order), color = phase)) +
  geom_segment(aes(x=up, xend=0, y=reorder(ss, order), 
                   yend=reorder(ss, order)),
               alpha= 0.6) +
  geom_point( size=5, alpha = 0.6) +
  #  facet_wrap(~direction, scales = "free_x") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        strip.text = element_text(vjust = 1),
        axis.title = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(labels = abs,
                     limits=c(0, max(c(abs(dat$down), abs(dat$up)), na.rm = T))
  ) +
 # scale_y_discrete(position = "right") +
 # geom_vline(xintercept = 0, color = "white", linewidth = 4) +
  scale_color_manual(values=c("red", "green", "violet"))
  
g_down <- ggplot(dat, aes(x = down, y = reorder(ss, order), color = phase)) +
  geom_segment(aes(x=down, xend=0, y=reorder(ss, order), 
                   yend=reorder(ss, order)),
               alpha = 0.6) +
  geom_point( size=5, alpha = 0.6) +
  #  facet_wrap(~direction, scales = "free_x") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(),
        panel.grid.minor.x = element_line(),
        strip.text = element_text(vjust = 1),
        axis.title = element_blank(), 
        axis.text.y.right = element_text(hjust = 0.5, margin= margin(r = 0, l = 0))) +
  scale_x_continuous(labels = abs,
                     limits=c(-max(c(abs(dat$down), abs(dat$up)), na.rm = T), 0)
  ) +
  scale_y_discrete(position = "right") +
 # geom_vline(xintercept = 0, color = "white", linewidth = 4) +
  scale_color_manual(values=c("red", "green", "violet"))
  
g_down + g_up + plot_layout(guides = "collect")

#ggsave("results/figs/010824_fluxSampling_subsystems_nReactions.tiff")


