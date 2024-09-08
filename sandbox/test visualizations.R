library(ggplot2)
library(ggthemes)
library(ggrepel)

tar_load(binary_LP_result)


rates <- binary_LP_result$rate_df


rates_long <- rates %>% 
  pivot_longer(cols = 1:3)


ggplot(rates_long, aes(group = name, colour = name, shape = boundry, x = iteration, y = value)) + 
  geom_point(size = 5, alpha = 0.5) + 
  geom_line(lty = 2, colour = "black") + 
  theme_few() + 
  geom_label_repel(data = rates_long %>% 
              dplyr::filter(boundry == "lower") %>% 
              group_by(name) %>% 
              slice_max(iteration, n = 1),
            aes(label = round(value, 3), size = 2),
            nudge_y = 1.5)
