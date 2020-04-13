## Tracey Mangin
## November 12, 2019
## BP1 Nature manuscript -- theoretical curves

library(tidyverse)
library(stats)


## backward bending supply curve
calc_quant <- function(r_val, c_val, p_val, K_val, q_val) {
  
  quant_val <- (r_val * c_val) / (p_val * q_val) * (1 - (c_val / (p_val * q_val * K_val)))
  
}
bb_curve_df <- data.frame(p_val = seq(0, 1, 0.1)) %>%
  rowwise() %>%
  mutate(quant_val = calc_quant(r_val = 0.1, c_val = 10, p_val = p_val, K_val = 500, q_val = 1)) %>%
  ungroup() %>%
  mutate(quant_val = ifelse(p_val == 0, 0, quant_val),
         q_mang = ifelse())

ggplot(bb_curve_df, aes(x = quant_val, y = p_val)) +
  geom_path(size = 2)

## log normal distribution
bb_curve_df2 <- data.frame(price = seq(0, 3, 0.1)) %>%
  rowwise() %>%
  mutate(quantity = dlnorm(price, meanlog = 0, sdlog = 1, log = FALSE)) %>%
  ungroup()

dcurve_df <- data.frame(price = seq(0, 3, 0.1)) %>%
  rowwise() %>%
  mutate(quantity = (-0.5 * price) + 0.5) %>%
  ungroup() %>%
  filter(quantity >= 0)
  
dcurve_df2 <- data.frame(price = seq(0, 3, 0.1)) %>%
  rowwise() %>%
  mutate(q2 = (-0.4 * price) + 1.1) %>%
  ungroup() %>%
  filter(q2 >= 0)

dcurves <- left_join(dcurve_df2, dcurve_df)

rp_fig <-
ggplot(bb_curve_df2 %>% filter(price <= 2.3), aes(x = quantity, y = price)) +
  geom_path(size = 2) +
  geom_line(data = dcurves, aes(x = quantity, y = price), color = "blue", size = 2) +
  xlim(0, 1.25) +
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

rp_fig2 <-
  ggplot(bb_curve_df2 %>% filter(price <= 2.3), aes(x = quantity, y = price)) +
  geom_path(size = 2) +
  xlim(0, 1.25) +
  geom_line(data = dcurve_df, aes(x = quantity, y = price), color = "blue", size = 2, alpha = 0.5) +
  geom_line(data = dcurve_df2, aes(x = q2, y = price), color = "blue", size = 2) +
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


rp_fig3 <- ggplot(dcurves, aes(x = quantity, y = price), color = "blue", size = 2, alpha = 0.5) +
  geom_line(size = 2, color = "blue") +
  # geom_line(data = dcurve_df2, aes(x = q2, y = price), color = "blue", size = 2) +
  xlim(0, 1.25) +
  geom_vline(xintercept = 0.9, size = 2, color = "black") +
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

rp_fig4 <-
  ggplot(bb_curve_df2, aes(x = quantity, y = price)) +
  geom_path(size = 2) +
  xlim(0, 1.25) +
  geom_line(data = dcurve_df, aes(x = quantity, y = price), color = "blue", size = 2, alpha = 0.5) +
  geom_line(data = dcurve_df2, aes(x = q2, y = price), color = "blue", size = 2) +
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

rp_fig5 <-
  ggplot(bb_curve_df2 %>% filter(price <= 2.3), aes(x = quantity, y = price)) +
  geom_path(size = 2, alpha = 0.5) +
  geom_line(data = dcurves, aes(x = quantity, y = price), color = "blue", size = 2) +
  xlim(0, 1.25) +
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 12),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))


## we never derived what would happen if you didn't intervene
## for mariculture... we either need to make one up or not need one... which is dicy



