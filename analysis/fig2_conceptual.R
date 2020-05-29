## Future food from the sea
## main text figure 2
## conceptual curves

library(tidyverse)
library(ggrepel)


N = 10000

DF1 = data.frame(PwOA=seq(.88,4.5,length.out=N)) %>%
  mutate(PwMSY=seq(.5,4.5,length.out=N)) %>%
  mutate(QwM = seq(1,3.422,length.out = N)) %>%
  mutate(QwOA = 10/PwOA - 8/PwOA^2) %>%
  mutate(PwM = -.3/(QwM-3.5) +.6 ) %>%
  mutate(QwMSY = 3.5) %>%
  select(QwOA,PwOA,QwM,PwM,QwMSY,PwMSY)

DF2 = data.frame(Pm1=seq(.88,4.5,length.out=N)) %>%
  mutate(Pm2=seq(.9,4.5,length.out=N)) %>%
  mutate(tmp = seq(1,3.422,length.out = N)) %>%
  mutate(Qm1 = 10/Pm1 - 8/Pm1^2) %>%
  mutate(Pm3 = -.3/(tmp-3.5) +.7 ) %>%
  mutate(Qm2 = 1.4) %>%
  mutate(Qm4 = .7*(tmp-1)+tmp) %>%
  mutate(Qm3 = .1*(tmp-1)+tmp) %>%
  mutate(Pm4 =  -.3/(tmp-3.5) +.7 ) %>%
  select(Qm1,Pm1,Qm2,Pm2,Qm3,Pm3,Qm4,Pm4)

DF_concept = bind_cols(DF1,DF2) #This is the dataframe you need


P2a = ggplot(data=DF_concept) +
  geom_path(aes(x=QwOA,y=PwOA),size=2) +
  geom_path(aes(x=QwM,y=PwM),size=2,color="blue") +
  geom_line(aes(x=QwMSY,y=PwMSY),size=2,color="green") +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  theme(axis.line.x=element_line(color="black",size=2)) +
  theme(axis.line.y=element_line(color="black",size=2)) +
  theme(axis.title=element_text(color="black",size=20)) +
  xlab("Q") +
  ylab("P")
P2a

P2b = ggplot(data=DF_concept) +
  geom_path(aes(x=Qm1,y=Pm1),size=2) +
  geom_line(aes(x=Qm2,y=Pm2),size=2,color="green") +
  geom_path(aes(x=Qm3,y=Pm3),size=2,color="blue") +
  geom_path(aes(x=Qm4,y=Pm4),size=2,color="red") +
  theme(axis.text=element_blank(),axis.ticks=element_blank()) +
  theme(axis.line.x=element_line(color="black",size=2)) +
  theme(axis.line.y=element_line(color="black",size=2)) +
  theme(axis.title=element_text(color="black",size=20)) +
  xlab("Q") +
  ylab("P")
P2b


## organize the data
## -------------------------------

wild1 <- DF_concept %>%
  select(quantity = QwOA, price = PwOA) %>%
  mutate(sector = "Wild fisheries",
         scenario = "SwOA")

wild2 <- DF_concept %>%
  select(quantity = QwM, price = PwM) %>%
  mutate(sector = "Wild fisheries",
         scenario = "SwM")

wild3 <- DF_concept %>%
  select(quantity = QwMSY, price = PwMSY) %>%
  mutate(sector = "Wild fisheries",
         scenario = "SwMSY")

m1 <- DF_concept %>%
  select(quantity = Qm1, price = Pm1) %>%
  mutate(sector = "Mariculture",
         scenario = "Sm1")

m2 <- DF_concept %>%
  select(quantity = Qm2, price = Pm2) %>%
  mutate(sector = "Mariculture",
         scenario = "Sm2")

m3 <- DF_concept %>%
  select(quantity = Qm3, price = Pm3) %>%
  mutate(sector = "Mariculture",
         scenario = "Sm3")


m4 <- DF_concept %>%
  select(quantity = Qm4, price = Pm4) %>%
  mutate(sector = "Mariculture",
         scenario = "Sm4")

all_curves <- rbind(wild1, wild2, wild3, m1, m2, m3, m4)

all_curves$sector <- factor(all_curves$sector, levels = c("Wild fisheries", "Mariculture"))

fig2_wild <- all_curves %>%
  filter(sector == "Wild fisheries") %>%
  rename(management = scenario)

fig2_mar <- all_curves %>%
  filter(sector == "Mariculture")

##
wild_labs <- fig2_wild %>%
  group_by(management) %>%
  filter(price == max(price)) %>%
  ungroup()

wild_labs2 <- data.frame(lab = c('OA', 'R', 'MSY'))

wild_labs3 <- cbind(wild_labs, wild_labs2) %>%
  select(-management)


mar_labs <- fig2_mar %>%
  group_by(scenario) %>%
  filter(price == max(price)) %>%
  ungroup() 

mar_labs2 <- data.frame(lab = c('M1', 'M2', 'M3', 'M4'))

mar_labs3 <- cbind(mar_labs, mar_labs2) %>%
  select(-scenario)

all_labs <- rbind(mar_labs3, wild_labs3)

## figure with letters
fig2_wild2 <- fig2_wild %>%
  mutate(sector_lab = paste0("a ", sector))

fig2_mar2 <- fig2_mar %>%
  mutate(sector_lab = paste0("b ", sector))

all_labs2 <- all_labs %>%
  mutate(sector_lab = ifelse(sector == "Mariculture", paste0("b ", sector), paste0("a ", sector)))

conceptual_fig <- ggplot(data = fig2_mar2, aes(x = quantity, y = price, group = scenario, color = scenario)) +
  geom_path(alpha = 0.9, size = 0.4) +
  geom_path(data = fig2_wild2, aes(x = quantity, y = price, group = management, lty = management), color = "black", size = 0.4, alpha = 0.9, inherit.aes = F) +
  facet_wrap(~sector_lab, scales = "free") + 
  xlab("Quantity") +
  ylab("Price") +
  geom_text_repel(data = all_labs2, aes(x = quantity, y = price - 0.2, label = lab), 
                  size = 2, inherit.aes = F) +
  # scale_color_manual(values = c("#00798c", "#edae49", "#d1495b")) +
  scale_color_manual(breaks = c("Sm1", "Sm2", "Sm3", "Sm4"),
                     values = c("Sm1" = "#D55E00",
                                "Sm2" = "#F0E442",
                                "Sm3" = "#0072B2",
                                "Sm4" = "#009E73")) +
  scale_linetype_manual(breaks = c("SwOA", "SwM", "SwMSY"),
                        values = c("SwOA" = 1,
                                   "SwM" = 4,
                                   "SwMSY" = 3)) +
  # geom_hline(yintercept = pval0, lty = "dashed", color = "black") +
  # annotate("text", x = 300, y = pval0 + 75, label = paste0("Current weighted average global price = $", format(round(pval0), nsmall=0, big.mark=","), " / mt"), size = 5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 7),
        title = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.position = "none",
        # legend.text = element_text(size = 15),
        strip.text = element_text(size = 8, hjust = 0),
        strip.background = element_rect(color = "white", fill = "white"))

## save path for figures
savepath <- "/Volumes/GoogleDrive/Shared\ drives/emlab/projects/current-projects/blue-paper-1/project-materials/nature-revision-2/figures/"
# ggsave(filename =  paste0(savepath, "fig2_conceptual.png"), conceptual_fig, width = 14, height = 8, units = "in", dpi = 300)
ggsave(filename =  paste0(savepath, "fig2.pdf"), conceptual_fig, width = 120, height = 60, units = "mm", dpi = 600)

conceptual_fig2 <- ggplot(data = fig2_mar2, aes(x = quantity, y = price, group = scenario, color = scenario)) +
  geom_path(alpha = 0.9, size = 0.6) +
  geom_path(data = fig2_wild2, aes(x = quantity, y = price, group = management, lty = management), color = "black", size = 0.6, alpha = 0.9, inherit.aes = F) +
  facet_wrap(~sector_lab, scales = "free", ncol = 1) + 
  xlab("Quantity") +
  ylab("Price") +
  geom_text_repel(data = all_labs2, aes(x = quantity, y = price - 0.2, label = lab),
                  size = 5, inherit.aes = F) +
  # scale_color_manual(values = c("#00798c", "#edae49", "#d1495b")) +
  scale_color_manual(breaks = c("Sm1", "Sm2", "Sm3", "Sm4"),
                     values = c("Sm1" = "#D55E00",
                                "Sm2" = "#F0E442",
                                "Sm3" = "#009E73",
                                "Sm4" = "#0072B2")) +
  scale_linetype_manual(breaks = c("SwOA", "SwM", "SwMSY"),
                        values = c("SwOA" = 1,
                                   "SwM" = 4,
                                   "SwMSY" = 3)) +
  # geom_hline(yintercept = pval0, lty = "dashed", color = "black") +
  # annotate("text", x = 300, y = pval0 + 75, label = paste0("Current weighted average global price = $", format(round(pval0), nsmall=0, big.mark=","), " / mt"), size = 5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 10),
        title = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.position = "none",
        # legend.text = element_text(size = 15),
        strip.text = element_text(size = 10, hjust = 0),
        strip.background = element_rect(color = "white", fill = "white"))

ggsave(filename =  paste0(savepath, "fig2_1col.pdf"), conceptual_fig2, width = 89, height = 150, units = "mm", dpi = 600)


## figure 2 in main text
## note to user: update path and save
ggsave(filename = "fig2_1col.pdf", conceptual_fig2, width = 89, height = 150, units = "mm", dpi = 600)





