#===============================================================================
#
# Load some "dependencies" and set up directory structure. Some 
# of this is done automatically in the processUSDOS() function below,
# but this ensures all packages and colors are loaded for plotting.
#
#===============================================================================
{
  # This will be the default directory
  path0 <- dirname(rstudioapi::getActiveDocumentContext()$path)
  # Location of output files
  pathfiles <- file.path(path0, "Files_To_Process/")
  
  # Generate directories for maps (map_output), non-map figures (plot_output), and data files (data_output)
  plot_output <- file.path(path0, "Figures/")
  map_output <- file.path(path0, "Maps/")
  data_output <- file.path(path0, "Data/")
  
  # This directory contains R scripts with some necessary functions
  dependencies <- file.path(path0,"Include_Files/")
  
  # Colors for comparison figures
  static_col <- "#BC5369"
  state_dep_col <- "#3E8CC9"
  no_control_col <- "#CECD42"
  delay1_col <- "grey90"
  delay2_col <- "grey70"
  delay3_col <- "grey50"
  delay_colors <- colorRampPalette(c("black",state_dep_col))(3)
  
  # 1) Load necessary packages, 2) load functions for importing data files, 3) load map by fips functions
  source(paste0(dependencies,"Package_Manager.R"))
  source(paste0(dependencies,"Data_Manager.R"))
  source(paste0(dependencies,"map_by_fips_standalone.R"))
  
  Dur_min = 23
  Dur_cutoff = 125
  PremInf_min = 10 
  PremInf_cutoff = 5000
  ReportedPrems_min = 5
  ReportedPrems_cutoff = 100
  EpidExt_min= 1
  EpidExt_cutoff=50
}

#===============================================================================
#
# 1) Generate files necessary for cost calculations using processUSDOS().
# 2) All necessary files for operations below this section will be generated
# and placed in a directory labeled "Data".
# 3) processUSDOS() will also generate violin plots and maps for EDA 
# to compare different USDOS run types.
# 4) Warning: this function can take up to an hour to complete, depending on 
# whether or not your machine has 4 or more cores to parallelize some operations.
# The number of USDOS runs being processed will also affect run time.
#
#===============================================================================

setwd(path0)
gc() # clear all unused memory because this function can use quite a bit of memory
source("postProcessing_USDOSv2.1.R")

processUSDOS(export.datafiles = 3,
             duration = T,
             premInf = T,
             epidemicExtent = T,
             localSpread = F,
             controlValue = F,
             completionProportion = F,
             plots = F,
             maps = F,
             dataExist = F)

#===============================================================================
#
#Create Total.long to plot outbreak metric panel
#
#===============================================================================

setwd(path0)
Dur.long <- read.csv("Data/Duration_long.csv")
EpidExt.long <- read.csv("Data/EpidemicExtent_long.csv")
PremInf.long <- read.csv("Data/PremisesInfected_long.csv")

Dur.long$metric <- "Duration (days)"
EpidExt.long$metric <-  "Number of infected counties"
PremInf.long$metric <-  "Number of infected premises"

list <- list(Dur.long, 
             EpidExt.long, 
             PremInf.long)

total.all <- do.call("rbind", list)

# Create a new column "Delay" using case_when function from dplyr
# Remove "1Day_" or "2Days_" prefix from the "type" column using separate function from tidyr
total.all <- total.all %>%
  filter(!(str_detect(type, "cull_cull_3000_10000_earliest_earliest") | 
             str_detect(type, "cull_cull_0_10000_earliest_earliest") | 
             str_detect(type, "largest") | 
             str_detect(type, "smallest") | 
             str_detect(type, "farthest") | 
             str_detect(type, "closest") | 
             str_detect(type, "decrease") | 
             str_detect(type, "availability"))) %>%
  mutate(delay = case_when(
    str_detect(type, "earliest_1_1") ~ "1",
    str_detect(type, "earliest_2_2") ~ "2",
    str_detect(type, "earliest_3_3") ~ "3",
    str_detect(type, "noControl") ~ "No control",
    TRUE ~ "0"
  )) %>%
  mutate(cType = case_when(
    str_detect(type, "noControl_noDiagnostics") ~ "No control",
    str_detect(type, "flex") ~ "Adaptive control",
    str_detect(type, "static") ~ "Fixed control")) %>% 
  mutate(type = str_replace(type, ".*?_", "")) %>%
  mutate(type = str_replace(type, "^(1|2)(Day|Days)_", "")) %>%
  mutate(trigger = str_extract(type, "^[^_]+")) %>%
  mutate(type = str_replace(type, "^[^_]+_[^_]+_[^_]+_[^_]+_", "")) %>%
  mutate(type = case_when(str_detect(type,"cull_vax_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Vax",
                          str_detect(type,"cull_cull_0_-1_earliest_earliest") ~ "MB, IP Cull, DC Cull",
                          str_detect(type,"cull_vax_0_3000_earliest_earliest") ~ "MB, IP Cull, 3km Vax",
                          str_detect(type,"cull_vax_3000_10000_earliest_earliest") ~ "MB, IP Cull, 3km Cull, 10km Vax",
                          str_detect(cType, "No control") ~ "No control"))

setwd(data_output)
write.csv(total.all, file = "Total.long.csv")
#===============================================================================
#
# Load some data
#
#===============================================================================
setwd(data_output)
total.all <- read.csv("Total.long.csv")

#===============================================================================
#
# Create outbreak metric panels (no control runs not included)
#
#===============================================================================

plot_data <- total.all %>%
  filter(delay == 0 | delay == 3) %>%
  filter(str_detect(type, "MB, IP Cull, DC Vax") |
           str_detect(type, "MB, IP Cull, 3km Cull, 10km Vax") |
           str_detect(type, "MB, IP Cull, 3km Vax") |
           str_detect(type, "MB, IP Cull, DC Cull")) %>%
  mutate(level = case_when(metric == "Duration (days)" & Value > Dur_cutoff ~ "Top 2.5% of outbreaks",
                           metric == "Duration (days)" & Value <= Dur_cutoff ~ "97.5% of outbreaks",
                           metric == "Number of infected premises" & Value > PremInf_cutoff ~ "Top 2.5% of outbreaks",
                           metric == "Number of infected premises" & Value <= PremInf_cutoff ~ "97.5% of outbreaks",
                           metric == "Number of infected counties" & Value > EpidExt_cutoff ~ "Top 2.5% of outbreaks",
                           metric == "Number of infected counties" & Value <= EpidExt_cutoff ~ "97.5% of outbreaks")) %>%
  mutate(level = factor(level, levels = c("Top 2.5% of outbreaks","97.5% of outbreaks")))

duration <- plot_data %>%
  filter(metric == "Duration (days)") %>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  labs(x = NULL, y = "Duration (days)",fill = "Control Strategy", color = "Control Strategy")+
  facet_grid(level~type, switch = "y", scales = "free_y", 
             labeller = label_wrap_gen())+
  scale_fill_manual(values = c(state_dep_col,static_col))+
  scale_color_manual(values = rep("black",2))+
  theme_bw()+
  theme(strip.text.x = element_text(size = 12, face = "bold"), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size =12),
        axis.title.x.bottom = element_text(size = 12, face = "bold"),
        axis.title.y.left = element_text(size = 12),
        axis.text.x.bottom = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.background.y = element_blank(),
        strip.placement = "outside")

preminf <- plot_data %>%
  filter(metric == "Number of infected premises") %>%
  ggplot(aes(x = cType, y = Value, fill = cType, col = cType)) +
  geom_violin()+
  labs(x = NULL, y = "Number of infected premises",fill = "Control Strategy", color = "Control Strategy")+
  facet_grid(level~type, switch = "y", scales = "free_y", 
             labeller = label_wrap_gen())+
  scale_fill_manual(values = c(state_dep_col,static_col))+
  scale_color_manual(values = rep("black",2))+
  theme_bw()+
  theme(strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size =12),
        axis.title.x.bottom = element_text(size = 12, face = "bold"),
        axis.title.y.left = element_text(size = 12),
        axis.text.x.bottom = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.background.y = element_blank(),
        strip.placement = "outside")

epidext <- plot_data %>%
  filter(metric == "Number of infected counties") %>%
  ggplot(aes(x = cType, y = (Value), fill = cType, col = cType)) +
  geom_violin()+
  labs(x = NULL, y = "Number of infected counties",fill = "Control Strategy", color = "Control Strategy")+
  facet_grid(level~type, switch = "y", scales = "free_y", 
             labeller = label_wrap_gen())+
  scale_fill_manual(values = c(state_dep_col,static_col))+
  scale_color_manual(values = rep("black",2))+
  theme_bw()+
  theme(strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size =12),
        axis.title.x.bottom = element_text(size = 12, face = "bold"),
        axis.title.y.left = element_text(size = 12),
        axis.text.x.bottom = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.background.y = element_blank(),
        strip.placement = "outside")

Outbreak_Metrics_All <- ggarrange(duration+labs(tag = "(a)")+theme(plot.tag = element_text(size = 14, face = "bold")), 
                                  preminf+labs(tag = "(b)")+theme(plot.tag = element_text(size = 14, face = "bold")), 
                                  epidext+labs(tag = "(c)")+theme(plot.tag = element_text(size = 14, face = "bold")),
                                  ncol = 1, common.legend = TRUE, legend = "bottom", align = "v")

setwd(plot_output)
jpeg("Outbreak_Metrics_All.jpeg", width = 900, height = 800, units = 'px', res = 100)
ggarrange(duration+labs(tag = "(a)")+theme(plot.tag = element_text(size = 14, face = "bold")), 
          preminf+labs(tag = "(b)")+theme(plot.tag = element_text(size = 14, face = "bold")), 
          epidext+labs(tag = "(c)")+theme(plot.tag = element_text(size = 14, face = "bold")),
          ncol = 1, common.legend = TRUE, legend = "bottom", align = "v")
dev.off()

#===============================================================================
#
# Decision delay
#
#===============================================================================
plot_data <- total.all %>%
  tibble() %>%
  filter(((type == "MB, IP Cull, DC Vax") |
            (type == "No control")) &
           (metric == "Number of infected premises")
  ) %>%
  mutate(level = case_when(Value > PremInf_cutoff ~ "Top 2.5% of outbreaks",
                           Value <= PremInf_cutoff ~ "97.5% of outbreaks"),
         level = factor(level, levels = c("Top 2.5% of outbreaks","97.5% of outbreaks"))) %>%
  drop_na()

delay_colors <- colorRampPalette(c("darkblue",state_dep_col))(3)

setwd(plot_output)
jpeg("DecisionDelay_PremInf.jpeg", width = 900, height = 800, units = 'px', res = 100)
plot_data %>%
  ggplot(aes(x = delay, y = log10(Value), fill = delay, col = delay)) +
  geom_violin()+
  scale_fill_manual(values = c(static_col,delay_colors,no_control_col))+
  scale_color_manual(values = c("black","black","black","black","black"))+
  theme_bw() +
  labs(x = "Days required for decision-making", colour = "Control Strategy", fill = "Control Strategy") +
  ylab(expression(log[10]("Number of infected premises"))) +
  facet_grid(level~., switch = "y", scales = "free_y",
             labeller = label_wrap_gen()) +
  theme(legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32))
dev.off()

#===============================================================================
#
# Cost panels with all three scenarios
#
#===============================================================================
setwd(data_output)
Anim.long_new <- read.csv("Dur_Cull_Vax_NumInf_2024-07-26.csv") # change date to when file was generated

outbreak_cost <- Anim.long %>%
  filter(str_detect(type, 
                    "0_0_cull_vax_0_-1_earliest_earliest_0_0|0_10_cull_vax_0_-1_earliest_earliest_3_3")) %>%
  filter(Duration < 365) %>%
  mutate(cost_all = ((Duration * 12000000) + (cullEffective*255.5) + (vaxEffective*6))/1e9,
         cost_live = ((Duration * 12000000) + ((Num_Inf_No_Control+cullEffective)*255.5) + (vaxEffective*6))/1e9,
         cost_kill = ((Duration * 12000000) + ((Num_Inf_No_Control+cullEffective)*255.5) + (vaxEffective*255.5))/1e9)

cost_cutoff = round(quantile(outbreak_cost$cost_all, 0.975, na.rm = T), digits = 1)
cost_cutoff_live = round(quantile(outbreak_cost$cost_live, 0.975, na.rm = T), digits = 1)
cost_cutoff_kill = round(quantile(outbreak_cost$cost_kill, 0.975, na.rm = T), digits = 1)

cost_long <- outbreak_cost %>%
  pivot_longer(cols = starts_with("cost"), 
               names_to = "cost_type", 
               values_to = "cost_value") %>%
  mutate(cost_mag = case_when(cost_value > cost_cutoff & cost_type == "cost_all" ~ "Top 2.5% of outbreaks",
                              cost_value <= cost_cutoff & cost_type == "cost_all" ~ "97.5% of outbreaks",
                              cost_value > cost_cutoff_live & cost_type == "cost_live" ~ "Top 2.5% of outbreaks",
                              cost_value <= cost_cutoff_live & cost_type == "cost_live" ~ "97.5% of outbreaks",
                              cost_value > cost_cutoff_kill & cost_type == "cost_kill" ~ "Top 2.5% of outbreaks",
                              cost_value <= cost_cutoff_kill & cost_type == "cost_kill" ~ "97.5% of outbreaks"),
         cost_type = case_when(cost_type == "cost_all" ~ "recovered animals live",
                               cost_type == "cost_live" ~ "vaccinate-to-live",
                               cost_type == "cost_kill" ~ "vaccinate-to-kill"),
         cost_type = factor(cost_type, levels = c("recovered animals live",
                                                  "vaccinate-to-live",
                                                  "vaccinate-to-kill"))) %>%
  filter(!str_detect(control_type, "No control"))

cost_long %>%
  filter(str_detect(cost_mag,"97.5")) %>%
  ggplot(aes(x = control_type, y = cost_value, 
             fill = control_type, color = control_type)) +
  geom_violin() +
  scale_y_continuous(
    breaks = round(seq(min(cost_long$cost_value),cost_cutoff, length.out = 6),1),
    limits = c(min(cost_long$cost_value),cost_cutoff)) + 
  labs(x = "Economic scenario", y = "Cost (USD)", fill = "Control strategy")+
  scale_fill_manual(values = c(state_dep_col,static_col))+
  scale_color_manual(values = c("black","black","black"))+
  theme_bw() +
  facet_grid(cost_type~cost_mag, scales = "free_y", switch = "y") + 
  theme(strip.text.y = element_text(size = 20),
        axis.title.y = element_text(size = 26),
        strip.background.y = element_blank(),
        strip.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.x = element_blank()) +
  coord_flip() -> cost_low

cost_long %>%
  filter(str_detect(cost_mag,"2.5")) %>%
  ggplot(aes(x = control_type, y = cost_value, 
             fill = control_type, color = control_type)) +
  geom_violin() +
  scale_y_continuous(
    breaks = round(seq(cost_cutoff_kill, max(cost_long$cost_value), length.out = 6),1),
    limits = c(cost_cutoff,max(cost_long$cost_value))) +
  labs(x = "Economic scenario", y = "Cost (USD)", fill = "Control strategy", color = "Control strategy")+
  scale_fill_manual(values = c(state_dep_col,static_col))+
  scale_color_manual(values = c("black","black","black"))+
  theme_bw() +
  facet_grid(cost_type~cost_mag, scales = "free_y", switch = "y") + 
  theme(strip.text.y = element_blank(),
        strip.background.y = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = 22, face = "bold"),
        legend.position = c(0.75,0.75),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 22),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  coord_flip() -> cost_high

cost_arranged <- ggarrange(cost_low, cost_high, align = "h")

library(grid)
jpeg("Cost_Panel_AllScenarios.jpeg", width = 1000, height = 1000, units = 'px', res = 100)
annotate_figure(cost_arranged,bottom = textGrob("Cost (in billion USD)",gp = gpar(fontsize = 26)))
dev.off()

#===============================================================================
#
# P(Complete/Fade-Out) Panel
#
#===============================================================================

setwd(data_output)
Anim.long <- read.csv("Control_Completion_2023-06-07.csv") # Change date to when the file was generated

#1 assigned to take off & Fade out, respectively for analyses
Anim.long$Over5000 = Anim.long$Num_Inf > 5000
Anim.long$Over5000 = ifelse(Anim.long$Over5000, 1, 0)

Anim.long$Fade.Out = Anim.long$Num_Inf > 1 & Anim.long$Num_Inf < 5000 & Anim.long$Duration < 365
Anim.long$Fade.Out = ifelse(Anim.long$Fade.Out, 1, 0)

fadeout <- glm(Fade.Out~factor(delay)*Duration, data = subset(Anim.long, Num_Inf > 1), family = binomial(link = "logit"))
dur_values <- round(seq(14, 365, length.out = 1000),0)
newdata <- data.frame(expand_grid(dur_values,factor(c(0,1,2,3))))
colnames(newdata) <- c("Duration","delay")
Prob_FadeOut_Data <- predict(fadeout, newdata = newdata, se.fit = TRUE) %>% 
  as.data.frame() %>% 
  mutate(
    # model object mod1 has a component called linkinv that 
    # is a function that inverts the link function of the GLM:
    lower = fadeout$family$linkinv(fit - 1.96*se.fit), 
    prob = fadeout$family$linkinv(fit), 
    upper = fadeout$family$linkinv(fit + 1.96*se.fit)) %>%
  bind_cols(newdata) %>%
  mutate(delay = as.character(delay),
         delay = case_when(delay == 0 ~ "Fixed control",
                           TRUE ~ delay),
         delay = factor(delay))

tidy_model <- tidy(fadeout)

tidy_model$p.value <- format(tidy_model$p.value, scientific = TRUE, digits = 2)

kable(tidy_model, format = "latex", booktabs = TRUE, linesep = "")

completed <- glm(completed~factor(delay)*Duration, data = subset(Anim.long, Duration < 365 & Num_Inf > 1), family = binomial(link = "logit"))
dur_values <- round(seq(14, max(Anim.long$Duration), length.out = 1000),0)
newdata <- data.frame(expand_grid(dur_values,factor(c(0,1,2,3))))
colnames(newdata) <- c("Duration","delay")
tidy(completed)
Prob_Completed_Data <- predict(completed, newdata = newdata, se.fit = TRUE) %>% 
  as.data.frame() %>% 
  mutate(
    # model object mod1 has a component called linkinv that 
    # is a function that inverts the link function of the GLM:
    lower = completed$family$linkinv(fit - 1.96*se.fit), 
    prob = completed$family$linkinv(fit), 
    upper = completed$family$linkinv(fit + 1.96*se.fit)) %>%
  bind_cols(newdata) %>%
  mutate(delay = as.character(delay),
         delay = case_when(delay == 0 ~ "Fixed control",
                           TRUE ~ delay),
         delay = factor(delay))

tidy_model <- tidy(completed)

tidy_model$p.value <- format(tidy_model$p.value, scientific = TRUE, digits = 2)

kable(tidy_model, format = "latex", booktabs = TRUE, linesep = "")

delay_colors <- colorRampPalette(c("darkblue",state_dep_col))(3)

Prob_completed <- Prob_Completed_Data %>%
  ggplot(aes(x = Duration, y = prob, color = factor(delay), fill = factor(delay)))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = c(0,100,200,300,365)) +  
  scale_color_manual(values = c(delay_colors, static_col)) + 
  scale_fill_manual(values = c(delay_colors, static_col)) +
  labs(y="P(Complete Control Sequence)", 
       x="Duration (Days)", 
       fill = "Delay (Days)", color = "Delay (Days)", tag = "(a)" ## removed because no longer in a panel
  ) +
  theme(strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        # axis.text.x = element_text(size = 28),
        # axis.title.x = element_text(size = 32),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.8,0.55),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        plot.tag = element_text(size = 24, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size=1))) +
  coord_cartesian(xlim = c(0,365))


Prob_FadeOut <- Prob_FadeOut_Data %>%
  ggplot(aes(x = Duration, y = prob, color = factor(delay), fill = factor(delay)))+
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, color = NA) +
  geom_line(size = 1.5) +
  theme_bw() +
  scale_x_continuous(breaks = c(0,100,200,300,365)) +
  scale_color_manual(values = c(delay_colors, static_col)) + 
  scale_fill_manual(values = c(delay_colors, static_col)) +
  labs(y="P(Outbreak Fade-Out)", 
       x="Duration (Days)", 
       fill = "Delay", color = "Delay", tag = "(b)") +
  theme(strip.text.x = element_blank(), 
        strip.text.y = element_blank(),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        legend.position = "none",
        plot.tag = element_text(size = 24, face = "bold"))+
  guides(color = guide_legend(override.aes = list(size=1))) +
  coord_cartesian(xlim = c(0,365))

Prob_Complete_FadeOut <- ggarrange(Prob_completed, Prob_FadeOut, common.legend = FALSE, ncol = 1, align = "v")

ggsave(filename = "Prob_Complete_FadeOut.jpeg", plot = Prob_Complete_FadeOut, 
       path = plot_output, width = 8, height = 10, units = "in", dpi = 800)

setwd(plot_output)
jpeg("Prob_Complete_FadeOut.jpeg", width = 800, height = 1000, units = 'px', res = 100)
ggarrange(Prob_completed, Prob_FadeOut, common.legend = FALSE, ncol = 1, align = "v")
dev.off()

#===============================================================================
#
# Prioritization
#
#===============================================================================
setwd(data_output)
pattern <- 'smallest|largest|closest|farthest|cull_vax_0_-1_earliest_earliest_3_3|cull_vax_0_-1_earliest_earliest_0_0|cull_vax_0_3000_earliest_earliest_0_0|cull_vax_0_3000_earliest_earliest_3_3'
PremInf.long <- fread("/webblab-nas/Webblab_Storage/DHS/USAMM_USDOS/USDOS/Post_Processing_Rework/Data/PremisesInfected_long.csv")
# Filter rows based on the 'type' column
PremInf_filtered <- PremInf.long[grep(pattern, type, ignore.case = TRUE), ]

PremInf <- PremInf_filtered %>%
  mutate(metric = "Number of infected premises",
         control_type = case_when(str_detect(type, "noControl_noDiagnostics") ~ "No control",
                           str_detect(type, "flex") ~ "State-dependent control",
                           str_detect(type, "static") ~ "Static control"),
         priority = case_when(str_detect(type,"largest") ~ "Largest",
                              str_detect(type,"smallest") ~ "Smallest",
                              str_detect(type,"earliest") ~ "Earliest",
                              str_detect(type,"closest") ~ "Closest",
                              str_detect(type,"farthest") ~ "Farthest"),
         type = case_when(str_detect(type,"cull_vax_0_-1") ~ "MB, IP Cull, DC Vax",
                          str_detect(type,"cull_vax_0_3000") ~ "MB, IP Cull, 3km Vax"))

PremInf <- PremInf %>%
  filter(Value > PremInf_cutoff) %>%
  drop_na()

setwd(plot_output)
jpeg("Priortization_PremInfHigh.jpeg", width = 1500, height = 1800, units = 'px', res = 100)
PremInf %>%
  ggplot(aes(x = type, y = Value, fill = priority)) +
  geom_violin()+
  theme_bw() +
  labs(y = "Number of infected premises", x = "Control strategy", colour = "Priority", fill = "Priority") + 
  theme(axis.text.y = element_text(size = 28),
        axis.title.y = element_text(size = 32),
        axis.text.x = element_text(size = 28),
        axis.title.x = element_text(size = 32),
        strip.text.x.top = element_text(size = 28),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 24),
        legend.position = c(0.9,0.9)) +
  facet_grid(~control_type)
dev.off()
