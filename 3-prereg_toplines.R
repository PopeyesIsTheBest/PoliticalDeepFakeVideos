# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Create some figures that summarise analyses in step 3 (for
# Science submission).
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Runtime: <1min
# 
# Input:
# - code/deepfake.Rdata
# - code/03-deepfake_prereg_analysis.R
#
# Output:
# - figures/topline*.pdf
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

rm(list=ls())
setwd("~/Desktop/INF2178/Paper 3/Political-Deepfakes-Fx-main")
load("~/Desktop/INF2178/Paper 3/Political-Deepfakes-Fx-main/deepfake.RData")
# source("code/03-deepfake_prereg_analysis.R")

library(optparse)
library(cowplot)
library(tidyverse)
library(ggplot2)
library(broom)
library(stargazer)


# #####------------------------------------------------------#
# ##### Experiment 1: Stacked coefficient plots ####
# #####------------------------------------------------------#
# 
# ### diff-in-means
# dat %>% filter(treat != "ad") %>%
#     mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#     do(tidy(lm(believed_true ~ treat, data = .))) %>% 
#     filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>%
#     ggplot(aes(x=term, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
#     geom_pointrange(aes(shape=term), size=1, color="black", fill="black") +
#     geom_text(aes(label=term, y=estimate+1.96*std.error), nudge_y=0.025, size=5) +
#     scale_shape_manual(values=c(21,22,24)) +
#     ylab("") + xlab("") + coord_flip() + theme_bw() + ylim(c(0, 1.5)) +
#     theme(title = element_text(size=5),
#           legend.position = "none",
#           axis.title.y = element_text(size=14),
#           axis.ticks.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.text.x = element_text(size=12),
#           axis.title.x = element_text(size=14, hjust=0))
# 
# ### info vs. no info
# dat %>% group_by(exp_1_prompt) %>% filter(treat != "ad") %>%
#     mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#     do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
#     filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>%
#     ggplot(aes(x=exp_1_prompt, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
#     geom_pointrange(aes(shape=term), size=1, color="black", fill="black", position=position_dodge(0.5)) +
#     geom_text(aes(group=term, label=term, y=estimate+1.96*std.error+0.1), position=position_dodge(0.5), size=5) +
#     scale_shape_manual(values=c(21,22,24)) +
#     scale_x_discrete(labels=c("control\ngroup","information\nrecipients")) +
#     ylab("") + xlab("") + coord_flip() + theme_bw() + ylim(c(0, 1.5)) +
#     theme(title = element_text(size=5),
#           legend.position = "none",
#           axis.title.y = element_text(size=14),
#           # axis.ticks.y = element_blank(),
#           axis.text.y = element_text(size=14),
#           axis.text.x = element_text(size=12),
#           axis.title.x = element_text(size=14, hjust=0))
# 
# bind_rows(
#     ## by party
#     dat %>% filter(!is.na(PID)) %>% group_by(PID) %>% filter(treat != "ad", treat !=" control") %>%
#         mutate(treat = as.character(treat)) %>%
#         mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#         do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
#         filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=PID),
#     ## by age
#     dat %>% filter(!is.na(age_65)) %>% group_by(age_65) %>% filter(treat != "ad", treat !=" control") %>%
#         mutate(treat = as.character(treat)) %>%
#         mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#         do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
#         filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=age_65),
#     ## by CRT
#     dat %>% filter(!is.na(crt)) %>% 
#         mutate(crt=cut(crt, breaks=c(-1,0,.34,1.1), 
#                    labels=c("low cognitive\nreflection", "moderate cognitive\nreflection", "high cognitive\nreflection"))) %>%
#         group_by(crt) %>% filter(treat != "ad", treat !=" control") %>%
#         mutate(treat = as.character(treat)) %>%
#         mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#         do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
#         filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=crt),
#     ## by sexism
#     dat %>% mutate(ambivalent_sexism=cut(ambivalent_sexism, breaks=c(1,2.33,3.66,5), 
#                                          labels=c("low ambivalent\nsexism", "moderate ambivalent\nsexism", "high ambivalent\nsexism"))) %>%
#         filter(!is.nan(ambivalent_sexism), !is.na(ambivalent_sexism)) %>% 
#         group_by(ambivalent_sexism) %>% filter(treat != "ad", treat !=" control", !is.na(treat)) %>%
#         mutate(treat = as.character(treat)) %>%
#         mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
#         do(tidy(lm(believed_true ~ treat, data = .))) %>% ungroup() %>%
#         filter(term != "(Intercept)") %>% mutate(term=gsub("treat","",term)) %>% rename(group=ambivalent_sexism)
# ) %>% 
#     arrange(desc(row_number())) %>%
#     mutate(group=as_factor(group)) %>%
#     ggplot(aes(x=group, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
#     geom_pointrange(aes(shape=term), size=1, color="black", fill="black", position=position_dodge(0.5)) +
#     geom_text(aes(group=term, label=term, y=estimate+1.96*std.error+0.1), position=position_dodge(0.5), size=5) +
#     scale_shape_manual(values=c(21,22,24)) +
#     # scale_x_discrete(labels=c("control\ngroup","information\nrecipients")) +
#     ylab("") + xlab("") + coord_flip() + theme_bw() + ylim(c(0, 1.5)) +
#     theme(title = element_text(size=5),
#           legend.position = "none",
#           axis.title.y = element_text(size=14),
#           # axis.ticks.y = element_blank(),
#           axis.text.y = element_text(size=14),
#           axis.text.x = element_text(size=12),
#           axis.title.x = element_text(size=14, hjust=0))

#####------------------------------------------------------#
##### Experiment 1: Stacked means -- deception ####
#####------------------------------------------------------#

# convert the age group to factors

colorscheme <- scale_fill_manual(values=c("#C77CFF","#00BFC4","#7CAE00","#F8766D"))

### diff-in-means
panel1 <- dat %>% filter(treat != "ad", treat != "control") %>%
    mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
    group_by(treat) %>% summarise(estimate=mean(believed_true, na.rm=T), std.error=sd(believed_true, na.rm=T)/sqrt(n())) %>%
    mutate(panel="All\n") %>%
    ggplot(aes(x=treat, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    geom_segment(x=0, xend=4.5, 
                 y=mean(dat$believed_true[!(dat$treat %in% c("ad","control","skit"))], na.rm=T),
                 yend=mean(dat$believed_true[!(dat$treat %in% c("ad","control","skit"))], na.rm=T), 
               size=2, lty=1, color="red", inherit.aes = F) +
    geom_pointrange(aes(shape=treat, fill=treat), position=position_dodge(width=.9), color="black", size=1, stroke=.5) +
    geom_text(aes(group=treat, label=treat, y=estimate+1.96*std.error+0.2), position=position_dodge(width=.9), size=4) +
    scale_shape_manual(values=c(21,22,23,24)) +
    facet_grid(panel ~ .) + 
    scale_x_discrete(expand=c(0.2,0.2)) + colorscheme +
    ylab("") + xlab("") + theme_linedraw() + coord_flip(ylim=c(1,5)) +
    theme(title = element_text(size=5),
          legend.position = "none",
          axis.title.y = element_text(size=16),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=14, hjust=0))

dd <- bind_rows(
    dat %>% filter(!is.na(agegroup)) %>% filter(treat != "ad", treat !="control") %>%
        mutate(agegroup = as.factor(agegroup)) %>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "skit","audio","text","video")) %>%
        group_by(agegroup,treat) %>%
        summarise(estimate=mean(believed_true, na.rm=T), std.error=sd(believed_true, na.rm=T)/sqrt(n())) %>%
        rename(group=agegroup) %>% mutate(type="By Age\nGroup"))

dd$group <- as_factor(dd$group)
    
panel3 <- dd %>%
    ggplot(aes(x=group, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    # geom_bar(aes(fill=treat), position=position_dodge(width=.9), color="black", stat="identity") +
    # geom_errorbar(aes(group=treat), position=position_dodge(width=.9), width=.2, size=1) +
    scale_shape_manual(values=c(21,22,23,24)) +
    geom_segment(data = dd %>% 
                     filter(treat != "skit") %>% 
                     group_by(type, treat) %>% mutate(group_x=1:n()) %>% ungroup() %>%
                     group_by(type,group,group_x) %>% 
                     summarise(yint=mean(estimate)),
               aes(x=group_x-.5, xend=group_x+.5, y=yint, yend=yint), 
               size=2, lty=1, color="red", inherit.aes = F) +
    geom_vline(aes(xintercept=1.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=2.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=3.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=4.5), lty=2, color="grey", alpha=0.8) +
    geom_pointrange(aes(shape=treat, fill=treat), position=position_dodge(width=.9), color="black", size=1, stroke=.5) +
    geom_text(aes(group=treat, label=treat, y=estimate+1.96*std.error+0.2), position=position_dodge(width=.9), size=4) +
    facet_grid(type ~ ., space="free", scales="free") +
    scale_x_discrete(expand=c(0.2,0.2)) + colorscheme +
    ylab("mean level of deception in group (95% CI)") + xlab("") + coord_flip(ylim=c(1,5)) + theme_linedraw() +
    theme(title = element_text(size=5),
          legend.position = "none",
          axis.title.y = element_text(size=14),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=15),
          axis.text.y = element_text(size=14),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=18))

pp <- cowplot::plot_grid(panel1, panel3, nrow = 8,
                         align="v",rel_heights = c(0.3, 1))
cowplot::save_plot("figures/topline_exp1_a.pdf", plot = pp, base_height=25.5, base_width=8)
system("open figures/topline_exp1_a.pdf")

