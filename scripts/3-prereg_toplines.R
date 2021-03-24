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


##### Experiment 1: Stacked means -- affect #### Figure 2
#####------------------------------------------------------#
colorscheme <- scale_fill_manual(values=c("black","white","#C77CFF","#00BFC4","#7CAE00","#F8766D"))

### diff-in-means
panel1.b <- dat %>% filter(!is.na(treat)) %>%
    mutate(treat = fct_relevel(treat, "ad","control","skit","audio","text","video")) %>%
    group_by(treat) %>% summarise(estimate=mean(post_favor_Warren, na.rm=T), std.error=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>%
    mutate(panel="All\n") %>%
    ggplot(aes(x=treat, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    geom_segment(x=0, xend=6.5, 
                 y=mean(dat$post_favor_Warren[!(dat$treat %in% c("ad","control","skit"))], na.rm=T),
                 yend=mean(dat$post_favor_Warren[!(dat$treat %in% c("ad","control","skit"))], na.rm=T), 
                 size=2, lty=1, color="red", inherit.aes = F) +
    geom_pointrange(aes(shape=treat, fill=treat), position=position_dodge(width=.9), color="black", size=.5, stroke=.5) +
    geom_text(aes(group=treat, label=treat, y=estimate+1.96*std.error+2.7), position=position_dodge(width=.9), size=3) +
    scale_shape_manual(values=c(8,25,21,22,23,24)) +
    facet_grid(panel ~ .) + 
    scale_x_discrete(expand=c(0.2,0.2)) + colorscheme +
    ylab("") + xlab("") + theme_linedraw() + coord_flip(ylim=c(15,75)) +
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
    dat %>% filter(!is.na(agegroup)) %>% filter(!is.na(treat)) %>%
        mutate(agegroup= as.factor(agegroup))%>%
        mutate(treat = as.character(treat)) %>%
        mutate(treat = fct_relevel(treat, "ad","control","skit","audio","text","video")) %>%
        group_by(agegroup,treat) %>%
        summarise(estimate=mean(post_favor_Warren, na.rm=T), std.error=sd(post_favor_Warren, na.rm=T)/sqrt(n())) %>%
        rename(group=agegroup) %>% mutate(type="By Age\nGroup")
)


dd= na.omit(dd)
panel3.b <- dd %>%
    filter(!is.na(group))%>%
    ggplot(aes(x=group, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    scale_shape_manual(values=c(8,25,21,22,23,24)) +
    geom_segment(data = dd %>% 
                     filter(treat != "skit") %>% 
                     group_by(type, treat) %>% mutate(group_x=1:n()) %>% ungroup() %>%
                     group_by(type,group,group_x) %>% 
                     summarise(yint=mean(estimate)),
                 aes(x=group_x-.5, xend=group_x+.5, y=yint, yend=yint), 
                 size=2, lty=1, color="red", inherit.aes = F) +
    geom_vline(aes(xintercept=0.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=2), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=3.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=4.75), lty=2, color="grey", alpha=0.8) +
    geom_pointrange(aes(shape=treat, fill=treat), position=position_dodge(width=.9), color="black", size=1, stroke=.5) +
    geom_text(aes(group=treat, label=treat, y=estimate+1.96*std.error+0.2), position=position_dodge(width=.9), size=4) +
    facet_grid(type ~ ., space="free", scales="free") +
    scale_x_discrete(expand=c(0.2,0.2)) + colorscheme +
    ylab("mean feeling thermometer\ntowards Elizabeth Warren (95% CI)") + xlab("") + coord_flip(ylim=c(15,75)) + theme_linedraw() +
    theme(title = element_text(size=5),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.text = element_text(size=15),
          axis.text.y = element_text(size=14),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size=12),
          axis.title.x = element_text(size=18))

panel3.b

pp.b <- cowplot::plot_grid(panel1.b, panel3.b,
                           nrow=8, align="v", rel_heights = c(0.3,1))
cowplot::save_plot("figures/topline_exp1_b_2.pdf", plot = pp.b, base_height=25.5, base_width=8)
system("open figures/topline_exp1_b_2.pdf")


#####------------------------------------------------------#
##### Experiment 2: Stacked means ####
#####------------------------------------------------------#
d <- dat %>%
    gather(key="metric", value="val", exp_2_pct_correct, exp_2_pct_false_fake, exp_2_pct_false_real) %>%
    mutate(metric = as.character(metric)) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_correct", "Accuracy")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_fake", "False Positive Rate")) %>%
    mutate(metric = replace(metric, metric == "exp_2_pct_false_real", "False Negative Rate")) %>%
    mutate(exp_2 = as.character(exp_2)) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "lofake", "low-fake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "hifake", "high-fake")) %>%
    mutate(exp_2 = replace(exp_2, exp_2 == "nofake", "no-fake"))

polknow_quantile <- quantile(d$polknow, probs=c(0,0.5,1), include.lowest=T)
polknow_quantile[1] <- -Inf
polknow_quantile[3] <- Inf

diglit_quantile <- quantile(d$post_dig_lit, probs=c(0,0.33,0.66,1),na.rm=T,include.lowest=T)
diglit_quantile[1] <- -Inf
diglit_quantile[4] <- Inf

dd <- bind_rows(
    ## diff-in-means
    d %>% filter(!is.na(exp_2)) %>%
        group_by(exp_2, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        filter(!is.na(std.error)) %>% mutate(type="All", group="")
    ,
    ## age
    d %>% filter(!is.na(agegroup)) %>%
        mutate(agegroup = as.factor(agegroup)) %>%
        group_by(exp_2, agegroup, metric) %>% summarise(estimate=mean(val, na.rm=T), std.error=sd(val, na.rm=T)/sqrt(n())) %>%
        filter(!is.na(std.error), !is.na(agegroup)) %>% rename(group=agegroup) %>% mutate(type="By Age\nGroup") 
)

dd$group <- as_factor(dd$group)
dd$type <- as_factor(dd$type)

pp <- dd %>%
    mutate(nn=ifelse(grepl("Acc",metric), 0.04, ifelse(grepl("Neg",metric), 0.035, 0.1))) %>%
    ggplot(aes(x=group, y=estimate, ymin=estimate-1.96*std.error, ymax=estimate+1.96*std.error)) +
    scale_shape_manual(values=c(21,22,24)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
    geom_vline(aes(xintercept=1.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=2.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=3.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=4.5), lty=2, color="grey", alpha=0.8) +
    geom_vline(aes(xintercept=5.5), lty=2, color="grey", alpha=0.8) +
    # geom_vline(data = dd %>% filter(type=="By Knowledge\nSubgroups"), aes(xintercept=2.5), lty=1, color="grey", alpha=1) +
    geom_segment(data = dd %>%
                     group_by(metric,type,exp_2) %>% mutate(group_x=1:n()) %>% ungroup() %>%
                     group_by(metric,type,group,group_x) %>%
                     summarise(yint=mean(estimate)),
                 aes(x=group_x-.5, xend=group_x+.5, y=yint, yend=yint),
                 size=2, lty=1, color="red", inherit.aes = F) +
    geom_pointrange(aes(shape=exp_2, fill=exp_2), position=position_dodge(width=.7), color="black", size=.5) +
    geom_text(aes(group=exp_2, label=exp_2, y=estimate+1.96*std.error+nn), position=position_dodge(width=.7), size=3) +
    facet_grid(type ~ metric, space="free_y", scales="free") + 
    scale_x_discrete(expand=c(0,0)) +
    ylab("mean score in group (95% CI)") + xlab("") + theme_linedraw() + coord_flip() +
    theme(title = element_text(size=5),
          legend.position = "none",
          axis.title.y = element_text(size=16),
          axis.ticks.y = element_blank(),
          panel.spacing.x = unit(1.25, "lines"),
          # axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background.x = element_blank(),
          strip.text.x = element_text(size=13, colour="black"),
          strip.text.y = element_text(size=10),
          # axis.text.x = element_blank(),
          # axis.ticks.x = element_blank(),
          axis.title.x = element_text(size=14))
ggsave("figures/Topline_exp2.pdf", plot = pp, height=10.5, width=8)
system("open figures/Topline_exp2.pdf")
