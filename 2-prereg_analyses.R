# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Run through pre-registered analyses on deepfake studies.
# 
# Author: Soubhik Barari
# 
# Environment:
# - must use R 3.6
# 
# Runtime: <1 min
# 
# Input:
# - code/deepfake.Rdata:
#       contains `dat` object with weights appended to a 
#       column from step 1.
#
# Output:
# - figures/*
# - tables/*
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

library(optparse)
library(tidyverse)
library(ggplot2)
library(broom)
library(stargazer)

rm(list=ls())
setwd("~/Desktop/INF2178/Paper 3/Political-Deepfakes-Fx-main")
load("~/Desktop/INF2178/Paper 3/Political-Deepfakes-Fx-main/deepfake.Rdata")

if (!file.exists("tables")) {
    system("mkdir tables")
}
if (!file.exists("figures")) {
    system("mkdir figures")
}

COVARS <- c("educ", "meta_OS", "age_65", "PID", "crt", "gender", "polknow", 
            "internet_usage", "ambivalent_sexism")

#####------------------------------------------------------#
##### Settings ####
#####------------------------------------------------------#

arg_list <- list(     
    # make_option(c("--response_quality"), type="character", default="all", 
    #     help="Which quality of responses to condition on.",
    #     metavar="response_quality"),
    # make_option(c("--weight"), type="numeric", default=0,
    #             help="Use weights?",
    #             metavar="weight"),
    make_option(c("--show_pdfs"), type="numeric", default=0,
                help="Show PDFs in real time?",
                metavar="show_pdfs")
)
ARGS <- parse_args(OptionParser(option_list=arg_list))

SHOW_PDFS <- ARGS$show_pdfs

dat$lowq <- FALSE
dat$lowq[dat$quality_pretreat_duration_tooquick | dat$quality_pretreat_duration_tooslow | dat$quality_demographic_mismatch] <- TRUE

# if (ARGS$response_quality == "low") {
#     dat <- dat[dat$quality_pretreat_duration_tooquick | dat$quality_pretreat_duration_tooslow | dat$quality_demographic_mismatch,] ## cond on low quality
# }
#
# if (ARGS$response_quality == "high") {
#     dat <- dat[!dat$quality_pretreat_duration_tooquick & !dat$quality_pretreat_duration_tooslow & !dat$quality_demographic_mismatch,] ## cond on high quality
# }
# if (ARGS$weight == 0 | !("weight" %in% colnames(dat))) {
#     dat$weight <- 1
# }

#####------------------------------------------------------#
##### Helpers ####
#####------------------------------------------------------#

coefviz <- function(df, ylab_="y", title_="") {
    df %>% 
        mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, 
                                      "black", "gray"))) %>%
        ggplot(aes(x=term, 
                   y=estimate, 
                   ymin=estimate-1.96*std.error, 
                   ymax=estimate+1.96*std.error,
                   color=sig)) + 
        geom_pointrange(lwd=1) + 
        coord_flip() + 
        scale_color_identity() + 
        xlab("") + ylab(ylab_) +
        theme_bw() + geom_hline(yintercept=0, lty=2, alpha=0.5) + 
        theme(
            legend.position = "none",
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
        ) + ggtitle(title_)
}

groupcoefviz <- function(df, ylab_="y", title_="", nudge=0.05) {
    df %>% 
        mutate(sig = as.factor(ifelse(estimate-1.96*std.error > 0 | estimate+1.96*std.error < 0, 
                                      "black", "gray"))) %>%
        ggplot(aes(x=term, 
                   y=estimate, 
                   ymin=estimate-1.96*std.error, 
                   ymax=estimate+1.96*std.error,
                   group=model,
                   shape=model,
                   label=model,
                   colour=sig)) + 
        geom_pointrange(position=position_dodge(width=0.5), lwd=1) + 
        geom_text(aes(y=ifelse(estimate+1.96*std.error > 0, 
                               estimate-1.96*std.error-nudge,
                               estimate+1.96*std.error+nudge),
                  hjust=ifelse(estimate+1.96*std.error > 0, 
                               "right",
                               "left")),
                  position=position_dodge(width=0.5), 
                  vjust=0.5, colour="black") + 
        coord_flip() + 
        scale_shape_manual(values=c(15, 17)) +
        scale_color_identity() +
        xlab("") + ylab(ylab_) +
        theme_bw() + geom_hline(yintercept=0, lty=2, alpha=0.5) + 
        theme(
            legend.position = "none",
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            axis.title.x = element_text(size=14),
            axis.title.y = element_text(size=14)
        ) + ggtitle(title_)
}

weighted.sd <- function(x, w, na.rm = FALSE) {
    if (na.rm) {
        w <- w[i <- !is.na(x)]
        x <- x[i]
    }
    sum.w <- sum(w)
    sqrt((sum(w*x^2) * sum.w - sum(w*x)^2) / (sum.w^2 - sum(w^2)))
}

#####------------------------------------------------------#
##### H1: Deepfakes more deceptive than text/audio/skit (no-warn cohort) ####
#####------------------------------------------------------#

n <- dat %>% 
    filter(!(treat%in%c("ad","control")), !is.na(treat), exp_1_prompt_control==T, !is.na(believed_true)) %>% nrow()

## difference-in-means
(h1.m <- lm(believed_true ~ treat, dat %>% filter(treat != "ad",exp_1_prompt_control==T))); summary(h1.m);
(h1.m.wt <- lm(believed_true ~ treat, dat %>% filter(treat != "ad",exp_1_prompt_control==T), weights=weight)); summary(h1.m.wt); 
(h1.m.hq <- lm(believed_true ~ treat, dat %>% filter(treat != "ad",exp_1_prompt_control==T, !lowq))); summary(h1.m.hq); 

# convert the age group to factors
dat<- dat %>% 
    mutate(agegroup = as.factor(agegroup))

## adjustments
(h1.m.adj <- lm(believed_true ~ treat + meta_OS + agegroup + educ + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",exp_1_prompt_control==T))); summary(h1.m.adj);
(h1.m.adj.wt <- lm(believed_true ~ treat + meta_OS + agegroup + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",exp_1_prompt_control==T), weights=weight)); summary(h1.m.adj.wt);
(h1.m.adj.hq <- lm(believed_true ~ treat + meta_OS + agegroup + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",exp_1_prompt_control==T,!lowq))); summary(h1.m.adj.hq);
(h1.m.adj.wt.hq <- lm(believed_true ~ treat + meta_OS + agegroup + educ + PID + crt + I(gender=="Male") + polknow + internet_usage + ambivalent_sexism, dat %>% filter(!is.na(treat),treat != "ad",exp_1_prompt_control==T,!lowq), weights=weight)); summary(h1.m.adj.wt.hq);

### regression tables
stargazer(h1.m,
          h1.m.wt,
          h1.m.hq,
          h1.m.adj,
          h1.m.adj.wt,
          h1.m.adj.hq,
          h1.m.adj.wt.hq,
          header = FALSE,
          no.space=TRUE,
          digits=2,
          table.layout ="=d#-t-a-s=n",
          notes.align = "l",
          title="\\textbf{Models of Belief in Exposure-Stage (News Feed) Scandal Clipping}",
          notes = c("\\textit{Notes}: Reference category for medium is Video. CRT is scaled 0-1, political knowledge and ambivalent",
                    "sexism are 0-1, internet usage is 1-7. Sample did not receive information in the first stage."),
          add.lines = list(c("Weighted?", "", "\\checkmark", "", "", "\\checkmark","","\\checkmark"),
                           c("Low-Quality Dropped?","","","\\checkmark","","","\\checkmark","\\checkmark")),
          covariate.labels = c("Audio",
                               "Text",
                               "Skit",
                               # "Attack Ad",
                               "On Mobile",
                               "Age 25-34",
                               "Age 35-44",
                               "Age 45-64",
                               "Age 65+ ",
                               "High School", "College", "Postgrad",
                               "Independent PID", "Republican PID",
                               "CRT",
                               "Male",
                               "Political Knowledge",
                               "Internet Usage",
                               "Ambivalent Sexism"),
          dep.var.labels = c("\\normalsize Extent of belief that clipping was not fake or doctored [1-5]"),
          omit.stat=c("f", "ser"),
          column.sep.width = "1pt",
          font.size = "footnotesize",
          style = "apsr",
          label = "firststage_deception",
          out="tables/firststage_deception.tex")

