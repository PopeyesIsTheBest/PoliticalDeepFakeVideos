
library(optparse)
library(tidyverse)
library(ggplot2)
library(broom)
library(stargazer)
library(AMR)


load("deepfake.RData")

dfsurvdat$age <- as.numeric(as.character(dfsurvdat$age))
dfsurvdat$agegroup <- age_groups(dfsurvdat$age, split_at = c(25, 35, 45, 65), na.rm = FALSE)
levels(dfsurvdat$agegroup) <- c("18-24","25-34","35-44","45-64","65+")
dfsurvdat <- na.omit(dfsurvdat)



scores_byAge <- dfsurvdat[,c("agegroup", nofake_vids, lowfake_vids, hifake_vids)] %>%
  gather(key="video", value="response", -agegroup) %>%
  filter(!is.na(as.character(response))) %>%
  mutate(is_real=grepl("real", video)) %>%
  mutate(video=gsub("_\\d$", "", video)) %>%
  mutate(video=ifelse(video=="real_bidenfight", "real_biden_fight", video)) %>%
  mutate(correct=ifelse(is_real==TRUE & response=="This video is not fake or doctored", 1, 
                        ifelse(is_real==FALSE & response=="This video is fake or doctored", 1, 0))) %>%
  mutate(correct=replace_na(correct, 0)) %>%
  group_by(agegroup, video) %>%
  summarise(pct_correct=mean(correct, na.rm=T), .keep = "all") %>% 
  mutate(video_lbl=case_when(video == "fake_hilary2" ~ "Hillary Clinton\n(fake debate)",
                             video == "fake_obama_buzzfeed" ~ "Barack Obama\n(fake news announcement)",
                             video == "real_obama_missile" ~ "Barack Obama\n(Russian president hot mic)",
                             video == "fake_bernie1" ~ "Bernie Sanders\n(fake debate)",
                             video == "real_obama_smoking" ~ "Barack Obama\n(smoking hot mic)",
                             video == "real_warrenbeer" ~ "Elizabeth Warren\n(Instagram beer gaffe)",
                             video == "real_trump_soup" ~ 'Donald Trump\n("soup" press conference gaffe)',
                             video == "real_trump_apple" ~ "Donald Trump\n(Apple press conference gaffe)",
                             video == "real_biden_fight" ~ "Joe Biden\n(town hall 'push-up contest' gaffe)",
                             video == "fake_boris" ~ "Boris Johnson\n(fake Brexit announcement)",
                             video == "real_warrenliar" ~ "Elizabeth Warren\n(post-debate hot mic)",
                             video == "real_biden_stumble" ~ "Joe Biden\n(stutter gaffe)",
                             video == "real_trump_covid" ~ "Donald Trump\n(COVID-19 precautions announcement)",
                             video == "fake_trump_aids" ~ "Donald Trump\n(fake AIDS cure announcment)",
                             video == "fake_trump_resign" ~ "Donald Trump\n(fake resignation announcement)")) %>%
  as.data.frame()

age_scores_fake2 <- scores_byAge %>% mutate(is_fake=grepl("fake_", video)) %>% filter(is_fake) %>% 
  mutate(is_fake="fake clips") %>%
  arrange(desc(agegroup), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
  mutate(agegroup=case_when(agegroup == "18-24" ~ "18-24",
                            agegroup == "25-34" ~ "25-34",
                            agegroup == "35-44" ~ "35-44",
                            agegroup == "45-64" ~ "45-64",
                            agegroup == "65+" ~ "65+",)) %>%
  ggplot(aes(x=video_lbl, y=pct_correct, fill=agegroup, label=agegroup)) + 
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
  geom_text(aes(y=pct_correct+0.02), position = position_dodge(width = 0.8), size=2.5) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_discrete(expand=c(-0.1, 0)) +
  scale_fill_manual(values=c("blue","grey","red","yellow","green")) +
  coord_flip() +
  facet_grid(is_fake ~ .) +
  xlab("") + ylab("") +
  theme_bw() + 
  theme(title = element_text(size=5),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        strip.text = element_text(size=16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

age_scores_real2 <- scores_byAge %>% mutate(is_real=grepl("real_", video)) %>% filter(is_real) %>% 
  mutate(is_real="real clips") %>%
  arrange(desc(agegroup), -pct_correct) %>% mutate(video_lbl=as_factor(video_lbl)) %>%
  mutate(agegroup=case_when(agegroup == "18-24" ~ "18-24",
                            agegroup == "25-34" ~ "25-34",
                            agegroup == "35-44" ~ "35-44",
                            agegroup == "45-64" ~ "45-64",
                            agegroup == "65+" ~ "65+",)) %>%
  ggplot(aes(x=video_lbl, y=pct_correct, fill=agegroup, label=agegroup)) + 
  geom_bar(stat="identity", position=position_dodge(width=0.8), width=0.8, color="black") +
  geom_text(aes(y=pct_correct+0.02), position = position_dodge(width = 0.8), size=2.5) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_x_discrete(expand=c(-0.1, 0)) +
  scale_fill_manual(values=c("blue","grey","red","yellow","green")) +
  coord_flip() +
  facet_grid(is_real ~ .) +
  xlab("") + ylab("% of correct detections") +
  theme_bw() + 
  theme(title = element_text(size=5),
        legend.position = "none",
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        strip.text = element_text(size=16),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14))

pp2 <- cowplot::plot_grid(age_scores_fake2, age_scores_real2, nrow=2, rel_heights = c(1,1.4), align="v")


cowplot::save_plot("figures/secondstage_byclip_byAge.pdf", plot=pp2, base_width=6.5, base_height=7)
system("open figures/secondstage_byclip_byAge.pdf")
