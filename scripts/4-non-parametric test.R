#####------------------------------------------------------#
##### Experiment 1: Non-parametric tests ####
#####------------------------------------------------------#

## deception
t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_fake_audio == 1])) ##t = -1.9296, df = 1767.1, p-value = 0.05382, \delta = -0.119805

t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_fake_text == 1])) ##t = -1.2151, df = 1761.7, p-value = 0.2245, \delta = -0.075769

t.test(na.omit(dat$believed_true[dat$treat_fake_video == 1]), 
       na.omit(dat$believed_true[dat$treat_skit == 1])) ##t = 8.4754, df = 1083.9, p-value < 2.2e-16, \delta = 0.653852

## affect
t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_fake_audio == 1])) ##t = -1.647, df = 1802.3, p-value = 0.09974, \delta = -2.64796

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_fake_text == 1])) ##t = -1.8447, df = 1794.4, p-value = 0.06525, \delta = -2.94543

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_skit == 1])) ##t = -1.0868, df = 1793.6, p-value = 0.2773, \delta = -1.72203

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_attackad == 1])) ##t = -0.13861, df = 1787.2, p-value = 0.8898, \delta = -0.22528

t.test(na.omit(dat$post_favor_Warren[dat$treat_fake_video == 1]), 
       na.omit(dat$post_favor_Warren[dat$treat_control == 1])) ##t = -2.793, df = 1767.1, p-value = 0.005278, \delta = -4.53598

## info
t.test(dat$believed_true[dat$exp_1_prompt_info],
       dat$believed_true[dat$exp_1_prompt_control]) ##t = -6.8445, df = 4177.9, p-value = 8.79e-12, \delta = -0.284111

t.test(dat$believed_true[dat$exp_1_prompt_info & dat$treat_fake_video],
       dat$believed_true[dat$exp_1_prompt_control & dat$treat_fake_video]) ##t = -3.918, df = 853.94, p-value = 9.641e-05, \delta = -0.354745

## heterogeneity by age

t.test(dat$post_favor_Warren[dat$age_65 == ">65"],
       dat$post_favor_Warren[dat$age_65 == "<=65"]) ##t = -5.7333, df = 4698, p-value = 1.047e-08, \delta = -5.42

## heterogeneity by medium within age
t.test(dat$post_favor_Warren[dat$age_65 == ">65" & dat$treat_fake_video==1],
       dat$post_favor_Warren[dat$age_65 == ">65" & dat$treat_fake_text==1]) ##t = -1.4803, df = 771.99, p-value = 0.1392, \delta = -3.81

## heterogeneity by sexism
t.test(dat$believed_true[dat$ambivalent_sexism <= 2.33],
       dat$believed_true[dat$ambivalent_sexism > 3.66]) ##t = -7.9801, df = 1250.3, p-value = 3.284e-15, \delta = -0.558898

t.test(dat$post_favor_Warren[dat$ambivalent_sexism <= 2.33],
       dat$post_favor_Warren[dat$ambivalent_sexism > 3.66]) ##t = 17.772, df = 1525.8, p-value < 2.2e-16, \delta = -25.9583

## heterogeneity by medium within sexism
t.test(dat$believed_true[dat$ambivalent_sexism > 3.66 & dat$treat_fake_video==1],
       dat$believed_true[dat$ambivalent_sexism > 3.66 & dat$treat_fake_audio==1]) ##t = 1.122, df = 276.71, p-value = 0.2628, \delta = 0.180147

t.test(dat$post_favor_Warren[dat$ambivalent_sexism > 3.66 & dat$treat_fake_video==1],
       dat$post_favor_Warren[dat$ambivalent_sexism > 3.66 & dat$treat_fake_audio==1]) ##t = -2.0674, df = 277.91, p-value = 0.03962, \delta = -8.41421

## heterogeneity by PID
t.test(dat$believed_true[dat$PID=="Democrat"],
       dat$believed_true[dat$PID=="Republican"]) ##t = -13.771, df = 3621.7, p-value < 2.2e-16, \delta = -0.60564

t.test(dat$post_favor_Warren[dat$PID=="Democrat"],
       dat$post_favor_Warren[dat$PID=="Republican"]) ##t = 53.531, df = 4753, p-value < 2.2e-16, \delta = -42.58104

## heterogeneity by polknow
t.test(dat$believed_true[dat$polknow <= 0.5],
       dat$believed_true[dat$polknow > 0.5]) ##t = 0.10765, df = 1115.6, p-value = 0.9143, \delta = 0.005508

t.test(dat$post_favor_Warren[dat$polknow <= 0.5],
       dat$post_favor_Warren[dat$polknow > 0.5]) ##t = -3.6516, df = 1600.4, p-value = 0.000269, \delta = 3.85

#####------------------------------------------------------#
##### Experiment 2: Non-parametric tests ####
#####------------------------------------------------------#

## real vs. fake clips
t.test(scores$pct_correct[grepl("real_",scores$video)]*100, 
       scores$pct_correct[grepl("fake_",scores$video)]*100) ##t = 0.5702, df = 5.8213, p-value = 0.5899, \delta = 0.0720895

## logo vs. no logo clips
t.test(scores$pct_correct[scores$is_logo], 
       scores$pct_correct[!scores$is_logo]) ##t = 0.53689, df = 5.9601, p-value = 0.6108, \delta = 0.0603977

## obama missile
chisq.test(
  table(as.character(dfsurvdat$real_obama_missile[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_obama_missile) != "I don't know"]),
        as.character(dfsurvdat$PID[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_obama_missile) != "I don't know"]))
) ##X-squared = 333.34, df = 1, p-value < 2.2e-16

## trump apple
chisq.test(
  table(as.character(dfsurvdat$real_trump_apple[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_apple) != "I don't know"]),
        as.character(dfsurvdat$PID[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_apple) != "I don't know"]))
) ##X-squared = 75.155, df = 1, p-value < 2.2e-16

## trump covid
chisq.test(
  table(as.character(dfsurvdat$real_trump_covid[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_covid) != "I don't know"]),
        as.character(dfsurvdat$PID[as.character(dfsurvdat$PID) %in% c("Democrat", "Republican") & as.character(dfsurvdat$real_trump_covid) != "I don't know"]))
) ##X-squared = 169.96, df = 1, p-value < 2.2e-16

