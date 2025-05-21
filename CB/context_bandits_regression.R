library("ggplot2")
#library("sjPlot")
library(tidyverse) 
library(dplyr)
library(brms)
library(rstan)
library(loo)
library(bayestestR)
library(tidybayes)
library(lme4)

#theme_set(theme_sjplot())

set.seed(10)

setwd("/Users/nharhen/Desktop/Projects/context_bandits/context_bandits_2day_6contexts_day2/data_analysis")
df <- read.csv("glm_design_matrix_drop_final.csv")
#df <- read.csv("glm_design_matrix_drop_w_total_rwd.csv")

#scale_this <- function(x) as.vector(scale(x))

#df$context_rwd_z <- scale_this(df$context_rwd)
#df$context_rwd_z <- as.numeric(df$context_rwd_z)

prior <- c(set_prior("normal(0,2)", class = "b"), # weakly informative priors 
           set_prior("student_t(10,0,1)", class = "sd"))

m.full.model <- brms::brm(choice ~  identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_reward + context_rwd + age_z + age_z*identity + age_z*reward_1
           + age_z*reward_2 + age_z*reward_3 + age_z*probed_identity  + age_z*probed_reward + age_z*context_rwd + (1 | sub_id), data = df, family = "bernoulli",prior = prior, iter = 12000, warmup=2000,cores=4)

rand.slope.model <- brms::brm(choice ~  identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_reward + context_rwd + age_z + age_z*identity + age_z*reward_1
                              + age_z*reward_2 + age_z*reward_3 + age_z*probed_identity  + age_z*probed_reward + age_z*context_rwd + (1 +identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_reward + context_rwd | sub_id), data = df, family = "bernoulli",prior = prior, iter = 2000, warmup=500,cores=4)

rand.slope.model.mle <- glmer(choice ~  identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_reward + context_rwd + age_z + age_z*identity + age_z*reward_1
                              + age_z*reward_2 + age_z*reward_3 + age_z*probed_identity  + age_z*probed_reward + age_z*context_rwd + (1 +identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_reward + context_rwd | sub_id), data = df, family = binomial,optimizer="bobyqa")


summary(m.full.model)

summary(rand.slope.model)
summary(rand.slope.model.mle)

loo(ctx.rwd.model,total.rwd.model,compare=TRUE) # best fitting model is on top, but |elpd_diff/se_diff| < 2 so not noteworthy difference

# posterior predictive check 
pp_check(m.full.model) 

m.full.model %>%
  spread_draws(`b_context_rwd:age_z`) %>%
  median_qi(`b_context_rwd:age_z`)

# calculate pd 
pd<-p_direction(m.full.model)
plot(pd)

# calculate bayes factor
bayesfactor_parameters(m.full.model,direction="two-sided")

# plot the effect of recent experience 
bayesplot::color_scheme_set("lightblue")
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 20, family = "Arial",color="black"))
mcmc_plot( m.full.model,type = "areas", prob=0.95, variable=c("b_identity","b_reward_1","b_reward_2","b_reward_3"),transformations = "exp") + ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("reward-3","reward-2","reward-1","last choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25) +
  xlim(0.8, 2.8) + theme(text = element_text(size = 20),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black",family="Arial",face = "plain"))
ggplot2::ggsave("cb_exp1_effect_recent_experience_final.png",bg = "white",width = 12, height = 10, units = "cm")

# interaction of age and recent experience
mcmc_plot( m.full.model,type = "intervals",variable=c("b_identity:age_z","b_reward_1:age_z","b_reward_2:age_z","b_reward_3:age_z"),transformations = "exp",outer_size = 2,inner_size = 3,point_size=6) + ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("reward-3*age","reward-2*age","reward-1*age","last choice*age"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25)
ggplot2::ggsave("cb_exp1_effect_recent_experience_inter_age_no_Pe_include_context_choice.png",bg = "white")

# plot the effect of distant experience
darkpurple_scheme <- c("#db9deb", "#c562de","#ad2acf","#7c1e94","#631876","#4a1258")
bayesplot::color_scheme_set(darkpurple_scheme)
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 20, family = "Arial",color="black"))
mcmc_plot( m.full.model,type = "areas",prob=0.95,variable=c("b_probed_identity","b_probed_reward","b_context_rwd"),transformations = "exp")+ ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("probed context rwd","probed trial reward","probed trial choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25) +
  theme(text = element_text(size = 20),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black",family="Arial",face = "plain"))
ggplot2::ggsave("cb_exp1_effect_distant_experience_final.png",bg = "white",width = 15, height = 10, units = "cm")

# interaction of age and distant experience

mcmc_plot( m.full.model,type = "intervals",prob=0.95,variable=c("b_probed_identity:age_z","b_probed_reward:age_z","b_context_rwd:age_z"),transformations = "exp",outer_size = 2,inner_size = 3,point_size=6)+ ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("probed context rwd*age","probed trial reward*age","probed trial identity*age"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25)
ggplot2::ggsave("cb_exp1_effect_distant_experience_inter_age_no_pe_include_context_choice.png",bg = "white")

predict(m.full.model)

# marginal effects plots 
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 100, family = "Arial",color="black"))
plot(conditional_effects(m.full.model, effects="identity:age_z"))[[1]] + 
  scale_color_manual(breaks=c('-1', '0', '1'),values=c("-1"= "#db9deb", 
                                                       "0"= "#ad2acf",
                                                       "1" ="#631876"),name="age group",labels=c("-1"= "child", 
                                                                                                 "0"= "adolescent",
                                                                                                 "1" ="adult")) +
  scale_fill_manual(breaks=c('-1', '0', '1'), values=c("-1"= "#db9deb", 
                                                       "0"= "#ad2acf",
                                                       "1" ="#631876" ),name="age group",labels=c("-1"= "child", 
                                                                                                  "0"= "adolescent",
                                                                                                  "1" ="adult"))  +
  theme(text = element_text(size = 32),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))+
  labs(x = "last trial choice", y="p(choice)") 


# marginal effects plots 
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 100, family = "Arial",color="black"))
plot(conditional_effects(m.full.model, effects="probed_identity:age_z"))[[1]] + 
  scale_color_manual(breaks=c('-1', '0', '1'),values=c("-1"= "#db9deb", 
                                                       "0"= "#ad2acf",
                                                       "1" ="#631876"),name="age group",labels=c("-1"= "child", 
                                                                                                     "0"= "adolescent",
                                                                                                     "1" ="adult")) +
  scale_fill_manual(breaks=c('-1', '0', '1'), values=c("-1"= "#db9deb", 
                                                       "0"= "#ad2acf",
                                                       "1" ="#631876" ),name="age group",labels=c("-1"= "child", 
                                                                                                      "0"= "adolescent",
                                                                                                      "1" ="adult"))  +
  theme(text = element_text(size = 32),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))+
  labs(x = "probed trial choice", y="p(choice)") 
ggplot2::ggsave("cb_exp1_effect_marginal_effects_probed_trial_identity_age.png",bg = "white")


# marginal effects probed ctx
plot(conditional_effects(ctx.rwd.model, effects="context_rwd:age_z"))[[1]] + 
  scale_color_manual(breaks=c('-1', '0', '1'),values=c("-1"= "#db9deb", 
                                                       "0"= "#ad2acf",
                                                       "1" ="#631876"),name="age group",labels=c("-1"= "child", 
                                                                                                 "0"= "adolescent",
                                                                                                 "1" ="adult")) +
  scale_fill_manual(breaks=c('-1', '0', '1'), values=c("-1"= "#db9deb", 
                                                       "0"= "#ad2acf",
                                                       "1" ="#631876" ),name="age group",labels=c("-1"= "child", 
                                                                                                  "0"= "adolescent",
                                                                                                  "1" ="adult"))  +
  labs(x = "probed context reward", y="p(choice)") +
  theme(text = element_text(size = 32),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))

ggplot2::ggsave("cb_exp1_effect_marginal_effects_probed_context_rwd_age.png",bg = "white")


#
# MARGINAL EFFECTS - TOTAL DAY 1 REWARD
plot(conditional_effects(total.rwd.model, effects="total_rwd:age_z"))[[1]] + 
  scale_color_manual(breaks=c('-1', '0', '1'),values=c("-1"= "#db9deb", 
                                                       "0"= "#ad2acf",
                                                       "1" ="#631876"),name="age group",labels=c("-1"= "child", 
                                                                                                 "0"= "adolescent",
                                                                                                 "1" ="adult")) +
  scale_fill_manual(breaks=c('-1', '0', '1'), values=c("-1"= "#db9deb", 
                                                       "0"= "#ad2acf",
                                                       "1" ="#631876" ),name="age group",labels=c("-1"= "child", 
                                                                                                  "0"= "adolescent",
                                                                                                  "1" ="adult"))  +
  labs(x = "total day 1 reward", y="p(choice)") +
  theme(text = element_text(size = 32),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"))

ggplot2::ggsave("cb_exp1_effect_marginal_effects_probed_total_rwd_age.png",bg = "white")



# for comparison with simulated data 
bayesplot::color_scheme_set("teal")
mcmc_plot( m.full.model_1,type = "areas",prob=0.95,variable=c("b_identity","b_reward_1","b_reward_2","b_reward_3","b_probed_identity","b_probed_reward","b_context_rwd"),transformations = "exp",outer_size = 2,inner_size = 3,point_size=6)+ ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("probed context rwd","probed trial reward","probed trial identity","reward-3","reward-2","reward-1","last choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25) +
  xlim(0.95, 2.6)
ggplot2::ggsave("real_data_reg_recent_distant_rewards.png",bg = "white")


# plot the effect of recent experience 
bayesplot::color_scheme_set("teal")
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 16, family = "sans"))
mcmc_plot( m.full.model,type = "areas",prob=0.95,variable=c("b_identity","b_reward_1","b_reward_2","b_reward_3","b_probed_identity","b_probed_reward","b_context_rwd"),transformations = "exp",outer_size = 2,inner_size = 3,point_size=6) + ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("probed context rwd","probed trial reward","probed trial identity","reward-3","reward-2","reward-1","last choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25)
ggplot2::ggsave("cb_exp1_effect_recent_experience_no_PE_include_context_choice.png",bg = "white")

################################################################################
# do one sample t-test - recognition memory performance, different from chance
mem_df <- read.csv("memory_df_dropped_subs.csv")

d_prime <- mem_df$d_prime 
recog_mem_model <- brm(
  bf(d_prime ~ 1), 
  prior = c(set_prior("normal(0, 2)", class = "Intercept"), 
            set_prior("cauchy(0, 1)", class = "sigma")),
  chains = 4, iter = 12000, warmup = 2000, cores=4, data = tibble(d_prime = d_prime))

summary(recog_mem_model)
bayesfactor_parameters(recog_mem_model,direction="two-sided")

pd <- p_direction(recog_mem_model)
pd

# do one sample t-test - source memory performance, different from chance 
source_acc <- mem_df$source_acc 

source_acc_model <- brm(
  bf(source_acc ~ 1), 
  prior = c(set_prior("normal(0, 2)", class = "Intercept"), 
            set_prior("cauchy(0, 1)", class = "sigma")),
  chains = 4, iter = 12000, warmup = 2000, cores=4, data = tibble(source_acc = source_acc))

summary(source_acc_model)
bayesfactor_parameters(source_acc_model,direction="two-sided",null=0.1667)

pd <- p_direction(source_acc_model)
pd

################################################################################
# correlation between age and memory measures 
# standardize values 
mem_df <-
  mem_df %>% 
  mutate(d_prime_s = (d_prime - mean(d_prime)) / sd(d_prime),
         source_acc_s = (source_acc - mean(source_acc)) / sd(source_acc),
         age_s = (age - mean(age)) / sd(age))
  
age_d_prime_corr <- 
  brm(data = mem_df, 
      family = gaussian,
      bf(mvbind(d_prime_s, age_s) ~ 0) + set_rescor(TRUE),
      prior = c(prior(normal(1, 1), class = sigma, resp = dprimes),
                prior(normal(1, 1), class = sigma, resp = ages),
                prior(lkj(2), class = rescor)), sample_prior=TRUE,
      chains = 4, cores = 4,  iter = 12000, warmup = 2000,
      seed = 1)

summary(age_d_prime_corr)

pd <- mean(posterior_samples(age_d_prime_corr)$rescor__dprimes__ages>0) # alternative?
pd 

prior_samples <- prior_draws(age_d_prime_corr)
post_samples <- posterior_samples(age_d_prime_corr)
BF_pars <-bayesfactor_parameters(post_samples$rescor__dprimes__ages,prior=prior_samples$rescor,direction="two-sided")


# correlation between age and source memory 
age_source_acc_corr <- 
  brm(data = mem_df, 
      family = gaussian,
      bf(mvbind(source_acc_s, age_s) ~ 0) + set_rescor(TRUE),
      prior = c(prior(normal(1, 1), class = sigma, resp = sourceaccs),
                prior(normal(1, 1), class = sigma, resp = ages),
                prior(lkj(2), class = rescor)),sample_prior=TRUE,
      chains = 4, cores = 4,  iter = 12000, warmup = 2000,
      seed = 1)


summary(age_source_acc_corr)

pd <- mean(posterior_samples(age_source_acc_corr)$rescor__sourceaccs__ages>0) # alternative?
pd 

prior_samples <- prior_draws(age_source_acc_corr)
post_samples <- posterior_samples(age_source_acc_corr)
BF_pars <-bayesfactor_parameters(post_samples$rescor__sourceaccs__ages,prior=prior_samples$rescor,direction="two-sided")
  
################################################################################


# get the individual probe identity regressor 
# to do: increase above 0.8 and decrease adapt delta
m.remove.age.identity <- brm(choice ~  identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_PE + context_rwd + (probed_identity | sub_id), data = df, family = "bernoulli", iter = 4000)
summary(m.remove.age.identity)

res <- ranef(m.remove.age.identity)
fes <- fixef(m.remove.age.identity)
sub_id <- row.names(res$sub_id)
probed_identity <- fes[, 1][6] + res$sub_id[, , 2]

probed_identity_index <- data.frame(sub_id, probed_identity[,1]) 
write.csv(probed_identity_index, "probed_identity_glm_bayes.csv", row.names=FALSE)

m.full.model %>%
tidybayes::spread_draws(b_Intercept, b_context_rwd) %>%
  mutate(context_rwd = list(seq(-1, 1, 0.01))) %>% #the observed value range of MSESC
  unnest(context_rwd) %>%
  mutate(pred = exp(b_Intercept + b_context_rwd*context_rwd)/(1+exp(b_Intercept + b_context_rwd*context_rwd))) %>%
  group_by(context_rwd) %>%
  summarise(pred_m = mean(pred, na.rm = TRUE),
            pred_low = quantile(pred, prob = 0.025),
            pred_high = quantile(pred, prob = 0.975)) %>%
  ggplot(aes(x = context_rwd, y = pred_m)) +
  geom_line() +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_high), alpha=0.2) +
  ylab("Predicted Probability of Repeating a Grade") +
  scale_y_continuous(breaks = seq(0, 0.22, 0.01))

sjPlot::plot_model(m.full.model,type="pred",terms=c("context_rwd[all]","age_z"),show.data=FALSE, title = " ", line.size=0.5, colors = c("#99CCFF","#9999FF","#6666CC"),show.p
           =TRUE)+
  #scale_color_discrete(name='age',labels = c("child", "adolescent", "adult"))+
  xlab("context reward") + #x axis label
  ylab("p(choice)")  #y axis label+ #y axis label
theme(title= element_text(size=26, vjust=2, face="bold"), #use these settings for titles unless otherwise specified
      axis.title.x= element_text(size=26),
      axis.title.y= element_text(size=26, vjust=1.5),
      axis.text.x= element_text(size=24, colour="black", vjust=0.6),
      axis.text.y= element_text(size=24, colour="black"),
      legend.title= element_text(size=24), #no legend, but just in case you need one
      legend.text= element_text(size=22),
      #legend.position="bottom",
      strip.text = element_text(size=24, face="bold"), #text in strip headings
      panel.grid.major = element_blank(), #take out grid marks
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"))

##### add in reaction times ####################################################
df_rt <- read.csv("glm_drop_rt.csv")

rt.model <- brms::brm(choice ~  identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_reward + rt + context_rwd  + age_z +  identity*age_z + reward_1*age_z + reward_2*age_z + reward_3*age_z + probed_identity*age_z + probed_reward*age_z + rt*age_z + context_rwd*age_z + context_rwd*rt + context_rwd*age_z*rt  +(1 | sub_id), data = df_rt, family = "bernoulli")
summary(rt.model)

bayesplot::color_scheme_set("green")
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 16, family = "sans"))
mcmc_plot(rt.model,type = "intervals",variable=c("b_context_rwd","b_context_rwd:age_z","b_rt:context_rwd","b_rt:context_rwd:age_z"),transformations = "exp",outer_size = 2,inner_size = 3,point_size=6) + ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("reward-3","reward-2","reward-1","last choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25)
ggplot2::ggsave("cb_exp1_effect_rt_no_PE.png",bg = "white")

##### run regression in subs simulated  ###############################################
sim_df <- read.csv("modeling_context_bandits/ctx_sampler/design_mat_simulated_subs.csv")

sim_model <- brms::brm(choice ~  identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_reward + context_rwd + age_z + age_z*identity + age_z*reward_1+ age_z*reward_2 + age_z*reward_3 + age_z*probed_identity  + age_z*probed_reward + age_z*context_rwd + (1 | sub_id), data = df_2, family = "bernoulli")

summary(sim_model)

# plot the effect of distant experience 
bayesplot::color_scheme_set("teal")
mcmc_plot( sim.model,type = "intervals",variable=c("b_identity","b_reward_1","b_reward_2","b_reward_3","b_probed_identity","b_probed_reward","b_context_rwd"),transformations = "exp",outer_size = 2,inner_size = 3,point_size=6)+ ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("probed context rwd","probed trial reward","probed trial identity","reward-3","reward-2","reward-1","last choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25) +
  xlim(0.95, 2.6)
ggplot2::ggsave("simulated_data_reg_recent_distant_rewards.png",bg = "white")


######### children only ######### ######### ######### ######### ######### 

child <-subset(df,age_group=='child')

child.model <- brms::brm(choice ~  identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_reward + context_rwd  + (1 | sub_id), data = child, family = "bernoulli",prior = prior, iter = 12000, warmup=2000,cores=4)

# plot the effect of recent experience 
bayesplot::color_scheme_set("brightblue")
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 20, family = "Arial",color="black"))
mcmc_plot( child.model,type = "areas", prob=0.95, variable=c("b_identity","b_reward_1","b_reward_2","b_reward_3"),transformations = "exp") + ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("reward-3","reward-2","reward-1","last choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25) +
  xlim(0.8, 4) + theme(text = element_text(size = 20),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black",family="Arial",face = "plain"))
ggplot2::ggsave("child_recent_experience.png",bg = "white",width = 12, height = 10, units = "cm")


# plot the effect of distant experience
darkpurple_scheme <- c("#db9deb", "#c562de","#ad2acf","#7c1e94","#631876","#4a1258")
bayesplot::color_scheme_set(darkpurple_scheme)
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 20, family = "Arial",color="black"))
mcmc_plot( child.model,type = "areas",prob=0.95,variable=c("b_probed_identity","b_probed_reward","b_context_rwd"),transformations = "exp")+ ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("probed context rwd","probed trial reward","probed trial choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25) +
  theme(text = element_text(size = 20),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black",family="Arial",face = "plain"))
ggplot2::ggsave("child_distant_experience.png",bg = "white",width = 12, height = 10, units = "cm")

######### #########  adolescent ######### ######### ######### ######### ######### 

adolescent <-subset(df,age_group=='adolescent')

adolescent.model <- brms::brm(choice ~  identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_reward + context_rwd  + (1 | sub_id), data = adolescent, family = "bernoulli",prior = prior, iter = 12000, warmup=2000,cores=4)

# plot the effect of recent experience 
bayesplot::color_scheme_set("brightblue")
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 20, family = "Arial",color="black"))
mcmc_plot( adolescent.model,type = "areas", prob=0.95, variable=c("b_identity","b_reward_1","b_reward_2","b_reward_3"),transformations = "exp") + ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("reward-3","reward-2","reward-1","last choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25) +
  xlim(0.8, 4) + theme(text = element_text(size = 20),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black",family="Arial",face = "plain"))
ggplot2::ggsave("adolescent_recent_experience.png",bg = "white",width = 12, height = 10, units = "cm")


# plot the effect of distant experience
darkpurple_scheme <- c("#db9deb", "#c562de","#ad2acf","#7c1e94","#631876","#4a1258")
bayesplot::color_scheme_set(darkpurple_scheme)
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 20, family = "Arial",color="black"))
mcmc_plot( adolescent.model,type = "areas",prob=0.95,variable=c("b_probed_identity","b_probed_reward","b_context_rwd"),transformations = "exp")+ ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("probed context rwd","probed trial reward","probed trial choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25) +
  theme(text = element_text(size = 20),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black",family="Arial",face = "plain"))
ggplot2::ggsave("adolescent_distant_experience.png",bg = "white",width = 12, height = 10, units = "cm")

######### #########  adult  ######### ######### ######### ######### ######### 

adult <-subset(df,age_group=='adult')

adult.model <- brms::brm(choice ~  identity + reward_1 + reward_2 + reward_3 + probed_identity + probed_reward + context_rwd  + (1 | sub_id), data = adult, family = "bernoulli",prior = prior, iter = 12000, warmup=2000,cores=4)

# plot the effect of recent experience 
bayesplot::color_scheme_set("brightblue")
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 20, family = "Arial",color="black"))
mcmc_plot( adult.model,type = "areas", prob=0.95, variable=c("b_identity","b_reward_1","b_reward_2","b_reward_3"),transformations = "exp") + ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("reward-3","reward-2","reward-1","last choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25) +
  xlim(0.8, 4) + theme(text = element_text(size = 20),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black",family="Arial",face = "plain"))
ggplot2::ggsave("adult_recent_experience.png",bg = "white",width = 12, height = 10, units = "cm")


# plot the effect of distant experience
darkpurple_scheme <- c("#db9deb", "#c562de","#ad2acf","#7c1e94","#631876","#4a1258")
bayesplot::color_scheme_set(darkpurple_scheme)
bayesplot::bayesplot_theme_update(text = ggplot2::element_text(size = 20, family = "Arial",color="black"))
mcmc_plot( adult.model,type = "areas",prob=0.95,variable=c("b_probed_identity","b_probed_reward","b_context_rwd"),transformations = "exp")+ ggplot2::xlab("Odds Ratio") + ggplot2::scale_y_discrete(labels = c("probed context rwd","probed trial reward","probed trial choice"),limits=rev) + bayesplot::vline_at(1,linetype = 5, linewidth = 0.25) +
  theme(text = element_text(size = 20),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black",family="Arial",face = "plain"))
ggplot2::ggsave("adult_distant_experience.png",bg = "white",width = 12, height = 10, units = "cm")



