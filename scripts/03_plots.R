source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "01_helpers.R"))

fc_task = read.csv(here("data", "at_csc.csv")) %>% 
  filter(!is.na(RESPONSE_CORRECT.))

mod = read_rds(here("models", "model_b.rds"))

percentage = fc_task %>% 
  group_by(PROMPT_TYPE, SPEAKER_TYPE) %>% 
  summarize(total = n()) %>% 
  mutate(p_s = paste(PROMPT_TYPE, SPEAKER_TYPE, sep = "_")) %>% 
  select(total, p_s) %>% 
  left_join(desc, by = "p_s") %>% 
  mutate(pct = n/total)


# The Percentage of Correct Responses for autistic and neurotypical speakers for each sentence type") +

percentage %>% 
  filter(RESPONSE_CORRECT. == 1) %>% 
  ggplot(aes(y = pct, fill = SPEAKER_TYPE, 
             x = PROMPT_TYPE.y)) + 
  geom_col(color = "black", position = "dodge") +
  xlab("Prompt type") +
  ylab("Percentage Correct Responses") +
  theme_minimal() +
  labs(fill = "Speaker type") +
  geom_text(aes(label=round(pct, digits = 2)), 
            position=position_dodge(width=0.9), vjust=-0.3, size = 2.5) +
  ggsave(here("docs", "img", "desc_pct.png"), dpi = 600)


# Model plot 
plot_df = conditional_effects(mod)[["PROMPT_TYPE:SPEAKER_TYPE"]]

## Probability of a correct response as a function of group and condition  
plot_df %>% 
  ggplot(aes(x = effect1__, y = estimate__, fill = effect2__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), shape = 21, 
                  position = position_dodge(width = .2)) +
  theme_minimal() + ylab("Probability") + xlab("Condition") + 
  labs(fill = "Speaker type") + 
  ggsave(here("docs", "img", "mod_plot.png"), dpi = 600)


### Appendix plots

posterior <- as.data.frame(mod)

pars = colnames(posterior[1:6])

fixef_df = fixef(mod) %>% 
  as.data.frame() %>% 
  rownames_to_column("parameter") %>% 
  mutate(parameter = paste0("b_", parameter))

bayesplot::mcmc_areas(posterior,
                      pars = pars,
                      prob = 0.8) + 
  geom_text(data = mutate_if(fixef_df, is.numeric, round, 2),
            aes(label = paste(Estimate, "[", Q2.5, "-", Q97.5, "]"), x = Inf), 
            hjust = "inward", size = 2) + 
  xlim(-9,9) +
  theme(text = element_text(size=8)) +
  ggsave(here("docs",
              "img", "mcmc_plot.png"), dpi = 600)



