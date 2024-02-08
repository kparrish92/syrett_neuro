source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "01_helpers.R"))

fc_task = read.csv(here("data", "at_csc.csv")) %>% 
  filter(!is.na(RESPONSE_CORRECT.))

mod1 = brm(RESPONSE_CORRECT. ~ PROMPT_TYPE*SPEAKER_TYPE + (1 | PARTICIPANT_ID) 
    + (1 | VIDEO_SCENE), data = fc_task,
    family = bernoulli(link = "logit"),
    file = here("models", "model_b.rds"))

