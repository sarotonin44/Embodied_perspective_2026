library(readxl)
acc_df <- read_excel(file.choose())
View(acc_df)
str(acc_df)

library(dplyr)
library(tidyr)

long_acc <- acc_df %>%
  pivot_longer(
    cols = matches("^(60|160)_(inv|val)_(cong|incong)_(vis|emb)$"),
    names_to = c("Angle", "CueValidity", "PostureCongruency", "Task"),
    names_pattern = "(60|160)_(inv|val)_(cong|incong)_(vis|emb)",
    values_to = "Accuracy"
  ) %>%
  mutate(
    Angle = factor(Angle, levels = c("60", "160")),
    CueValidity = factor(CueValidity,
                         levels = c("val", "inv"),
                         labels = c("Valid", "Invalid")),
    PostureCongruency = factor(PostureCongruency,
                               levels = c("cong", "incong"),
                               labels = c("Congruent", "Incongruent")),
    Task = factor(Task,
                  levels = c("vis", "emb"),
                  labels = c("Visual", "Embodied"))
  )


dim(long_acc)
table(long_acc$Participant)

library(afex)

anova_acc <- aov_ez(
  id = "Participant",
  dv = "Accuracy",
  within = c("Angle", "CueValidity", "PostureCongruency", "Task"),
  data = long_acc,
  type = 3
)
nice(anova_acc, es = "pes")

library(emmeans)

emm_angle_by_task_acc <- emmeans(anova_acc, ~ Angle | Task)
pairs(emm_angle_by_task_acc, adjust = "bonferroni")

emm_cue_by_task_acc <- emmeans(anova_acc, ~ CueValidity | Task)
pairs(emm_cue_by_task_acc, adjust = "bonferroni")

library(ggplot2)
emm_AT_acc <- emmeans(anova_acc, ~ Angle * Task, infer = c(TRUE, TRUE))
emm_AT_acc_df <- as.data.frame(emm_AT_acc)

emm_AT_acc_df$Angle <- factor(gsub("\\D","", emm_AT_acc_df$Angle),
                              levels = c("60","160"))

pd <- ggplot2::position_dodge(0.25)

ggplot(emm_AT_acc_df,
       aes(x = Angle, y = emmean, colour = Task, group = Task)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pd, width = .08) +
  labs(x = "Angle (degrees)", y = "Accuracy", colour = "Task") +
  theme_minimal(base_size = 13)

emm_task_by_angle_acc <- emmeans(anova_acc, ~ Task | Angle)

pairs(emm_task_by_angle_acc, adjust = "bonferroni")

library(emmeans)

emm_CV_PC <- emmeans(
  anova_acc,
  ~ CueValidity * PostureCongruency,
  infer = c(TRUE, TRUE)
)

emm_df_CV_PC <- as.data.frame(emm_CV_PC)

library(ggplot2)

pd <- position_dodge(width = 0.25)

ggplot(emm_df_CV_PC,
       aes(x = CueValidity,
           y = emmean,
           colour = PostureCongruency,
           group = PostureCongruency)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pd,
                width = 0.08) +
  labs(
    x = "Cue Validity",
    y = "Accuracy",
    colour = "Posture Congruency"
  ) +
  theme_minimal(base_size = 13)

library(emmeans)
library(ggplot2)

# 1) Estimated marginal means for the interaction
emm_T_CV <- emmeans(anova_acc, ~ CueValidity * Task, infer = c(TRUE, TRUE))
emm_df_T_CV <- as.data.frame(emm_T_CV)

# 2) Plot
pd <- position_dodge(width = 0.25)

ggplot(emm_df_T_CV,
       aes(x = CueValidity,
           y = emmean,
           colour = Task,
           group = Task)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pd, width = 0.08) +
  labs(
    x = "Cue Validity",
    y = "Accuracy",
    colour = "Task"
  ) +
  theme_minimal(base_size = 13)



ggplot(emm_df_T_CV,
       aes(x = Task,
           y = emmean,
           colour = CueValidity,
           group = CueValidity)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pd, width = 0.08) +
  labs(
    x = "Task",
    y = "Accuracy",
    colour = "Cue Validity"
  ) +
  theme_minimal(base_size = 13)
