library(readxl)

X215_Beh_data_all_21_10 <- read_excel(file.choose())



library(dplyr)

df <- X215_Beh_data_all_21_10

# get all CW columns
cw_cols <- names(df)[grepl("_CW_", names(df))]

for (cw in cw_cols) {
  cc <- sub("_CW_", "_CC_", cw)
  new_name <- sub("_CW_", "_", cw)
  
  df[[new_name]] <- rowMeans(df[, c(cw, cc)], na.rm = TRUE)
}

X215_Beh_data_all_21_10 <- df

library(dplyr)
library(tidyr)

df <- X215_Beh_data_all_21_10

long_df <- df %>%
  pivot_longer(
    cols = matches("^(60|160)_(val|inv)_(cong|incong)_(vis|emb)$"),
    names_to = c("Angle", "CueValidity", "PostureCongruency", "Task"),
    names_pattern = "(60|160)_(val|inv)_(cong|incong)_(vis|emb)",
    values_to = "RT"
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

library(afex)

anova_4way <- aov_ez(
  id = "Participant",
  dv = "RT",
  within = c("Angle", "CueValidity", "PostureCongruency", "Task"),
  data = long_df,
  type = 3
)

nice(anova_4way, es = "pes")
anova_table <- nice(anova_4way, es = "pes")

anova_table[, c("Effect", "df", "F", "p.value", "pes")]

library(emmeans)

library(ggplot2)

emm_angle_task <- emmeans(anova_4way, ~ Angle * Task)
emm_df <- as.data.frame(emm_angle_task)
head(emm_df)

ggplot(emm_df,
       aes(x = Angle,
           y = emmean,
           colour = Task,
           group = Task)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = emmean - SE,
                    ymax = emmean + SE),
                width = .1) +
  labs(
    x = "Angle (degrees)",
    y = "Reaction Time (ms)",
    colour = "Task"
  ) +
  theme_minimal(base_size = 13)
emm_angle_task <- emmeans(anova_4way, ~ Angle * Task, infer = c(TRUE, TRUE))
emm_df <- as.data.frame(emm_angle_task)


emm_df$Angle <- factor(emm_df$Angle,
                       levels = c("X60", "X160"),
                       labels = c("60", "160"))

levels(emm_df$Angle)

unique(emm_df$Angle)

library(ggplot2)

pd <- position_dodge(width = 0.25)

ggplot(emm_df,
       aes(x = Angle, y = emmean, colour = Task, group = Task)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pd, width = 0.08) +
  labs(x = "Angle (degrees)", y = "Reaction Time (ms)", colour = "Task") +
  theme_minimal(base_size = 13)

library(emmeans)

emm_angle_by_task <- emmeans(anova_4way, ~ Angle | Task)

pairs(emm_angle_by_task, adjust = "bonferroni")

pd <- position_dodge(width = 0.25)

ggplot(emm_df3, aes(x = Angle, y = emmean, colour = Task, group = Task)) +
  geom_line(position = pd, linewidth = 1) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pd, width = 0.08) +
  facet_wrap(~ CueValidity) +
  labs(x = "Angle (degrees)", y = "Reaction Time (ms)", colour = "Task") +
  theme_minimal(base_size = 13)
