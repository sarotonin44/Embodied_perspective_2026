library(readxl)
df <- read_excel(file.choose())


names(df) <- names(df) |>
  gsub("^C4-", "", x = _) |>
  gsub("_avg$", "", x = _) |>
  gsub("Inval", "inv", x = _) |>
  gsub("Val", "val", x = _)
View(df)


library(tidyr)
library(dplyr)

names(df)[1] <- "Participants"

long_df <- df %>%
  pivot_longer(
    cols = -Participants,
    names_to = c("CueValidity", "Task", "PostureCongruency", "Angle"),
    names_sep = "_",
    values_to = "Amplitude"
  )


nrow(long_df)

View(long_df)

long_df <- long_df %>%
  mutate(
    Participants = factor(Participants),
    Angle = factor(Angle, levels = c("60", "160")),
    CueValidity = factor(CueValidity,
                         levels = c("val", "inv"),
                         labels = c("Valid", "Invalid")),
    Task = factor(Task,
                  levels = c("vis", "emb"),
                  labels = c("Visual", "Embodied")),
    PostureCongruency = factor(PostureCongruency,
                               levels = c("cong", "incong"),
                               labels = c("Congruent", "Incongruent"))
  )


library(afex)

anova_eeg <- aov_ez(
  id = "Participants",
  dv = "Amplitude",
  within = c("Angle", "CueValidity", "PostureCongruency", "Task"),
  data = long_df,
  type = 3
)

nice(anova_eeg, es = "pes")


library(emmeans)

emm_angle_by_task <- emmeans(anova_eeg, ~ Angle | Task)

pairs(emm_angle_by_task, adjust = "bonferroni")

emm_task_by_angle <- emmeans(anova_eeg, ~ Task | Angle)

pairs(emm_task_by_angle, adjust = "bonferroni")

library(emmeans)
library(ggplot2)

# EMMs for plotting
emm_angle_task <- emmeans(anova_eeg, ~ Angle * Task)
emm_df <- as.data.frame(emm_angle_task)

# Robust relabel: remove leading X if present + add degree symbol
emm_df$Angle <- gsub("^X", "", as.character(emm_df$Angle))
emm_df$Angle <- factor(emm_df$Angle,
                       levels = c("60", "160"),
                       labels = c("60°", "160°"))



p <- ggplot(emm_df, aes(x = Angle, y = emmean, colour = Task, group = Task)) +
  geom_line(linewidth = 1.2) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pd, width = 0.06, linewidth = 0.8) +
  labs(x = "Angle (degrees)",
       y = "Mean Amplitude (µV, 300–500 ms, C4)",
       colour = "Task") +
  theme_classic(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

p

pd <- position_dodge(width = 0.25)

p <- ggplot(emm_df,
            aes(x = Angle,
                y = emmean,
                colour = Task,
                group = Task)) +
  geom_line(position = pd, linewidth = 1.2) +
  geom_point(position = pd, size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pd,
                width = 0.06,
                linewidth = 0.8) +
  labs(x = "Angle (degrees)",
       y = "Mean Amplitude (µV, 300–500 ms, C4)",
       colour = "Task") +
  theme_classic(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

p






library(emmeans)
library(ggplot2)

# Task within each Angle
emm_task_by_angle <- emmeans(anova_eeg, ~ Task | Angle)
emm_df_T <- as.data.frame(emm_task_by_angle)

# Clean Angle labels
emm_df_T$Angle <- gsub("^X", "", as.character(emm_df_T$Angle))
emm_df_T$Angle <- factor(emm_df_T$Angle,
                         levels = c("60", "160"),
                         labels = c("60°", "160°"))

p_bar <- ggplot(emm_df_T, aes(x = Task, y = emmean, fill = Task)) +
  geom_col(width = 0.6, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15, linewidth = 0.8) +
  facet_wrap(~ Angle) +
  labs(x = "Task",
       y = "Mean Amplitude (µV, 300–500 ms, C4)") +
  theme_classic(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

p_bar

# Use the interaction EMMs
emm_angle_task <- emmeans(anova_eeg, ~ Angle * Task)
emm_df <- as.data.frame(emm_angle_task)

emm_df$Angle <- gsub("^X", "", as.character(emm_df$Angle))
emm_df$Angle <- factor(emm_df$Angle,
                       levels = c("60", "160"),
                       labels = c("60°", "160°"))

pd <- position_dodge(width = 0.7)

p_bar_interaction <- ggplot(emm_df, aes(x = Angle, y = emmean, fill = Task)) +
  geom_col(position = pd, width = 0.6, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = pd, width = 0.15, linewidth = 0.8) +
  labs(x = "Angle (degrees)",
       y = "Mean Amplitude (µV, 300–500 ms, C4)",
       fill = "Task") +
  theme_classic(base_size = 14) +
  theme(
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

p_bar_interaction






