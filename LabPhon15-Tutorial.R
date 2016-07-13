# Author: Patrick Reidy


options(stringsAsFactors = FALSE)


library(arm)
library(extrafont)
library(fontcm)
library(ggplot2)
library(grid)
library(lme4)



# Read in the full data.frame.
pdlg <- read.csv('./LabPhon15-Psychoacoustic.csv')

# Take a look at the variables in the data.frame.
names(pdlg)
head(pdlg)




# Generate orthogonal time-polynomials that are
# evaluated on values of `Window`.
time_polys <- poly(
  x      = sort(unique(pdlg$Window)), 
  degree = 3,    # linear, quadratic, and cubic powers
  raw    = FALSE # orthogonal
)
# Values of the time-polynomials are stored in matrix.
# Columns correspond to degrees (powers of time).
time_polys[1:10, ]

# ``Orthogonal'' in the sense that inner product is 0.
time_polys[, 1] %*% time_polys[, 2]
time_polys[, 1] %*% time_polys[, 3]
time_polys[, 2] %*% time_polys[, 3]

# Correlation matrix for the time-polynomials.
cor(time_polys)


# Add time-polynomials to the `pdlg` data.frame.
pdlg[, paste0('Time', 1:3)] <- 
  time_polys[pdlg$Window, ]

# Extract the productions of Am. English [s] and [S].
english <- subset(pdlg, Language == 'English')

# A quadratic model for Am. English [s] and [S]
english_model <- lmer(
  data    = english,
  formula = PeakERB ~ 
    # Fixed effects:
    (1 + Time1 + Time2) * Sex * Consonant +
    # Random effects:
    (1 + Time1 + Time2 | Participant) +
    (1 + Time1 + Time2 | Participant:Consonant)
)

english$Residual <- residuals(english_model)

# Plot residuals as a function of time-window.
ggplot(data = english, 
       aes(x = Window, y = Residual)) +
  geom_jitter(
    position = position_jitter(width = 0.05)) +
  geom_hline(
    aes(yintercept = mean(Residual)+2*sd(Residual))) +
  geom_hline(
    aes(yintercept = mean(Residual)-2*sd(Residual)))

# Exclude measurements near the endpoints.
english <- subset(english, Window %in% 5:47)
english_model <- lmer(
  data    = english,
  formula = PeakERB ~ 
    # Fixed effects:
    (1 + Time1 + Time2) * Sex * Consonant +
    # Random effects:
    (1 + Time1 + Time2 | Participant) +
    (1 + Time1 + Time2 | Participant:Consonant)
)

# Coefficients for the fixed effects.
fixef(english_model)
summary(english_model)$coefficients

# Make predictions on just the fixed effects to describe expected peak 
# frequency values at the population level, i.e. ignoring random effects.
# Population-level data.
population <- expand.grid(
  Sex       = c('F', 'M'),
  Consonant = c('s', 'S'),
  Window    = 5:47
)
population <- within(population, {
  Time1 <- english$Time1[match(Window, english$Window)]
  Time2 <- english$Time2[match(Window, english$Window)]
})
# Population-level predictions on just fixed effects.
population$Prediction <- predict(
  object  = english_model,
  newdata = population,
  re.form = NA # ignore random effects
)


# Random effects for the participant group.
ranef(english_model)[['Participant']]

# Random effects for the participant-consonant group.
ranef(english_model)[['Participant:Consonant']]


# Make predictions on the fixed effects and random effects to describe expected
# peak frequency values at the participant level.
# Participant-level data.
participants <- expand.grid(
  Participant = sort(unique(english$Participant)),
  Consonant   = sort(unique(english$Consonant)),
  Window      = sort(unique(english$Window))
)
participants <- within(participants, {
  Sex   <- english$Sex[match(Participant, 
                             english$Participant)]
  Time1 <- english$Time1[match(Window, english$Window)]
  Time2 <- english$Time2[match(Window, english$Window)]
})
# Predictions on fixed and random effects.
participants$Prediction <- predict(
  object  = english_model,
  newdata = participants,
  re.form = NULL # use random effects
)


# Bootstrap confidence intervals for the coefficients.
confint(object = english_model, 
        level  = 0.95, 
        method = 'boot', 
        nsim   = 1000,
        seed   = 3122 # for reproducibility
)

# Simulate beta coefficients for the fitted model.
set.seed(3122)
beta_sim <- fixef(
  arm::sim(object = english_model,
           n.sims = 1000)
)
# 95% intervals for the coefficients.
apply(X = beta_sim, MARGIN = 2, 
      FUN = quantile, probs = c(.025, .975))



# A function for population-level predictions.
population_prediction <- function(model) {
  .prediction <- predict(
    object   = model,
    newdata = population,
    re.form  = NA
  )
  names(.prediction) <- with(
    population, paste0(Sex, Consonant, Window)
  )
  return(.prediction)
}
# Bootstrap a population-level prediction.
population_boot <- bootMer(
  x     = english_model, 
  FUN   = population_prediction, 
  nsim  = 1, 
  use.u = FALSE, 
  type  = 'parametric',
  seed  = 3122
)
population_boot$t








# Compare peak frequency of [s] across two languages.
# Set up data.
s <- subset(
  pdlg, 
  Consonant == 's' & 
    Vowel %in% c('e', 'a', 'o', 'u') & 
    Window %in% 13:39
)

# Fit model.
s_model <- lmer(
  data    = s,
  formula = PeakERB ~ 
    (1 + Time1 + Time2) * Language +
    (1 + Time1 + Time2 | Participant)
)


