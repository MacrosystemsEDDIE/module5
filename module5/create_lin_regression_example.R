library(ggplot2)
cols <- RColorBrewer::brewer.pal(8, "Dark2")
set.seed(1)

airt <- read.csv("data/CRAM_airt_celsius.csv")
wtemp <- read.csv("data/CRAM_wtemp_celsius.csv")

wtemp <- wtemp[(wtemp[, 2] == min(wtemp[, 2])), c(1, 3)]

df <- merge(airt, wtemp, by = 1)

df <- na.exclude(df)


rem <- sample(1:nrow(df), 0.5 * nrow(df), replace = FALSE)
df2 <- df[-rem, ]

fit <- lm(df2[, 3] ~ df2[, 2])


coeff <- fit$coefficients
# coeff <- as.character(round(coeff, 2))
eqn <- paste0("y = ", round(coeff[2], 2), "x + ", round(coeff[1], 2))

latex <- paste0("$$ y = ", round(coeff[2], 2), " * x + ", round(coeff[1], 2), "$$")
  
txt <- data.frame(x = 6, y = 27, txt = eqn)


p <- ggplot(df2) +
  geom_point(aes_string(names(df)[2], names(df)[3]), size = 2) +
  geom_abline(slope = coeff[2], intercept = coeff[1], color = cols[2], linetype = "dashed", size = 1.2) +
  geom_hline(yintercept = 0, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_text(data = txt, aes(x, y, label = txt), color = cols[2], size = 8) +
  # annotate("text", x = 6, y = 27, label = latex2exp::TeX(eqn, output = "character"), parse = T) +
  xlab("Air temperature (\u00B0C)") +
  ylab("Surface water temperature (\u00B0C)") +
  coord_equal(xlim = c(-5, 30), ylim = c(-5, 30)) +
  theme_classic(base_size = 24) +
  theme(panel.background = element_rect(fill = NULL, color = "black"))
p

ggsave("www/linear_regression_example.png", p, device = "png", dpi = 300, units = "mm", width = 211, height = 211)

