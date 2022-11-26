#import the libraries in environment
if (!require('ggplot2')) {
  install.packages('ggplot2')
  library('ggplot2')
}

if (!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
}

if (!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}

if (!require('cowplot')) {
  install.packages('cowplot')
  library('cowplot')
}

if (!require('multcomp')) {
  install.packages('multcomp')
  library('multcomp')
}
# read in the data
telecom <-
  read.csv("data/telecoms_churn.csv", stringsAsFactors = TRUE)
# Exclude the nulls
telecom = telecom[complete.cases(telecom), ]

# source the anova tab function
source('anovatab.R')

#attach(telecom)

# Exploratory Data Analysis
# Asses first few data observations
head(telecom)

# structure of the data values
str(telecom)

# convert to factor
telecom$churn <- as.factor(telecom$churn)

# data cleanup
telecom <- subset(telecom, select = -c(X))

# bin continuos variable to better understand the spread
telecom <-
  telecom %>% mutate(monthlycharges_bin = cut(monthlycharges , breaks = 6))

# means of the data values
summary(telecom)

# Figure 1 showing percentage of churn
PlotImage1 <-  telecom %>%
  group_by(churn) %>%
  summarise(Count = n()) %>%
  mutate(percent = prop.table(Count) * 100) %>%
  ggplot(aes(reorder(churn,-percent), percent), fill = churn) +
  geom_col(fill = c("#FC4E07", "#E7B800")) +
  geom_text(
    aes(label = sprintf("%.2f%%", percent)),
    hjust = 0.01,
    vjust = -0.5,
    size = 8
  ) +
  theme_classic() +
  xlab("Churn (1=Churned, 0=did not Churn)") +
  ylab("Percent") +
  ggtitle("Churn Percent") +
  theme_classic() +
  # control theme
  ylab("Number of Customers") +
  xlab('Churn (1=Churned, 0=did not Churn)') +
  labs(title = "Figure 1. Churn distribution.") +
  # changing the legend title:
  guides(fill = guide_legend("Churn")) +
  theme(
    title = element_text(size = 18),
    axis.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    plot.subtitle = element_text(color = "gray30"),
    # changing the legend style:
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(color = "black")
  )
ggsave(
  file = "Figure1.jpg",
  plot = PlotImage1,
  width = 20,
  height = 10
)

# total number of churn
table(telecom$churn)

#percentage of churn and not churning
round(prop.table(table(telecom$churn)), 3)

PlotImage2 <-
  plot_grid(
    ggplot(telecom, aes(x = gender, fill = churn)) + geom_bar(position = 'fill') + theme_classic() +
      ylab("Percentage") +
      xlab('Gender') +
      labs(title = "Gender impacting Churning") +
      # changing the legend title:
      guides(fill = guide_legend("Churn")) +
      theme(
        title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.subtitle = element_text(color = "gray30"),
        # changing the legend style:
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(color = "black")
      ),
    
    
    # 2nd plot
    ggplot(telecom, aes(x = internetservice, fill = churn)) + geom_bar(position = 'fill') +
      theme_classic() +
      #ylab("Number of Customers") +
      xlab('Internet Service') +
      labs(title = "Internet Service impacting Churning") +
      # changing the legend title:
      guides(fill = guide_legend("Churn")) +
      theme(
        title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.subtitle = element_text(color = "gray30"),
        axis.title.y = element_blank(),
        # changing the legend style:
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(color = "black")
      ),
    
    # 3rd plot
    ggplot(telecom, aes(x = partner, fill = churn)) + geom_bar(position = 'fill') +
      theme_classic() +
      ylab("Percentage") +
      xlab('Partner') +
      labs(title = "Partner impacting Churning") +
      # changing the legend title:
      guides(fill = guide_legend("Churn")) +
      theme(
        title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.subtitle = element_text(color = "gray30"),
        
        # changing the legend style:
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(color = "black")
      ),
    
    # 4th Plot
    ggplot(telecom, aes(x = paymentmethod, fill = churn)) + geom_bar(position = 'fill') +
      theme_classic() +
      xlab('Payment Method') +
      labs(title = "Payment method impacting Churning") +
      # changing the legend title:
      guides(fill = guide_legend("Churn")) +
      theme(
        title = element_text(size = 16),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 14),
        plot.subtitle = element_text(color = "gray30"),
        axis.title.y = element_blank(),
        # changing the legend style:
        legend.title = element_text(face = "bold"),
        legend.background = element_rect(color = "black")
      ) +
      scale_x_discrete(
        "Payment Method",
        labels = c(
          "Bank transfer (automatic)" = "BT",
          "Electronic check" = "EC",
          "Credit card (automatic)" = "CC",
          "Mailed check" = "MC"
        )
        
      )
  )

ggsave(
  file = "Figure2.jpg",
  plot = PlotImage2,
  width = 10,
  height = 10
)

# plotting the Bins of the monthly charges 
PlotImage3 <-
  ggplot(telecom, aes(x = monthlycharges_bin, fill = churn)) +
  geom_bar(position = 'fill') +
  theme_classic() +
  xlab("Churn Percent-") +
  ylab("Percent") +
  ggtitle("Churn Percent") +
  theme_classic() +
  # control theme
  ylab("Percentage") +
  xlab('Monthly Charges bins') +
  labs(title = "Figure 3. Churn per Monthly Charges bins.") +
  # changing the legend title:
  guides(fill = guide_legend("Churn")) +
  theme(
    title = element_text(size = 18),
    axis.text = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 18),
    plot.subtitle = element_text(color = "gray30"),
    # changing the legend style:
    legend.title = element_text(face = "bold"),
    legend.background = element_rect(color = "black") +
      scale_x_binned(n.breaks = 30)
  )
ggsave(
  file = "Figure3.jpg",
  plot = PlotImage3,
  width = 20,
  height = 10
)

# main
fit1 = glm(
  churn ~ factor(gender) + factor(internetservice) + factor(partner) + monthlycharges +
    factor(paymentmethod),
  family = binomial(),
  data = telecom
)
summary(fit1)
confint(fit1)
logLik(fit1)

#calculate an estimate of the probability of churning for: a male, living with
#his partner, monthly charges=70, internet service=fiber optic, payment method=
#  credit card

# another method
nd <- data.frame(
  gender = "Male",
  internetservice = "Fiber optic",
  partner = "Yes",
  monthlycharges = 70.00,
  paymentmethod = "Credit card (automatic)"
)
pred <-
  predict(fit1,
          newdata = nd,
          se.fit = T,
          interval = 'confidence')
# calculate probability of churning
exp(pred$fit)/(1+exp(pred$fit))
predci <- c(pred$fit - 1.96 * pred$se.fit, pred$fit + 1.96 * pred$se.fit)
exp(predci) ## odds CI
exp(predci) / (1 + exp(predci)) ## probability CI

#LRT
drop1 (fit1 , test = "LRT")

# without gender variable
fit2 = glm(
  churn ~ factor(internetservice) + factor(partner) + monthlycharges +
    factor(paymentmethod),
  family = binomial(),
  data = telecom
)
summary(fit2)
confint(fit2)
logLik(fit2)



# Odds for customers with a bank transfer compared to credit card  
L <- cbind(0,	0,	0,	0,	0,	1,	0,	0)
glh2 <-  glht(fit2, linfct = L)
summary(glh2)
confint(glh2)$confint # CI on linear predictor scale
exp(confint(glh2)$confint) # CI on OR scale

# Odds for customers with a Fibre Optic compared to DSL internet service 
L <- cbind(0,	-1,	0,	0,	0,	0,	0,	0)
glh3 <-  glht(fit2, linfct = L)
summary(glh3)
confint(glh3)$confint # CI on linear predictor scale
exp(confint(glh3)$confint) # CI on OR scale

# Odds for customers living with a partner compared to 
# customers not living with a partner 
L <- cbind(0,	0,	0,	1,	0,	0,	0,	0)
glh4 <-  glht(fit2, linfct = L)
summary(glh4)
confint(glh4)$confint # CI on linear predictor scale
exp(confint(glh4)$confint) # CI on OR scale

# 
fit3 = glm(
  churn ~ factor(internetservice) + factor(partner) + factor(monthlycharges_bin) +
    factor(paymentmethod),
  family = binomial(),
  data = telecom
)
summary(fit3)

