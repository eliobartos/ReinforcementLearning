#setwd("C:/Users/Elio/Documents/R/Reinforcement Learning - David Silver/ReinforcementLearning")
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(extrafont)


loadfonts(device = "win")

load("SimpleBlackjackResults.RData")

print(res_100k)


res = res_1m

## Process Policy
policy = res$policy
policy = data.frame(policy)
policy$state = rownames(policy)
policy = gather(policy, action, take_action, hit, stand)

Q = res$Q
Q = data.frame(Q)
Q$state = rownames(Q)
Q = Q %>% gather(action, pct_win, hit, stand)

N = res$N
N = data.frame(N)
N$state = rownames(N)
N = N %>% gather(action, times_visited, hit, stand)

results = policy %>% merge(Q) %>% merge(N)
results = results %>% 
  filter(times_visited > 0) %>% 
         #filter(!grepl("21", state)) %>% 
  mutate(take_action = as.factor(take_action),
         is_soft = grepl("soft", state),
         order = as.numeric(gsub("([0-9]+).*", "\\1", c(state))))

#### Two plot solution
hard = results %>% 
  filter(!is_soft)

p1 = ggplot(hard, aes(x = action, 
                 y = reorder(state, order, mean))) + 
  geom_tile(aes(color = take_action, fill = take_action)) +
  geom_text(aes(label = paste0(round(pct_win*100,1), "%")),
            color = "gray10", fontface = "bold") +
  ylab("") +
  xlab("") +
  ggtitle("Hard") +
  scale_color_manual(values = c("grey35", "grey35")) +
  scale_fill_manual(values = c("hotpink1", "skyblue1")) +
  guides(fill= FALSE,
         color = FALSE) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(face = "bold", family = "Courier New"))
p1

soft = results %>% 
  filter(is_soft)

p2 = ggplot(soft, aes(x = action, 
                 y = reorder(state, order, mean))) + 
  geom_tile(aes(color = take_action, fill = take_action)) +
  geom_text(aes(label = paste0(round(pct_win*100,1), "%")),
            color = "gray10", fontface = "bold") +
  ylab("") +
  xlab("") +
  ggtitle("Soft") +
  scale_color_manual(labels = c("Don't", "Take"), values = c("grey35", "grey35")) +
  scale_fill_manual(labels = c("Don't", "Take"), values = c("hotpink1", "skyblue1")) +
  guides(fill=guide_legend(title="Take action?"),
         color = guide_legend(title="Take action?")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 20),
        text = element_text(face = "bold", family = "Courier New"))
p2
grid.arrange(p1, p2, nrow = 1, widths = c(0.80, 1))

