library(ggplot2)
library(tidyverse)
load("SimpleBlackjackResults_100k.RData")

print(res_100k)


## Process Policy
policy = res_100k$policy
policy = data.frame(policy)
policy$state = rownames(policy)
policy = gather(policy, action, take_action, hit, stand)

Q = res_100k$Q
Q = data.frame(Q)
Q$state = rownames(Q)
Q = Q %>% gather(action, pct_win, hit, stand)

N = res_100k$N
N = data.frame(N)
N$state = rownames(N)
N = N %>% gather(action, times_visited, hit, stand)

results = policy %>% merge(Q) %>% merge(N)
results = results %>% 
  filter(times_visited > 0,
         !grepl("21", state)) %>% 
  mutate(take_action = as.factor(take_action),
         is_soft = grepl("soft", state),
         order = as.numeric(gsub("([0-9]+).*", "\\1", c(state))))


hard = results %>% 
  filter(!is_soft)

p1 = ggplot(hard, aes(x = action, 
                 y = reorder(state, order, mean))) + 
  geom_tile(aes(color = take_action, fill = take_action)) +
  geom_text(aes(label = paste0(round(pct_win*100,1), "%"))) +
  ylab("") +
  xlab("") +
  ggtitle("Hard") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(labels = c("Don't", "Take"), values = c("red", "skyblue")) +
  scale_fill_manual(labels = c("Don't", "Take"), values = c("red", "skyblue")) +
  guides(fill=guide_legend(title="Take action?"),
         color = guide_legend(title="Take action?"))


soft = results %>% 
  filter(is_soft)

p2 = ggplot(soft, aes(x = action, 
                 y = reorder(state, order, mean))) + 
  geom_tile(aes(color = take_action, fill = take_action)) +
  geom_text(aes(label = paste0(round(pct_win*100,1), "%"))) +
  ylab("") +
  xlab("") +
  ggtitle("Soft") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(labels = c("Don't", "Take"), values = c("red", "skyblue")) +
  scale_fill_manual(labels = c("Don't", "Take"), values = c("red", "skyblue")) +
  guides(fill=guide_legend(title="Take action?"),
         color = guide_legend(title="Take action?"))

library(gridExtra)
grid.arrange(p1, p2, nrow = 1)
