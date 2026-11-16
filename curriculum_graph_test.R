library(CurricularAnalytics)
library(dplyr)
library(ggplot2)
# for nicer plots https://curricularanalytics.org/curricula

# for reference!! 
#https://github.com/cran/CurricularAnalytics/blob/master/vignettes/data/DS-Major-Max-2023-2024.csv

fiu_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/FIU/curriculum_test.csv")
new_fiu_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/FIU/new_curriculum.csv")

# no_gen_ed_or_elective <- all_unis %>% filter(Blocking!=0)

# compare complexity (combination of blocking and delay)

fiu_comp <- fiu_graph$node_list
new_fiu <- new_fiu_graph$node_list
plot_curriculum_graph(fiu_graph)
plot_curriculum_graph(new_fiu_graph)


t.test(fiu_comp$sc, new_fiu$sc)

df_plot <- data.frame(
  sc = c(fiu_comp$sc, new_fiu$sc),
  group = c(rep("Original CS Curriculum", length(fiu_comp$sc)),
            rep("New CS Curriculum", length(new_fiu$sc)))
)

ggplot(df_plot, aes(x = group, y = sc, fill = group)) +
  geom_violin(alpha = 0.7) + geom_boxplot(width=0.1) + 
  labs(title = "Complexity Across FIU BS CS Curricula",
       x = "", y = "Structural Complexity") + theme(legend.title = element_blank())



plot_curriculum_graph(new_fiu_graph)

plot_curriculum_graph(fiu_graph)

# for electives try to use dummy coded elective, can suffice 
# three dummy pillars, assign each fake course number 

# pre-req list code of prereq..
# three dummy elec for each foundation, system, app, then additional dummies
# listing those as prereqs
uf_graph <-curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/UF/curriculum_test_uf.csv")
plot_curriculum_graph(uf_graph)


ucf_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/UCF/curriculum_test_ucf.csv")
plot_curriculum_graph(ucf_graph)


#FAMU prereq information is inconsistent across places??
famu_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/FAMU/famu_curriculum_test.csv")
plot_curriculum_graph(famu_graph)


# curriculum is brand new :(
# 2021 -> 2024/2025 -> 25/26
fau_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/FAU/fau_curriculum_map.csv")
plot_curriculum_graph(fau_graph)

fgcu_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/FGCU/fgcu_basic_curric.csv")
plot_curriculum_graph(fgcu_graph)

fpu_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/FPU/fpu_basic_curric.csv")
plot_curriculum_graph(fpu_graph)

# surprisingly low complexity here!!
# confused about foreign language requirement
fsu_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/FSU/fsu_curriculum_test.csv")
plot_curriculum_graph(fsu_graph)

unf_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/UNF/unf_cs_curriculum.csv")
plot_curriculum_graph(unf_graph)

# for usf ask how do we handle 3 electives from 1 group and 1 elective from another, then electives whereever
# do we use the dummy prereq model?? --> if not we can fix later

usf_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/USF/usf_test_curriculum.csv")
plot_curriculum_graph(usf_graph)


uwf_graph <- curriculum_graph_from_csv("C:/Users/cpanc/OneDrive/Documents/IDC6940/UWF/uwf_curriculum_test.csv")
plot_curriculum_graph(uwf_graph)


# examine pathway through major 
# --> completion rates, (drop_rate = retention in (major/uni)/drop-course) 
# outflow for grad-rates for ftic



# Define helper function for printing courses
print_top_two_rows <- function(df, column) {
  ordered_df <- df[rev(order(df[[column]])), ]
  #top_two <- head(ordered_df, 3)
  print(ordered_df)
}

# Print top two courses ordered by each metric
columns <- colnames(fiu_graph$node_list[,c("bf","df","cf","sc")])
for (column in columns) {
  print(paste("Ordering by column:", column))
  print_top_two_rows(fiu_graph$node_list, column)
}

fpu_graph$node_list




