library(dplyr)

names <- readxl::read_xlsx("main.xlsx", col_names = F)

main_items <- names %>%
  filter(is.na(names$X__1)) 

main_items$main_id <- 0

for (i in 1:nrow(main_items)) {
  main_items$main_id[i] <- 1
}

names$main <- ifelse(names$X__1 %in% main_items$X__1, 1, 0)


subs <-readxl::read_xlsx("subcat.xlsx" )

subs <- data.frame(subs)
subs[,-1] <- as.numeric(unlist(subs[,-1]))


sapply(subs, class)

  # for(i in 1:length(main_items)) {
  #   if(is.na(names$X__2[i])) {
  #     num1 <- i
  #     print("first number: ", i)
  #   }
  #   
  #   if(is.na(names$X__2[i]) & i != num1) {
  #     num2 <- 1
  #   }
  # }

sub_num <- which(is.na(names$X__2), arr.ind=TRUE)

for(i in 1:length(sub_num)){
  if(i == length(sub_num)) {
    nam <- paste0("sub_", i)
    assign(nam, tail(names, i)[1])
  }
  nam <- paste0("sub_", i)
  assign(nam, names[i:sub_num[i+1] -1,1])
}

arm_names <- colnames(subs)
colnames(subs) = c("items", "id", "date", "time-frame", "export_tones", "export_1000$", "import_tones", "import_1000$")

group_subs <- subs %>%
  group_by(items, id) %>%
  summarise_at(c("export_tones", "export_1000$", "import_tones", "import_1000$"), sum)
  
group_subs_date <- subs %>%
  group_by(items, id, date) %>%
  summarise_at(c("export_tones", "export_1000$", "import_tones", "import_1000$"), sum)

sub1 <- group_subs %>%
  filter(items %in% sub_1$X__1)

sub1_comb <- colSums(sub1[c("export_tones", "export_1000$", "import_tones", "import_1000$")])
sub1_comb$items <- main_items[sub_num[1],1]
sub1_comb$id <- NA
sub1_comb <- as.data.frame(sub1_comb)

