library(dplyr)

names <- readxl::read_xlsx("main.xlsx", col_names = F)

main_items <- names %>%
  filter(is.na(names$X__2)) 

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
    return(assign(nam, tail(names, i)[2]))
  }
  nam <- paste0("sub_", i)
  assign(nam, names[i:(sub_num[i+1] -1),2])
}

arm_names <- colnames(subs)
colnames(subs) = c("items", "id", "date", "time-frame", "export_tones", "export_1000$", "import_tones", "import_1000$")
arise_at(c("export_tones", "export_1000$", "import_tones", "import_1000$"), sum)

# no date

comb_normal <- function(main, df, i= 1) {
  group_subs <- df %>%
    group_by(items, id) %>%
    summarise_at(c("export_tones", "export_1000$", "import_tones", "import_1000$"), sum)
  temp <- paste0("sub",i)
  temp_num <- lapply(paste0("sub_",i), get)
  temp_num <- as.data.frame(temp_num)
  temp_group <- group_subs %>%
    filter(id %in% temp_num$X__2)
  temp_group_i <- t(colSums(temp_group[c("export_tones", "export_1000$", "import_tones", "import_1000$")]))
  temp_group_i <- data.frame(temp_group_i, items = as.character(main_items[i,1]), id = NA)
  temp_group_i$items <- as.character(temp_group_i$items)
  temp_group_i$id <- as.numeric(temp_group_i$id)
  colnames(temp_group_i) <-c("export_tones", "export_1000$", "import_tones", "import_1000$", "items", "id")
  combi <- bind_rows(temp_group_i, temp_group)
  combi <- combi[c("items", "id","export_tones", "export_1000$", "import_tones", "import_1000$")]
  return(assign(temp, combi))
  
}

for (i in 1:nrow(main_items)) {
  temp <-paste0("comb",i)
  assign(temp, comb_normal(main_items, subs, i))
}
complete <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c(colnames(comb1)))

for (i in 1:nrow(main_items)) {
  temp <- lapply(paste0("comb",i), get)
  b <- temp[[1]]
  complete <- rbind(complete, b)
}

##### with date



comb_date <- function(main, df, i= 1, year) {
  group_subs <- df %>%
    group_by(items, id, date) %>%
    filter(date == year) %>%
    summarise_at(c("export_tones", "export_1000$", "import_tones", "import_1000$"), sum)
  temp <- paste0("sub",i)
  temp_num <- lapply(paste0("sub_",i), get)
  temp_num <- as.data.frame(temp_num)
  temp_group <- group_subs %>%
    filter(id %in% temp_num$X__2)
  temp_group_i <- t(colSums(temp_group[c("export_tones", "export_1000$", "import_tones", "import_1000$")]))
  temp_group_i <- data.frame(temp_group_i, items = as.character(main_items[i,1]), id = NA)
  temp_group_i$items <- as.character(temp_group_i$items)
  temp_group_i$id <- as.numeric(temp_group_i$id)
  temp_group_i$date <- year
  colnames(temp_group_i) <-c("export_tones", "export_1000$", "import_tones", "import_1000$", "items", "id", "date")
  combi <- bind_rows(temp_group_i, temp_group)
  combi <- combi[c("items", "id", "date", "export_tones", "export_1000$", "import_tones", "import_1000$")]
  return(assign(temp, combi))
  
}
for (i in 1:nrow(main_items)) {
  temp <-paste0("comb",i)
  assign(temp, comb_date(main_items, subs, i, 2017))
}
complete_date <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), c(colnames(comb1)))

for (i in 1:nrow(main_items)) {
  temp <- lapply(paste0("comb",i), get)
  b <- temp[[1]]
  complete_date <- rbind(complete_date, b)
}
z