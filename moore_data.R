library('dplyr')
joined_df <-
  left_join(
    select(nrfa0404, id, center, nrfa2, nrfa4b, nrfa24) ,
    select(cjia1201, id, center, CJIA1),
    by = c("id", "center"))


cleaned_df <-
  joined_df %>% filter(
    !is.na(CJIA1),!is.na(id),!is.na(center),!is.na(nrfa2), !is.na(nrfa4b),!is.na(nrfa24))

cleaned_df_new <- cleaned_df %>%
  mutate(nrfa_new = if_else(nrfa24 >= 3, 1, 0))# makes new variable 1 if nrfa24 >= 2, 0 otherwise

save(cleaned_df_new, file = "MainDataProject_df.RData")

prop.table(table(cleaned_df_new$CJIA1))
prop.table(table(cleaned_df_new$nrfa24))
prop.table(table(cleaned_df_new$nrfa4b))
prop.table(table(cleaned_df_new$nrfa2))
prop.table(table(cleaned_df_new$center))

prop.table(table(cleaned_df_new$`nrfa24`, cleaned_df_new$CJIA1))
table1 <- table(cleaned_df_new$`nrfa24`, cleaned_df_new$CJIA1,
                cleaned_df_new$`center`)
table1 <- table(cleaned_df_new$`nrfa24`, cleaned_df_new$CJIA1,
                cleaned_df_new$`nrfa2`)

ftable(round(prop.table(table1), 3))

model = lm(CJIA1 ~ nrfa_new, data = cleaned_df_new)
model = lm(CJIA1 ~ nrfa_new + factor(center), data = cleaned_df_new)
model = lm(CJIA1 ~ nrfa_new + factor(center) * nrfa_new + factor(nrfa2) * nrfa_new + factor(nrfa4b) * nrfa_new, data = cleaned_df_new)
summary(model)

