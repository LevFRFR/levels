portfolio <-
  NVTA %>% bind_cols(QCOM, SQ, WMT) %>% 
  select(date = date...1, contains(".Adjusted")) %>% 
  mutate(day = day(date))
