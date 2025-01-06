# load data saved in the end of script 8.1


chains_K1_dfy <- chains_K1_df %>% 
  mutate(null_decision = if_else(ego == alter, T,F),
         one = 1) %>% 
  group_by(imp, run, period, ego, alter) %>% 
  mutate(same_tie = cumsum(one)) %>% 
  ungroup() %>% 
  select(-one) %>% 
  mutate(revoked_prev = if_else(ego == alter, NA, if_else(same_tie > 1, TRUE, FALSE))) %>% 
  group_by(imp,period) %>% 
  summarise(perc_null_dec = mean(null_decision),
            perc_revoked = mean(revoked_prev, na.rm = T))
 
chains_K1_dfy <- chains_K1_dfy %>% arrange(period)


# load data saved in the end of script 8.2


chains_K2_dfy <- chains_K2_df %>% 
  mutate(null_decision = if_else(ego == alter, T,F),
         one = 1) %>% 
  group_by(imp, run, period, ego, alter) %>% 
  mutate(same_tie = cumsum(one)) %>% 
  ungroup() %>% 
  select(-one) %>% 
  mutate(revoked_prev = if_else(ego == alter, NA, if_else(same_tie > 1, TRUE, FALSE))) %>% 
  group_by(imp,period) %>% 
  summarise(perc_null_dec = mean(null_decision),
            perc_revoked = mean(revoked_prev, na.rm = T)) %>% 
  arrange(period)