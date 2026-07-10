h9_task2_data <- readRDS("QSE-Tutorial/Topic_11/data/H9_model_inputs_from_task2.rds")


tcmatr_de <- h9_task2_data$trade_cost_mat
ttmatr_de <- h9_task2_data$travel_time_min_mat


# Save both to csv

write.csv(tcmatr_de, "QSE-Tutorial/Topic_11/data/tcmatr_de.csv", row.names = FALSE)
write.csv(ttmatr_de, "QSE-Tutorial/Topic_11/data/ttmatr_de.csv", row.names = FALSE)

