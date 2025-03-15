library(tidyverse)

# Функция McCrae's rpa (на вход по умолчанию T-баллы: mu = 50, sd = 10)
rpa <- function(p1, p2, mu = 50, sd = 10) {
  if (length(p1) != length(p2))
    stop("'p1' and 'p2' must have the same length")
  k <- length(p1)
  p1 <- (p1 - mu) / sd # переводим в Z-оценки
  p2 <- (p2 - mu) / sd # переводим в Z-оценки
  sumM.sq <- sum(((p1 + p2) / 2)^2)
  sumd.sq <- sum((p1 - p2)^2)
  ipa <- (k + 2 * sumM.sq - sumd.sq) / sqrt(10 * k)
  rpa <- ipa / sqrt(k - 2 + ipa^2)
  return(rpa)
}

p1 <- c(50, 80, 75, 65, 55)
p2 <- c(40, 60, 65, 35, 25)

rpa(p1, p2)
cor(p1, p2)


# Загружаем данные
LLM_data_profile <- read_csv("LLM_data_profile.csv")

# Рассчитываем средний профиль MMPI
# усредненные значения по каждой шкале для всех LLM
LLM_data_profile <- LLM_data_profile %>%
  mutate(AverageLLM = round(rowMeans(select(., -c(
    Шкала, TEZAL # исключая TEZAL
  )), na.rm = TRUE), 1)) %>%
  select(Шкала, DeepSeek_simple_prompt:`GROK-3`, AverageLLM, TEZAL)
glimpse(LLM_data_profile)

#Исходные данные
knitr::kable(LLM_data_profile)

# Вычисляем rpa каждой модели с TEZAL
models <- colnames(LLM_data_profile)[-c(1, ncol(LLM_data_profile))]
rpa_results <- map_dbl(models, ~ rpa(LLM_data_profile[[.x]], LLM_data_profile$TEZAL)) %>% round(.,2)

# Вычисляем корреляцию Пирсона каждой модели с TEZAL
cor_results <- map_dbl(models,
                       ~ cor(LLM_data_profile[[.x]], LLM_data_profile$TEZAL, method = "pearson")) %>% round(.,2)

# Вычисляем MAE для каждой модели относительно TEZAL
mae_results <- map_dbl(models, ~ mean(abs(LLM_data_profile[[.x]] - LLM_data_profile$TEZAL)))

# Итоговая таблица с rpa, cor и MAE
results_table <- tibble(Model = models, rpa = rpa_results, cor = cor_results, mae = mae_results) %>% arrange(desc(rpa))

# Вычисляем средние значения для rpa, cor, mae без учета AverageLLM
avg_rpa <- mean(rpa_results[models != "AverageLLM"]) %>% round(., 3)
avg_cor <- mean(cor_results[models != "AverageLLM"]) %>% round(., 3)
avg_mae <- mean(mae_results[models != "AverageLLM"]) %>% round(., 2)

# Добавляем строку со средними значениями в итоговую таблицу
results_table <- results_table %>%
  add_row(Model = "Среднее (без AverageLLM):", rpa = avg_rpa, cor = avg_cor, mae = avg_mae)

knitr::kable(results_table, digits = 2)

# Вычисляем MAE и SMAE для каждой шкалы MMPI (разница между моделью и TEZAL)
mae_by_scale <- LLM_data_profile %>%
  select(-AverageLLM) %>%  
  mutate(across(-Шкала, ~ abs(. - TEZAL))) %>%  
  select(-TEZAL) %>%
  pivot_longer(-Шкала, names_to = "Model", values_to = "MAE") %>%
  group_by(Шкала) %>%
  summarise(MAE = mean(MAE) %>% round(1)) %>%
  arrange(desc(MAE)) %>%
  mutate(sMAE = round(MAE / 10, 1))  # Делим MAE на 10 (т.к. SD = 10)

# Выводим таблицу с MAE и SMAE по шкалам
knitr::kable(mae_by_scale)



# Преобразуем данные в длинный формат
LLM_data_long <- LLM_data_profile %>%
  pivot_longer(cols = -Шкала, names_to = "Model", values_to = "Score")

highlight_models <- c("YandexGPT 5 Pro", "TEZAL", "AverageLLM")

LLM_data_long <- LLM_data_long %>%
  mutate(ColorGroup = case_when(
    Model == "AverageLLM" ~ "AverageLLM",
    Model %in% c("YandexGPT 5 Pro", "TEZAL") ~ Model,
    TRUE ~ "Other"
  )) %>% 
  mutate(ColorGroup = factor(ColorGroup,
                             levels = c(
                               "TEZAL", 
                               "YandexGPT 5 Pro",
                               "AverageLLM",
                               "Other"
                             )))
LLM_data_long$Шкала <- factor(LLM_data_long$Шкала, 
                              levels = c("Шкала ипохондрии (HS)", 
                                         "Шкала депрессии (D)", 
                                         "Шкала истерии (Hy)", 
                                         "Шкала психопатии (Pd)", 
                                         "Шкала маскулинности-феминности (Mf)", 
                                         "Шкала паранойи (Pa)", 
                                         "Шкала психастении (Pt)", 
                                         "Шкала шизофрении (Sc)", 
                                         "Шкала гипомании (Ma)", 
                                         "Шкала социальной интроверсии (Si)"))

ggplot() +
  geom_hline(yintercept = c(20, 30, 40, 50, 60, 70, 80), 
             linetype = "solid", 
             color = "grey80", 
             linewidth = 0.15, 
             alpha = 0.5) +  
  geom_hline(yintercept = 50, 
             linetype = "solid", 
             color = "grey40", 
             linewidth = 0.3) + 
  geom_line(data = LLM_data_long %>% filter(ColorGroup == "Other"),
            aes(x = Шкала, y = Score, group = Model, color = ColorGroup),
            linewidth = 0.5) +
  geom_point(data = LLM_data_long %>% filter(ColorGroup == "Other"),
             aes(x = Шкала, y = Score, color = ColorGroup),
             size = 2.5) +
  geom_line(data = LLM_data_long %>% filter(ColorGroup %in% c("YandexGPT 5 Pro", "TEZAL", "AverageLLM")), 
            aes(x = Шкала, y = Score, group = Model, color = ColorGroup), 
            linewidth = 1) +
  geom_point(data = LLM_data_long %>% filter(ColorGroup %in% c("YandexGPT 5 Pro", "TEZAL", "AverageLLM")), 
             aes(x = Шкала, y = Score, color = ColorGroup), 
             size = 2.5) +
  labs(title = "Профили MMPI для Мэрилин Монро на основе запроса к\nразным большим языковым моделям и системе TEZAL",
       subtitle = glue::glue("Наилучшее сходство профиля TEZAL и YandexGPT 5 Pro:\nrpa = {results_table$rpa[results_table$Model == 'YandexGPT 5 Pro']} | r = {results_table$cor[results_table$Model == 'YandexGPT 5 Pro']}\nВторое место — усредненный результат по всем БЯМ (AverageLLM):\nrpa = {results_table$rpa[results_table$Model == 'AverageLLM']} | r = {results_table$cor[results_table$Model == 'AverageLLM']}"),
       caption = "\nЮрий Тукачев, март 2025 @people_analytics",
       x = "Шкалы MMPI",
       y = "T-балл",
       color = "") +
  scale_x_discrete(labels = c("HS", "D", "Hy", "Pd", "Mf", "Pa", "Pt", "Sc", "Ma", "Si")) +
  scale_y_continuous(limits = c(20, 80), breaks = seq(20, 80, by = 10)) + 
  scale_colour_manual("",
    values = c("TEZAL" = "#FF8C00", "YandexGPT 5 Pro" = "red", "AverageLLM" = "gray30", "Other" = "gray90"),
    labels = c("TEZAL", "YandexGPT 5 Pro", "AverageLLM", "Другие LLM"),
    breaks = c("TEZAL", "YandexGPT 5 Pro", "AverageLLM", "Other")) +
  theme(plot.background = element_blank(), 
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.background = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        plot.caption = element_text(hjust = 0, size = 10, color = "gray70"),
        axis.ticks = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),  
        legend.position = "top",
        plot.margin = margin(20, 20, 20, 20)) 

ggsave("mmpi_llm_profile.png", bg = "white", width = 6, height = 6, dpi = 300)
