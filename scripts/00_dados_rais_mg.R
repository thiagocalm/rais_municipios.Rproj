

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(basedosdados)

#dicionario_rais <- readxl::read_xlsx("./dados/Tratado/dicionario_rais.xlsx")

# Definindo projeto para acesso aos dados ---------------------------------

set_billing_id("consulta-oit")

# 2014  --------------------------------------

# Definicao de parametros de busca
query_rais <-
  c("SELECT ano, sigla_uf, vinculo_ativo_3112, cnae_2, faixa_etaria, sexo,
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = 'MG' and ano = 2014")

# importacao dos parametros
rais <- read_sql(query_rais)

openxlsx::write.xlsx(rais, "./dados/Bruto/rais_mg_2014.xlsx")

# Emprego formal ambos os sexos

rais_cultivo_cafe_tt <- rais |>
  filter(vinculo_ativo_3112 == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_tt = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_feminino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 2) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_fem = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_masculino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_masc = sum(as.numeric(vinculo_ativo_3112)))

rais_pop_ocupada <- rais_cultivo_cafe_tt |>
  bind_cols(rais_cultivo_cafe_feminino |> select(pop_ocupada_fem)) |>
  bind_cols(rais_cultivo_cafe_masculino |> select(pop_ocupada_masc)) |>
  select(ano = ano...1, pop_ocupada_tt, pop_ocupada_fem, pop_ocupada_masc)

openxlsx::write.xlsx(rais_pop_ocupada, "./dados/Tratado/pop_ocupada_mg_2014.xlsx")

# 2015  --------------------------------------

# Definicao de parametros de busca
query_rais <-
  c("SELECT ano, sigla_uf, vinculo_ativo_3112, cnae_2, faixa_etaria, sexo,
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = 'MG' and ano = 2015")

# importacao dos parametros
rais <- read_sql(query_rais)

openxlsx::write.xlsx(rais, "./dados/Bruto/rais_mg_2015.xlsx")

# Emprego formal ambos os sexos

rais_cultivo_cafe_tt <- rais |>
  filter(vinculo_ativo_3112 == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_tt = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_feminino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 2) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_fem = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_masculino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_masc = sum(as.numeric(vinculo_ativo_3112)))

rais_pop_ocupada <- rais_cultivo_cafe_tt |>
  bind_cols(rais_cultivo_cafe_feminino |> select(pop_ocupada_fem)) |>
  bind_cols(rais_cultivo_cafe_masculino |> select(pop_ocupada_masc)) |>
  select(ano = ano...1, pop_ocupada_tt, pop_ocupada_fem, pop_ocupada_masc)

openxlsx::write.xlsx(rais_pop_ocupada, "./dados/Tratado/pop_ocupada_mg_2015.xlsx")

# 2016  --------------------------------------

# Definicao de parametros de busca
query_rais <-
  c("SELECT ano, sigla_uf, vinculo_ativo_3112, cnae_2, faixa_etaria, sexo,
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = 'MG' and ano = 2016")

# importacao dos parametros
rais <- read_sql(query_rais)

openxlsx::write.xlsx(rais, "./dados/Bruto/rais_mg_2016.xlsx")

# Emprego formal ambos os sexos

rais_cultivo_cafe_tt <- rais |>
  filter(vinculo_ativo_3112 == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_tt = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_feminino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 2) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_fem = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_masculino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_masc = sum(as.numeric(vinculo_ativo_3112)))

rais_pop_ocupada <- rais_cultivo_cafe_tt |>
  bind_cols(rais_cultivo_cafe_feminino |> select(pop_ocupada_fem)) |>
  bind_cols(rais_cultivo_cafe_masculino |> select(pop_ocupada_masc)) |>
  select(ano = ano...1, pop_ocupada_tt, pop_ocupada_fem, pop_ocupada_masc)

openxlsx::write.xlsx(rais_pop_ocupada, "./dados/Tratado/pop_ocupada_mg_2016.xlsx")

# 2017  --------------------------------------

# Definicao de parametros de busca
query_rais <-
  c("SELECT ano, sigla_uf, vinculo_ativo_3112, cnae_2, faixa_etaria, sexo,
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = 'MG' and ano = 2017")

# importacao dos parametros
rais <- read_sql(query_rais)

openxlsx::write.xlsx(rais, "./dados/Bruto/rais_mg_2017.xlsx")

# Emprego formal ambos os sexos

rais_cultivo_cafe_tt <- rais |>
  filter(vinculo_ativo_3112 == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_tt = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_feminino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 2) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_fem = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_masculino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_masc = sum(as.numeric(vinculo_ativo_3112)))

rais_pop_ocupada <- rais_cultivo_cafe_tt |>
  bind_cols(rais_cultivo_cafe_feminino |> select(pop_ocupada_fem)) |>
  bind_cols(rais_cultivo_cafe_masculino |> select(pop_ocupada_masc)) |>
  select(ano = ano...1, pop_ocupada_tt, pop_ocupada_fem, pop_ocupada_masc)

openxlsx::write.xlsx(rais_pop_ocupada, "./dados/Tratado/pop_ocupada_mg_2017.xlsx")

# 2018  --------------------------------------

# Definicao de parametros de busca
query_rais <-
  c("SELECT ano, sigla_uf, vinculo_ativo_3112, cnae_2, faixa_etaria, sexo,
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = 'MG' and ano = 2018")

# importacao dos parametros
rais <- read_sql(query_rais)

#openxlsx::write.xlsx(rais, "./dados/Bruto/rais_mg_2018.xlsx")

# Emprego formal ambos os sexos

rais_cultivo_cafe_tt <- rais |>
  filter(vinculo_ativo_3112 == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  filter(faixa_etaria != 1) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_tt = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_feminino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 2) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  filter(faixa_etaria != 1) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_fem = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_masculino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  filter(faixa_etaria != 1) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_masc = sum(as.numeric(vinculo_ativo_3112)))

rais_pop_ocupada <- rais_cultivo_cafe_tt |>
  bind_cols(rais_cultivo_cafe_feminino |> select(pop_ocupada_fem)) |>
  bind_cols(rais_cultivo_cafe_masculino |> select(pop_ocupada_masc)) |>
  select(ano = ano...1, faixa_etaria, pop_ocupada_tt, pop_ocupada_fem, pop_ocupada_masc)

#openxlsx::write.xlsx(rais_pop_ocupada, "./dados/Tratado/pop_ocupada_mg_2018.xlsx")

# 2019  --------------------------------------

# Definicao de parametros de busca
query_rais <-
  c("SELECT ano, sigla_uf, vinculo_ativo_3112, cnae_2, faixa_etaria, sexo,
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = 'MG' and ano = 2019")

# importacao dos parametros
rais <- read_sql(query_rais)

#openxlsx::write.xlsx(rais, "./dados/Bruto/rais_mg_2019.xlsx")

# Emprego formal ambos os sexos

rais_cultivo_cafe_tt <- rais |>
  filter(vinculo_ativo_3112 == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_tt = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_feminino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 2) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_fem = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_masculino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_masc = sum(as.numeric(vinculo_ativo_3112)))

rais_pop_ocupada <- rais_cultivo_cafe_tt |>
  bind_cols(rais_cultivo_cafe_feminino |> select(pop_ocupada_fem)) |>
  bind_cols(rais_cultivo_cafe_masculino |> select(pop_ocupada_masc)) |>
  select(ano = ano...1, faixa_etaria, pop_ocupada_tt, pop_ocupada_fem, pop_ocupada_masc)

#openxlsx::write.xlsx(rais_pop_ocupada, "./dados/Tratado/pop_ocupada_mg_2019.xlsx")

# 2020  --------------------------------------

# Definicao de parametros de busca
query_rais <-
  c("SELECT ano, sigla_uf, vinculo_ativo_3112, cnae_2, faixa_etaria, sexo,
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = 'MG' and ano = 2020")

# importacao dos parametros
rais <- read_sql(query_rais)

openxlsx::write.xlsx(rais, "./dados/Bruto/rais_mg_2020.xlsx")

# Emprego formal ambos os sexos

rais_cultivo_cafe_tt <- rais |>
  filter(vinculo_ativo_3112 == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_tt = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_feminino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 2) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_fem = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_masculino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_masc = sum(as.numeric(vinculo_ativo_3112)))

rais_pop_ocupada <- rais_cultivo_cafe_tt |>
  bind_cols(rais_cultivo_cafe_feminino |> select(pop_ocupada_fem)) |>
  bind_cols(rais_cultivo_cafe_masculino |> select(pop_ocupada_masc)) |>
  select(ano = ano...1, faixa_etaria,  pop_ocupada_tt, pop_ocupada_fem, pop_ocupada_masc)

#openxlsx::write.xlsx(rais_pop_ocupada, "./dados/Tratado/pop_ocupada_mg_2020.xlsx")

# Pop. ocupada - MG -------------------------------------------------------

# Importacao dos dados tratados
# OBS: Linhas abaixo não são necessárias de serem rodadas, caso utilizado os dados
# diretamente do datalake

# pop_ocupada_2014 <- readxl::read_xlsx("./dados/Tratado/pop_ocupada_mg_2014.xlsx")
# pop_ocupada_2015 <- readxl::read_xlsx("./dados/Tratado/pop_ocupada_mg_2015.xlsx")
# pop_ocupada_2016 <- readxl::read_xlsx("./dados/Tratado/pop_ocupada_mg_2016.xlsx")
# pop_ocupada_2017 <- readxl::read_xlsx("./dados/Tratado/pop_ocupada_mg_2017.xlsx")
#
# pop_ocupada_2018 <- readxl::read_xlsx("./dados/Tratado/pop_ocupada_mg_2018.xlsx")
# pop_ocupada_2018 <- pop_ocupada_2018 |> filter(faixa_etaria != "1")
#
# pop_ocupada_2019 <- readxl::read_xlsx("./dados/Tratado/pop_ocupada_mg_2019.xlsx")
# pop_ocupada_2020 <- readxl::read_xlsx("./dados/Tratado/pop_ocupada_mg_2020.xlsx")

# Junção dos dados

pop_ocupada_mg <- pop_ocupada_2014 |>
  bind_rows(pop_ocupada_2015) |>
  bind_rows(pop_ocupada_2016) |>
  bind_rows(pop_ocupada_2017)

pop_ocupada_mg <- pop_ocupada_mg |>
  mutate(faixa_etaria = rep(seq(2,8,1),4)) |>
  select(ano, faixa_etaria, pop_ocupada_tt, pop_ocupada_fem, pop_ocupada_masc)

pop_ocupada_mg <- pop_ocupada_mg |>
  bind_rows(pop_ocupada_2018 |> mutate(faixa_etaria = as.double(faixa_etaria))) |>
  bind_rows(pop_ocupada_2019 |> mutate(faixa_etaria = as.double(faixa_etaria))) |>
  bind_rows(pop_ocupada_2020 |> mutate(faixa_etaria = as.double(faixa_etaria)))

# Tratamento de variaveis

pop_ocupada_mg <- pop_ocupada_mg |>
  mutate(idade = case_when(
    faixa_etaria == 2 ~ "15 a 17 anos",
    faixa_etaria == 3 ~ "18 a 24 anos",
    faixa_etaria == 4 ~ "25 a 29 anos",
    faixa_etaria == 5 ~ "30 a 39 anos",
    faixa_etaria == 6 ~ "40 a 49 anos",
    faixa_etaria == 7 ~ "50 a 64 anos",
    faixa_etaria == 8 ~ "65 mais")) |>
  select(- faixa_etaria) |>
  select(ano, idade, everything())

# Salvando dados
# openxlsx::write.xlsx(pop_ocupada_mg, "./dados/Tratado/pop_ocupada_mg.xlsx")

# Grafico por grupo etario ------------------------------------------------

pop_ocupada_mg |>
  pivot_longer(pop_ocupada_tt:pop_ocupada_masc, names_to = "sexo", values_to = "pop_ocupada") |>
  mutate(sexo = case_when(
    sexo == "pop_ocupada_tt" ~ "Total",
    sexo == "pop_ocupada_fem" ~ "Feminino",
    TRUE ~ "Masculino")) |>
  mutate(sexo = as.factor(sexo),
         idade = as.factor(idade),
         ano = as.factor(ano)) |>
  ggplot() +
  aes(x = idade, y = pop_ocupada, group = ano, fill = ano) +
  geom_col(stat = "summary",
           position="dodge") +
  lemon::facet_rep_grid(sexo ~ ., repeat.tick.labels = TRUE) +
  theme_light() +
  labs(
    title = "População ocupada formalmente em todas as atividades econômicas - Minas Gerais, 2014-2020",
    subtitle = "Por idade e sexo",
    x = "Idade, segundo divisão da RAIS",
    y = "Pop. ocupada (absoluto)",
    caption = "Ministério da Economia, RAIS (2014-2020). Elaborado por @thiagocalm.") +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "bold", size = 14),
    plot.caption = element_text(face = "italic", size = 8),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 10, colour = "white")
  ) +
  scale_fill_brewer(palette = "BuPu")



