
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(basedosdados)
library(fedmatch)

dicionario_rais <- readxl::read_xlsx("./dados/Tratado/dicionario_rais.xlsx")

# Definindo projeto para acesso aos dados ---------------------------------

set_billing_id("consulta-oit")

# Importing datas  --------------------------------------

# Definicao de parametros de busca
query_rais <-
  c("SELECT ano, sigla_uf, id_municipio, vinculo_ativo_3112, cnae_2, faixa_etaria, sexo,
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = 'MG' and ano = 2020")

# importacao dos parametros
rais <- read_sql(query_rais)

openxlsx::write.xlsx(rais, "./dados/Bruto/rais_mg_munic_2020.xlsx")

# importação dos dados referentes aos municipios produtores de cafe

query_munic <-
  c("SELECT sigla_uf, id_municipio, nome
  FROM `basedosdados.br_bd_diretorios_brasil.municipio`
    WHERE sigla_uf = 'MG'")

# importacao dos parametros
munic_bd <- read_sql(query_munic)
openxlsx::write.xlsx(munic_bd, "./dados/Bruto/munic_bd.xlsx")

# importação
munic_proj <- readxl::read_xlsx("./dados/Nomes_municipios.xlsx")

# Manipulação -------------------------------------------------------------

# Base de municipios da BD
munic_bd_clean <- munic_bd |>
  mutate(nome = stringi::stri_trans_general(nome, "Latin-ASCII")) |>
  mutate(nome = tolower(nome))

# Base de municipios da FJP
munic_proj_clean <- munic_proj |>
  mutate(nome = tolower(Municipio)) |>
  mutate(nome = stringi::stri_trans_general(nome, "Latin-ASCII"))

munic <- munic_proj_clean |>
  select(nome) |>
  left_join(munic_bd_clean |> select(-sigla_uf),by = c("nome"))

# Filtrar RAIS


# RAIS MG - cultivo do café

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
  select(ano = ano...1, faixa_etaria,
         Total_cultivo_cafe = pop_ocupada_tt,
         feminino_cultivo_cafe = pop_ocupada_fem,
         masculino_cultivo_cafe = pop_ocupada_masc)

# RAIS MG - todas as atividades

rais_tt <- rais |>
  filter(vinculo_ativo_3112 == 1) |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_tt = sum(as.numeric(vinculo_ativo_3112)))

rais_feminino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 2) |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_fem = sum(as.numeric(vinculo_ativo_3112)))

rais_masculino <- rais |>
  filter(vinculo_ativo_3112 == 1 & sexo == 1) |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_masc = sum(as.numeric(vinculo_ativo_3112)))

rais_pop_ocupada_tt <- rais_tt |>
  bind_cols(rais_feminino |> select(pop_ocupada_fem)) |>
  bind_cols(rais_masculino |> select(pop_ocupada_masc)) |>
  select(ano = ano...1, faixa_etaria,
         Total_tt = pop_ocupada_tt,
         feminino_tt = pop_ocupada_fem,
         masculino_tt = pop_ocupada_masc)

openxlsx::write.xlsx(rais_pop_ocupada_tt, "./dados/Tratado/pop_ocupada_tt_mg_2020.xlsx")
openxlsx::write.xlsx(rais_pop_ocupada, "./dados/Tratado/pop_ocupada_cafe_mg_2020.xlsx")



# Municipios selecionados de MG -------------------------------------------

rais_munic <- subset(rais, id_municipio %in% munic_unique)

# RAIS MG - cultivo do café

rais_cultivo_cafe_tt_munic <- rais_munic |>
  filter(vinculo_ativo_3112 == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(faixa_etaria, vinculo_ativo_3112) |>
  group_by(faixa_etaria) |>
  summarise(pop_ocupada_tt = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_feminino_munic <- rais_munic |>
  filter(vinculo_ativo_3112 == 1 & sexo == 2) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(faixa_etaria, vinculo_ativo_3112) |>
  group_by(faixa_etaria) |>
  summarise(pop_ocupada_fem = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_masculino_munic <- rais_munic |>
  filter(vinculo_ativo_3112 == 1 & sexo == 1) |>
  filter(cnae_2 == "01342") |>
  filter(faixa_etaria != 99) |>
  select(faixa_etaria, vinculo_ativo_3112) |>
  group_by(faixa_etaria) |>
  summarise(pop_ocupada_masc = sum(as.numeric(vinculo_ativo_3112)))

rais_pop_ocupada_munic <- rais_cultivo_cafe_tt_munic |>
  bind_cols(rais_cultivo_cafe_feminino_munic |> select(pop_ocupada_fem)) |>
  bind_cols(rais_cultivo_cafe_masculino_munic |> select(pop_ocupada_masc)) |>
  select(Total_cultivo_cafe = pop_ocupada_tt,
         feminino_cultivo_cafe = pop_ocupada_fem,
         masculino_cultivo_cafe = pop_ocupada_masc)

# RAIS MG - todas as atividades

rais_tt_munic <- rais_munic |>
  filter(vinculo_ativo_3112 == 1) |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_tt = sum(as.numeric(vinculo_ativo_3112)))

rais_feminino_munic <- rais_munic |>
  filter(vinculo_ativo_3112 == 1 & sexo == 2) |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_fem = sum(as.numeric(vinculo_ativo_3112)))

rais_masculino_munic <- rais_munic |>
  filter(vinculo_ativo_3112 == 1 & sexo == 1) |>
  filter(faixa_etaria != 99) |>
  select(ano, faixa_etaria, vinculo_ativo_3112) |>
  group_by(ano, faixa_etaria) |>
  summarise(pop_ocupada_masc = sum(as.numeric(vinculo_ativo_3112)))

rais_pop_ocupada_tt_munic <- rais_tt_munic |>
  bind_cols(rais_feminino_munic |> select(pop_ocupada_fem)) |>
  bind_cols(rais_masculino_munic |> select(pop_ocupada_masc)) |>
  select(ano = ano...1, faixa_etaria,
         Total_tt = pop_ocupada_tt,
         feminino_tt = pop_ocupada_fem,
         masculino_tt = pop_ocupada_masc)

openxlsx::write.xlsx(rais_pop_ocupada_tt_munic, "./dados/Tratado/pop_ocupada_tt_munic_2020.xlsx")
openxlsx::write.xlsx(rais_pop_ocupada_munic, "./dados/Tratado/pop_ocupada_cafe_mg_munic_2020.xlsx")
