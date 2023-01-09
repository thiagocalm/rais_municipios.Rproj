options(scipen = 9999999)


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(openxlsx)
library(basedosdados)

# Definindo projeto para acesso aos dados ---------------------------------

set_billing_id("consulta-oit")


# Importacao de dados - Auxilio Emergencial ----------------------------------------

# Definicao de parametros de busca
query_ae <-
  c("SELECT mes, sigla_uf, id_municipio, enquadramento, valor_beneficio
    FROM `basedosdados.br_mc_auxilio_emergencial.microdados`
    WHERE sigla_uf = 'MG'")

# importacao dos parametros
df_ae <- read_sql(query_ae)

# openxlsx::write.xlsx(df_ae, "./Bruto/auxilio_emergencial_MG.xlsx")


# Manipulando dados -------------------------------------------------------
# Ibiraci: id_municipio == 3129707
# Campos Altos: id_municipio == 3111507
# Patrocínio: id_municipio == 3148103

df_ae_munic <- df_ae |> 
  filter(id_municipio == 3129707 |
           id_municipio == 3111507 |
           id_municipio == 3148103)

df_ae_munic <- df_ae_munic |> 
  mutate(nome_municipio = case_when(
    id_municipio == 3129707 ~ "Ibiraci",
    id_municipio == 3111507 ~ "Campos Altos",
    TRUE ~ "Patrocínio"),
    beneficiarios = 1)

# Salvando dados dos municípios
openxlsx::write.xlsx(df_ae_munic, "./Bruto/auxilio_emergencial_munic.xlsx")

# Gerando tabela

df_ae_munic_valor <- df_ae_munic |> 
  group_by(nome_municipio, mes, enquadramento) |> 
  summarise(valor = sum(valor_beneficio),
            beneficiarios = sum(beneficiarios))

# Salvando dados dos municípios
openxlsx::write.xlsx(df_ae_munic_valor, "./Auxilio Emergencial Municipios.xlsx")
