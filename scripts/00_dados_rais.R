

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(basedosdados)


# Definindo projeto para acesso aos dados ---------------------------------

set_billing_id("consulta-oit")


# Importacao de dados - RAIS ----------------------------------------

# Definicao de parametros de busca
query_rais <-
  c("SELECT ano, sigla_uf, id_municipio, tipo_vinculo, vinculo_ativo_3112, id_municipio_trabalho,
  valor_remuneracao_media, subsetor_ibge, cbo_2002,cnae_2,cnae_2_subclasse, faixa_etaria, sexo, raca_cor
    FROM `basedosdados.br_me_rais.microdados_vinculos`
    WHERE sigla_uf = 'MG' and ano > 2011 and id_municipio = '3129707'")

# importacao dos parametros
rais <- read_sql(query_rais)

query_dicionario <- c("SELECT * FROM `basedosdados.br_me_rais.dicionario`")

dicionario_rais <- read_sql(query_dicionario)

openxlsx::write.xlsx(rais, "./dados/Bruto/rais_ibiraci.xlsx")
openxlsx::write.xlsx(dicionario_rais, "./dados/Tratado/dicionario_rais.xlsx")

# Importacao de dados - Populacao ----------------------------------------
# Dados de população foram utilizados das estimativas prospectivas por município
# do Ministério da Saúde.

## População Total

# 2020
pop_2020 <- readxl::read_xlsx("./dados/Bruto/pop_ibiraci_2020.xlsx")

pop_2020_total <- pop_2020 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2020 = Total)

pop_2020_mulheres <- pop_2020 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2020 = Feminino)

pop_2020_homens <- pop_2020 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2020 = Masculino)

# 2019
pop_2019 <- readxl::read_xlsx("./dados/Bruto/pop_ibiraci_2019.xlsx")

pop_2019_total <- pop_2019 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2019 = Total) |> select(pop_2019)

pop_2019_mulheres <- pop_2019 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2019 = Feminino) |> select(pop_2019)

pop_2019_homens <- pop_2019 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2019 = Masculino) |> select(pop_2019)

# 2018
pop_2018 <- readxl::read_xlsx("./dados/Bruto/pop_ibiraci_2018.xlsx")

pop_2018_total <- pop_2018 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2018 = Total) |> select(pop_2018)

pop_2018_mulheres <- pop_2018 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2018 = Feminino) |> select(pop_2018)

pop_2018_homens <- pop_2018 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2018 = Masculino) |> select(pop_2018)

# 2017
pop_2017 <- readxl::read_xlsx("./dados/Bruto/pop_ibiraci_2017.xlsx")

pop_2017_total <- pop_2017 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2017 = Total) |> select(pop_2017)

pop_2017_mulheres <- pop_2017 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2017 = Feminino) |> select(pop_2017)

pop_2017_homens <- pop_2017 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2017 = Masculino) |> select(pop_2017)

# 2016
pop_2016 <- readxl::read_xlsx("./dados/Bruto/pop_ibiraci_2016.xlsx")

pop_2016_total <- pop_2016 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2016 = Total) |> select(pop_2016)

pop_2016_mulheres <- pop_2016 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2016 = Feminino) |> select(pop_2016)

pop_2016_homens <- pop_2016 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2016 = Masculino) |> select(pop_2016)

# 2015
pop_2015 <- readxl::read_xlsx("./dados/Bruto/pop_ibiraci_2015.xlsx")

pop_2015_total <- pop_2015 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2015 = Total) |> select(pop_2015)

pop_2015_mulheres <- pop_2015 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2015 = Feminino) |> select(pop_2015)

pop_2015_homens <- pop_2015 |>
  select(grupo_etario = `Faixa Etária 2`, pop_2015 = Masculino) |> select(pop_2015)

# PIA ambos os sexos
pop_ibiraci_pia_total <- pop_2020_total |>
  bind_cols(pop_2019_total) |>
  bind_cols(pop_2018_total) |>
  bind_cols(pop_2017_total) |>
  bind_cols(pop_2016_total) |>
  bind_cols(pop_2015_total) |>
  pivot_longer(pop_2020:pop_2015, names_to = "pop_ano", values_to = "populacao") |>
  filter(grupo_etario != "De 0 a 4 anos" &
           grupo_etario != "De 5 a 9 anos" &
           grupo_etario != "De 10 a 14 anos" &
           grupo_etario != "De 65 a 69 anos" &
           grupo_etario != "De 70 a 74 anos" &
           grupo_etario != "De 75 a 79 anos" &
           grupo_etario != "De 80 anos ou mais") |>
  group_by(pop_ano) |>
  summarise(populacao = sum(populacao))

openxlsx::write.xlsx(pop_ibiraci_pia_total, "./dados/Tratado/pop_ibiraci_pia_total.xlsx")

# PIA feminina
pop_ibiraci_pia_mulheres <- pop_2020_mulheres |>
  bind_cols(pop_2019_mulheres) |>
  bind_cols(pop_2018_mulheres) |>
  bind_cols(pop_2017_mulheres) |>
  bind_cols(pop_2016_mulheres) |>
  bind_cols(pop_2015_mulheres) |>
  pivot_longer(pop_2020:pop_2015, names_to = "pop_ano", values_to = "populacao") |>
  filter(grupo_etario != "De 0 a 4 anos" &
           grupo_etario != "De 5 a 9 anos" &
           grupo_etario != "De 10 a 14 anos" &
           grupo_etario != "De 65 a 69 anos" &
           grupo_etario != "De 70 a 74 anos" &
           grupo_etario != "De 75 a 79 anos" &
           grupo_etario != "De 80 anos ou mais") |>
  group_by(pop_ano) |>
  summarise(populacao = sum(populacao))

openxlsx::write.xlsx(pop_ibiraci_pia_mulheres, "./dados/Tratado/pop_ibiraci_pia_mulheres.xlsx")

# PIA masculina
pop_ibiraci_pia_homens <- pop_2020_homens |>
  bind_cols(pop_2019_homens) |>
  bind_cols(pop_2018_homens) |>
  bind_cols(pop_2017_homens) |>
  bind_cols(pop_2016_homens) |>
  bind_cols(pop_2015_homens) |>
  pivot_longer(pop_2020:pop_2015, names_to = "pop_ano", values_to = "populacao") |>
  filter(grupo_etario != "De 0 a 4 anos" &
           grupo_etario != "De 5 a 9 anos" &
           grupo_etario != "De 10 a 14 anos" &
           grupo_etario != "De 65 a 69 anos" &
           grupo_etario != "De 70 a 74 anos" &
           grupo_etario != "De 75 a 79 anos" &
           grupo_etario != "De 80 anos ou mais") |>
  group_by(pop_ano) |>
  summarise(populacao = sum(populacao))

openxlsx::write.xlsx(pop_ibiraci_pia_homens, "./dados/Tratado/pop_ibiraci_pia_homens.xlsx")

# Tratamento numerador  --------------------------------------

dicionario_rais <- readxl::read_xlsx("./dicionario_rais.xlsx")

# Emprego formal ambos os sexos

rais_total_tt <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014) |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(total = sum(as.numeric(vinculo_ativo_3112)))

rais_agricultura_tt <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014 & subsetor_ibge == 25) |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(agricultura = sum(as.numeric(vinculo_ativo_3112)))

rais_servicos_tt <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014) |>
  filter(subsetor_ibge >= 18 & subsetor_ibge <= 23) |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(servicos = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_tt <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014) |>
  filter(cnae_2 == "01342") |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(cultivo_cafe = sum(as.numeric(vinculo_ativo_3112)))

emprego_total <- rais_total_tt |>
  bind_cols(rais_agricultura_tt |> select(agricultura)) |>
  bind_cols(rais_servicos_tt |> select(servicos)) |>
  bind_cols(rais_cultivo_cafe_tt |> select(cultivo_cafe)) |>
  select(ano = ano...1, total, agricultura, servicos, cultivo_cafe)

# Emprego formal - sexo feminino

rais_total_fem <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014 & sexo == 2) |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(total = sum(as.numeric(vinculo_ativo_3112)))

rais_agricultura_fem <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014 & subsetor_ibge == 25 & sexo == 2) |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(agricultura = sum(as.numeric(vinculo_ativo_3112)))

rais_servicos_fem <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014) |>
  filter(subsetor_ibge >= 18 & subsetor_ibge <= 23 & sexo == 2) |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(servicos = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_fem <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014 & sexo == 2) |>
  filter(cnae_2 == "01342") |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(cultivo_cafe = sum(as.numeric(vinculo_ativo_3112)))

emprego_mulheres <- rais_total_fem |>
  bind_cols(rais_agricultura_fem |> select(agricultura)) |>
  bind_cols(rais_servicos_fem |> select(servicos)) |>
  bind_cols(rais_cultivo_cafe_fem |> select(cultivo_cafe)) |>
  select(ano = ano...1, total, agricultura, servicos, cultivo_cafe)

# Emprego formal - sexo masculino

rais_total_masc <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014 & sexo == 1) |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(total = sum(as.numeric(vinculo_ativo_3112)))

rais_agricultura_masc <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014 & subsetor_ibge == 25 & sexo == 1) |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(agricultura = sum(as.numeric(vinculo_ativo_3112)))

rais_servicos_masc <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014 & sexo == 1) |>
  filter(subsetor_ibge >= 18 & subsetor_ibge <= 23 & sexo == 1) |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(servicos = sum(as.numeric(vinculo_ativo_3112)))

rais_cultivo_cafe_masc <- rais |>
  filter(vinculo_ativo_3112 == 1 & ano > 2014 & sexo == 1) |>
  filter(cnae_2 == "01342") |>
  select(ano, id_municipio, subsetor_ibge, vinculo_ativo_3112) |>
  group_by(ano, id_municipio) |>
  summarise(cultivo_cafe = sum(as.numeric(vinculo_ativo_3112)))

emprego_homens <- rais_total_masc |>
  bind_cols(rais_agricultura_masc |> select(agricultura)) |>
  bind_cols(rais_servicos_masc |> select(servicos)) |>
  bind_cols(rais_cultivo_cafe_masc |> select(cultivo_cafe)) |>
  select(ano = ano...1, total, agricultura, servicos, cultivo_cafe)


# Taxa de participação ----------------------------------------------------

# Ambos os sexos

taxas_participacao_pia_total <- emprego_total |>
  bind_cols(pop_ibiraci_pia_total |> select(-pop_ano)) |>
  mutate(
    Total = (total/populacao)*100,
    Agricultura = (agricultura/populacao)*100,
    Servicos = (servicos/populacao)*100,
    `Cultivo de café` = (cultivo_cafe/populacao)*100,
  )

openxlsx::write.xlsx(taxas_participacao_pia_total, "./taxas_pia_total.xlsx")

taxas_participacao_pia_total |>
  select(ano, Total, Agricultura, Servicos, `Cultivo de café`) |>
  pivot_longer(Total:`Cultivo de café`, names_to = "tipo", values_to = "tx") |>
  ggplot() +
  aes(x = as.numeric(ano), y = tx, colour = tipo) +
  geom_point(size = 3) +
  geom_line(size = 1.3) +
  theme_classic() +
  labs(
    title = "Emprego formal em relação à população na PIA, \n por setor de atividade econômica, Ibiraci, ambos os sexos, 2015-2020",
    caption = "Fonte: Ministério da Economia/RAIS, 2015-2020. Ministério da Saúde, Estimativas populacionais, 2015-2020.",
    y = "Taxa de participação (formal)",
    x = "Ano"
  ) +
  scale_color_brewer(palette = "Blues",direction = 1) +
  coord_cartesian(ylim = c(0,28)) +
  scale_y_continuous(breaks = seq(0,28,2)) +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = .5, vjust = .5),
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    plot.caption = element_text(face = "bold", size = 7, hjust = .5, vjust = .5)
  )

# Sexo feminino

taxas_participacao_pia_feminino <- emprego_mulheres |>
  bind_cols(pop_ibiraci_pia_mulheres |> select(-pop_ano)) |>
  mutate(
    Total = (total/populacao)*100,
    Agricultura = (agricultura/populacao)*100,
    Servicos = (servicos/populacao)*100,
    `Cultivo de café` = (cultivo_cafe/populacao)*100,
  )

openxlsx::write.xlsx(taxas_participacao_pia_feminino, "taxas_pia_feminino.xlsx")

taxas_participacao_pia_feminino |>
  select(ano, Total, Agricultura, Servicos, `Cultivo de café`) |>
  pivot_longer(Total:`Cultivo de café`, names_to = "tipo", values_to = "tx") |>
  ggplot() +
  aes(x = as.numeric(ano), y = tx, colour = tipo) +
  geom_point(size = 3) +
  geom_line(size = 1.3) +
  theme_classic() +
  labs(
    title = "Emprego formal em relação à população na PIA, \n por setor de atividade econômica, Ibiraci, sexo feminino, 2015-2020",
    caption = "Fonte: Ministério da Economia/RAIS, 2015-2020. Ministério da Saúde, Estimativas populacionais, 2015-2020.",
    y = "Taxa de participação (formal)",
    x = "Ano"
  ) +
  scale_color_brewer(palette = "Blues",direction = 1) +
  coord_cartesian(ylim = c(0,28)) +
  scale_y_continuous(breaks = seq(0,28,2)) +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = .5, vjust = .5),
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    plot.caption = element_text(face = "bold", size = 7, hjust = .5, vjust = .5)
  )

# Sexo masculino

taxas_participacao_pia_masculino <- emprego_homens |>
  bind_cols(pop_ibiraci_pia_homens |> select(-pop_ano)) |>
  mutate(
    Total = (total/populacao)*100,
    Agricultura = (agricultura/populacao)*100,
    Servicos = (servicos/populacao)*100,
    `Cultivo de café` = (cultivo_cafe/populacao)*100,
  )

openxlsx::write.xlsx(taxas_participacao_pia_masculino, "taxas_pia_homens.xlsx")

taxas_participacao_pia_masculino |>
  select(ano, Total, Agricultura, Servicos, `Cultivo de café`) |>
  pivot_longer(Total:`Cultivo de café`, names_to = "tipo", values_to = "tx") |>
  ggplot() +
  aes(x = as.numeric(ano), y = tx, colour = tipo) +
  geom_point(size = 3) +
  geom_line(size = 1.3) +
  theme_classic() +
  labs(
    title = "Emprego formal em relação à população na PIA, \n por setor de atividade econômica, Ibiraci, sexo masculino, 2015-2020",
    caption = "Fonte: Ministério da Economia/RAIS, 2015-2020. Ministério da Saúde, Estimativas populacionais, 2015-2020.",
    y = "Taxa de participação (formal)",
    x = "Ano"
  ) +
  scale_color_brewer(palette = "Blues",direction = 1) +
  coord_cartesian(ylim = c(0,28)) +
  scale_y_continuous(breaks = seq(0,28,2)) +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14, hjust = .5, vjust = .5),
    axis.title = element_text(face = "bold", size = 12, hjust = .5, vjust = .5),
    axis.text = element_text(face = "bold", size = 10, hjust = .5, vjust = .5),
    plot.caption = element_text(face = "bold", size = 7, hjust = .5, vjust = .5)
  )
