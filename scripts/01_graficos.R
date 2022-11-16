# Libraries ---------------------------------------------------------------

library(tidyverse)
library(basedosdados)


# Importação de dados -----------------------------------------------------

taxas_participacao_pia_total <- readxl::read_xlsx("./dados/Tratado/taxas_pia_total.xlsx")

taxas_participacao_pia_feminino <- readxl::read_xlsx("./dados/Tratado/taxas_pia_feminino.xlsx")

taxas_participacao_pia_masculino <- readxl::read_xlsx("./dados/Tratado/taxas_pia_homens.xlsx")

# Taxa de participação ----------------------------------------------------

# Ambos os sexos

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
