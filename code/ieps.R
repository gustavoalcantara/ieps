#####
#Código de Tratamento e Análise dos Dados 
#Gustavo C. Alcântara
#github.com/gustavoalcantara
#Processo Seletivo para Analista de Politicas Públicas - IEPS/SP

#####
#Tratamento
#Options para números decimais
options(scipen  = 999, digits = 6)


#Chamar as Bibliotecas
library(ggplot2)
library(basedosdados)

#Ler as bases
x <- data.table::fread('aih_teste.csv')
y <- data.table::fread('cnes_teste.csv')

#####
#ETL
#Transformação em String para futuros Joins
x$N_AIH <- as.character(x$N_AIH)
x$CNES <- as.character(x$CNES)
y$cnes <- as.character(y$cnes)

#Verificar CNES que não batem em ambas as tabelas:
unmatched_cnes <- setdiff(x$CNES, y$cnes)
print(unmatched_cnes)


#Total de internações em cada tipo (Porte) de hospital e atenção primária
#Atenção Primária por Porte


x|>
  dplyr::left_join(y, by = c('CNES' = 'cnes'))|>
  dplyr::group_by(porte, csap)|>
  dplyr::summarise(internacao = dplyr::n_distinct(N_AIH, na.rm = TRUE))|>
  #dplyr::summarise(conta = sum(internacao))
  dplyr::mutate(
    csap = dplyr::case_when(
    csap == 'FALSE' ~ 'Causas Não sensíveis de Atenção Primária',
    csap == 'TRUE' ~ 'Causas sensíveis de Atenção Primária'),
    total_internacao = sum(internacao),
    porcentagem = (internacao / total_internacao)*100)|>
  dplyr::select(-total_internacao)|>
  ggplot(aes(x = porte, y = internacao, fill = csap)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Porte do Hospital", y = "Quantidade de Internações",
       fill = 'Causas de Atenção Primária',
       title = "Quantidade de Internações por Porte e CSAP em 2023 - SP") +
  theme_minimal()+
  theme(
      legend.position = c(0.8, 0.8),  
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5) 
    )

#####
#Taxa de Ocupação de Leitos
n_internacao <- x|>
  dplyr::left_join(y, by = c('CNES' = 'cnes'))|>
  dplyr::group_by(porte, csap)|>
  dplyr::summarise(internacao = dplyr::n_distinct(N_AIH, na.rm = TRUE))

n_leitos <- y|>
  dplyr::group_by(porte)|>
  dplyr::summarise(leitos = sum(n_leito_total))

total_internacao <- n_leitos|>
  dplyr::full_join(n_internacao, by = 'porte') |>
  dplyr::mutate(
    taxa_ocupacao = (internacao / leitos) * 100,
    csap = dplyr::case_when(
      csap == FALSE ~ 'Causas Não sensíveis de Atenção Primária',
      csap == TRUE ~ 'Causas sensíveis de Atenção Primária'
    )
  )

total_internacao

y|>
  dplyr::group_by(porte)|>
  dplyr::summarise(conta = sum(n_leito_total))


ggplot(total_internacao, aes(x = porte, y = taxa_ocupacao, fill = csap)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      x = "Porte do Hospital",
      y = "Taxa de Ocupação de Leitos",
      fill = "Causas de Atenção Primária",
      title = "Taxa de Ocupação de Leitos por Porte e CSAP em 2023 - SP"
    ) +
    theme_minimal() +
    theme(
      legend.position = c(0.8, 0.8),  
      legend.background = element_rect(fill = "white", color = "black"),
      legend.text = element_text(size = 14, face = "bold"),
      legend.title = element_text(size = 16, face = "bold"),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5)  
    )

##### 
#Percentual de Internações por Causas Sensiveis à Atenção Primária
y|>
  dplyr::full_join(x, by = c('cnes' = 'CNES'))|>
  dplyr::group_by(porte, csap)|>
  dplyr::summarise(internacao = dplyr::n_distinct(N_AIH, na.rm = TRUE))|>
  dplyr::mutate(porcentagem = internacao / sum(internacao)*100,
                csap = dplyr::case_when(
                  csap == 'FALSE' ~ 'Causas Não sensíveis de Atenção Primária',
                  csap == 'TRUE' ~ 'Causas sensíveis de Atenção Primária'))

#####
#Tempo médio (em dias) de Internação dos pacientes
x|>
  dplyr::left_join(y, by = c('CNES' = 'cnes')) |>
  dplyr::group_by(porte, csap)|>
  dplyr::summarise(tempo_medio = 
                    mean(DIAS_PERM, na.rm = TRUE))|>
  dplyr::mutate(csap = dplyr::case_when(
    csap == 'FALSE' ~ 'Causas Não sensíveis de Atenção Primária',
    csap == 'TRUE' ~ 'Causas sensíveis de Atenção Primária'))|>
  dplyr::filter(!is.na(porte))|>
  ggplot(aes(x = porte, y = tempo_medio, fill = csap)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Porte do Hospital", y = "Tempo Médio de Internação em Dias",
       fill = 'Causas de Atenção Primária',
       title = "Tempo Médio de Internação por Porte e CSAP") +
  theme_minimal()+
  theme(
    legend.position = c(0.8, 0.8),  
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5) 
  )

#####
#Valor médio de internação por tipo de hospital e CSAP
q|>
  dplyr::group_by(porte, csap)|>
  dplyr::summarise(conta = mean(VAL_TOT))|>
  dplyr::mutate(csap = dplyr::case_when(
    csap == 'FALSE' ~ 'Causas Não sensíveis de Atenção Primária',
    csap == 'TRUE' ~ 'Causas sensíveis de Atenção Primária'))|>
  dplyr::filter(!is.na(csap))|>
  ggplot(aes(x = porte, y = conta, fill = csap))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Porte do Hospital", y = "Valor Médio em R$",
       fill = 'Causas de Atenção Primária',
       title = "Valor Médio de Internação (em R$) por Porte e CSAP") +
  theme_minimal()+
  theme(
    legend.position = c(0.8, 0.8),  
    legend.background = element_rect(fill = "white", color = "black"),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5) 
  )
  

#####
#Mapa em conjunto com os dados da Base dos Dados
df <- basedosdados::read_sql(
  "SELECT 
  a.cep,  
  id_estabelecimento_cnes,
  nome_municipio,
  centroide
 FROM `basedosdados.br_ms_cnes.estabelecimento`  a
 INNER JOIN `basedosdados.br_bd_diretorios_brasil.cep` b
 ON a.cep = b.cep
WHERE a.sigla_uf = 'SP' AND ano = 2023 AND mes = 12"
)

x |>
  dplyr::inner_join(y, by = c('CNES' = 'cnes')) |>
  dplyr::group_by(CNES, csap) |>
  dplyr::summarise(conta = dplyr::n_distinct(N_AIH)) |>
  dplyr::inner_join(df, by = c('CNES' = 'id_estabelecimento_cnes')) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(names_from = csap, values_from = conta, values_fill = 0) |>
  dplyr::mutate(internacao_status = dplyr::case_when(
    `TRUE` > `FALSE` ~ "Mais Internação CSAP",
    `TRUE` <= `FALSE` ~ "Menos Internação CSAP"
  )) -> df

df|>
  dplyr::mutate(geometry = sf::st_as_sfc(centroide)) |>
  sf::st_as_sf(crs = 4326) -> df

ggplot(df) +
  geom_sf(data = subset(df, internacao_status == "Mais Internação CSAP"), 
          aes(color = internacao_status, size = `TRUE` + `FALSE`), alpha = 0.9) +
  geom_sf(data = subset(df, internacao_status == "Menos Internação CSAP"), 
          aes(color = internacao_status, size = `TRUE` + `FALSE`), alpha = 0.5) +
  scale_size_continuous(range = c(4, 15), name = "Quantidade de Internações") +
  scale_color_manual(values = c("Mais Internação CSAP" = "darkblue", "Menos Internação CSAP" = "lightcoral"), 
                     name = "Status de Internação CSAP") +
  labs(title = "Mapa de Internações por CSAP no Estado",
       subtitle = "Destaque para os pontos com mais internações por CSAP") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5)
  )
