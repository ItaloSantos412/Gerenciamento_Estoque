if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite", repos = "https://cloud.r-project.org")
}

ler_linha <- function(prompt = "") {
  cat(prompt)
  linha <- readLines(con = "stdin", n = 1, warn = FALSE)
  if (length(linha) == 0) return("")
  trimws(linha)
}

script_path <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  match <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
  if (length(match) > 0) dirname(normalizePath(match[1])) else "."
}, error = function(e) ".")
ARQUIVO_JSON <- file.path(script_path, "dados_plantio.json")
if (!file.exists(ARQUIVO_JSON)) {
  ARQUIVO_JSON <- "dados_plantio.json"
}

carregar_dados <- function() {
  if (!file.exists(ARQUIVO_JSON)) {
    cat(sprintf("Arquivo '%s' não encontrado.\n", ARQUIVO_JSON))
    cat("Execute primeiro o programa Python para gerar os dados.\n")
    return(NULL)
  }
  json <- jsonlite::fromJSON(ARQUIVO_JSON, simplifyDataFrame = TRUE)
  resultado <- list()
  for (cultura in names(json)) {
    df <- as.data.frame(json[[cultura]])
    if (nrow(df) > 0) {
      for (col in names(df)) df[[col]] <- as.numeric(df[[col]])
    }
    resultado[[cultura]] <- df
  }
  resultado
}

mostrar_dados <- function(dados) {
  for (cultura in names(dados)) {
    regs <- dados[[cultura]]
    if (nrow(regs) == 0) next
    cat(sprintf("\n=== Dados de %s (%d registros) ===\n",
                tools::toTitleCase(cultura), nrow(regs)))
    for (i in seq_len(nrow(regs))) {
      r <- regs[i, ]
      cat(sprintf("  Pos %d | Tamanho=%.1fm | Área=%.1fm² | Ruas=%d | Fert/rua=%.1fL | Fert total=%.1fL\n",
                  i - 1, r$tamanho, r$area, r$ruas, r$fert_rua, r$fert_total))
    }
  }
}

estatisticas <- function(dados) {
  for (cultura in names(dados)) {
    regs <- dados[[cultura]]
    if (nrow(regs) == 0) next

    cat(sprintf("\n========== Estatísticas de %s (%d registros) ==========\n",
                tools::toTitleCase(cultura), nrow(regs)))

    campos <- c("tamanho", "area", "ruas", "fert_rua", "fert_total")
    rotulos <- c("Tamanho (m)", "Área (m²)", "Ruas",
                 "Fert/rua (L)", "Fert total (L)")

    for (j in seq_along(campos)) {
      vetor <- regs[[campos[j]]]
      cat(sprintf("  %s:\n", rotulos[j]))
      cat(sprintf("    Média         = %.2f\n", mean(vetor)))
      cat(sprintf("    Desvio padrão = %.2f\n", ifelse(length(vetor) > 1, sd(vetor), 0)))
      cat(sprintf("    Mínimo        = %.2f\n", min(vetor)))
      cat(sprintf("    Máximo        = %.2f\n", max(vetor)))
      cat(sprintf("    Soma          = %.2f\n", sum(vetor)))
    }
  }
}

consultar_clima <- function() {
  cat("Consulta climática via API Open-Meteo (dados atuais).\n")
  cat("Coordenadas padrão: São Paulo (-23.55, -46.63)\n")
  usar_padrao <- tolower(ler_linha("Usar coordenadas padrão? (s/n): "))

  if (usar_padrao == "n") {
    lat <- as.numeric(ler_linha("Latitude (ex: -23.55): "))
    lon <- as.numeric(ler_linha("Longitude (ex: -46.63): "))
  } else {
    lat <- -23.55
    lon <- -46.63
  }

  url <- sprintf(
    "https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f&current_weather=true&timezone=America%%2FSao_Paulo",
    lat, lon
  )

  tryCatch({
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      cat("Instalando pacote jsonlite...\n")
      install.packages("jsonlite", repos = "https://cloud.r-project.org")
    }

    resposta <- jsonlite::fromJSON(url)
    clima <- resposta$current_weather

    codigo_tempo <- function(code) {
      descricoes <- c(
        "0" = "Céu limpo", "1" = "Parcialmente limpo",
        "2" = "Parcialmente nublado", "3" = "Nublado",
        "45" = "Nevoeiro", "48" = "Nevoeiro com geada",
        "51" = "Garoa leve", "53" = "Garoa moderada", "55" = "Garoa forte",
        "61" = "Chuva leve", "63" = "Chuva moderada", "65" = "Chuva forte",
        "71" = "Neve leve", "73" = "Neve moderada", "75" = "Neve forte",
        "80" = "Pancada leve", "81" = "Pancada moderada", "82" = "Pancada forte",
        "95" = "Trovoada", "96" = "Trovoada com granizo leve",
        "99" = "Trovoada com granizo forte"
      )
      desc <- descricoes[as.character(code)]
      if (is.na(desc)) return(paste("Código WMO:", code))
      desc
    }

    cat("\n===== Dados Meteorológicos Atuais =====\n")
    cat(sprintf("  Coordenadas    : %.4f, %.4f\n", lat, lon))
    cat(sprintf("  Temperatura    : %.1f °C\n", clima$temperature))
    cat(sprintf("  Vento          : %.1f km/h\n", clima$windspeed))
    cat(sprintf("  Direção vento  : %d°\n", clima$winddirection))
    cat(sprintf("  Condição       : %s\n", codigo_tempo(clima$weathercode)))
    cat(sprintf("  Horário (local): %s\n", clima$time))

  }, error = function(e) {
    cat(sprintf("Erro ao consultar API: %s\n", e$message))
    cat("Verifique sua conexão com a internet.\n")
  })
}

MENU <- "
==== FarmTech Solutions - Análise de Dados (R) ====
Dados importados do arquivo JSON gerado pelo Python.

1 - Carregar/Recarregar dados do JSON
2 - Mostrar dados importados
3 - Estatísticas (média, desvio padrão, mín, máx)
4 - Consultar clima (API meteorológica)
5 - Sair do programa
"

dados <- NULL

repeat {
  cat(MENU)
  escolha <- ler_linha("Escolha uma opção: ")
  if (escolha == "5") {
    cat("Saindo do programa.\n")
    break
  }
  switch(escolha,
    "1" = {
      dados <<- carregar_dados()
      if (!is.null(dados)) cat("Dados carregados com sucesso.\n")
    },
    "2" = {
      if (is.null(dados)) { cat("Carregue os dados primeiro (opção 1).\n") }
      else mostrar_dados(dados)
    },
    "3" = {
      if (is.null(dados)) { cat("Carregue os dados primeiro (opção 1).\n") }
      else estatisticas(dados)
    },
    "4" = consultar_clima(),
    cat("Opção inválida.\n")
  )
}
