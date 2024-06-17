if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, Rcrawler, robotstxt, rvest, RColorBrewer, gt, webshot2)

get_data = function() {
  endereco = "https://codeforces.com/ratings/organization/2647"
  
  # lê as handles
  handles = read_html(endereco) %>%
    html_nodes("#pageContent td:nth-child(2)") %>%
    html_text() %>%
    str_trim %>%
    unlist()
  
  # gera um tibble vazio
  unb = tibble()
  
  # divisoes do codeforces
  divisions = tibble(
    division = as.factor(c("Legendary Grandmaster","International Grandmaster","Grandmaster","International Master","Master","Candidate Master","Expert","Specialist","Pupil","Newbie")),
    rating = c(3000,2600,2400,2300,2100,1900,1600,1400,1200,0),
    color = c("red4","red2","red","orange","yellow","purple","blue","cyan","green3","gray")
  )
  
  # coleta as informações de cada membro da organizacao
  for(handle in handles) {
    pagina = read_html(paste0("https://codeforces.com/profile/", handle))
    
    division = pagina %>%
      html_nodes(".user-rank") %>%
      html_text() %>%
      str_trim() %>%
      str_split(pattern = " ") %>%
      unlist() %>%
      str_replace_all(pattern = "[^[:alpha:]]", replacement = "") %>%
      paste(collapse = " ")
    
    dados = pagina %>%
      html_nodes(".info li:nth-child(1)") %>%
      html_text() %>%
      str_trim() %>%
      str_split(pattern = " ") %>%
      unlist() %>%
      str_replace_all(pattern = "[^[:alnum:]]", replacement = "") %>%
      keep(~!(. == "" | . == "Contest" | . == "rating" | . == "max"))
      
    problemas = pagina %>%
      html_nodes("._UserActivityFrame_countersRow:nth-child(1) ._UserActivityFrame_counter:nth-child(1) ._UserActivityFrame_counterValue") %>%
      html_text() %>%
      str_trim() %>%
      str_split(pattern = " ") %>%
      unlist() %>%
      str_replace_all(pattern = "[^[:digit:]]", replacement = "") %>%
      keep(. != "") %>%
      as.numeric()
    
    # adiciona ao tibble
    unb = bind_rows(unb, tibble(handle = handle,
                                division = division,
                                max_division = str_to_title(paste(dados[-c(1,length(dados))], collapse = " ")),
                                rating = as.numeric(dados[1]),
                                max_rating = as.numeric(dados[length(dados)]),
                                total_problems = problemas))
  }
  
  # transforma as divisoes em factors
  unb$division = factor(unb$division, levels = divisions$division)
  unb$max_division = factor(unb$max_division, levels = divisions$division)
  
  # organiza do maior pro menor rating
  unb = unb %>%
    arrange(desc(rating))
  
  # retorna o tibble
  return(unb)
}

process_data = function(unballoon_data) {
  # data de execucao
  data = Sys.Date()
  
  # cria a pasta
  if(!any(str_detect(list.files("dados/"), pattern = paste0(data,"")))) dir.create(path = file.path("dados",data))
  
  # cria o arquivo .csv
  write.table(unballoon_data, file = file.path("dados",data,paste0(data,".csv")))
  
  # divisoes do codeforces
  divisions = tibble(
    division = as.factor(c("Legendary Grandmaster","International Grandmaster","Grandmaster","International Master","Master","Candidate Master","Expert","Specialist","Pupil","Newbie")),
    rating = c(3000,2600,2400,2300,2100,1900,1600,1400,1200,0),
    color = c("red4","red2","red","orange","yellow","purple","blue","cyan","green3","gray")
  )
  
  # paleta de cores
  pallet = divisions$color
  names(pallet) = divisions$division
  
  # gera um grafico scatter
  pontos = unballoon_data %>%
    ggplot() +
    geom_jitter(aes(x = log10(total_problems), y = rating, color = division)) +
    scale_color_manual(values = pallet) +
    labs(x = "Log10 do total de problemas resolvidos", y = "Rating",
         color = "Divisão", title = paste0("Rating x Problemas resolvidos - ",data)) +
    theme_bw()
  
  # salva o grafico
  ggsave(file.path("dados", data, paste0("pxr_",data,".png")), plot = pontos)
  
  # cria um grafico de densidade
  densidade = unballoon_data %>%
    ggplot(aes(x = rating)) +
    geom_density(alpha=.4,fill = "blue") + 
    geom_vline(data = divisions,
               mapping = aes(xintercept = rating), linetype = "dashed", color = divisions$color) + 
    geom_text(data = divisions, 
              mapping = aes(x = rating,
                            y = 0,
                            label = division,
                            hjust = -0.5,
                            vjust = -0.5),
              angle = 90,
              size = 2.75,
              color = divisions$color) +
    labs(x = "Rating", y = "Densidade", title = paste0("Densidade dos ratings - ",data)) +
    theme_bw()
    
  # salva o grafico
  ggsave(file.path("dados",data, paste0("dens_",data,".png")), plot = densidade)
  
  
  # cria um grafico de pizza
  pizza = unballoon_data %>%
    group_by(division) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    mutate(prop = n / sum(n) * 100) %>%
    mutate(ypos = cumsum(prop) - 0.5*prop) %>%
    
    ggplot(aes(x = "", y = prop, fill = division)) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = pallet) +
    labs(x = NULL, y = NULL, title = paste0("Proporção das divisões, em % - ",data), fill = "Divisões") +
    theme_bw()
  
  # salva o grafico
  ggsave(file.path("dados",data, paste0("pizza_",data,".png")),plot = pizza)
  
  # cria uma tabela com o rank
  tabela = unballoon_data %>%
    gt() %>%
    tab_header(title = paste0("Rank UnBalloon - ", data))
  
  # salva a tabela
  gtsave(tabela, file = file.path("dados",data, paste0("rank_",data,".pdf")))
 
}

main = function() {
  tryCatch(
    expr = {
      message("Coletando os dados...")
      
      data = get_data()
      
      message("Dados coletados com sucesso!")
      
      message("Processando os dados...")
      
      process_data(data)
      
    }, error = function(e) {
      message(e,". Tente novamente.")
      
    }, finally = {
      message("Dados salvos com sucesso!")
      
    }
  )
  
}

main()



