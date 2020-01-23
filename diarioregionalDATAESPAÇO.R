baixar_paginas_diarioregional <- function(diretorio = ".",caderno = "politica", paginas = 10){

  url <- paste0("https://www.diarioregional.com.br/editorias/noticias-gerais/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{

    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")

    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))


  }),NULL))
}

p <- baixar_paginas_diarioregional(diretorio = "pagina")


ler_paginas_diarioregional <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)


  }

  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{

  x <- xml2::read_html(.x)

  url <- x %>%
        xml2::xml_find_all('//section[@class="recent-posts"]//h3[@class="entry-title"]/a') %>%
        xml2::xml_attr("href")

  headline <- x %>%
    xml2::xml_find_all('//section[@class="recent-posts"]//h3[@class="entry-title"]') %>%
    xml2::xml_text()

  autor <- x %>%
    xml2::xml_find_all('//section[@class="recent-posts"]//span[@class="entry-author"]') %>%
    xml2::xml_text() %>%
    stringr::str_sub(5)

  data_publicacao <- x %>%
    xml2::xml_find_all('//section[@class="recent-posts"]//time[@class="entry-date"]') %>%
    xml2::xml_text()

  tibble::tibble(headline,data_publicacao,autor,url)

  }),NULL))

}

drg <- ler_paginas_diarioregional(arquivos = arquivos)


baixar_noticias_diarioregional <- function(url = NULL, diretorio = "."){

  if (is.null(url)){

    stop("Você tem de informar a url")

  }

  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{

   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>%
     stringr::str_replace_all("-","_")

   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_diarioregional_",artigo,".html")

   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))



   }),NULL))


}



baixar_noticias_diarioregional(drg$url,"noticias")



ler_noticias_diarioregional <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{

    x <- xml2::read_html(.x)

    headline <- x %>%
         xml2::xml_find_all('//h1[@class="entry-title"]') %>%
         xml2::xml_text()

   texto <- x %>%
      xml2::xml_find_all('//div[@class="entry-content"]//p') %>%
     xml2::xml_text() %>%
     stringr::str_c(collapse = "\n")

   data_publicacao <- x %>%
     xml2::xml_find_all('//div[@class="post-meta"]/time[@class="entry-date published updated"]') %>%
     xml2::xml_text()%>%
     stringr::str_extract(".+?\\d{4}")

   autor <- x %>%
     xml2::xml_find_first('//header[@class="entry-header"]//div[@class="post-meta"]//a[@class="url fn n"]') %>%
     xml2::xml_text()

   tibble::tibble(headline,texto,data_publicacao,autor)

  }),NULL))

}


noticias <- ler_noticias_diarioregional(arquivos = arquivos)
