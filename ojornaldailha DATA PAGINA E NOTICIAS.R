baixar_paginas_ojornaldailha <- function(diretorio = ".",caderno = "cidade", paginas = 10){

  url <- paste0("https://ojornaldailha.com/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{

    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")

    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))


  }),NULL))
}

p <- baixar_paginas_ojornaldailha(diretorio = "pagina")


ler_paginas_ojornaldailha <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)


  }

  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{

  x <- xml2::read_html(.x)

  url <- x %>%
        xml2::xml_find_all('//div[@class="col-sm-8 content-column"]//h2[@class="title"]/a') %>%
        xml2::xml_attr("href")

  headline <- x %>%
    xml2::xml_find_all('//div[@class="col-sm-8 content-column"]//h2[@class="title"]/a') %>%
    xml2::xml_text()

  descricao <- x %>%
    xml2::xml_find_all('//div[@class="post-summary"]') %>%
    xml2::xml_text()

  data_publicacao <- x %>%
    xml2::xml_find_all('//div[@class="col-sm-8 content-column"]//time[@class="post-published updated"]') %>%
    xml2::xml_text()

  tibble::tibble(headline,descricao,data_publicacao,url)

  }),NULL))

}

oji <- ler_paginas_ojornaldailha(arquivos = arquivos)


baixar_noticias_ojornaldailha <- function(url = NULL, diretorio = "."){

  if (is.null(url)){

    stop("Você tem de informar a url")

  }

  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{

   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>%
     stringr::str_replace_all("-","_")

   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_ojornaldailha_",artigo,".html")

   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))



   }),NULL))


}



baixar_noticias_ojornaldailha(oji$url[1:10],"noticias")



ler_noticias_ojornaldailha <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{

    x <- xml2::read_html(.x)

    headline <- x %>%
         xml2::xml_find_all('//h1[@class="single-post-title"]/span[@class="post-title"]') %>%
         xml2::xml_text()

   texto <- x %>%
      xml2::xml_find_all('//div[@class="entry-content clearfix single-post-content"]/p') %>%
     xml2::xml_text() %>%
     stringr::str_c(collapse = "\n")

   data_publicacao <- x %>%
     xml2::xml_find_all('//time[@class="post-published updated"]//b') %>%
     xml2::xml_text()



   tibble::tibble(headline,texto,data_publicacao)

  }),NULL))

}


noticias <- ler_noticias_ojornaldailha(arquivos = arquivos)
