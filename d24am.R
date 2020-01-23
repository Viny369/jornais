baixar_paginas_d24am <- function(diretorio = ".",caderno = "amazonas", paginas = 10){

  url <- paste0("https://d24am.com/",caderno,"/pagina/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{

    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")

    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))


  }),NULL))
}

p <- baixar_paginas_d24am(diretorio = "pagina")


ler_paginas_d24am <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)


  }

  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{

  x <- xml2::read_html(.x)

  url <- x %>%
        xml2::xml_find_all('//div[@class="row home-list"]//div[@class="row"]/a') %>%
        xml2::xml_attr("href")

  headline <- x %>%
    xml2::xml_find_all('//h1[@class="blog-post-title"]') %>%
    xml2::xml_text()

  descricao <- x %>%
    xml2::xml_find_all('//div[@class="blog-post-excerpt"]/p') %>%
    xml2::xml_text()

  data_publicacao <- x %>%
    xml2::xml_find_all('//span[@class="blog-post-time"]') %>%
    xml2::xml_text()

  tibble::tibble(headline,descricao,data_publicacao,url)

  }),NULL))

}

d24 <- ler_paginas_d24am(arquivos = arquivos)


baixar_noticias_d24am <- function(url = NULL, diretorio = "."){

  if (is.null(url)){

    stop("Você tem de informar a url")

  }

  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{

   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>%
     stringr::str_replace_all("-","_")

   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_d24am_",artigo,".html")

   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))



   }),NULL))


}



baixar_noticias_d24am(d24$url[1:10],"noticias")



ler_noticias_d24am <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{

    x <- xml2::read_html(.x)

    headline <- x %>%
         xml2::xml_find_all('//h1[@class="blog-post-title"]') %>%
         xml2::xml_text()

   texto <- x %>%
      xml2::xml_find_all('//section[@class="sticky-wraper"]//p') %>%
     xml2::xml_text() %>%
     stringr::str_c(collapse = "\n")

   data_publicacao <- x %>%
     xml2::xml_find_all('//time[@class="blog-post-meta"]') %>%
     xml2::xml_text() %>%
     stringr::str_sub(start = 24, end = 45)



   tibble::tibble(headline,texto,data_publicacao)


  }),NULL))

}


noticias <- ler_noticias_d24am(arquivos = arquivos)
