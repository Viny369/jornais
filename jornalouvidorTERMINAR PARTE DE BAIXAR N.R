baixar_paginas_jornalouvidor <- function(diretorio = ".",caderno = "nacional", paginas = 10){
  
  url <- paste0("http://www.jornalouvidor.com.br/cidade/",caderno,"/6/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_jornalouvidor(diretorio = "pagina")


ler_paginas_jornalouvidor <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//td[1]/a') %>% 
        xml2::xml_attr("href") %>% 
    paste0("http://www.jornalouvidor.com.br",.)
  
  headline <- x %>% 
    xml2::xml_find_all('//td[1]/a') %>% 
    xml2::xml_text()

  resumo <- x %>% 
    xml2::xml_find_all('//table[@class="table table-striped"]//td[2]') %>% 
    xml2::xml_text()
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//table[@class="table table-striped"]//td[3]') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,resumo,data_publicacao,url)
    
  }),NULL))
  
}

jo <- ler_paginas_jornalouvidor(arquivos = arquivos)  


baixar_noticias_jornalouvidor <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_jornalouvidor_",artigo,".html")
  
   httr::RETRY("GET",.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_jornalouvidor(jo$url[1:10],"noticias")



ler_noticias_jornalnossafolha <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="name post-title entry-title"]/span') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="entry"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="post-inner"]//p[@class="post-meta"]/span[@class="tie-date"]') %>% 
     xml2::xml_text()
   

   
   tibble::tibble(headline,texto,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_gazetadoriopardo(arquivos = arquivos)
