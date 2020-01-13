baixar_paginas_vaidape <- function(diretorio = ".",caderno = "aruagrita", paginas = 10){
  
  url <- paste0("http://vaidape.com.br/categoria/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_vaidape(diretorio = "pagina")


ler_paginas_vaidape <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="meta"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="meta"]//h4') %>% 
    xml2::xml_text() 
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="meta"]/small') %>% 
    xml2::xml_text()%>% 
    lubridate::dmy()
  
  tibble::tibble(headline,data_publicacao,url)
    
  }),NULL))
  
}

vdp <- ler_paginas_vaidape(arquivos = arquivos)  


baixar_noticias_vaidape <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_vaidape_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_vaidape(vdp$url[1:20],"noticias")



ler_noticias_vaidape <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
      xml2::xml_find_all('//h1[@id="post-title"]') %>% 
      xml2::xml_text()
    
    texto <- x %>% 
      xml2::xml_find_all('//div[@class="section col-sm-offset-1 col-sm-10 col-md-offset-2 col-md-8 col-lg-offset-2 col-lg-8"]/div/p') %>% 
      xml2::xml_text() %>% 
      stringr::str_c(collapse = "\n")
    
    text2 <- x %>% 
      xml2::xml_find_all('//div[@dir="auto"]') %>% 
      xml2::xml_text() %>% 
      stringr::str_c(collapse = "\n")
    
    texto <- c(text2,texto)
    
    data_publicacao <- x %>% 
      xml2::xml_find_all('//h6[@id="post-date"]') %>% 
      xml2::xml_text()

   
   tibble::tibble(headline,texto,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_vaidape(arquivos = arquivos)
