baixar_paginas_jornalistaslivres <- function(diretorio = ".",caderno = "moradia", paginas = 10){
  
  url <- paste0("https://jornalistaslivres.org/categoria/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_jornalistaslivres(diretorio = "pagina")


ler_paginas_jornalistaslivres <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="blog-post-title"]/h2/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="blog-post-title"]/h2/a') %>% 
    xml2::xml_text()

  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="blog-post-content"]') %>% 
    xml2::xml_text()
  
  autor <- x %>% 
    xml2::xml_find_all('//div[@class="blog-post-author"]/a') %>% 
    xml2::xml_text()
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="blog-post-date"]') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,descricao,autor,data_publicacao,url)
    
  }),NULL))
  
}

jl <- ler_paginas_jornalistaslivres(arquivos = arquivos)  


baixar_noticias_jornalistaslivres <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_jornalistaslivres_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_jornalistaslivres(jl$url,"noticias")



ler_noticias_jornalistaslivres <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1') %>% 
         xml2::xml_text()
    
    descricao <- x %>% 
      xml2::xml_find_all('//div[@id="post-page-subtitle"]/h2') %>% 
      xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="content  no-floating-icons"]/p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   autor <- x %>% 
     xml2::xml_find_all('//a[@rel="author"]') %>% 
     xml2::xml_text()
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//span[@class="post-page-date"]') %>% 
     xml2::xml_text()
   

   
   tibble::tibble(headline,descricao,texto,autor,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_jornalistaslivres(arquivos = arquivos)
