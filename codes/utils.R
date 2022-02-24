repeat_rows <- function(.data, repeat_number = 2000) {
  slice(.data, rep(1:n(), each = repeat_number))
}

splitted_mutate <- function(.data, ..., split_number = 100, .keep = NULL, .progress = TRUE) {
  if (nrow(.data) < split_number) split_number <- nrow(.data)
  out <- tibble()
  if (.progress) {
    pb <- txtProgressBar(min = 0, max = split_number, initial = 0, width = 50, style = 3)
  }
  for (i in 1:split_number) {
    current_result <- .data %>% 
      filter(cut(row_number(), 100, FALSE) == i) %>% 
      mutate(
        ...
      )
    
    if (!is.null(.keep)) { # remove cols >> save memory
      current_result <- current_result %>% 
        select(names(.data), .keep)
    }
    out <- bind_rows(
      out, current_result
    )
    
    if (.progress) {
      setTxtProgressBar(pb,i)
    }
  }
  if (.progress) {
    close(pb)
  }
  out
}

splitted_transmute <- function(.data, ..., split_number = 100) {
  out <- tibble()
  if (.progress) {
    pb <- txtProgressBar(min = 0, max = split_number, initial = 0, width = 50, style = 3)
  }
  for (i in 1:split_number) {
    current_result <- .data %>% 
      filter(cut(row_number(), 100, FALSE) == i) %>% 
      transmute(
        ...
      )
    
    if (!is.null(.keep)) { # remove cols >> save memory
      current_result <- current_result %>% 
        select(names(.data), .keep)
    }
    out <- bind_rows(
      out, current_result
    )
    
    if (.progress) {
      setTxtProgressBar(pb,i)
    }
  }
  if (.progress) {
    close(pb)
  }
  out
}

extractRData <- function(file, object) {
  #' Function for extracting an object from a .RData file
  #' Inputs: RData file, object name
  E <- new.env()
  load(file=file, envir=E)
  return(get(object, envir=E, inherits=F))
}

future_mutate <- function(.data, ..., split_number = 100, .keep = NULL, .progress = TRUE) {
  if (nrow(.data) < split_number) split_number <- nrow(.data)

  future_map(1:split_number, function(split_number) {
    current_result <- .data %>% 
      filter(cut(row_number(), 100, FALSE) == i) %>% 
      mutate(
        ...
      )
    
    if (!is.null(.keep)) { # remove cols >> save memory
      current_result <- current_result %>% 
        select(names(.data), .keep)
    }
    current_result
  }, .progress = .progress) %>% 
  bind_rows()

}

