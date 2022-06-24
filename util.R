util = new.env()

util$make_factor <- function(vec) {
  if (!is.factor(vec)) {
    quants <- unique(quantile(vec, na.rm = T))
    vec <- if (length(quants) > 1) cut(vec, quants) else factor(vec)
  }
  return(vec)
}

util$linebreak_html <- function(vec) {
  gsub("\\n","<br>",vec)
}

util$df_summary <- function(df) {
  
  var_summary <- function(x) {
    
    # helper functions
    get_nas <- function(vec) {
      s <- sum(is.na(vec))
      p <- format(100*round(s/length(vec),3), nsmall = 1)
      paste0(s, " (", p, "%)")
    }
    
    # summary info: variable(var), NAs(nas)
    vec <- x[,1]
    var <- paste0(names(x), ":\n", 
                  paste("-", class(vec), collapse = "\n"))
    nas <- get_nas(vec)
    
    # summary info: values(val), freq (%)(stats)
    if (is.factor(vec) || is.character(vec) || is.logical(vec)) {
      labels <- names(table(vec))
      counts <- table(vec)
      props  <- format(100*round(prop.table(counts),3), nsmall = 1)
      
      val    <- paste0(1:length(labels),". ", labels, collapse = "\n")
      stats  <- paste0(paste0(counts, " (",props,"%)"), collapse = "\n")
      
    } else if (is.numeric(vec)) {
      val    <- paste("range:", min(vec, na.rm = T), "to", max(vec, na.rm = T), "\n",
                      "mean:",  round(mean(vec, na.rm = T),2), "\n",
                      "median:",round(median(vec, na.rm = T),2), "\n",
                      "sd:",    round(sd(vec, na.rm = T),2))
      
      vec    <- make_factor(vec)
      labels <- names(table(vec))
      counts <- table(vec)
      props  <- format(100*round(prop.table(counts),3), nsmall = 1)
      
      stats  <- paste0(paste0(labels," ", counts, " (",props,"%)"), collapse = "\n")
      
    } else {
      val    <- paste("length:", length(vec))
      stats  <- ""
      
    } 
    
    return(c(Variable = var, Values = val, `Freq (%)` = stats, NAs = nas))
  }
  
  dat <-  sapply(1:ncol(df), \(i) var_summary(df[i]))
  as.data.frame(t(dat))
  
}

# HTML tools
util$h.ol <- function(vec) {
  li <- sapply(vec , \(x) paste0("<li>", x, "</li>"))
  paste0("<ol>", paste0(li, collapse = ""),"</ol>\n")
}

util$h.table <- function(df, class = "table") {
  
  tr_ths <- function(vec) {
    ths <- sapply(vec, \(x) paste0("  <th>", x, "</th>"))
    paste0("<tr>", paste0(ths, collapse = ""), "</tr>\n")
  }
  
  tr_tds <- function(vec) {
    tds <- sapply(vec, \(x) paste0("  <td>", x, "</td>"))
    paste0("<tr>", paste0(tds, collapse = ""),"</tr>\n")
  }
  
  thead <- function(vec) {
    paste0("<thead>", paste(vec, collapse = ""), "</thead>\n")
  }
  
  tbody <- function(vec) {
    paste0("<tbody>", paste(vec, collapse = ""), "</tbody>\n")
  }
  
  table_head <- thead(tr_ths(names(df)))
  table_body <- tbody(apply(df[ , ], 1, tr_tds))
  table      <- paste0("<table class=\"",class,"\">", table_head, table_body,"</table>\n")
  return(table)
}

while("util" %in% search())
  detach("util")
attach(util)