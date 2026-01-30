# Reddit Media Downloader - Preview & Select Version
library(shiny)
library(shinyFiles)
library(httr)
library(jsonlite)
library(stringr)
library(later)

fetch_page <- function(url, after = NULL) {
  if (!grepl("\\.json", url)) url <- paste0(gsub("/$", "", url), "/.json")
  if (!is.null(after) && after != "") url <- paste0(url, if (grepl("\\?", url)) "&" else "?", "after=", after)
  resp <- tryCatch(httr::GET(url, httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"), httr::timeout(30)), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyDataFrame = FALSE)
}

# Redgifs API
get_redgifs_url <- function(redgifs_url) {
  id <- NULL
  m <- str_match(redgifs_url, "redgifs\\.com/watch/([a-zA-Z0-9]+)")
  if (!is.na(m[1, 2])) id <- m[1, 2]
  if (is.null(id)) {
    m <- str_match(redgifs_url, "redgifs\\.com/ifr/([a-zA-Z0-9]+)")
    if (!is.na(m[1, 2])) id <- m[1, 2]
  }
  if (is.null(id)) {
    m <- str_match(redgifs_url, "gfycat\\.com/([a-zA-Z]+)")
    if (!is.na(m[1, 2])) id <- m[1, 2]
  }
  if (is.null(id)) return(NULL)
  
  token_resp <- tryCatch({
    httr::GET("https://api.redgifs.com/v2/auth/temporary",
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
      httr::timeout(15)
    )
  }, error = function(e) { cat("Token error:", e$message, "\n"); NULL })
  
  if (is.null(token_resp) || httr::status_code(token_resp) != 200) {
    cat("Token failed, status:", if (!is.null(token_resp)) httr::status_code(token_resp) else "NULL", "\n")
    return(NULL)
  }
  
  token_data <- tryCatch(jsonlite::fromJSON(httr::content(token_resp, "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(token_data$token)) return(NULL)
  
  token <- token_data$token
  api_url <- paste0("https://api.redgifs.com/v2/gifs/", tolower(id))
  
  resp <- tryCatch({
    httr::GET(api_url, 
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
      httr::add_headers("Authorization" = paste("Bearer", token)),
      httr::timeout(15)
    )
  }, error = function(e) { cat("API error:", e$message, "\n"); NULL })
  
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  
  data <- tryCatch(jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(data)) return(NULL)
  
  video_url <- NULL
  if (!is.null(data$gif$urls$hd) && nchar(data$gif$urls$hd) > 0) {
    video_url <- data$gif$urls$hd
  } else if (!is.null(data$gif$urls$sd) && nchar(data$gif$urls$sd) > 0) {
    video_url <- data$gif$urls$sd
  }
  
  return(list(url = video_url, id = id))
}

normalize_url <- function(url) {
  url <- gsub("\\?.*$", "", url)
  url <- gsub("preview\\.redd\\.it", "i.redd.it", url)
  url
}

get_preview_url <- function(url) {
  # For i.redd.it images, we can use them directly as preview
  if (grepl("i\\.redd\\.it", url)) return(url)
  if (grepl("preview\\.redd\\.it", url)) return(gsub("&amp;", "&", url))
  if (grepl("\\.(jpg|jpeg|png|gif|webp)($|\\?)", url, ignore.case = TRUE)) return(url)
  # For videos and redgifs, return NULL (we'll show icon instead)
  return(NULL)
}

get_media <- function(p) {
  items <- list()
  
  if (!is.null(p$url)) {
    url <- p$url
    preview <- NULL
    title <- if (!is.null(p$title)) substr(p$title, 1, 50) else "Media"
    
    # Try to get preview from post
    if (!is.null(p$preview$images[[1]]$source$url)) {
      preview <- gsub("&amp;", "&", p$preview$images[[1]]$source$url)
    }
    
    if (grepl("i\\.redd\\.it/", url)) {
      items <- append(items, list(list(url = url, preview = if (is.null(preview)) url else preview, title = title, type = "image")))
    } else if (grepl("\\.(jpg|jpeg|png|gif|gifv|webp|mp4)($|\\?)", url, ignore.case = TRUE)) {
      ftype <- if (grepl("\\.(mp4|gifv)($|\\?)", url, ignore.case = TRUE)) "video" else "image"
      items <- append(items, list(list(url = url, preview = preview, title = title, type = ftype)))
    } else if (grepl("redgifs\\.com/(watch|ifr)/", url, ignore.case = TRUE)) {
      items <- append(items, list(list(url = paste0("REDGIFS:", url), preview = NULL, title = title, type = "video")))
    } else if (grepl("gfycat\\.com/", url, ignore.case = TRUE)) {
      items <- append(items, list(list(url = paste0("REDGIFS:", url), preview = NULL, title = title, type = "video")))
    }
  }
  
  if (isTRUE(p$is_video)) {
    video_url <- NULL
    preview <- NULL
    title <- if (!is.null(p$title)) substr(p$title, 1, 50) else "Video"
    
    if (!is.null(p$preview$images[[1]]$source$url)) {
      preview <- gsub("&amp;", "&", p$preview$images[[1]]$source$url)
    }
    
    if (!is.null(p$media$reddit_video$fallback_url)) {
      video_url <- p$media$reddit_video$fallback_url
    } else if (!is.null(p$secure_media$reddit_video$fallback_url)) {
      video_url <- p$secure_media$reddit_video$fallback_url
    }
    if (!is.null(video_url)) {
      video_url <- gsub("\\?source=fallback", "", video_url)
      items <- append(items, list(list(url = video_url, preview = preview, title = title, type = "video")))
    }
  }
  
  if (!is.null(p$preview$reddit_video_preview$fallback_url)) {
    preview <- NULL
    if (!is.null(p$preview$images[[1]]$source$url)) {
      preview <- gsub("&amp;", "&", p$preview$images[[1]]$source$url)
    }
    items <- append(items, list(list(
      url = gsub("\\?source=fallback", "", p$preview$reddit_video_preview$fallback_url),
      preview = preview,
      title = if (!is.null(p$title)) substr(p$title, 1, 50) else "Video Preview",
      type = "video"
    )))
  }
  
  if (isTRUE(p$is_gallery) && !is.null(p$media_metadata)) {
    title <- if (!is.null(p$title)) substr(p$title, 1, 50) else "Gallery"
    idx <- 1
    for (item in p$media_metadata) {
      if (!is.null(item$s)) {
        if (!is.null(item$s$u)) {
          img_url <- gsub("&amp;", "&", item$s$u)
          img_url <- gsub("preview\\.redd\\.it", "i.redd.it", img_url)
          img_url <- gsub("\\?.*$", "", img_url)
          items <- append(items, list(list(url = img_url, preview = gsub("&amp;", "&", item$s$u), title = paste0(title, " (", idx, ")"), type = "image")))
        } else if (!is.null(item$s$gif)) {
          items <- append(items, list(list(url = gsub("&amp;", "&", item$s$gif), preview = NULL, title = paste0(title, " (", idx, ")"), type = "gif")))
        } else if (!is.null(item$s$mp4)) {
          items <- append(items, list(list(url = gsub("&amp;", "&", item$s$mp4), preview = NULL, title = paste0(title, " (", idx, ")"), type = "video")))
        }
        idx <- idx + 1
      }
    }
  }
  
  if (!is.null(p$preview$images)) {
    for (img in p$preview$images) {
      if (!is.null(img$variants$mp4$source$url)) {
        mp4_url <- gsub("&amp;", "&", img$variants$mp4$source$url)
        items <- append(items, list(list(url = mp4_url, preview = NULL, title = "Animated", type = "video")))
      } else if (!is.null(img$variants$gif$source$url)) {
        gif_url <- gsub("&amp;", "&", img$variants$gif$source$url)
        items <- append(items, list(list(url = gif_url, preview = NULL, title = "GIF", type = "gif")))
      }
    }
  }
  
  # Convert gifv to mp4
  for (i in seq_along(items)) {
    items[[i]]$url <- gsub("\\.gifv$", ".mp4", items[[i]]$url, ignore.case = TRUE)
  }
  
  # Remove duplicates based on normalized URL
  if (length(items) > 0) {
    norm <- sapply(items, function(x) normalize_url(gsub("^REDGIFS:", "", x$url)))
    items <- items[!duplicated(norm)]
  }
  
  items
}

fname <- function(url, i) {
  n <- gsub("\\?.*$", "", basename(url))
  if (grepl("DASH_\\d+", n)) {
    n <- paste0("video_", sprintf("%04d", i), ".mp4")
  } else if (nchar(n) < 3 || !grepl("\\.", n)) {
    ext <- if (grepl("\\.gif", url, TRUE)) ".gif" else if (grepl("\\.mp4|DASH|redgifs", url, TRUE)) ".mp4" else ".jpg"
    n <- paste0("media_", sprintf("%04d", i), ext)
  }
  gsub("\\.gifv$", ".mp4", gsub("[<>:\"/\\|?*]", "_", n), ignore.case = TRUE)
}

dl <- function(url, path, is_redgifs = FALSE) {
  url <- gsub("\\.gifv$", ".mp4", url, ignore.case = TRUE)
  
  tryCatch({
    if (is_redgifs) {
      r <- httr::GET(url, 
        httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
        httr::add_headers(
          "Accept" = "*/*",
          "Origin" = "https://www.redgifs.com",
          "Referer" = "https://www.redgifs.com/"
        ),
        httr::write_disk(path, TRUE), 
        httr::timeout(180)
      )
    } else {
      r <- httr::GET(url, httr::user_agent("Mozilla/5.0"), httr::write_disk(path, TRUE), httr::timeout(120))
    }
    status <- httr::status_code(r)
    size <- file.info(path)$size
    status == 200 && !is.na(size) && size > 100
  }, error = function(e) { FALSE })
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$style(HTML("
      *{margin:0;padding:0;box-sizing:border-box}body{background:#0a0a0a;color:#fff;font-family:'Courier New',monospace}
      .bar{background:#1a1a1a;border-bottom:1px solid #333;padding:8px 15px;display:flex;align-items:center;gap:15px}
      .dots{display:flex;gap:6px}.dot{width:12px;height:12px;border-radius:50%}.r{background:#ff5f56}.y{background:#ffbd2e}.g{background:#27ca40}
      .title{font-size:13px;color:#888;flex:1;text-align:center}
      .main{padding:30px;max-width:1200px;margin:0 auto}
      .box{border:1px solid #333;padding:20px;margin-bottom:20px}
      h1{font-size:20px;font-weight:400;letter-spacing:2px;text-transform:uppercase;text-align:center}
      label{display:block;font-size:10px;text-transform:uppercase;letter-spacing:1px;color:#666;margin-bottom:6px}
      input[type=text]{width:100%;background:#0a0a0a;border:1px solid #333;color:#fff;padding:10px;font-family:inherit;font-size:13px}
      input:focus{outline:none;border-color:#fff}
      .btns{display:flex;gap:10px;margin-top:15px;flex-wrap:wrap}
      .btn{padding:12px 20px;border:1px solid #333;background:transparent;color:#fff;font-family:inherit;font-size:11px;text-transform:uppercase;letter-spacing:1px;cursor:pointer}
      .btn:hover{background:#1a1a1a}.btn.p{border-color:#fff}.btn.p:hover{background:#fff;color:#0a0a0a}
      .btn:disabled{opacity:0.5;cursor:not-allowed}
      .stats{display:grid;grid-template-columns:repeat(4,1fr);gap:10px}
      .stat{border:1px solid #333;padding:15px;text-align:center}.stat b{font-size:24px;display:block}.stat span{font-size:9px;color:#666;text-transform:uppercase}
      .pbar{height:3px;background:#1a1a1a;margin:15px 0}.pfill{height:100%;background:#fff;width:0%;transition:width .2s}
      .status{font-size:11px;color:#666;text-align:center}
      .log{max-height:150px;overflow-y:auto;font-size:11px;line-height:1.6;padding-top:10px;border-top:1px solid #333;margin-top:10px}
      .s{color:#27ca40}.e{color:#ff5f56}.i{color:#666}
      .shiny-input-container{width:100%!important}.irs--shiny .irs-bar,.irs--shiny .irs-handle{background:#fff;border-color:#fff}.irs--shiny .irs-line{background:#333}
      
      .preview-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:15px;margin-top:15px;max-height:500px;overflow-y:auto;padding:5px}
      .preview-item{border:1px solid #333;padding:10px;position:relative;transition:all 0.2s}
      .preview-item:hover{border-color:#666}
      .preview-item.selected{border-color:#27ca40;background:rgba(39,202,64,0.1)}
      .preview-thumb{width:100%;height:120px;object-fit:cover;background:#1a1a1a;display:flex;align-items:center;justify-content:center}
      .preview-thumb img{max-width:100%;max-height:100%;object-fit:contain}
      .preview-icon{font-size:40px;color:#444}
      .preview-info{margin-top:8px;font-size:10px;color:#888;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
      .preview-type{position:absolute;top:5px;right:5px;background:rgba(0,0,0,0.7);padding:2px 6px;font-size:9px;text-transform:uppercase}
      .preview-check{position:absolute;top:5px;left:5px;width:20px;height:20px;background:#333;border:1px solid #555;cursor:pointer;display:flex;align-items:center;justify-content:center}
      .preview-check.checked{background:#27ca40;border-color:#27ca40}
      .preview-check i{color:#fff;font-size:12px}
      
      .folder-input{display:flex;gap:10px;align-items:flex-end}
      .folder-input .shiny-input-container{flex:1}
      .folder-btn{height:38px;white-space:nowrap}
      
      .selection-bar{display:flex;justify-content:space-between;align-items:center;padding:10px 0;border-bottom:1px solid #333;margin-bottom:10px}
      .selection-info{font-size:12px;color:#888}
    "))
  ),
  div(class="bar",div(class="dots",span(class="dot r"),span(class="dot y"),span(class="dot g")),span(class="title","REDDIT MEDIA DOWNLOADER")),
  div(class="main",
    div(class="box",h1("REDDIT DOWNLOADER")),
    div(class="box",
      tags$label(tags$i(class="fa fa-link")," URL"),
      textInput("url",NULL,placeholder="https://www.reddit.com/r/pics/.json",width="100%"),
      
      tags$label(style="margin-top:10px",tags$i(class="fa fa-folder")," Download Folder"),
      div(class="folder-input",
        textInput("folder_path", NULL, value = getwd(), width = "100%"),
        shinyDirButton("folder_select", "Browse", "Select Download Folder", class="btn folder-btn")
      ),
      
      tags$label(style="margin-top:10px",tags$i(class="fa fa-list")," Pages to Scan"),
      sliderInput("pages",NULL,1,1000,10,width="100%"),
      div(class="btns",
        actionButton("scan",tagList(tags$i(class="fa fa-search")," SCAN"),class="btn p"),
        actionButton("stop",tagList(tags$i(class="fa fa-stop")," STOP"),class="btn")
      )
    ),
    
    # Preview section (hidden until scan)
    uiOutput("preview_section"),
    
    div(class="box",
      div(class="stats",
        div(class="stat",tags$b(id="n1","0"),span("found")),
        div(class="stat",tags$b(id="n2","0"),span("downloaded")),
        div(class="stat",tags$b(id="n3","0"),span("failed")),
        div(class="stat",tags$b(id="n4","0%"),span("progress"))
      ),
      div(class="pbar",div(id="pb",class="pfill")),
      div(class="status",id="st","Ready"),
      div(class="log",id="lg")
    )
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('upd', function(d) {
      if(d.n1 !== undefined) document.getElementById('n1').textContent = d.n1;
      if(d.n2 !== undefined) document.getElementById('n2').textContent = d.n2;
      if(d.n3 !== undefined) document.getElementById('n3').textContent = d.n3;
      if(d.n4 !== undefined) document.getElementById('n4').textContent = d.n4 + '%';
      if(d.pb !== undefined) document.getElementById('pb').style.width = d.pb + '%';
      if(d.st !== undefined) document.getElementById('st').textContent = d.st;
      if(d.lg !== undefined) {
        var el = document.getElementById('lg');
        el.innerHTML = d.lg + el.innerHTML.substring(0, 3000);
      }
    });
    
    // Item selection handling
    Shiny.addCustomMessageHandler('toggleItem', function(d) {
      var item = document.querySelector('[data-index=\"' + d.index + '\"]');
      if (item) {
        var check = item.querySelector('.preview-check');
        if (d.selected) {
          item.classList.add('selected');
          check.classList.add('checked');
          check.innerHTML = '<i class=\"fa fa-check\"></i>';
        } else {
          item.classList.remove('selected');
          check.classList.remove('checked');
          check.innerHTML = '';
        }
      }
    });
    
    Shiny.addCustomMessageHandler('updateSelectionCount', function(d) {
      var el = document.getElementById('selection-count');
      if (el) el.textContent = d.count + ' of ' + d.total + ' selected';
    });
  "))
)

server <- function(input, output, session) {
  # Environment for state
  e <- new.env()
  e$run <- FALSE
  e$items <- list()
  e$selected <- c()
  e$i <- 0
  e$dir <- ""
  e$pg <- 0
  e$maxpg <- 5
  e$base <- ""
  e$after <- NULL
  e$found <- 0
  e$done <- 0
  e$fail <- 0
  e$downloaded_hashes <- c()
  e$scanning <- FALSE
  e$downloading <- FALSE
  
  # Reactive values
  rv <- reactiveValues(
    items = list(),
    selected = c(),
    show_preview = FALSE
  )
  
  # Setup folder chooser
  volumes <- c(Home = Sys.getenv("USERPROFILE"), getVolumes()())
  shinyDirChoose(input, "folder_select", roots = volumes, session = session)
  
  observeEvent(input$folder_select, {
    if (!is.null(input$folder_select) && !is.integer(input$folder_select)) {
      folder_path <- parseDirPath(volumes, input$folder_select)
      if (length(folder_path) > 0) {
        updateTextInput(session, "folder_path", value = folder_path)
      }
    }
  })
  
  upd <- function(...) session$sendCustomMessage("upd", list(...))
  log <- function(m, t="i") upd(lg = paste0("<div class='",t,"'><i class='fa fa-", 
    if(t=="s") "check" else if(t=="e") "times" else "info-circle", "'></i> ",m,"</div>"))
  
  # Preview section UI
  output$preview_section <- renderUI({
    if (!rv$show_preview || length(rv$items) == 0) return(NULL)
    
    div(class="box",
      div(class="selection-bar",
        span(id="selection-count", class="selection-info", 
          paste0(length(rv$selected), " of ", length(rv$items), " selected")),
        div(
          actionButton("select_all", tagList(tags$i(class="fa fa-check-square"), " Select All"), class="btn"),
          actionButton("deselect_all", tagList(tags$i(class="fa fa-square"), " Deselect All"), class="btn"),
          actionButton("download_selected", tagList(tags$i(class="fa fa-download"), " Download Selected"), class="btn p",
            disabled = if(length(rv$selected) == 0) "disabled" else NULL)
        )
      ),
      div(class="preview-grid",
        lapply(seq_along(rv$items), function(i) {
          item <- rv$items[[i]]
          is_selected <- i %in% rv$selected
          
          # Determine icon based on type
          icon_class <- switch(item$type,
            "video" = "fa-film",
            "gif" = "fa-images",
            "fa-image"
          )
          
          div(class=paste("preview-item", if(is_selected) "selected" else ""),
            `data-index` = i,
            onclick = paste0("Shiny.setInputValue('toggle_item', ", i, ", {priority: 'event'})"),
            div(class=paste("preview-check", if(is_selected) "checked" else ""),
              if(is_selected) tags$i(class="fa fa-check") else NULL
            ),
            span(class="preview-type", item$type),
            div(class="preview-thumb",
              if (!is.null(item$preview) && nchar(item$preview) > 0) {
                tags$img(src = item$preview, loading = "lazy", 
                  onerror = "this.style.display='none'; this.nextElementSibling.style.display='flex';")
              } else NULL,
              div(class="preview-icon", style = if(!is.null(item$preview) && nchar(item$preview) > 0) "display:none" else "",
                tags$i(class = paste("fa", icon_class)))
            ),
            div(class="preview-info", title = item$title, item$title)
          )
        })
      )
    )
  })
  
  # Toggle individual item
  observeEvent(input$toggle_item, {
    i <- input$toggle_item
    if (i %in% rv$selected) {
      rv$selected <- rv$selected[rv$selected != i]
      session$sendCustomMessage("toggleItem", list(index = i, selected = FALSE))
    } else {
      rv$selected <- c(rv$selected, i)
      session$sendCustomMessage("toggleItem", list(index = i, selected = TRUE))
    }
    session$sendCustomMessage("updateSelectionCount", list(count = length(rv$selected), total = length(rv$items)))
  })
  
  # Select all
  observeEvent(input$select_all, {
    rv$selected <- seq_along(rv$items)
    for (i in seq_along(rv$items)) {
      session$sendCustomMessage("toggleItem", list(index = i, selected = TRUE))
    }
    session$sendCustomMessage("updateSelectionCount", list(count = length(rv$selected), total = length(rv$items)))
  })
  
  # Deselect all
  observeEvent(input$deselect_all, {
    for (i in rv$selected) {
      session$sendCustomMessage("toggleItem", list(index = i, selected = FALSE))
    }
    rv$selected <- c()
    session$sendCustomMessage("updateSelectionCount", list(count = 0, total = length(rv$items)))
  })
  
  # Scan function
  scan_next <- function() {
    tryCatch({
      if (!e$run || e$pg >= e$maxpg) {
        # Scanning complete
        e$scanning <- FALSE
        e$found <- length(e$items)
        rv$items <- e$items
        rv$selected <- seq_along(e$items)  # Select all by default
        rv$show_preview <- TRUE
        upd(n1 = e$found, st = paste("Found", e$found, "media files - Select and download"))
        log(paste("Scan complete.", e$found, "files found"), "s")
        return()
      }
      
      e$pg <- e$pg + 1
      upd(st = paste("Scanning page", e$pg, "/", e$maxpg))
      log(paste("Scanning page", e$pg))
      
      data <- fetch_page(e$base, e$after)
      
      # Check if we got valid data
      if (is.null(data) || is.null(data$data) || is.null(data$data$children) || length(data$data$children) == 0) {
        # No more pages available - finish scan
        e$found <- length(e$items)
        rv$items <- e$items
        rv$selected <- seq_along(e$items)
        rv$show_preview <- TRUE
        upd(n1 = e$found, st = paste("Found", e$found, "media files - Select and download"))
        log(paste("Reached end of available pages. Scan complete.", e$found, "files found"), "s")
        e$scanning <- FALSE
        return()
      }
      
      for (child in data$data$children) {
        if (!is.null(child$data)) {
          new_items <- get_media(child$data)
          e$items <- append(e$items, new_items)
        }
      }
      
      # Remove duplicates
      if (length(e$items) > 0) {
        norm <- sapply(e$items, function(x) normalize_url(gsub("^REDGIFS:", "", x$url)))
        e$items <- e$items[!duplicated(norm)]
      }
      
      upd(n1 = length(e$items))
      
      # Check for next page token
      e$after <- data$data$after
      if (is.null(e$after) || is.na(e$after) || e$after == "") {
        # No more pages - stop scanning
        e$pg <- e$maxpg
      }
      
      later::later(scan_next, 0.5)
    }, error = function(err) {
      # Handle any unexpected errors gracefully
      log(paste("Scan error:", err$message), "e")
      e$found <- length(e$items)
      rv$items <- e$items
      rv$selected <- seq_along(e$items)
      rv$show_preview <- TRUE
      upd(n1 = e$found, st = paste("Found", e$found, "media files - Select and download"))
      log(paste("Scan stopped due to error.", e$found, "files found"), "s")
      e$scanning <- FALSE
    })
  }
  
  # Download function
  dl_next <- function() {
    if (!e$run || e$i > length(e$download_list)) {
      upd(st = paste("Download complete:", e$done, "files saved"))
      log(paste("Download finished!", e$done, "successful,", e$fail, "failed"), "s")
      e$downloading <- FALSE
      return()
    }
    
    idx <- e$download_list[e$i]
    item <- e$items_for_download[[idx]]
    url <- item$url
    is_redgifs <- grepl("^REDGIFS:", url)
    
    if (is_redgifs) {
      redgifs_url <- gsub("^REDGIFS:", "", url)
      upd(st = paste0(e$i, "/", length(e$download_list), " - Fetching redgifs..."))
      log(paste("Processing Redgifs:", basename(redgifs_url)), "i")
      
      result <- get_redgifs_url(redgifs_url)
      
      if (is.null(result) || is.null(result$url)) {
        e$fail <- e$fail + 1
        upd(n3 = e$fail)
        log("Redgifs fetch failed", "e")
        pct <- round((e$i / length(e$download_list)) * 100)
        upd(n4 = pct, pb = pct)
        e$i <- e$i + 1
        later::later(dl_next, 0.3)
        return()
      }
      
      url <- result$url
      fn <- paste0(result$id, ".mp4")
    } else {
      fn <- fname(url, e$i)
    }
    
    fp <- file.path(e$dir, fn)
    
    # Handle duplicate filenames
    if (file.exists(fp)) {
      bn <- tools::file_path_sans_ext(fn)
      ex <- tools::file_ext(fn)
      cnt <- 1
      while (file.exists(fp)) {
        fn <- paste0(bn, "_", cnt, ".", ex)
        fp <- file.path(e$dir, fn)
        cnt <- cnt + 1
      }
    }
    
    upd(st = paste0(e$i, "/", length(e$download_list), " - ", fn))
    
    ok <- dl(url, fp, is_redgifs)
    if (ok) {
      e$done <- e$done + 1
      upd(n2 = e$done)
      log(fn, "s")
    } else {
      e$fail <- e$fail + 1
      upd(n3 = e$fail)
      if (file.exists(fp)) file.remove(fp)
      log(paste("Failed:", fn), "e")
    }
    
    pct <- round((e$i / length(e$download_list)) * 100)
    upd(n4 = pct, pb = pct)
    e$i <- e$i + 1
    
    later::later(dl_next, 0.2)
  }
  
  # Scan button
  observeEvent(input$scan, {
    req(input$url)
    
    e$run <- TRUE
    e$scanning <- TRUE
    e$items <- list()
    e$i <- 0
    e$pg <- 0
    e$after <- NULL
    e$found <- 0
    e$done <- 0
    e$fail <- 0
    
    rv$items <- list()
    rv$selected <- c()
    rv$show_preview <- FALSE
    
    upd(n1 = 0, n2 = 0, n3 = 0, n4 = 0, pb = 0, st = "Scanning...")
    session$sendCustomMessage("upd", list(lg = ""))
    
    sub <- str_match(input$url, "/r/([^/]+)")[1, 2]
    if (is.na(sub)) sub <- "reddit"
    
    e$base <- input$url
    if (!grepl("\\.json", e$base)) e$base <- paste0(gsub("/$", "", e$base), "/.json")
    e$maxpg <- input$pages
    
    log(paste("Target:", sub))
    log(paste("Scanning", e$maxpg, "pages..."))
    
    later::later(scan_next, 0.2)
  })
  
  # Download selected button
  observeEvent(input$download_selected, {
    req(length(rv$selected) > 0)
    
    e$run <- TRUE
    e$downloading <- TRUE
    e$download_list <- rv$selected
    e$items_for_download <- rv$items  # Copy items to environment for async access
    e$i <- 1
    e$done <- 0
    e$fail <- 0
    
    # Get download folder
    e$dir <- input$folder_path
    if (is.null(e$dir) || e$dir == "") {
      e$dir <- getwd()
    }
    
    # Create subfolder with subreddit name and timestamp
    sub <- str_match(input$url, "/r/([^/]+)")[1, 2]
    if (is.na(sub)) sub <- "reddit"
    e$dir <- file.path(e$dir, paste0(sub, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    dir.create(e$dir, recursive = TRUE, showWarnings = FALSE)
    
    upd(n2 = 0, n3 = 0, n4 = 0, pb = 0, st = "Starting download...")
    log(paste("Downloading", length(rv$selected), "files to", e$dir))
    
    later::later(dl_next, 0.2)
  })
  
  # Stop button
  observeEvent(input$stop, {
    e$run <- FALSE
    upd(st = "Stopped")
    log("Operation stopped", "e")
  })
}

shinyApp(ui, server)
