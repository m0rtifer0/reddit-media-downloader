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
  
  # Safety check - ensure p is a list
 if (is.null(p) || !is.list(p)) {
    return(items)
  }
  
  # Extract common metadata with tryCatch for safety
  author <- tryCatch({
    if (!is.null(p$author) && is.character(p$author)) p$author else "unknown"
  }, error = function(e) "unknown")
  
  created_utc <- tryCatch({
    if (!is.null(p$created_utc) && is.numeric(p$created_utc)) p$created_utc else 0
  }, error = function(e) 0)
  
  # Extract upvotes
  upvotes <- tryCatch({
    if (!is.null(p$ups) && is.numeric(p$ups)) p$ups else if (!is.null(p$score) && is.numeric(p$score)) p$score else 0
  }, error = function(e) 0)
  
  post_date <- tryCatch({
    format(as.POSIXct(created_utc, origin = "1970-01-01", tz = "UTC"), "%Y%m%d")
  }, error = function(e) format(Sys.Date(), "%Y%m%d"))
  
  # Safely get URL
  url <- tryCatch({
    if (!is.null(p$url) && is.character(p$url)) p$url else NULL
  }, error = function(e) NULL)
  
  if (!is.null(url)) {
    preview <- NULL
    title <- tryCatch({
      if (!is.null(p$title) && is.character(p$title)) substr(p$title, 1, 50) else "Media"
    }, error = function(e) "Media")
    
    # Try to get preview from post
    tryCatch({
      if (!is.null(p$preview$images[[1]]$source$url)) {
        preview <- gsub("&amp;", "&", p$preview$images[[1]]$source$url)
      }
    }, error = function(e) NULL)
    
    if (grepl("i\\.redd\\.it/", url)) {
      items <- append(items, list(list(url = url, preview = if (is.null(preview)) url else preview, title = title, type = "image", author = author, date = post_date, upvotes = upvotes)))
    } else if (grepl("\\.(jpg|jpeg|png|gif|gifv|webp|mp4)($|\\?)", url, ignore.case = TRUE)) {
      ftype <- if (grepl("\\.(mp4|gifv)($|\\?)", url, ignore.case = TRUE)) "video" else "image"
      items <- append(items, list(list(url = url, preview = preview, title = title, type = ftype, author = author, date = post_date, upvotes = upvotes)))
    } else if (grepl("redgifs\\.com/(watch|ifr)/", url, ignore.case = TRUE)) {
      items <- append(items, list(list(url = paste0("REDGIFS:", url), preview = NULL, title = title, type = "video", author = author, date = post_date, upvotes = upvotes)))
    } else if (grepl("gfycat\\.com/", url, ignore.case = TRUE)) {
      items <- append(items, list(list(url = paste0("REDGIFS:", url), preview = NULL, title = title, type = "video", author = author, date = post_date, upvotes = upvotes)))
    }
  }
  
  # Check for Reddit video
  is_video <- tryCatch(isTRUE(p$is_video), error = function(e) FALSE)
  if (is_video) {
    video_url <- NULL
    preview <- NULL
    title <- tryCatch({
      if (!is.null(p$title) && is.character(p$title)) substr(p$title, 1, 50) else "Video"
    }, error = function(e) "Video")
    
    tryCatch({
      if (!is.null(p$preview$images[[1]]$source$url)) {
        preview <- gsub("&amp;", "&", p$preview$images[[1]]$source$url)
      }
    }, error = function(e) NULL)
    
    tryCatch({
      if (!is.null(p$media$reddit_video$fallback_url)) {
        video_url <- p$media$reddit_video$fallback_url
      } else if (!is.null(p$secure_media$reddit_video$fallback_url)) {
        video_url <- p$secure_media$reddit_video$fallback_url
      }
    }, error = function(e) NULL)
    
    if (!is.null(video_url)) {
      video_url <- gsub("\\?source=fallback", "", video_url)
      items <- append(items, list(list(url = video_url, preview = preview, title = title, type = "video", author = author, date = post_date, upvotes = upvotes)))
    }
  }
  
  # Check for reddit video preview
  tryCatch({
    if (!is.null(p$preview$reddit_video_preview$fallback_url)) {
      preview <- NULL
      if (!is.null(p$preview$images[[1]]$source$url)) {
        preview <- gsub("&amp;", "&", p$preview$images[[1]]$source$url)
      }
      items <- append(items, list(list(
        url = gsub("\\?source=fallback", "", p$preview$reddit_video_preview$fallback_url),
        preview = preview,
        title = tryCatch(if (!is.null(p$title)) substr(p$title, 1, 50) else "Video Preview", error = function(e) "Video Preview"),
        type = "video",
        author = author,
        date = post_date,
        upvotes = upvotes
      )))
    }
  }, error = function(e) NULL)
  
  # Check for gallery
  is_gallery <- tryCatch(isTRUE(p$is_gallery), error = function(e) FALSE)
  has_metadata <- tryCatch(!is.null(p$media_metadata) && is.list(p$media_metadata), error = function(e) FALSE)
  
  if (is_gallery && has_metadata) {
    title <- tryCatch({
      if (!is.null(p$title) && is.character(p$title)) substr(p$title, 1, 50) else "Gallery"
    }, error = function(e) "Gallery")
    idx <- 1
    for (item in p$media_metadata) {
      tryCatch({
        if (!is.null(item$s)) {
          if (!is.null(item$s$u)) {
            img_url <- gsub("&amp;", "&", item$s$u)
            img_url <- gsub("preview\\.redd\\.it", "i.redd.it", img_url)
            img_url <- gsub("\\?.*$", "", img_url)
            items <- append(items, list(list(url = img_url, preview = gsub("&amp;", "&", item$s$u), title = paste0(title, " (", idx, ")"), type = "image", author = author, date = post_date, upvotes = upvotes)))
          } else if (!is.null(item$s$gif)) {
            items <- append(items, list(list(url = gsub("&amp;", "&", item$s$gif), preview = NULL, title = paste0(title, " (", idx, ")"), type = "gif", author = author, date = post_date, upvotes = upvotes)))
          } else if (!is.null(item$s$mp4)) {
            items <- append(items, list(list(url = gsub("&amp;", "&", item$s$mp4), preview = NULL, title = paste0(title, " (", idx, ")"), type = "video", author = author, date = post_date, upvotes = upvotes)))
          }
          idx <- idx + 1
        }
      }, error = function(e) NULL)
    }
  }
  
  # Check for preview images
  tryCatch({
    if (!is.null(p$preview$images) && is.list(p$preview$images)) {
      for (img in p$preview$images) {
        if (!is.null(img$variants$mp4$source$url)) {
          mp4_url <- gsub("&amp;", "&", img$variants$mp4$source$url)
          items <- append(items, list(list(url = mp4_url, preview = NULL, title = "Animated", type = "video", author = author, date = post_date, upvotes = upvotes)))
        } else if (!is.null(img$variants$gif$source$url)) {
          gif_url <- gsub("&amp;", "&", img$variants$gif$source$url)
          items <- append(items, list(list(url = gif_url, preview = NULL, title = "GIF", type = "gif", author = author, date = post_date, upvotes = upvotes)))
        }
      }
    }
  }, error = function(e) NULL)
  
  # Convert gifv to mp4
  for (i in seq_along(items)) {
    items[[i]]$url <- gsub("\\.gifv$", ".mp4", items[[i]]$url, ignore.case = TRUE)
  }
  
  # Remove duplicates based on normalized URL and title
  if (length(items) > 0) {
    # Create unique key from URL base and title
    keys <- sapply(items, function(x) {
      url_norm <- normalize_url(gsub("^REDGIFS:", "", x$url))
      # Extract filename/id from URL for comparison
      url_base <- basename(gsub("\\?.*$", "", url_norm))
      paste0(x$title, "|", url_base)
    })
    items <- items[!duplicated(keys)]
    
    # Also deduplicate by title alone for same post (keep first occurrence)
    titles <- sapply(items, function(x) x$title)
    items <- items[!duplicated(titles)]
  }
  
  items
}

# Generate filename from metadata: {date}_{author}_{title}.ext
fname <- function(url, i, item = NULL) {
  # Determine extension from URL
  n <- gsub("\\?.*$", "", basename(url))
  if (grepl("DASH_\\d+", n) || grepl("\\.mp4|DASH|redgifs", url, TRUE)) {
    ext <- ".mp4"
  } else if (grepl("\\.gif", url, TRUE)) {
    ext <- ".gif"
  } else if (grepl("\\.png", url, TRUE)) {
    ext <- ".png"
  } else if (grepl("\\.webp", url, TRUE)) {
    ext <- ".webp"
  } else if (nchar(n) >= 3 && grepl("\\.", n)) {
    ext <- paste0(".", tools::file_ext(n))
  } else {
    ext <- ".jpg"
  }
  
  # Use metadata if available
  if (!is.null(item) && !is.null(item$date) && !is.null(item$author) && !is.null(item$title)) {
    # Clean title for filename (remove special chars, limit length)
    clean_title <- gsub("[<>:\"/\\\\|?*]", "", item$title)
    clean_title <- gsub("\\s+", "_", clean_title)
    clean_title <- substr(clean_title, 1, 40)  # Limit title length
    clean_title <- gsub("_+$", "", clean_title)  # Remove trailing underscores
    
    clean_author <- gsub("[<>:\"/\\\\|?*]", "_", item$author)
    
    # Format: date_author_title.ext
    filename <- paste0(item$date, "_", clean_author, "_", clean_title, ext)
  } else {
    # Fallback to old naming
    if (grepl("DASH_\\d+", n)) {
      filename <- paste0("video_", sprintf("%04d", i), ext)
    } else if (nchar(n) < 3 || !grepl("\\.", n)) {
      filename <- paste0("media_", sprintf("%04d", i), ext)
    } else {
      filename <- n
    }
  }
  
  # Clean any remaining invalid chars and convert gifv
  filename <- gsub("\\.gifv$", ".mp4", filename, ignore.case = TRUE)
  filename <- gsub("[<>:\"/\\\\|?*]", "_", filename)
  filename
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
      @import url('https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&family=Space+Grotesk:wght@500;700&display=swap');
      
      *{margin:0;padding:0;box-sizing:border-box}
      body{background:#F0F0EB;color:#222222;font-family:'Space Mono',monospace;font-size:14px}
      
      /* Header bar */
      .bar{background:#FFFFFF;border:2px solid #000000;padding:12px 20px;display:flex;align-items:center;gap:15px;box-shadow:4px 4px 0px 0px #000000;margin:15px 15px 0 15px}
      .dots{display:flex;gap:8px}
      .dot{width:14px;height:14px;border:2px solid #000000}
      .r{background:#FF6B6B}.y{background:#FFE066}.g{background:#7CB342}
      .title{font-family:'Space Grotesk',sans-serif;font-size:14px;font-weight:700;color:#222222;flex:1;text-align:center;letter-spacing:1px;text-transform:uppercase}
      
      /* Main container */
      .main{padding:20px 15px;max-width:1200px;margin:0 auto}
      
      /* Box cards */
      .box{background:#FFFFFF;border:2px solid #000000;padding:20px;margin-bottom:20px;box-shadow:4px 4px 0px 0px #000000}
      
      /* Headers */
      h1{font-family:'Space Grotesk',sans-serif;font-size:22px;font-weight:700;letter-spacing:2px;text-transform:uppercase;text-align:center;color:#222222}
      
      /* Labels */
      label{display:block;font-size:12px;text-transform:uppercase;letter-spacing:1px;color:#222222;margin-bottom:8px;font-weight:700}
      
      /* Text inputs */
      input[type=text]{width:100%;background:#FFFFFF;border:2px solid #000000;color:#222222;padding:12px;font-family:'Space Mono',monospace;font-size:14px;box-shadow:3px 3px 0px 0px #000000}
      input:focus{outline:none;border-color:#4F46E5;box-shadow:3px 3px 0px 0px #4F46E5}
      
      /* Select inputs */
      select,.selectize-input{width:100%;background:#FFFFFF;border:2px solid #000000;color:#222222;padding:12px;font-family:'Space Mono',monospace;font-size:14px;box-shadow:3px 3px 0px 0px #000000;cursor:pointer;-webkit-appearance:none;appearance:none;background-image:url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='12' height='12' viewBox='0 0 12 12'%3E%3Cpath fill='%23222' d='M6 8L1 3h10z'/%3E%3C/svg%3E\");background-repeat:no-repeat;background-position:right 12px center}
      select:focus,.selectize-input.focus{outline:none;border-color:#4F46E5;box-shadow:3px 3px 0px 0px #4F46E5}
      .selectize-dropdown{border:2px solid #000000;background:#FFFFFF;box-shadow:3px 3px 0px 0px #000000}
      .selectize-dropdown-content .option{padding:10px 12px;font-family:'Space Mono',monospace}
      .selectize-dropdown-content .option.active{background:#4F46E5;color:#FFFFFF}
      
      /* Button container */
      .btns{display:flex;gap:12px;margin-top:15px;flex-wrap:wrap}
      
      /* Buttons */
      .btn{padding:12px 24px;border:2px solid #000000;background:#FFFFFF;color:#222222;font-family:'Space Mono',monospace;font-size:12px;font-weight:700;text-transform:uppercase;letter-spacing:1px;cursor:pointer;box-shadow:4px 4px 0px 0px #000000;transition:transform 0.1s,box-shadow 0.1s}
      .btn:hover{background:#F5F5F0}
      .btn:active{transform:translate(4px,4px);box-shadow:0px 0px 0px 0px #000000}
      .btn.p{background:#4F46E5;color:#FFFFFF;border-color:#000000}
      .btn.p:hover{background:#4338CA}
      .btn.p:active{transform:translate(4px,4px);box-shadow:0px 0px 0px 0px #000000}
      .btn:disabled{opacity:0.5;cursor:not-allowed;transform:none;box-shadow:4px 4px 0px 0px #000000}
      
      /* Stats grid */
      .stats{display:grid;grid-template-columns:repeat(5,1fr);gap:12px}
      .stat{background:#FFFFFF;border:2px solid #000000;padding:15px;text-align:center;box-shadow:3px 3px 0px 0px #000000}
      .stat b{font-family:'Space Grotesk',sans-serif;font-size:28px;display:block;color:#222222}
      .stat span{font-size:10px;color:#666666;text-transform:uppercase;letter-spacing:1px}
      
      /* Progress bar */
      .pbar{height:8px;background:#E5E5E0;border:2px solid #000000;margin:15px 0}
      .pfill{height:100%;background:#4F46E5;width:0%;transition:width .2s}
      
      /* Status text */
      .status{font-size:12px;color:#666666;text-align:center;margin-top:10px}
      
      /* Log area */
      .log{max-height:150px;overflow-y:auto;font-size:12px;line-height:1.8;padding-top:12px;border-top:2px solid #000000;margin-top:12px}
      .s{color:#7CB342}.e{color:#E53935}.i{color:#666666}
      
      /* Shiny input overrides */
      .shiny-input-container{width:100%!important}
      .irs--shiny .irs-bar,.irs--shiny .irs-handle{background:#4F46E5;border:2px solid #000000}
      .irs--shiny .irs-line{background:#E5E5E0;border:2px solid #000000}
      .irs--shiny .irs-handle{box-shadow:2px 2px 0px 0px #000000}
      
      /* Scan All checkbox styling */
      #scan_all{width:18px;height:18px;margin:0;cursor:pointer;accent-color:#4F46E5}
      .checkbox label{margin:0;padding:0}
      
      /* Sort mode checkboxes */
      .sort-check{background:#FFFFFF;border:2px solid #000;padding:6px 8px;font-size:11px}
      .sort-check .shiny-input-container{width:auto!important;margin:0}
      .sort-check .checkbox{margin:0}
      .sort-check label{font-size:11px;font-weight:700;cursor:pointer}
      .sort-check input[type=checkbox]{width:14px;height:14px;accent-color:#4F46E5;cursor:pointer}
      
      /* Preview grid */
      .preview-grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(180px,1fr));gap:15px;margin-top:15px;max-height:500px;overflow-y:auto;padding:5px}
      
      /* Preview items */
      .preview-item{background:#FFFFFF;border:2px solid #000000;padding:10px;position:relative;transition:transform 0.1s,box-shadow 0.1s;cursor:pointer;box-shadow:3px 3px 0px 0px #000000}
      .preview-item:hover{transform:translate(-2px,-2px);box-shadow:5px 5px 0px 0px #000000}
      .preview-item.selected{border-color:#7CB342;background:#F5FAF0;box-shadow:3px 3px 0px 0px #7CB342}
      .preview-item.failed{border-color:#E53935;background:#FFF5F5;box-shadow:3px 3px 0px 0px #E53935}
      
      /* Preview thumbnail */
      .preview-thumb{width:100%;height:120px;object-fit:cover;background:#E5E5E0;border:2px solid #000000;display:flex;align-items:center;justify-content:center;cursor:pointer}
      .preview-thumb img{max-width:100%;max-height:100%;object-fit:contain}
      .preview-icon{font-size:40px;color:#999999}
      
      /* Preview info */
      .preview-info{margin-top:10px;font-size:11px;color:#222222;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
      
      /* Preview type badge */
      .preview-type{position:absolute;top:8px;right:8px;background:#222222;color:#FFFFFF;padding:3px 8px;font-size:10px;text-transform:uppercase;font-weight:700;border:2px solid #000000}
      
      /* Preview upvotes badge */
      .preview-upvotes{position:absolute;top:8px;right:65px;background:#FF5722;color:#FFFFFF;padding:3px 8px;font-size:10px;font-weight:700;border:2px solid #000000;display:flex;align-items:center;gap:4px}
      .preview-upvotes i{font-size:10px}
      
      /* Preview checkbox */
      .preview-check{position:absolute;top:8px;left:8px;width:22px;height:22px;background:#FFFFFF;border:2px solid #000000;cursor:pointer;display:flex;align-items:center;justify-content:center;z-index:10;box-shadow:2px 2px 0px 0px #000000}
      .preview-check.checked{background:#7CB342;border-color:#000000}
      .preview-check i{color:#FFFFFF;font-size:12px}
      
      /* Preview view button */
      .preview-view{position:absolute;bottom:50px;right:8px;width:26px;height:26px;background:#FFFFFF;border:2px solid #000000;cursor:pointer;display:flex;align-items:center;justify-content:center;z-index:10;box-shadow:2px 2px 0px 0px #000000;transition:transform 0.1s,box-shadow 0.1s}
      .preview-view:hover{transform:translate(-1px,-1px);box-shadow:3px 3px 0px 0px #000000}
      .preview-view:active{transform:translate(2px,2px);box-shadow:0px 0px 0px 0px #000000}
      .preview-view i{color:#222222;font-size:11px}
      
      /* Folder input */
      .folder-input{display:flex;gap:12px;align-items:flex-end}
      .folder-input .shiny-input-container{flex:1}
      .folder-btn{height:42px;white-space:nowrap}
      
      /* Selection bar */
      .selection-bar{display:flex;justify-content:space-between;align-items:center;padding:12px 0;border-bottom:2px solid #000000;margin-bottom:12px;flex-wrap:wrap;gap:12px}
      .selection-info{font-size:13px;color:#222222;font-weight:700}
      .selection-buttons{display:flex;gap:8px;flex-wrap:wrap}
      
      /* Media modal */
      .media-modal{position:fixed;top:0;left:0;width:100%;height:100%;background:rgba(0,0,0,0.85);z-index:1000;display:none;align-items:center;justify-content:center}
      .media-modal.active{display:flex}
      .media-modal-content{max-width:90%;max-height:90%;position:relative;border:4px solid #000000;background:#FFFFFF;padding:10px;box-shadow:8px 8px 0px 0px #000000}
      .media-modal-content img,.media-modal-content video{max-width:100%;max-height:90vh;object-fit:contain}
      .media-modal-close{position:absolute;top:-40px;right:0;color:#FFFFFF;font-size:28px;cursor:pointer;font-weight:700}
    "))
  ),
  div(class="bar",div(class="dots",span(class="dot r"),span(class="dot y"),span(class="dot g")),span(class="title","REDDIT MEDIA DOWNLOADER")),
  div(class="main",
    div(class="box",h1("REDDIT DOWNLOADER"),
      div(style="text-align:center;margin-top:8px;font-size:12px;color:#666",
        tags$i(class="fa fa-users"), " Subreddits ", tags$span(style="margin:0 8px","•"),
        tags$i(class="fa fa-user"), " User Profiles ", tags$span(style="margin:0 8px","•"),
        tags$i(class="fa fa-images"), " Galleries"
      )
    ),
    div(class="box",
      tags$label(tags$i(class="fa fa-link")," Reddit URL (Subreddit or User)"),
      textInput("url",NULL,placeholder="r/pics or u/username or full URL",width="100%"),
      div(style="font-size:11px;color:#666;margin-top:5px",
        "Examples: ", tags$code("https://reddit.com/r/pics"), ", ",
        tags$code("https://reddit.com/user/username"), ", ",
        tags$code("r/earthporn"), ", ",
        tags$code("u/gallowboob")
      ),
      
      tags$label(style="margin-top:10px",tags$i(class="fa fa-sort")," Sort By"),
      selectInput("sort_by", NULL, choices = list(
        "Hot" = "hot",
        "New" = "new",
        "Rising" = "rising",
        "Top - All Time" = "top_all",
        "Top - Year" = "top_year",
        "Top - Month" = "top_month",
        "Top - Week" = "top_week",
        "Top - Day" = "top_day"
      ), selected = "hot", width = "100%"),
      
      tags$label(style="margin-top:10px",tags$i(class="fa fa-folder")," Download Folder"),
      div(class="folder-input",
        textInput("folder_path", NULL, value = getwd(), width = "100%"),
        shinyDirButton("folder_select", "Browse", "Select Download Folder", class="btn folder-btn")
      ),
      
      tags$label(style="margin-top:10px",tags$i(class="fa fa-list")," Pages to Scan"),
      div(style="display:flex;align-items:center;gap:15px;margin-bottom:10px",
        div(style="flex:1", 
          conditionalPanel(
            condition = "!input.scan_all",
            sliderInput("pages",NULL,1,1000,10,width="100%")
          ),
          conditionalPanel(
            condition = "input.scan_all",
            div(style="padding:12px;background:#F5F5F0;border:2px solid #000;font-weight:700",
              div(style="text-align:center;margin-bottom:10px",
                tags$i(class="fa fa-infinity"), " FULL SCAN: Select sort modes to search"
              ),
              div(style="display:grid;grid-template-columns:repeat(4,1fr);gap:8px",
                div(class="sort-check",
                  checkboxInput("sort_new", "New", value = TRUE, width = "100%")
                ),
                div(class="sort-check",
                  checkboxInput("sort_hot", "Hot", value = TRUE, width = "100%")
                ),
                div(class="sort-check",
                  checkboxInput("sort_rising", "Rising", value = TRUE, width = "100%")
                ),
                div(class="sort-check",
                  checkboxInput("sort_top_all", "Top All", value = TRUE, width = "100%")
                ),
                div(class="sort-check",
                  checkboxInput("sort_top_year", "Top Year", value = TRUE, width = "100%")
                ),
                div(class="sort-check",
                  checkboxInput("sort_top_month", "Top Month", value = TRUE, width = "100%")
                ),
                div(class="sort-check",
                  checkboxInput("sort_top_week", "Top Week", value = FALSE, width = "100%")
                ),
                div(class="sort-check",
                  checkboxInput("sort_top_day", "Top Day", value = FALSE, width = "100%")
                )
              )
            )
          )
        ),
        div(style="display:flex;align-items:center;gap:8px;padding:10px 15px;background:#FFFFFF;border:2px solid #000;box-shadow:3px 3px 0px 0px #000;cursor:pointer",
          checkboxInput("scan_all", NULL, value = FALSE, width = "auto"),
          tags$label(`for`="scan_all", style="cursor:pointer;font-weight:700;font-size:12px;margin:0", "FULL SCAN")
        )
      ),
      div(style="font-size:11px;color:#666;margin-top:5px;margin-bottom:10px",
        tags$i(class="fa fa-info-circle"), " Full Scan searches selected sort modes to find maximum media. Duplicates are automatically skipped."
      ),
      
      # Upvote Filter Section
      div(style="margin-top:15px;padding:12px;background:#FFF8E1;border:2px solid #000;box-shadow:3px 3px 0px 0px #000",
        div(style="display:flex;align-items:center;gap:8px;margin-bottom:10px",
          tags$i(class="fa fa-arrow-up", style="color:#FF5722"),
          tags$b("Upvote Filter"),
          span(style="font-size:11px;color:#666", "(optional)")
        ),
        div(style="display:grid;grid-template-columns:1fr 1fr;gap:10px",
          div(
            tags$label(style="font-size:11px;font-weight:600", "Min Upvotes"),
            textInput("min_upvotes", NULL, value = "", placeholder = "e.g. 500", width = "100%")
          ),
          div(
            tags$label(style="font-size:11px;font-weight:600", "Max Upvotes"),
            textInput("max_upvotes", NULL, value = "", placeholder = "e.g. 10000", width = "100%")
          )
        ),
        div(style="font-size:10px;color:#666;margin-top:5px",
          tags$i(class="fa fa-info-circle"), " Leave empty for no limit. Use both for range filter."
        )
      ),
      
      div(class="btns",
        actionButton("scan",tagList(tags$i(class="fa fa-search")," SCAN"),class="btn p"),
        actionButton("stop",tagList(tags$i(class="fa fa-stop")," STOP"),class="btn"),
        actionButton("clear_history",tagList(tags$i(class="fa fa-trash")," CLEAR HISTORY"),class="btn")
      )
    ),
    
    # Preview section (hidden until scan)
    uiOutput("preview_section"),
    
    div(class="box",
      div(class="stats",
        div(class="stat",tags$b(id="n1","0"),span("found")),
        div(class="stat",tags$b(id="n5","0"),span("skipped")),
        div(class="stat",tags$b(id="n2","0"),span("downloaded")),
        div(class="stat",tags$b(id="n3","0"),span("failed")),
        div(class="stat",tags$b(id="n4","0%"),span("progress"))
      ),
      div(class="pbar",div(id="pb",class="pfill")),
      div(class="status",id="st","Ready"),
      div(class="log",id="lg")
    )
  ),
  # Media preview modal
  div(id="media-modal", class="media-modal", onclick="closeMediaModal(event)",
    div(class="media-modal-content", onclick="event.stopPropagation()",
      span(class="media-modal-close", onclick="closeMediaModal()", tags$i(class="fa fa-times")),
      div(id="media-modal-body")
    )
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('upd', function(d) {
      if(d.n1 !== undefined) document.getElementById('n1').textContent = d.n1;
      if(d.n2 !== undefined) document.getElementById('n2').textContent = d.n2;
      if(d.n3 !== undefined) document.getElementById('n3').textContent = d.n3;
      if(d.n4 !== undefined) document.getElementById('n4').textContent = d.n4 + '%';
      if(d.n5 !== undefined) document.getElementById('n5').textContent = d.n5;
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
    
    // Toggle item without causing scroll/re-render
    function toggleItemLocal(index, event) {
      if (event) event.stopPropagation();
      Shiny.setInputValue('toggle_item', index, {priority: 'event'});
    }
    
    // View media in modal popup
    function viewMedia(url, type, event) {
      if (event) event.stopPropagation();
      var modal = document.getElementById('media-modal');
      var body = document.getElementById('media-modal-body');
      
      // Clear previous content
      body.innerHTML = '';
      
      // Determine media type from URL
      var isVideo = type === 'video' || url.match(/[.](mp4|webm|mov)$/i) || url.includes('v.redd.it');
      var isGif = type === 'gif' || url.match(/[.]gif$/i);
      
      if (isVideo) {
        var video = document.createElement('video');
        video.src = url;
        video.controls = true;
        video.autoplay = true;
        video.style.maxWidth = '100%';
        video.style.maxHeight = '80vh';
        video.style.border = '2px solid #000000';
        video.style.background = '#000';
        
        // Error handler for unsupported video formats
        video.onerror = function() {
          body.innerHTML = '';
          var errorDiv = document.createElement('div');
          errorDiv.style.cssText = 'text-align:center;padding:40px;font-family:Space Mono,monospace;';
          errorDiv.innerHTML = '<p style=\"margin-bottom:20px;color:#222;\">Video format not supported in browser preview.</p>' +
            '<a href=\"' + url + '\" target=\"_blank\" style=\"display:inline-block;padding:12px 24px;background:#4F46E5;color:#fff;text-decoration:none;border:2px solid #000;box-shadow:4px 4px 0 #000;font-weight:700;\">OPEN IN NEW TAB</a>';
          body.appendChild(errorDiv);
        };
        
        body.appendChild(video);
      } else {
        var img = document.createElement('img');
        img.src = url;
        img.style.maxWidth = '100%';
        img.style.maxHeight = '80vh';
        img.style.border = '2px solid #000000';
        body.appendChild(img);
      }
      
      modal.classList.add('active');
      document.body.style.overflow = 'hidden';
    }
    
    // Close media modal
    function closeMediaModal(event) {
      if (event && event.target.id !== 'media-modal') return;
      var modal = document.getElementById('media-modal');
      var body = document.getElementById('media-modal-body');
      modal.classList.remove('active');
      document.body.style.overflow = '';
      // Stop video if playing
      var video = body.querySelector('video');
      if (video) video.pause();
      body.innerHTML = '';
    }
    
    // Close modal on Escape key
    document.addEventListener('keydown', function(e) {
      if (e.key === 'Escape') closeMediaModal();
    });
    
    // Mark item as failed
    Shiny.addCustomMessageHandler('markFailed', function(d) {
      var item = document.querySelector('[data-index=\"' + d.index + '\"]');
      if (item) {
        item.classList.add('failed');
        item.setAttribute('data-error', d.error || 'Download failed');
      }
    });
    
    // Clear failed status
    Shiny.addCustomMessageHandler('clearFailed', function(d) {
      var items = document.querySelectorAll('.preview-item.failed');
      items.forEach(function(item) {
        item.classList.remove('failed');
        item.removeAttribute('data-error');
      });
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
  e$base_url <- ""  # Store base subreddit URL
  e$after <- NULL
  e$found <- 0
  e$done <- 0
  e$fail <- 0
  e$skipped <- 0
  e$scan_all <- FALSE
  e$sort_modes <- c("new", "hot", "top_all", "top_year", "top_month", "top_week", "top_day", "rising")
  e$current_sort_idx <- 1
  e$downloaded_hashes <- c()
  e$all_found_urls <- c()  # Track all URLs found across all scans in this session
  e$scanning <- FALSE
  e$downloading <- FALSE
  
  # Reactive values
  rv <- reactiveValues(
    items = list(),
    selected = c(),
    show_preview = FALSE,
    failed_items = c(),  # Track indices of failed downloads
    skipped_count = 0  # Track duplicates skipped in current scan
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
    
    # Use isolate to prevent re-rendering when selection changes
    # Selection state is handled entirely via JavaScript
    initial_selected <- isolate(rv$selected)
    initial_count <- length(initial_selected)
    total_count <- length(rv$items)
    
    div(class="box",
      div(class="selection-bar",
        span(id="selection-count", class="selection-info", 
          paste0(initial_count, " of ", total_count, " selected")),
        div(class="selection-buttons",
          actionButton("select_all", tagList(tags$i(class="fa fa-check-square"), " Select All"), class="btn"),
          actionButton("deselect_all", tagList(tags$i(class="fa fa-square"), " Deselect All"), class="btn"),
          actionButton("retry_failed", tagList(tags$i(class="fa fa-redo"), " Retry Failed"), class="btn"),
          actionButton("download_selected", tagList(tags$i(class="fa fa-download"), " Download Selected"), class="btn p")
        )
      ),
      div(class="preview-grid", id="preview-grid",
        lapply(seq_along(rv$items), function(i) {
          item <- rv$items[[i]]
          is_selected <- i %in% initial_selected
          
          # Determine icon based on type
          icon_class <- switch(item$type,
            "video" = "fa-film",
            "gif" = "fa-images",
            "fa-image"
          )
          
          # Get view URL - for videos use original URL, for images use preview
          view_url <- if (item$type == "video" || item$type == "gif") {
            gsub("^REDGIFS:", "", item$url)
          } else if (!is.null(item$preview) && nchar(item$preview) > 0) {
            item$preview
          } else {
            gsub("^REDGIFS:", "", item$url)
          }
          
          div(class=paste("preview-item", if(is_selected) "selected" else ""),
            `data-index` = i,
            `data-url` = view_url,
            # Clicking the checkbox area toggles selection
            div(class=paste("preview-check", if(is_selected) "checked" else ""),
              onclick = paste0("event.stopPropagation(); Shiny.setInputValue('toggle_item', ", i, ", {priority: 'event'})"),
              if(is_selected) tags$i(class="fa fa-check") else NULL
            ),
            span(class="preview-type", item$type),
            # Upvote badge
            span(class="preview-upvotes", title = "Upvotes",
              tags$i(class="fa fa-arrow-up"),
              format(if (!is.null(item$upvotes)) item$upvotes else 0, big.mark = ",")
            ),
            # Clicking the thumbnail opens media in modal
            div(class="preview-thumb",
              onclick = paste0("event.stopPropagation(); viewMedia('", gsub("'", "\\'", view_url), "', '", item$type, "', event)"),
              if (!is.null(item$preview) && nchar(item$preview) > 0) {
                tags$img(src = item$preview, loading = "lazy", 
                  onerror = "this.style.display='none'; this.nextElementSibling.style.display='flex';")
              } else NULL,
              div(class="preview-icon", style = if(!is.null(item$preview) && nchar(item$preview) > 0) "display:none" else "",
                tags$i(class = paste("fa", icon_class)))
            ),
            # View button
            div(class="preview-view",
              onclick = paste0("event.stopPropagation(); viewMedia('", gsub("'", "\\'", view_url), "', '", item$type, "', event)"),
              title = "View media",
              tags$i(class="fa fa-eye")
            ),
            # Clicking title toggles selection
            div(class="preview-info", title = item$title, 
              onclick = paste0("event.stopPropagation(); Shiny.setInputValue('toggle_item', ", i, ", {priority: 'event'})"),
              item$title)
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
  
  # Helper function to get sort URL
  get_sort_url <- function(base_url, sort_mode) {
    if (sort_mode == "hot") {
      paste0(base_url, "/hot.json")
    } else if (sort_mode == "new") {
      paste0(base_url, "/new.json")
    } else if (sort_mode == "rising") {
      paste0(base_url, "/rising.json")
    } else if (sort_mode == "top_all") {
      paste0(base_url, "/top.json?t=all")
    } else if (sort_mode == "top_year") {
      paste0(base_url, "/top.json?t=year")
    } else if (sort_mode == "top_month") {
      paste0(base_url, "/top.json?t=month")
    } else if (sort_mode == "top_week") {
      paste0(base_url, "/top.json?t=week")
    } else if (sort_mode == "top_day") {
      paste0(base_url, "/top.json?t=day")
    } else {
      paste0(base_url, "/.json")
    }
  }
  
  # Helper to get friendly sort name
  get_sort_name <- function(sort_mode) {
    switch(sort_mode,
      "hot" = "Hot",
      "new" = "New",
      "rising" = "Rising",
      "top_all" = "Top (All Time)",
      "top_year" = "Top (Year)",
      "top_month" = "Top (Month)",
      "top_week" = "Top (Week)",
      "top_day" = "Top (Day)",
      sort_mode
    )
  }
  
  # Helper to apply upvote filter and sort
  apply_upvote_filter <- function(items) {
    if (length(items) == 0) return(items)
    
    # Get filter values from stored e$ values (captured at scan start)
    min_up <- e$min_upvotes
    max_up <- e$max_upvotes
    
    filtered_items <- items
    filtered_count <- 0
    
    # Apply min filter
    if (!is.na(min_up) && min_up > 0) {
      before_count <- length(filtered_items)
      filtered_items <- Filter(function(x) {
        ups <- if (!is.null(x$upvotes)) x$upvotes else 0
        ups >= min_up
      }, filtered_items)
      filtered_count <- filtered_count + (before_count - length(filtered_items))
    }
    
    # Apply max filter
    if (!is.na(max_up) && max_up > 0) {
      before_count <- length(filtered_items)
      filtered_items <- Filter(function(x) {
        ups <- if (!is.null(x$upvotes)) x$upvotes else 0
        ups <= max_up
      }, filtered_items)
      filtered_count <- filtered_count + (before_count - length(filtered_items))
    }
    
    # Sort by upvotes (descending)
    if (length(filtered_items) > 0) {
      upvote_order <- order(sapply(filtered_items, function(x) if (!is.null(x$upvotes)) x$upvotes else 0), decreasing = TRUE)
      filtered_items <- filtered_items[upvote_order]
    }
    
    # Log filter info
    if (filtered_count > 0) {
      filter_msg <- ""
      if (!is.na(min_up) && min_up > 0 && !is.na(max_up) && max_up > 0) {
        filter_msg <- paste0("Upvote filter: ", min_up, " - ", max_up)
      } else if (!is.na(min_up) && min_up > 0) {
        filter_msg <- paste0("Upvote filter: >= ", min_up)
      } else if (!is.na(max_up) && max_up > 0) {
        filter_msg <- paste0("Upvote filter: <= ", max_up)
      }
      log(paste0(filter_msg, " (", filtered_count, " filtered out)"), "i")
    }
    
    return(filtered_items)
  }
  
  # Scan function
  scan_next <- function() {
    tryCatch({
      # Check stop condition - for scan_all mode, only stop when all sort modes exhausted
      if (!e$run || (!e$scan_all && e$pg >= e$maxpg)) {
        # Scanning complete
        e$scanning <- FALSE
        total_found <- length(e$items)
        # Apply upvote filter and sort
        e$items <- apply_upvote_filter(e$items)
        e$found <- length(e$items)
        filtered_count <- total_found - e$found
        rv$items <- e$items
        rv$selected <- seq_along(e$items)  # Select all by default
        rv$show_preview <- TRUE
        rv$skipped_count <- e$skipped
        skip_msg <- if (e$skipped > 0) paste0(" (", e$skipped, " duplicates skipped)") else ""
        filter_msg <- if (filtered_count > 0) paste0(" (", filtered_count, " filtered by upvotes)") else ""
        upd(n1 = e$found, st = paste0("Found ", e$found, " media files", skip_msg, filter_msg, " - Select and download"))
        log(paste0("Scan complete. ", e$found, " files found", if (e$skipped > 0) paste0(", ", e$skipped, " duplicates skipped") else ""), "s")
        return()
      }
      
      e$pg <- e$pg + 1
      if (e$scan_all) {
        current_sort <- e$sort_modes[e$current_sort_idx]
        upd(st = paste0("[", e$current_sort_idx, "/", length(e$sort_modes), " ", get_sort_name(current_sort), "] Page ", e$pg))
      } else {
        upd(st = paste("Scanning page", e$pg, "/", e$maxpg))
      }
      log(paste("Scanning page", e$pg))
      
      data <- fetch_page(e$base, e$after)
      
      # Check if we got valid data
      if (is.null(data) || is.null(data$data) || is.null(data$data$children) || length(data$data$children) == 0) {
        # No more pages available for current sort
        if (e$scan_all && e$current_sort_idx < length(e$sort_modes)) {
          # Move to next sort mode
          e$current_sort_idx <- e$current_sort_idx + 1
          e$pg <- 0
          e$after <- NULL
          next_sort <- e$sort_modes[e$current_sort_idx]
          e$base <- get_sort_url(e$base_url, next_sort)
          log(paste0("Switching to: ", get_sort_name(next_sort), " [", e$current_sort_idx, "/", length(e$sort_modes), "]"), "i")
          later::later(scan_next, 0.5)
          return()
        }
        
        # All sort modes exhausted or not in scan_all mode - finish scan
        total_found <- length(e$items)
        # Apply upvote filter and sort
        e$items <- apply_upvote_filter(e$items)
        e$found <- length(e$items)
        filtered_count <- total_found - e$found
        rv$items <- e$items
        rv$selected <- seq_along(e$items)
        rv$show_preview <- TRUE
        rv$skipped_count <- e$skipped
        skip_msg <- if (e$skipped > 0) paste0(" (", e$skipped, " duplicates skipped)") else ""
        filter_msg <- if (filtered_count > 0) paste0(" (", filtered_count, " filtered by upvotes)") else ""
        upd(n1 = e$found, st = paste0("Found ", e$found, " media files", skip_msg, filter_msg, " - Select and download"))
        if (e$scan_all) {
          log(paste0("All sort modes scanned. ", e$found, " files found", if (e$skipped > 0) paste0(", ", e$skipped, " duplicates skipped") else ""), "s")
        } else {
          log(paste0("Reached end of available pages. ", e$found, " files found", if (e$skipped > 0) paste0(", ", e$skipped, " duplicates skipped") else ""), "s")
        }
        e$scanning <- FALSE
        return()
      }
      
      for (child in data$data$children) {
        if (!is.null(child$data)) {
          new_items <- get_media(child$data)
          
          # Filter out items already found in previous scans
          for (item in new_items) {
            url_key <- normalize_url(gsub("^REDGIFS:", "", item$url))
            url_base <- basename(gsub("\\?.*$", "", url_key))
            item_key <- paste0(item$title, "|", url_base)
            
            if (item_key %in% e$all_found_urls) {
              e$skipped <- e$skipped + 1
              upd(n5 = e$skipped)
            } else {
              e$all_found_urls <- c(e$all_found_urls, item_key)
              e$items <- append(e$items, list(item))
            }
          }
        }
      }
      
      # Remove duplicates within current scan
      if (length(e$items) > 0) {
        # Create unique key from title and URL base
        keys <- sapply(e$items, function(x) {
          url_norm <- normalize_url(gsub("^REDGIFS:", "", x$url))
          url_base <- basename(gsub("\\?.*$", "", url_norm))
          paste0(x$title, "|", url_base)
        })
        e$items <- e$items[!duplicated(keys)]
        
        # Also deduplicate by title alone (keep first occurrence)
        titles <- sapply(e$items, function(x) x$title)
        e$items <- e$items[!duplicated(titles)]
      }
      
      upd(n1 = length(e$items))
      
      # Check for next page token
      e$after <- data$data$after
      if (is.null(e$after) || is.na(e$after) || e$after == "") {
        # No more pages for current sort
        if (e$scan_all && e$current_sort_idx < length(e$sort_modes)) {
          # Move to next sort mode
          e$current_sort_idx <- e$current_sort_idx + 1
          e$pg <- 0
          e$after <- NULL
          next_sort <- e$sort_modes[e$current_sort_idx]
          e$base <- get_sort_url(e$base_url, next_sort)
          log(paste0("Switching to: ", get_sort_name(next_sort), " [", e$current_sort_idx, "/", length(e$sort_modes), "]"), "i")
        } else if (!e$scan_all) {
          # Normal mode - stop scanning
          e$pg <- e$maxpg
        }
        # In scan_all mode with all sorts done, the next iteration will complete
      }
      
      later::later(scan_next, 0.5)
    }, error = function(err) {
      # Handle any unexpected errors gracefully
      log(paste("Scan error:", err$message), "e")
      total_found <- length(e$items)
      # Apply upvote filter and sort
      e$items <- apply_upvote_filter(e$items)
      e$found <- length(e$items)
      filtered_count <- total_found - e$found
      rv$items <- e$items
      rv$selected <- seq_along(e$items)
      rv$show_preview <- TRUE
      rv$skipped_count <- e$skipped
      skip_msg <- if (e$skipped > 0) paste0(" (", e$skipped, " duplicates skipped)") else ""
      filter_msg <- if (filtered_count > 0) paste0(" (", filtered_count, " filtered by upvotes)") else ""
      upd(n1 = e$found, st = paste0("Found ", e$found, " media files", skip_msg, filter_msg, " - Select and download"))
      log(paste0("Scan stopped due to error. ", e$found, " files found", if (e$skipped > 0) paste0(", ", e$skipped, " duplicates skipped") else ""), "s")
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
        e$failed_indices <- c(e$failed_indices, idx)
        upd(n3 = e$fail)
        log("Redgifs fetch failed", "e")
        session$sendCustomMessage("markFailed", list(index = idx, error = "Redgifs API error"))
        pct <- round((e$i / length(e$download_list)) * 100)
        upd(n4 = pct, pb = pct)
        e$i <- e$i + 1
        later::later(dl_next, 0.3)
        return()
      }
      
      url <- result$url
      # Use metadata naming for redgifs too
      if (!is.null(item$date) && !is.null(item$author)) {
        clean_title <- gsub("[<>:\"/\\\\|?*]", "", item$title)
        clean_title <- gsub("\\s+", "_", clean_title)
        clean_title <- substr(clean_title, 1, 40)
        clean_author <- gsub("[<>:\"/\\\\|?*]", "_", item$author)
        fn <- paste0(item$date, "_", clean_author, "_", clean_title, ".mp4")
      } else {
        fn <- paste0(result$id, ".mp4")
      }
    } else {
      fn <- fname(url, e$i, item)
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
      e$failed_indices <- c(e$failed_indices, idx)
      upd(n3 = e$fail)
      if (file.exists(fp)) file.remove(fp)
      log(paste("Failed:", fn), "e")
      session$sendCustomMessage("markFailed", list(index = idx, error = "Download failed"))
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
    e$skipped <- 0  # Reset skipped count for this scan
    e$scan_all <- isTRUE(input$scan_all)  # Store scan_all setting
    # Note: e$all_found_urls is NOT reset - keeps tracking across scans
    
    # Store upvote filter values at scan start (reactive values captured here)
    e$min_upvotes <- suppressWarnings(as.numeric(input$min_upvotes))
    e$max_upvotes <- suppressWarnings(as.numeric(input$max_upvotes))
    
    rv$items <- list()
    rv$selected <- c()
    rv$show_preview <- FALSE
    rv$skipped_count <- 0
    
    upd(n1 = 0, n2 = 0, n3 = 0, n4 = 0, n5 = 0, pb = 0, st = "Scanning...")
    session$sendCustomMessage("upd", list(lg = ""))
    
    # Normalize and parse input URL - support many formats
    input_url <- trimws(input$url)
    
    # Remove any trailing slashes
    input_url <- gsub("/+$", "", input_url)
    
    # Detect user or subreddit from various input formats
    # Patterns to match:
    # - r/pics, /r/pics
    # - u/username, /u/username
    # - user/username, /user/username
    # - reddit.com/r/pics, www.reddit.com/r/pics
    # - https://reddit.com/r/pics, https://www.reddit.com/r/pics
    # - old.reddit.com/r/pics, etc.
    
    is_user <- FALSE
    target_name <- NULL
    
    # Check for user patterns first
    # Match: u/username, /u/username, user/username, /user/username
    user_patterns <- c(
      "^u/([^/\\s]+)",           # u/username
      "^/u/([^/\\s]+)",          # /u/username
      "^user/([^/\\s]+)",        # user/username
      "^/user/([^/\\s]+)",       # /user/username
      "/u/([^/\\s]+)",           # .../u/username
      "/user/([^/\\s]+)"         # .../user/username
    )
    
    for (pattern in user_patterns) {
      m <- str_match(input_url, pattern)
      if (!is.na(m[1, 2])) {
        is_user <- TRUE
        target_name <- m[1, 2]
        break
      }
    }
    
    # If not user, check for subreddit patterns
    if (!is_user) {
      sub_patterns <- c(
        "^r/([^/\\s]+)",          # r/pics
        "^/r/([^/\\s]+)",         # /r/pics
        "/r/([^/\\s]+)"           # .../r/pics
      )
      
      for (pattern in sub_patterns) {
        m <- str_match(input_url, pattern)
        if (!is.na(m[1, 2])) {
          target_name <- m[1, 2]
          break
        }
      }
    }
    
    # If still no match, treat the whole input as a subreddit name
    if (is.null(target_name) || target_name == "") {
      # Remove any URL prefixes and treat as subreddit name
      clean_input <- gsub("^(https?://)?(www\\.)?(old\\.)?(new\\.)?reddit\\.com/?", "", input_url)
      clean_input <- gsub("^/", "", clean_input)
      if (nchar(clean_input) > 0 && !grepl("/", clean_input)) {
        target_name <- clean_input
      } else {
        target_name <- "pics"  # Default fallback
      }
    }
    
    target_type <- if (is_user) "user" else "subreddit"
    
    # Construct base URL
    if (is_user) {
      base_url <- paste0("https://www.reddit.com/user/", target_name, "/submitted")
    } else {
      base_url <- paste0("https://www.reddit.com/r/", target_name)
    }
    
    # Store base URL for scan_all mode
    e$base_url <- base_url
    
    # Add sorting path and time parameter
    sort_opt <- input$sort_by
    
    if (e$scan_all) {
      # Build sort modes list from user selection
      selected_sorts <- c()
      if (isTRUE(input$sort_new)) selected_sorts <- c(selected_sorts, "new")
      if (isTRUE(input$sort_hot)) selected_sorts <- c(selected_sorts, "hot")
      if (isTRUE(input$sort_rising)) selected_sorts <- c(selected_sorts, "rising")
      if (isTRUE(input$sort_top_all)) selected_sorts <- c(selected_sorts, "top_all")
      if (isTRUE(input$sort_top_year)) selected_sorts <- c(selected_sorts, "top_year")
      if (isTRUE(input$sort_top_month)) selected_sorts <- c(selected_sorts, "top_month")
      if (isTRUE(input$sort_top_week)) selected_sorts <- c(selected_sorts, "top_week")
      if (isTRUE(input$sort_top_day)) selected_sorts <- c(selected_sorts, "top_day")
      
      if (length(selected_sorts) == 0) {
        # If nothing selected, default to "new"
        selected_sorts <- c("new")
        log("No sort modes selected, defaulting to New", "i")
      }
      
      e$sort_modes <- selected_sorts
      e$current_sort_idx <- 1
      e$base <- get_sort_url(base_url, e$sort_modes[1])
    } else {
      # Normal mode - use selected sort
      if (sort_opt == "hot") {
        e$base <- paste0(base_url, "/hot.json")
      } else if (sort_opt == "new") {
        e$base <- paste0(base_url, "/new.json")
      } else if (sort_opt == "rising") {
        e$base <- paste0(base_url, "/rising.json")
      } else if (sort_opt == "top_all") {
        e$base <- paste0(base_url, "/top.json?t=all")
      } else if (sort_opt == "top_year") {
        e$base <- paste0(base_url, "/top.json?t=year")
      } else if (sort_opt == "top_month") {
        e$base <- paste0(base_url, "/top.json?t=month")
      } else if (sort_opt == "top_week") {
        e$base <- paste0(base_url, "/top.json?t=week")
      } else if (sort_opt == "top_day") {
        e$base <- paste0(base_url, "/top.json?t=day")
      } else {
        e$base <- paste0(base_url, "/.json")
      }
    }
    
    e$maxpg <- input$pages
    
    # Log target info
    if (is_user) {
      log(paste0("Target: u/", target_name, " (User Profile)"))
    } else {
      log(paste0("Target: r/", target_name, " (Subreddit)"))
    }
    
    if (e$scan_all) {
      sort_names <- sapply(e$sort_modes, get_sort_name)
      log(paste0("FULL SCAN MODE: Searching ", length(e$sort_modes), " sort mode(s): ", paste(sort_names, collapse = ", ")))
      log(paste0("Starting with: ", get_sort_name(e$sort_modes[1])), "i")
    } else {
      log(paste("Scanning", e$maxpg, "pages..."))
    }
    
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
    e$failed_indices <- c()  # Reset failed indices
    
    # Clear previous failed status in UI
    session$sendCustomMessage("clearFailed", list())
    
    # Get download folder
    e$dir <- input$folder_path
    if (is.null(e$dir) || e$dir == "") {
      e$dir <- getwd()
    }
    
    # Parse input URL to detect user or subreddit (same logic as scan)
    input_url <- trimws(input$url)
    input_url <- gsub("/+$", "", input_url)
    
    is_user_dl <- FALSE
    target_name_dl <- NULL
    
    # Check for user patterns
    user_patterns <- c("^u/([^/\\s]+)", "^/u/([^/\\s]+)", "^user/([^/\\s]+)", "^/user/([^/\\s]+)", "/u/([^/\\s]+)", "/user/([^/\\s]+)")
    for (pattern in user_patterns) {
      m <- str_match(input_url, pattern)
      if (!is.na(m[1, 2])) {
        is_user_dl <- TRUE
        target_name_dl <- m[1, 2]
        break
      }
    }
    
    # Check for subreddit patterns
    if (!is_user_dl) {
      sub_patterns <- c("^r/([^/\\s]+)", "^/r/([^/\\s]+)", "/r/([^/\\s]+)")
      for (pattern in sub_patterns) {
        m <- str_match(input_url, pattern)
        if (!is.na(m[1, 2])) {
          target_name_dl <- m[1, 2]
          break
        }
      }
    }
    
    # Fallback
    if (is.null(target_name_dl) || target_name_dl == "") {
      clean_input <- gsub("^(https?://)?(www\\.)?(old\\.)?(new\\.)?reddit\\.com/?", "", input_url)
      clean_input <- gsub("^/", "", clean_input)
      if (nchar(clean_input) > 0 && !grepl("/", clean_input)) {
        target_name_dl <- clean_input
      } else {
        target_name_dl <- "reddit"
      }
    }
    
    # Create appropriate subfolder
    if (is_user_dl) {
      e$dir <- file.path(e$dir, "users", paste0("u_", target_name_dl, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    } else {
      e$dir <- file.path(e$dir, "subreddits", paste0("r_", target_name_dl, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
    dir.create(e$dir, recursive = TRUE, showWarnings = FALSE)
    
    upd(n2 = 0, n3 = 0, n4 = 0, pb = 0, st = "Starting download...")
    log(paste("Downloading", length(rv$selected), "files to", e$dir))
    
    later::later(dl_next, 0.2)
  })
  
  # Retry failed downloads
  observeEvent(input$retry_failed, {
    req(length(e$failed_indices) > 0)
    
    e$run <- TRUE
    e$downloading <- TRUE
    e$download_list <- e$failed_indices
    e$items_for_download <- rv$items
    e$i <- 1
    e$done <- 0
    e$fail <- 0
    old_failed <- e$failed_indices
    e$failed_indices <- c()
    
    # Clear failed status in UI for items being retried
    session$sendCustomMessage("clearFailed", list())
    
    # Use existing directory or create new one
    if (is.null(e$dir) || e$dir == "" || !dir.exists(e$dir)) {
      e$dir <- input$folder_path
      if (is.null(e$dir) || e$dir == "") {
        e$dir <- getwd()
      }
      sub <- str_match(input$url, "/r/([^/]+)")[1, 2]
      if (is.na(sub)) sub <- "reddit"
      e$dir <- file.path(e$dir, paste0(sub, "_retry_", format(Sys.time(), "%Y%m%d_%H%M%S")))
      dir.create(e$dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    upd(n2 = 0, n3 = 0, n4 = 0, pb = 0, st = "Retrying failed downloads...")
    log(paste("Retrying", length(old_failed), "failed downloads"))
    
    later::later(dl_next, 0.2)
  })
  
  # Stop button
  observeEvent(input$stop, {
    e$run <- FALSE
    upd(st = "Stopped")
    log("Operation stopped", "e")
  })
  
  # Clear History button - reset duplicate tracking
  observeEvent(input$clear_history, {
    e$all_found_urls <- c()
    e$skipped <- 0
    upd(n5 = 0, st = "History cleared - duplicates will no longer be skipped")
    log("Duplicate history cleared. Next scan will start fresh.", "i")
  })
}

shinyApp(ui, server)
