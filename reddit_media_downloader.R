# Reddit Media Downloader
# This script downloads all media files from Reddit pages
# Usage: download_reddit_media("https://www.reddit.com/r/subreddit/.json")

# Load required packages
required_packages <- c("httr", "jsonlite", "stringr")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org")
    library(pkg, character.only = TRUE)
  }
}

# Function to fetch data from Reddit API
fetch_reddit_page <- function(url, after = NULL) {
  # Format URL
  if (!grepl("\\.json", url)) {
    url <- paste0(gsub("/$", "", url), "/.json")
  }
  
  # Add pagination parameter
  if (!is.null(after) && after != "") {
    if (grepl("\\?", url)) {
      url <- paste0(url, "&after=", after)
    } else {
      url <- paste0(url, "?after=", after)
    }
  }
  
  cat("  Fetching:", url, "\n")
  
  # Make request (Reddit requires User-Agent)
  response <- tryCatch({
    httr::GET(
      url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) R Reddit Media Downloader/1.0"),
      httr::add_headers(Accept = "application/json"),
      httr::timeout(30)
    )
  }, error = function(e) {
    warning(paste("Connection error:", e$message))
    return(NULL)
  })
  
  if (is.null(response)) return(NULL)
  
  if (httr::status_code(response) != 200) {
    warning(paste("Page load failed - Status:", httr::status_code(response)))
    return(NULL)
  }
  
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(content, simplifyDataFrame = FALSE)
  
  return(data)
}

# Function to extract media URLs from post
extract_media_from_post <- function(post_data) {
  urls <- c()
  
  # Check main URL
  if (!is.null(post_data$url) && !is.na(post_data$url)) {
    url <- post_data$url
    
    # i.redd.it images
    if (grepl("i\\.redd\\.it", url)) {
      urls <- c(urls, url)
    }
    
    # Direct image extensions
    if (grepl("\\.(jpg|jpeg|png|gif|webp|mp4|gifv)($|\\?)", url, ignore.case = TRUE)) {
      urls <- c(urls, url)
    }
    
    # imgur images
    if (grepl("imgur\\.com", url) && !grepl("/a/|/gallery/", url)) {
      if (!grepl("\\.(jpg|jpeg|png|gif)$", url, ignore.case = TRUE)) {
        url <- gsub("imgur\\.com", "i.imgur.com", url)
        if (!grepl("\\.[a-z]+$", url, ignore.case = TRUE)) {
          url <- paste0(url, ".jpg")
        }
      }
      urls <- c(urls, url)
    }
  }
  
  # Check url_overridden_by_dest
  if (!is.null(post_data$url_overridden_by_dest)) {
    dest_url <- post_data$url_overridden_by_dest
    if (grepl("i\\.redd\\.it|\\.(jpg|jpeg|png|gif|webp|mp4)", dest_url, ignore.case = TRUE)) {
      urls <- c(urls, dest_url)
    }
  }
  
  # Check Reddit video
  if (!is.null(post_data$is_video) && post_data$is_video == TRUE) {
    if (!is.null(post_data$media$reddit_video$fallback_url)) {
      video_url <- post_data$media$reddit_video$fallback_url
      video_url <- gsub("\\?source=fallback", "", video_url)
      urls <- c(urls, video_url)
    }
  }
  
  # Check secure_media
  if (!is.null(post_data$secure_media$reddit_video$fallback_url)) {
    video_url <- post_data$secure_media$reddit_video$fallback_url
    video_url <- gsub("\\?source=fallback", "", video_url)
    urls <- c(urls, video_url)
  }
  
  # Preview images
  if (!is.null(post_data$preview$images)) {
    images <- post_data$preview$images
    for (img in images) {
      if (!is.null(img$source$url)) {
        img_url <- gsub("&amp;", "&", img$source$url)
        urls <- c(urls, img_url)
      }
      # GIF variants
      if (!is.null(img$variants$gif$source$url)) {
        gif_url <- gsub("&amp;", "&", img$variants$gif$source$url)
        urls <- c(urls, gif_url)
      }
      # MP4 variants
      if (!is.null(img$variants$mp4$source$url)) {
        mp4_url <- gsub("&amp;", "&", img$variants$mp4$source$url)
        urls <- c(urls, mp4_url)
      }
    }
  }
  
  # Gallery check
  if (!is.null(post_data$is_gallery) && post_data$is_gallery == TRUE) {
    if (!is.null(post_data$media_metadata)) {
      for (item in post_data$media_metadata) {
        if (!is.null(item$s$u)) {
          img_url <- gsub("&amp;", "&", item$s$u)
          urls <- c(urls, img_url)
        } else if (!is.null(item$s$gif)) {
          urls <- c(urls, gsub("&amp;", "&", item$s$gif))
        } else if (!is.null(item$s$mp4)) {
          urls <- c(urls, gsub("&amp;", "&", item$s$mp4))
        }
      }
    }
  }
  
  # Crosspost check
  if (!is.null(post_data$crosspost_parent_list)) {
    for (crosspost in post_data$crosspost_parent_list) {
      crosspost_urls <- extract_media_from_post(crosspost)
      urls <- c(urls, crosspost_urls)
    }
  }
  
  return(unique(urls))
}

# Extract filename from URL
get_filename_from_url <- function(url, index) {
  parsed <- basename(url)
  parsed <- gsub("\\?.*$", "", parsed)
  
  if (nchar(parsed) < 3 || !grepl("\\.", parsed)) {
    if (grepl("DASH_\\d+", url)) {
      ext <- ".mp4"
    } else if (grepl("\\.gif", url, ignore.case = TRUE)) {
      ext <- ".gif"
    } else if (grepl("\\.mp4", url, ignore.case = TRUE)) {
      ext <- ".mp4"
    } else if (grepl("\\.webm", url, ignore.case = TRUE)) {
      ext <- ".webm"
    } else if (grepl("\\.png", url, ignore.case = TRUE)) {
      ext <- ".png"
    } else if (grepl("\\.webp", url, ignore.case = TRUE)) {
      ext <- ".webp"
    } else {
      ext <- ".jpg"
    }
    parsed <- paste0("media_", sprintf("%04d", index), ext)
  }
  
  # Convert gifv to mp4
  parsed <- gsub("\\.gifv$", ".mp4", parsed, ignore.case = TRUE)
  
  # Clean invalid characters
  parsed <- gsub("[<>:\"/\\|?*]", "_", parsed)
  
  return(parsed)
}

# Download media file
download_media <- function(url, filepath) {
  # Convert gifv URLs to mp4
  url <- gsub("\\.gifv$", ".mp4", url, ignore.case = TRUE)
  
  tryCatch({
    response <- httr::GET(
      url,
      httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"),
      httr::write_disk(filepath, overwrite = TRUE),
      httr::timeout(60)
    )
    
    if (httr::status_code(response) == 200) {
      file_size <- file.info(filepath)$size
      if (!is.na(file_size) && file_size > 0) {
        return(TRUE)
      }
    }
    return(FALSE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Main download function
download_reddit_media <- function(reddit_url, pages = 5, output_dir = NULL) {
  cat("\n========================================\n")
  cat("     REDDIT MEDIA DOWNLOADER\n")
  cat("========================================\n\n")
  
  # Format URL
  base_url <- reddit_url
  if (!grepl("\\.json", base_url)) {
    base_url <- paste0(gsub("/$", "", base_url), "/.json")
  }
  
  # Extract subreddit name
  subreddit_match <- stringr::str_match(reddit_url, "/r/([^/]+)")
  if (!is.na(subreddit_match[1, 2])) {
    subreddit_name <- subreddit_match[1, 2]
  } else {
    subreddit_name <- "reddit_download"
  }
  
  # Determine output folder
  if (is.null(output_dir)) {
    output_dir <- file.path(getwd(), paste0(subreddit_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  cat("Subreddit: r/", subreddit_name, "\n", sep = "")
  cat("Output folder:", output_dir, "\n")
  cat("Pages to scan:", pages, "\n\n")
  
  all_media_urls <- c()
  after <- NULL
  
  # Scan pages
  for (page in 1:pages) {
    cat("Scanning page", page, "/", pages, "...\n")
    
    data <- fetch_reddit_page(base_url, after)
    
    if (is.null(data)) {
      cat("  Error: Failed to load page\n")
      break
    }
    
    # Get children list
    children <- data$data$children
    
    if (is.null(children) || length(children) == 0) {
      cat("  No posts on this page\n")
      break
    }
    
    page_media_count <- 0
    
    # Extract media URLs from each post
    for (child in children) {
      if (!is.null(child$data)) {
        post_urls <- extract_media_from_post(child$data)
        if (length(post_urls) > 0) {
          all_media_urls <- c(all_media_urls, post_urls)
          page_media_count <- page_media_count + length(post_urls)
        }
      }
    }
    
    cat("  Media found:", page_media_count, "\n")
    
    # Next page token
    after <- data$data$after
    if (is.null(after) || is.na(after) || after == "") {
      cat("  Reached last page\n")
      break
    }
    
    # Rate limiting
    Sys.sleep(1.5)
  }
  
  # Get unique URLs
  all_media_urls <- unique(all_media_urls)
  
  cat("\n========================================\n")
  cat("Total unique media:", length(all_media_urls), "\n")
  cat("========================================\n\n")
  
  if (length(all_media_urls) == 0) {
    cat("No media found to download.\n")
    return(invisible(NULL))
  }
  
  # Save URL list
  url_file <- file.path(output_dir, "urls.txt")
  writeLines(all_media_urls, url_file)
  cat("URL list saved:", url_file, "\n\n")
  
  # Download
  cat("Starting download...\n\n")
  downloaded <- 0
  failed <- 0
  
  for (i in seq_along(all_media_urls)) {
    url <- all_media_urls[i]
    filename <- get_filename_from_url(url, i)
    filepath <- file.path(output_dir, filename)
    
    # Handle duplicate filenames
    if (file.exists(filepath)) {
      base_name <- tools::file_path_sans_ext(filename)
      ext <- tools::file_ext(filename)
      counter <- 1
      while (file.exists(filepath)) {
        filename <- paste0(base_name, "_", counter, ".", ext)
        filepath <- file.path(output_dir, filename)
        counter <- counter + 1
      }
    }
    
    cat("[", i, "/", length(all_media_urls), "] ", filename, sep = "")
    
    success <- download_media(url, filepath)
    if (success) {
      cat(" [OK]\n")
      downloaded <- downloaded + 1
    } else {
      cat(" [FAIL]\n")
      failed <- failed + 1
      # Delete failed file
      if (file.exists(filepath)) file.remove(filepath)
    }
    
    # Rate limiting
    Sys.sleep(0.3)
  }
  
  cat("\n========================================\n")
  cat("DOWNLOAD COMPLETE\n")
  cat("========================================\n")
  cat("Successful:", downloaded, "\n")
  cat("Failed:", failed, "\n")
  cat("Folder:", output_dir, "\n")
  cat("========================================\n")
  
  return(invisible(output_dir))
}

# Batch download for multiple URLs
download_multiple <- function(urls, pages = 5) {
  results <- list()
  for (url in urls) {
    cat("\n\n###################################\n")
    cat("Processing new URL:", url, "\n")
    cat("###################################\n")
    result <- download_reddit_media(url, pages)
    results[[url]] <- result
  }
  return(invisible(results))
}

# Startup message
cat("\n")
cat("==========================================\n")
cat("     REDDIT MEDIA DOWNLOADER v2.0\n")
cat("==========================================\n")
cat(" Usage:\n")
cat(" download_reddit_media(url, pages)\n")
cat("\n")
cat(" Example:\n")
cat(" download_reddit_media(\n")
cat("   \"https://reddit.com/r/pics/.json\",\n")
cat("   pages = 5\n")
cat(" )\n")
cat("==========================================\n")
