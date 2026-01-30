# Reddit Media Downloader

A Shiny R application for downloading media files from Reddit. Features a preview system that lets you select which files to download before starting the download process.

## Features

- **Media Preview**: Scan Reddit pages and see thumbnails of all media before downloading
- **Selective Download**: Choose exactly which files you want to download using checkboxes
- **Batch Selection**: "Select All" and "Deselect All" buttons for quick selection
- **Custom Download Location**: Choose where to save your files using the folder browser
- **Multiple Media Types**: Supports images (JPG, PNG, GIF, WebP), videos (MP4), Reddit galleries, and Redgifs
- **Pagination Support**: Scan multiple pages of a subreddit (1-10 pages)

## Requirements

- R (version 4.0 or higher recommended)
- Required R packages:
  - shiny
  - shinyFiles
  - httr
  - jsonlite
  - stringr
  - later

## Installation

1. Clone or download this repository
2. Install required R packages:

```r
install.packages(c("shiny", "shinyFiles", "httr", "jsonlite", "stringr", "later"))
```

## Usage

### Windows

Double-click `Reddit_Downloader.bat` to start the application. Your browser will open automatically.

### Any Platform

Run from R console:

```r
setwd("path/to/reddit_downloader")
shiny::runApp("app.R", launch.browser = TRUE)
```

Or from command line:

```bash
Rscript -e "shiny::runApp('app.R', launch.browser=TRUE)"
```

## How to Use

1. **Enter URL**: Paste a Reddit URL (e.g., `https://www.reddit.com/r/pics/`)
2. **Set Pages**: Use the slider to choose how many pages to scan (1-10)
3. **Click SCAN**: The app will fetch all media URLs and display previews
4. **Select Files**: 
   - Click individual items to toggle selection
   - Use "Select All" or "Deselect All" for bulk selection
   - All items are selected by default
5. **Choose Folder**: Click "Browse" to select your download location
6. **Click Download Selected**: Only the selected files will be downloaded

## Supported Content

- Reddit hosted images (i.redd.it)
- Reddit hosted videos (v.redd.it)
- Reddit galleries (multiple images in one post)
- Imgur images
- Redgifs videos
- GIFs and animated content

## File Structure

```
reddit_downloader/
├── app.R                      # Main Shiny application
├── reddit_media_downloader.R  # Standalone R script version
├── Reddit_Downloader.bat      # Windows launcher
├── README.md                  # This file
├── LICENSE                    # MIT License
└── .gitignore                 # Git ignore file
```

## Notes

- Downloads are saved in a subfolder named `{subreddit}_{timestamp}`
- The app respects Reddit's rate limits to avoid being blocked
- Large galleries may take time to scan and preview
- Some Redgifs content may require additional time to fetch

## License

MIT License - see LICENSE file for details.

## Disclaimer

This tool is for personal use only. Please respect Reddit's Terms of Service and the rights of content creators. Do not use this tool to download or distribute copyrighted material without permission.
