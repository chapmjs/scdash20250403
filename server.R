# server.R
# Server logic for Supply Chain Dashboard

server <- function(input, output, session) {
  # Reactive values to store data
  values <- reactiveValues(
    excelData = NULL,
    partData = NULL,
    supplyData = NULL,
    demandData = NULL,
    monthlyData = NULL,
    selectedPart = NULL,
    currentDate = NULL,
    monthlyDates = NULL,
    dashboardData = NULL,
    editedData = list(),
    shortageWarnings = NULL
  )
  
  # Initialize the app by loading Excel data
  observe({
    # Set current date to first day of current month
    values$currentDate <- get_first_day_of_month()
    
    # Generate 12 monthly dates starting from current month
    values$monthlyDates <- generate_monthly_dates(values$currentDate, 12)
    
    # Load Excel file
    excelFilePath <- "data/Supply Chain Dashboard.xlsx"
    
    # Check if file exists
    if (!file.exists(excelFilePath)) {
      showNotification("Excel file not found! Please place it in the data directory.", type = "error", duration = NULL)
      return()
    }
    
    # Read all sheets from Excel
    tryCatch({
      values$partData <- read_excel(excelFilePath, sheet = "Part Data", col_names = TRUE, skip = 2)
      values$supplyData <- read_excel(excelFilePath, sheet = "Supply Data", col_names = TRUE, skip = 1)
      values$demandData <- read_excel(excelFilePath, sheet = "Demand Data", col_names = TRUE, skip = 1)
      
      # Extract part numbers from Part Data sheet (starting after header rows)
      # Assuming part numbers are in column 2 (B)
      partNumbers <- values$partData[[2]]
      partNumbers <- partNumbers[!is.na(partNumbers) & partNumbers != "Part Number"]
      
      # Sort part numbers
      sortedPartNumbers <- sort_part_numbers(partNumbers)
      
      # Update the part number dropdown
      updateSelectInput(session, "partNumber", choices = sortedPartNumbers)
      
      showNotification("Data loaded successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading Excel data:", e$message), type = "error", duration = NULL)
    })
  })
  
  # When Retrieve Data button is clicked
  observeEvent(input$retrieveData, {
    req(input$partNumber)
    values$selectedPart <- input$partNumber
    
    # Create part number column name based on how it appears in each sheet
    partCol_partData <- names(values$partData)[2]  # Likely "Part Number"
    partCol_supplyData <- names(values$supplyData)[2]  # Likely "Part#:"
    partCol_demandData <- "Part Number"  # Adjust if different
    
    # Filter part data for selected part
    partInfo <- values$partData %>%
      filter(.data[[partCol_partData]] == values$selectedPart)
    
    # Filter supply data for selected part
    supplyInfo <- values$supplyData %>%
      filter(.data[[partCol_supplyData]] == values$selectedPart)
    
    # Filter demand data for selected part
    demandInfo <- tryCatch({
      values$demandData %>%
        filter(.data[[partCol_demandData]] == values$selectedPart)
    }, error = function(e) {
      # Handle case where column might not exist
      data.frame()
    })
    
    # Create monthly data table
    monthlyData <- data.frame(
      Category = c("High Forecast", "Expected Forecast", "Low Forecast",
                   "Scheduled Receipts", "CM #1 Inventory", "CM #2 Inventory", 
                   "OEM Inventory", "Supplier Capacity", "Purchase Commitments"),
      stringsAsFactors = FALSE
    )
    
    # Add columns for each month
    for (i in 1:12) {
      monthName <- format(values$monthlyDates[i], "%b-%y")
      monthlyData[[monthName]] <- NA
    }
    
    # Populate with data from source sheets
    # This would require mapping specific cells from your Excel
    
    # For High/Expected/Low Forecast rows
    if (nrow(demandInfo) > 0) {
      # Assuming demand data has columns for each month and rows for forecast types
      # Exact implementation depends on your Excel structure
      
      # Example (adjust column indices based on your actual data):
      # Find forecast rows in demand data (high, expected, low)
      high_forecast <- demandInfo[demandInfo$`Forecast Type` == "High", ] 
      expected_forecast <- demandInfo[demandInfo$`Forecast Type` == "Expected", ]
      low_forecast <- demandInfo[demandInfo$`Forecast Type` == "Low", ]
      
      # Map month columns from demand data to our monthlyData
      for (i in 1:12) {
        month_col <- format(values$monthlyDates[i], "%b-%y")
        source_col <- which(names(demandInfo) == format(values$monthlyDates[i], "%Y-%m-%d"))
        
        if (length(source_col) > 0) {
          # Fill forecast values if found
          if (nrow(high_forecast) > 0)
            monthlyData[monthlyData$Category == "High Forecast", month_col] <- high_forecast[[source_col]]
          
          if (nrow(expected_forecast) > 0)
            monthlyData[monthlyData$Category == "Expected Forecast", month_col] <- expected_forecast[[source_col]]
          
          if (nrow(low_forecast) > 0)
            monthlyData[monthlyData$Category == "Low Forecast", month_col] <- low_forecast[[source_col]]
        }
      }
    }
    
    # For inventory, receipts, and capacity data
    if (nrow(partInfo) > 0) {
      # Find inventory columns in part data
      cm1_col <- which(names(partInfo) == "CM #1 Inventory (PP,WIP,FGI)")
      cm2_col <- which(names(partInfo) == "CM #2 Inventory (PP,WIP,FGI)")
      oem_col <- which(names(partInfo) == "OEM Inventory")
      
      if (length(cm1_col) > 0)
        monthlyData[monthlyData$Category == "CM #1 Inventory", 2] <- partInfo[[cm1_col]]
      
      if (length(cm2_col) > 0)
        monthlyData[monthlyData$Category == "CM #2 Inventory", 2] <- partInfo[[cm2_col]]
      
      if (length(oem_col) > 0)
        monthlyData[monthlyData$Category == "OEM Inventory", 2] <- partInfo[[oem_col]]
      
      # Map scheduled receipts from part data (assuming columns are by month)
      for (i in 1:12) {
        month_col <- format(values$monthlyDates[i], "%b-%y")
        receipt_col <- which(names(partInfo) == format(values$monthlyDates[i], "%Y-%m-%d"))
        
        if (length(receipt_col) > 0)
          monthlyData[monthlyData$Category == "Scheduled Receipts", month_col] <- partInfo[[receipt_col]]
      }
    }
    
    # For supplier capacity and purchase commitments
    if (nrow(supplyInfo) > 0) {
      # Map supplier capacity from supply data
      capacity_col <- which(names(supplyInfo) == "Capacity")
      
      if (length(capacity_col) > 0) {
        for (i in 1:12) {
          month_col <- format(values$monthlyDates[i], "%b-%y")
          monthlyData[monthlyData$Category == "Supplier Capacity", month_col] <- supplyInfo[[capacity_col]]
        }
      }
      
      # Map purchase commitments (if available in supply data)
      commit_cols <- grep("Commitment", names(supplyInfo))
      
      if (length(commit_cols) > 0) {
        for (i in 1:min(12, length(commit_cols))) {
          month_col <- format(values$monthlyDates[i], "%b-%y")
          monthlyData[monthlyData$Category == "Purchase Commitments", month_col] <- supplyInfo[[commit_cols[i]]]
        }
      }
    }
    
    # Set the populated data to our reactive value
    values$monthlyData <- monthlyData
    
    # Update dashboard fields
    updateDashboardFields(partInfo, supplyInfo)
    
    # Calculate shortage warnings based on selected forecast type
    calculateShortageWarnings()
  })
  
  # Calculate shortage warnings whenever the forecast type changes
  observeEvent(input$forecastType, {
    calculateShortageWarnings()
  })
  
  # When Save/Update Data button is clicked
  observeEvent(input$saveData, {
    req(values$monthlyData, values$editedData, values$selectedPart)
    
    # Create backup of original Excel file
    excelFilePath <- "data/Supply Chain Dashboard.xlsx"
    backupFilePath <- paste0("data/backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    
    file.copy(excelFilePath, backupFilePath)
    
    # Read all sheets from Excel to get original structure
    all_sheets <- list(
      "Dashboard" = read_excel(excelFilePath, sheet = "Dashboard"),
      "Part Data" = read_excel(excelFilePath, sheet = "Part Data"),
      "Supply Data" = read_excel(excelFilePath, sheet = "Supply Data"),
      "Demand Data" = read_excel(excelFilePath, sheet = "Demand Data")
    )
    
    # Get part data for the selected part
    partCol_partData <- names(all_sheets$`Part Data`)[2]
    partRow <- which(all_sheets$`Part Data`[[partCol_partData]] == values$selectedPart)
    
    # Get supply data for the selected part
    partCol_supplyData <- names(all_sheets$`Supply Data`)[2]
    supplyRow <- which(all_sheets$`Supply Data`[[partCol_supplyData]] == values$selectedPart)
    
    # Get demand data for the selected part
    partCol_demandData <- "Part Number"  # Adjust if needed
    demandRows <- which(all_sheets$`Demand Data`[[partCol_demandData]] == values$selectedPart)
    
    # Process each edited cell
    for (edit_key in names(values$editedData)) {
      edit <- values$editedData[[edit_key]]
      row_category <- values$monthlyData$Category[edit$row]
      col_month <- names(values$monthlyData)[edit$col]
      
      # Determine which sheet and cell to update based on the category
      if (grepl("Forecast", row_category)) {
        # Update in Demand Data sheet
        forecast_type <- gsub(" Forecast", "", row_category)
        type_row <- demandRows[which(all_sheets$`Demand Data`$`Forecast Type`[demandRows] == forecast_type)]
        
        if (length(type_row) > 0) {
          month_col <- which(names(all_sheets$`Demand Data`) == format(values$monthlyDates[edit$col - 1], "%Y-%m-%d"))
          if (length(month_col) > 0) {
            all_sheets$`Demand Data`[type_row, month_col] <- edit$value
          }
        }
      } else if (row_category %in% c("CM #1 Inventory", "CM #2 Inventory", "OEM Inventory")) {
        # Update in Part Data sheet
        col_name <- ifelse(row_category == "CM #1 Inventory", "CM #1 Inventory (PP,WIP,FGI)",
                           ifelse(row_category == "CM #2 Inventory", "CM #2 Inventory (PP,WIP,FGI)",
                                  "OEM Inventory"))
        
        inv_col <- which(names(all_sheets$`Part Data`) == col_name)
        if (length(inv_col) > 0 && length(partRow) > 0) {
          all_sheets$`Part Data`[partRow, inv_col] <- edit$value
        }
      } else if (row_category == "Scheduled Receipts") {
        # Update in Part Data sheet - scheduled receipts by month
        if (length(partRow) > 0) {
          month_col <- which(names(all_sheets$`Part Data`) == format(values$monthlyDates[edit$col - 1], "%Y-%m-%d"))
          if (length(month_col) > 0) {
            all_sheets$`Part Data`[partRow, month_col] <- edit$value
          }
        }
      } else if (row_category == "Supplier Capacity") {
        # Update in Supply Data sheet
        capacity_col <- which(names(all_sheets$`Supply Data`) == "Capacity")
        if (length(capacity_col) > 0 && length(supplyRow) > 0) {
          all_sheets$`Supply Data`[supplyRow, capacity_col] <- edit$value
        }
      } else if (row_category == "Purchase Commitments") {
        # Update in Supply Data sheet - purchase commitments
        if (length(supplyRow) > 0) {
          commit_cols <- grep("Commitment", names(all_sheets$`Supply Data`))
          if (length(commit_cols) >= edit$col - 1) {
            all_sheets$`Supply Data`[supplyRow, commit_cols[edit$col - 1]] <- edit$value
          }
        }
      }
    }
    
    # Write updated data back to Excel
    writexl::write_xlsx(all_sheets, excelFilePath)
    
    # Clear the edited data tracking
    values$editedData <- list()
    
    showNotification("Data changes saved successfully", type = "message")
  })
  
  # Helper function to update dashboard fields
  updateDashboardFields <- function(partInfo, supplyInfo) {
    # Update part description
    if (nrow(partInfo) > 0) {
      # Find the description column
      desc_col <- which(names(partInfo) %in% c("Description", "Desc", "Part Description"))
      supplier_col <- which(names(partInfo) %in% c("Supplier", "Supplier Name"))
      lead_time_col <- which(names(partInfo) %in% c("Lead Time", "LeadTime"))
      price_col <- which(names(partInfo) %in% c("Unit Price", "Price"))
      yield_col <- which(names(partInfo) %in% c("Yield", "Part Yield"))
      
      if (length(desc_col) > 0)
        updateTextInput(session, "partDescription", value = partInfo[[desc_col[1]]])
      
      if (length(supplier_col) > 0)
        updateTextInput(session, "supplier", value = partInfo[[supplier_col[1]]])
      
      if (length(lead_time_col) > 0)
        updateTextInput(session, "leadTime", value = paste0(partInfo[[lead_time_col[1]]], " month(s)"))
      
      if (length(price_col) > 0)
        updateTextInput(session, "unitPrice", value = paste0("$", partInfo[[price_col[1]]]))
      
      if (length(yield_col) > 0) {
        yield_value <- partInfo[[yield_col[1]]]
        # Check if yield is decimal (0-1) or percentage (0-100)
        if (!is.na(yield_value)) {
          if (yield_value <= 1)
            updateTextInput(session, "partYield", value = paste0(round(yield_value * 100, 1), "%"))
          else
            updateTextInput(session, "partYield", value = paste0(round(yield_value, 1), "%"))
        }
      }
    }
    
    # Could also use supply info if additional fields are needed
  }
  
  # Helper function to calculate shortage warnings
  calculateShortageWarnings <- function() {
    req(values$monthlyData, input$forecastType)
    
    # Extract forecast type from input
    forecastType <- paste0(input$forecastType, " Forecast")
    
    # Skip if the forecast column doesn't exist
    if (!forecastType %in% values$monthlyData$Category) {
      return()
    }
    
    # Get forecast row
    forecastRow <- values$monthlyData[values$monthlyData$Category == forecastType, ]
    
    # Skip if no data
    if (nrow(forecastRow) == 0) {
      return()
    }
    
    # Data columns (months)
    dataCols <- names(values$monthlyData)[-1]  # All but the first column
    dataCols <- dataCols[1:min(6, length(dataCols))]  # First 6 months
    
    # Supply categories
    supplyCategories <- c("Scheduled Receipts", "CM #1 Inventory", "CM #2 Inventory", 
                          "OEM Inventory", "Supplier Capacity")
    
    # Create empty results df
    shortageDF <- data.frame(
      Month = dataCols,
      Forecast = as.numeric(NA),
      Supply = as.numeric(NA),
      Shortage = as.numeric(NA),
      Status = character(length(dataCols)),
      stringsAsFactors = FALSE
    )
    
    # For each month
    for (i in seq_along(dataCols)) {
      col <- dataCols[i]
      
      # Get forecast
      forecast <- as.numeric(forecastRow[[col]])
      
      # Get supply
      supplyRows <- values$monthlyData[values$monthlyData$Category %in% supplyCategories, ]
      supply <- sum(as.numeric(supplyRows[[col]]), na.rm = TRUE)
      
      # Calculate shortage
      shortage <- supply - forecast
      
      # Set status
      status <- ifelse(shortage < 0, "SHORTAGE", "OK")
      
      # Store in results
      shortageDF$Forecast[i] <- forecast
      shortageDF$Supply[i] <- supply
      shortageDF$Shortage[i] <- shortage
      shortageDF$Status[i] <- status
    }
    
    # Update the reactive value
    values$shortageWarnings <- shortageDF
  }
  
  # Output for title block
  output$titleBlock <- renderText({
    req(values$selectedPart)
    
    # Get part description
    partCol_partData <- names(values$partData)[2]
    partInfo <- values$partData %>%
      filter(.data[[partCol_partData]] == values$selectedPart)
    
    desc_col <- which(names(partInfo) %in% c("Description", "Desc", "Part Description"))
    
    if (nrow(partInfo) > 0 && length(desc_col) > 0) {
      paste0(values$selectedPart, " (", toupper(partInfo[[desc_col[1]]]), ")")
    } else {
      toupper(values$selectedPart)
    }
  })
  
  # Output for current month display
  output$currentMonthDisplay <- renderText({
    format(values$currentDate, "%B %Y")
  })
  
  # Output for monthly data table
  output$monthlyDataTable <- DT::renderDT({
    req(values$monthlyData)
    
    DT::datatable(values$monthlyData, 
                  options = list(
                    dom = 't',
                    ordering = FALSE,
                    columnDefs = list(
                      list(className = 'dt-center', targets = 1:12)
                    )
                  ),
                  rownames = FALSE,
                  editable = TRUE
    )
  })
  
  # Output for shortage warning table
  output$shortageWarningTable <- DT::renderDT({
    req(values$shortageWarnings)
    
    DT::datatable(values$shortageWarnings,
                  options = list(
                    dom = 't',
                    ordering = FALSE,
                    columnDefs = list(
                      list(className = 'dt-center', targets = 1:4),
                      list(targets = 4, render = JS(
                        "function(data, type, row, meta) {
                      return (data < 0) ? 
                        '<span style=\"color:red;font-weight:bold\">' + data + '</span>' : 
                        '<span style=\"color:green\">' + data + '</span>';
                    }"
                      ))
                    )
                  ),
                  rownames = FALSE
    ) %>%
      DT::formatStyle(
        'Status',
        backgroundColor = DT::styleEqual(
          c('SHORTAGE', 'OK'), 
          c('#ffdddd', '#ddffdd')
        )
      )
  })
  
  # Handle table edits
  observeEvent(input$monthlyDataTable_cell_edit, {
    info <- input$monthlyDataTable_cell_edit
    row <- info$row + 1  # Adjust for DT indices
    col <- info$col + 1  # Adjust for Category column
    value <- info$value
    
    # Store the edited value
    values$monthlyData[row, col] <- value
    
    # Track edited cells for saving back to source
    values$editedData[[paste(row, col, sep = "_")]] <- list(
      row = row,
      col = col,
      value = value
    )
    
    # Recalculate shortage warnings if needed
    calculateShortageWarnings()
  })
}