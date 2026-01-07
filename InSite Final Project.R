library(tcltk)
library(tcltk2)
library(DBI)
library(RSQLite)
library(emayili) 
library(dplyr)

EMAIL_SIMULATION_MODE <- FALSE 

# [OPTION A] GMAIL (Active)
SMTP_SERVER <- "smtp.gmail.com"
SMTP_PORT   <- 587

# CREDENTIALS 
SENDER_EMAIL <- "insitehardware@gmail.com" 
SENDER_PASSWORD <- "ktkj mkan jzyd tobi" 

ADMIN_USER <- "admin"
ADMIN_PASS <- "admin123"

# Connect to Database
db_path <- "hardware_db.sqlite" 
con <- dbConnect(RSQLite::SQLite(), db_path)

# Initialize Tables
dbExecute(con, "CREATE TABLE IF NOT EXISTS Inventory (
    Item_ID INTEGER PRIMARY KEY AUTOINCREMENT, 
    Item_Name TEXT NOT NULL, 
    Supplier TEXT,
    Supplier_Email TEXT,
    Box_Qty INTEGER DEFAULT 0, 
    Piece_Qty INTEGER DEFAULT 0, 
    Items_Per_Box INTEGER DEFAULT 1, 
    Price_Per_Piece REAL DEFAULT 0.0,
    Reorder_Level INTEGER DEFAULT 10
)")

# Database Migration Check
fields <- dbListFields(con, "Inventory")
if(!"Supplier_Email" %in% fields) {
  tryCatch({
    dbExecute(con, "ALTER TABLE Inventory ADD COLUMN Supplier_Email TEXT")
  }, error = function(e) { message("Migration handled.") })
}


# Initialize Main Window
tt <- tktoplevel()
tkwm.title(tt, "InSite Inventory Manager")
tkwm.geometry(tt, "1300x850")
tcl("package", "require", "Ttk")
tcl("ttk::setTheme", "clam")

# --- COLOR PALETTE ---
COL_PRIMARY   <- "#2c3e50"  # Dark Blue
COL_ACCENT    <- "#34495e"  # Lighter Blue
COL_SUCCESS   <- "#27ae60"  # Green
COL_INFO      <- "#2980b9"  # Blue
COL_WARNING   <- "#e67e22"  # Orange
COL_DANGER    <- "#c0392b"  # Red
COL_BG        <- "#ecf0f1"  # Light Gray
COL_TEXT      <- "#2c3e50"

tkconfigure(tt, background=COL_BG)

var_user <- tclVar("")
var_pass <- tclVar("")

var_id_hidden <- tclVar("")
var_name <- tclVar("")
var_supp <- tclVar("")
var_supp_email <- tclVar("")
var_box_qty <- tclVar("0")
var_pc_qty <- tclVar("0")
var_per_box <- tclVar("1")
var_price <- tclVar("0.00")
var_reorder <- tclVar("10")
var_qty_sell <- tclVar("1")
var_search  <- tclVar("")
var_status_msg <- tclVar(paste("Welcome,", ADMIN_USER, "| Database Connected"))

# Forward declaration
load_dashboard <- NULL

show_login_screen <- function() {
  # Background Frame
  login_bg <- ttkframe(tt, padding=0)
  tkpack(login_bg, expand=TRUE, fill="both")
  
  # Main Card
  card_frame <- tkframe(login_bg, bg="white", bd=2, relief="raised")
  tkplace(card_frame, relx=0.5, rely=0.5, anchor="center", width=350, height=320)
  
  # Header
  header_lbl <- tklabel(card_frame, text="\nInSite Login", font="Helvetica 14 bold", bg=COL_PRIMARY, fg="white")
  tkpack(header_lbl, fill="x", pady=c(0, 20))
  
  # Content Container
  content_frame <- tkframe(card_frame, bg="white", padx=20)
  tkpack(content_frame, fill="both", expand=TRUE)
  
  # Inputs
  tkpack(tklabel(content_frame, text="Username", font="Helvetica 9 bold", bg="white", fg=COL_TEXT), anchor="w")
  ent_user <- tkentry(content_frame, textvariable=var_user, font="Helvetica 10", bg="white", fg=COL_TEXT, relief="solid", bd=1)
  tkpack(ent_user, fill="x", pady=c(2, 10), ipady=3)
  
  # Force Focus to Window then Input
  tkfocus(tt)
  tkfocus(ent_user)
  
  tkpack(tklabel(content_frame, text="Password", font="Helvetica 9 bold", bg="white", fg=COL_TEXT), anchor="w")
  ent_pass <- tkentry(content_frame, textvariable=var_pass, show="*", font="Helvetica 10", bg="white", fg=COL_TEXT, relief="solid", bd=1)
  tkpack(ent_pass, fill="x", pady=c(2, 20), ipady=3)
  
  # Login Action
  attempt_login <- function() {
    if(tclvalue(var_user) == ADMIN_USER && tclvalue(var_pass) == ADMIN_PASS) {
      tkdestroy(login_bg)    # Remove the login screen
      load_dashboard()       # Load the dashboard
    } else {
      tkmessageBox(parent=tt, icon="error", message="Invalid Username or Password", title="Access Denied")
      tclvalue(var_pass) <- ""
    }
  }
  
  # Button Style just for login
  tcl("ttk::style", "configure", "Login.TButton", background=COL_PRIMARY, foreground="white", font="Helvetica 10 bold")
  
  tkbind(ent_pass, "<Return>", attempt_login)
  tkpack(ttkbutton(content_frame, text="SECURE LOGIN", command=attempt_login, style="Login.TButton"), fill="x", ipady=5)
  
  # Footer
  tkpack(tklabel(content_frame, text="Authorized Personnel Only", font="Helvetica 8 italic", fg="#7f8c8d", bg="white"), side="bottom", pady=10)
}

# MAIN APPLICATION LOGIC


load_dashboard <- function() {
  
  # CUSTOM BUTTON STYLES
  tcl("ttk::style", "configure", "Success.TButton", background=COL_SUCCESS, foreground="white", font="Helvetica 9 bold")
  tcl("ttk::style", "map", "Success.TButton", "-background", c("active", "#2ecc71")) 
  
  tcl("ttk::style", "configure", "Info.TButton", background=COL_INFO, foreground="white", font="Helvetica 9 bold")
  tcl("ttk::style", "map", "Info.TButton", "-background", c("active", "#3498db"))
  
  tcl("ttk::style", "configure", "Danger.TButton", background=COL_DANGER, foreground="white", font="Helvetica 9 bold")
  tcl("ttk::style", "map", "Danger.TButton", "-background", c("active", "#e74c3c"))
  
  tcl("ttk::style", "configure", "Warning.TButton", background=COL_WARNING, foreground="white", font="Helvetica 9 bold")
  tcl("ttk::style", "map", "Warning.TButton", "-background", c("active", "#f39c12"))
  
  # CORE LOGIC
  
  update_status <- function(msg) {
    tclvalue(var_status_msg) <- paste0("Log: ", msg, " [", format(Sys.time(), "%H:%M:%S"), "]")
  }
  
  refresh_list <- function() {
    children <- as.character(tcl(tree, "children", ""))
    for (child in children) tcl(tree, "delete", child)
    
    search_term <- trimws(tolower(tclvalue(var_search)))
    data <- dbGetQuery(con, "SELECT * FROM Inventory")
    
    if(nrow(data) > 0) {
      if(search_term != "") {
        data <- data %>% filter(grepl(search_term, tolower(Item_Name)) | grepl(search_term, tolower(Supplier)))
      }
      
      for(i in 1:nrow(data)) {
        total_loose <- data[i, "Piece_Qty"] + (data[i, "Box_Qty"] * data[i, "Items_Per_Box"])
        status_val <- if(total_loose <= data[i, "Reorder_Level"]) "LOW STOCK" else "OK"
        
        row_id <- tcl(tree, "insert", "", "end", values=as.character(c(
          data[i, "Item_ID"], data[i, "Item_Name"], data[i, "Supplier"], 
          data[i, "Box_Qty"], data[i, "Piece_Qty"], total_loose, status_val
        )))
        
        if(status_val == "LOW STOCK") tcl(tree, "item", row_id, tags="lowstock")
      }
    }
  }
  
  on_tree_click <- function() {
    sel <- as.character(tcl(tree, "selection"))
    if(length(sel) == 0) return()
    id <- as.character(tcl(tree, "item", sel, "-values"))[1]
    
    item <- dbGetQuery(con, paste0("SELECT * FROM Inventory WHERE Item_ID = ", id))
    if(nrow(item) > 0) {
      tclvalue(var_id_hidden)   <- item$Item_ID
      tclvalue(var_name)        <- item$Item_Name
      tclvalue(var_supp)        <- item$Supplier
      tclvalue(var_supp_email)  <- ifelse(is.na(item$Supplier_Email), "", item$Supplier_Email)
      tclvalue(var_box_qty)     <- item$Box_Qty
      tclvalue(var_pc_qty)      <- item$Piece_Qty
      tclvalue(var_per_box)     <- item$Items_Per_Box
      tclvalue(var_price)       <- item$Price_Per_Piece
      tclvalue(var_reorder)     <- item$Reorder_Level
      
      tkconfigure(btn_add, state="disabled")
      tkconfigure(btn_upd, state="normal")
      tkconfigure(btn_del, state="normal")
      update_status(paste("Selected Item ID:", id))
    }
  }
  
  clear_form <- function() {
    tclvalue(var_id_hidden) <- ""; tclvalue(var_name) <- ""; tclvalue(var_supp) <- ""
    tclvalue(var_supp_email) <- ""; tclvalue(var_box_qty) <- "0"; tclvalue(var_pc_qty) <- "0"
    tclvalue(var_per_box) <- "1"; tclvalue(var_price) <- "0.00"; tclvalue(var_reorder) <- "10"
    
    tkconfigure(btn_add, state="normal")
    tkconfigure(btn_upd, state="disabled")
    tkconfigure(btn_del, state="disabled")
    update_status("Form Cleared")
  }
  
  save_data <- function(mode="ADD") {
    name <- tclvalue(var_name)
    if(name == "") { tkmessageBox(icon="error", message="Item Name is required!"); return() }
    
    params <- list(
      name, tclvalue(var_supp), tclvalue(var_supp_email),
      as.integer(tclvalue(var_box_qty)), as.integer(tclvalue(var_pc_qty)),
      as.integer(tclvalue(var_per_box)), as.numeric(tclvalue(var_price)),
      as.integer(tclvalue(var_reorder))
    )
    
    if(mode == "ADD") {
      dbExecute(con, "INSERT INTO Inventory (Item_Name, Supplier, Supplier_Email, Box_Qty, Piece_Qty, Items_Per_Box, Price_Per_Piece, Reorder_Level) VALUES (?,?,?,?,?,?,?,?)", params=params)
      update_status("New record saved.")
    } else {
      params[[9]] <- as.integer(tclvalue(var_id_hidden))
      dbExecute(con, "UPDATE Inventory SET Item_Name=?, Supplier=?, Supplier_Email=?, Box_Qty=?, Piece_Qty=?, Items_Per_Box=?, Price_Per_Piece=?, Reorder_Level=? WHERE Item_ID=?", params=params)
      update_status("Record updated.")
    }
    refresh_list(); clear_form()
  }
  
  delete_item <- function() {
    id <- tclvalue(var_id_hidden)
    if(id == "") return()
    confirm <- tkmessageBox(type="yesno", message=paste("Are you sure you want to delete ID", id, "?"), icon="warning")
    if(as.character(confirm) == "yes") {
      dbExecute(con, "DELETE FROM Inventory WHERE Item_ID=?", params=list(as.integer(id)))
      update_status(paste("Deleted Item ID:", id))
      refresh_list(); clear_form()
    }
  }
  
  sell_logic <- function() {
    id <- tclvalue(var_id_hidden)
    if(id == "") return()
    qty <- as.integer(tclvalue(var_qty_sell))
    
    data <- dbGetQuery(con, paste0("SELECT * FROM Inventory WHERE Item_ID = ", id))
    cur_b <- data$Box_Qty; cur_p <- data$Piece_Qty; ppb <- data$Items_Per_Box
    
    # Auto-breakdown logic
    while(cur_p < qty && cur_b > 0) {
      cur_b <- cur_b - 1
      cur_p <- cur_p + ppb
      update_status(paste("Automated: Box opened for", data$Item_Name))
    }
    
    if(cur_p >= qty) {
      dbExecute(con, "UPDATE Inventory SET Box_Qty=?, Piece_Qty=? WHERE Item_ID=?", params=list(cur_b, cur_p - qty, id))
      refresh_list()
      tkmessageBox(message=paste("Transaction Successful. Total: PHP", qty * data$Price_Per_Piece))
    } else {
      tkmessageBox(icon="error", message="Critical: Insufficient Stock Available.")
    }
  }
  
  send_emails <- function() {
    data <- dbGetQuery(con, "SELECT * FROM Inventory") %>%
      mutate(Total = Piece_Qty + (Box_Qty * Items_Per_Box)) %>%
      filter(Total <= Reorder_Level, !is.na(Supplier_Email), Supplier_Email != "")
    
    if(nrow(data) == 0) { tkmessageBox(message="No low-stock items requiring email."); return() }
    
    tkconfigure(tt, cursor="watch")
    success <- 0
    suppliers <- unique(data$Supplier_Email)
    error_log <- c()
    
    for(s_email in suppliers) {
      tryCatch({
        items_to_order <- data %>% filter(Supplier_Email == s_email)
        
        # --- IMPROVED EMAIL CONTENT ---
        subject_line <- paste0("Purchase Order: InSite Restock [", Sys.Date(), "]")
        
        body <- paste0(
          "Dear Supplier,\n\n",
          "Please process the following purchase order immediately to replenish our stock:\n\n",
          sprintf("%-35s | %-15s", "ITEM NAME", "QUANTITY"), "\n",
          paste(rep("-", 55), collapse=""), "\n"
        )
        
        for(j in 1:nrow(items_to_order)) {
          # Logic: Order enough to reach double the reorder level
          box_req <- ceiling((items_to_order$Reorder_Level[j]*2 - items_to_order$Total[j]) / items_to_order$Items_Per_Box[j])
          if(box_req < 1) box_req <- 1
          
          row_str <- sprintf("%-35s | %-15s", 
                             substr(items_to_order$Item_Name[j], 1, 33), 
                             paste(box_req, "Boxes"))
          body <- paste0(body, row_str, "\n")
        }
        
        body <- paste0(body, paste(rep("-", 55), collapse=""), "\n\n")
        body <- paste0(body, "Please confirm receipt and delivery timeline.\n\n")
        body <- paste0(body, "Best regards,\nInSite Inventory Manager")
        
        # --- SENDING ---
        email <- envelope() %>% 
          from(SENDER_EMAIL) %>% 
          to(s_email) %>% 
          subject(subject_line) %>% 
          text(body)
        
        smtp <- server(host=SMTP_SERVER, port=SMTP_PORT, username=SENDER_EMAIL, password=SENDER_PASSWORD)
        smtp(email, verbose=TRUE)
        
        success <- success + 1
        
        # --- AUTO-REPLENISHMENT ---
        # Update stock after successful email
        for(j in 1:nrow(items_to_order)) {
          box_req <- ceiling((items_to_order$Reorder_Level[j]*2 - items_to_order$Total[j]) / items_to_order$Items_Per_Box[j])
          if(box_req < 1) box_req <- 1
          
          new_qty <- items_to_order$Box_Qty[j] + box_req
          dbExecute(con, "UPDATE Inventory SET Box_Qty = ? WHERE Item_ID = ?", 
                    params=list(new_qty, items_to_order$Item_ID[j]))
        }
        
      }, error = function(e) { 
        update_status(paste("Email Error:", s_email))
        error_log <<- c(error_log, paste0(s_email, ": ", e$message))
      })
    }
    tkconfigure(tt, cursor="arrow")
    
    # Refresh the UI to reflect replenishment
    refresh_list()
    
    msg <- paste("Batch complete. Emails sent to", success, "suppliers.")
    if(success > 0) {
      msg <- paste(msg, "\n\nInventory has been auto-replenished based on orders.")
    }
    
    if(EMAIL_SIMULATION_MODE) {
      msg <- paste(msg, "\n(Simulation Mode: Check Console for email content)")
    }
    
    if(length(error_log) > 0) {
      msg <- paste(msg, "\n\nFAILED:\n", paste(error_log, collapse="\n"))
      msg <- paste(msg, "\n\nCheck R Console for technical details.")
    }
    tkmessageBox(message=msg)
  }
  
  populate_mock_data <- function() {
    mock_data <- list(
      list("Pro-Grip Hammer", "BuildRight Tools", "sales@buildright.com", 15, 0, 1, 450.00, 5),
      list("Power Drill 18V", "TechMechanic", "orders@techmech.com", 8, 0, 1, 3500.00, 3),
      list("Steel Nails (1kg)", "Fasteners Inc.", "supply@fastener.com", 50, 0, 1, 120.00, 10),
      list("Safety Helmet", "SiteSafe", "contact@sitesafe.com", 20, 5, 1, 250.00, 8),
      list("Cement (40kg)", "SolidBase", "depot@solidbase.com", 100, 0, 1, 230.00, 20)
    )
    
    for(item in mock_data) {
      dbExecute(con, "INSERT INTO Inventory (Item_Name, Supplier, Supplier_Email, Box_Qty, Piece_Qty, Items_Per_Box, Price_Per_Piece, Reorder_Level) VALUES (?,?,?,?,?,?,?,?)", params=item)
    }
    refresh_list()
    update_status("Sample data loaded successfully.")
    tkmessageBox(message="Added 5 sample items to the inventory.")
  }
  
  # --- UI CONSTRUCTION ---
  
  header_frame <- tkframe(tt, background=COL_PRIMARY, height=60)
  tkpack(header_frame, fill="x")
  tkpack(tklabel(header_frame, text="InSite Inventory Manager", fg="white", bg=COL_PRIMARY, font="Helvetica 16 bold"), side="left", padx=20, pady=15) # UPDATED HEADER
  
  search_frame <- tkframe(header_frame, bg=COL_PRIMARY)
  tkpack(search_frame, side="right", padx=20)
  tkpack(tklabel(search_frame, text="Search:", fg="white", bg=COL_PRIMARY), side="left")
  ent_search <- ttkentry(search_frame, textvariable=var_search, width=25)
  tkpack(ent_search, side="left", padx=5)
  tkbind(ent_search, "<KeyRelease>", function(...) refresh_list())
  
  main_container <- ttkframe(tt, padding=10)
  tkpack(main_container, fill="both", expand=TRUE)
  
  sidebar <- ttkframe(main_container, width=350, padding=10)
  tkpack(sidebar, side="left", fill="y")
  
  # Group 1: Item Details
  group1 <- ttklabelframe(sidebar, text=" Item Information ", padding=10)
  tkpack(group1, fill="x", pady=5)
  create_input <- function(parent, lab, var) {
    f <- ttkframe(parent); tkpack(f, fill="x", pady=2)
    tkpack(ttklabel(f, text=lab, width=15), side="left")
    tkpack(ttkentry(f, textvariable=var), side="right", fill="x", expand=TRUE)
  }
  create_input(group1, "Item Name:", var_name)
  create_input(group1, "Supplier:", var_supp)
  create_input(group1, "Supp. Email:", var_supp_email)
  
  # Group 2: Stock Control
  group2 <- ttklabelframe(sidebar, text=" Stock Levels & Pricing ", padding=10)
  tkpack(group2, fill="x", pady=5)
  create_input(group2, "Boxes in Stock:", var_box_qty)
  create_input(group2, "Loose Pieces:", var_pc_qty)
  create_input(group2, "Qty Per Box:", var_per_box)
  create_input(group2, "Price/Piece:", var_price)
  create_input(group2, "Reorder at:", var_reorder)
  
  # Group 3: Core Actions
  group3 <- ttkframe(sidebar, padding=5)
  tkpack(group3, fill="x", pady=10)
  
  # APPLYING CUSTOM STYLES HERE
  btn_add <- ttkbutton(group3, text="Add New", command=function() save_data("ADD"), style="Success.TButton")
  btn_upd <- ttkbutton(group3, text="Update", command=function() save_data("UPD"), state="disabled", style="Info.TButton")
  btn_del <- ttkbutton(group3, text="Delete", command=delete_item, state="disabled", style="Danger.TButton")
  btn_clr <- ttkbutton(group3, text="Clear", command=clear_form) # Default style for clear
  
  tkgrid(btn_add, row=0, column=0, sticky="ew", padx=2); tkgrid(btn_upd, row=0, column=1, sticky="ew", padx=2)
  tkgrid(btn_del, row=1, column=0, sticky="ew", padx=2); tkgrid(btn_clr, row=1, column=1, sticky="ew", padx=2)
  
  # Point of Sale
  tkpack(ttkseparator(sidebar), fill="x", pady=10)
  pos_group <- ttklabelframe(sidebar, text=" Point of Sale ", padding=10)
  tkpack(pos_group, fill="x")
  tkpack(ttklabel(pos_group, text="Qty to Sell:"), side="left")
  tkpack(ttkentry(pos_group, textvariable=var_qty_sell, width=8), side="left", padx=5)
  tkpack(ttkbutton(pos_group, text="SELL", command=sell_logic, style="Warning.TButton"), side="left")
  
  # Bulk Actions
  tkpack(ttkframe(sidebar), expand=TRUE, fill="both")
  # Use default style or specific if preferred for these actions
  ttkbutton(sidebar, text="Send Restock Emails", command=send_emails) %>% tkpack(fill="x", pady=2)
  ttkbutton(sidebar, text="Populate Sample Data", command=populate_mock_data) %>% tkpack(fill="x", pady=2)
  
  # Right Panel: Dashboard
  dashboard <- ttkframe(main_container, padding=10)
  tkpack(dashboard, side="right", fill="both", expand=TRUE)
  tkpack(ttklabel(dashboard, text="Live Inventory Status", font="Helvetica 12 bold"), anchor="w", pady=c(0,10))
  
  tree_frame <- ttkframe(dashboard)
  tkpack(tree_frame, fill="both", expand=TRUE)
  
  cols <- c("ID", "Item Name", "Supplier", "Boxes", "Pieces", "Total Stock", "Status")
  tree <- ttktreeview(tree_frame, columns=1:7, show="headings", height=30)
  tcl(tree, "tag", "configure", "lowstock", foreground=COL_DANGER, font="Helvetica 9 bold")
  
  for(i in 1:7) {
    tcl(tree, "heading", i, text=cols[i])
    tcl(tree, "column", i, width=if(i==2) 250 else 100, anchor="center")
  }
  
  scrollbar <- ttkscrollbar(tree_frame, orient="vertical", command=function(...) tkyview(tree, ...))
  tkconfigure(tree, yscrollcommand=function(...) tkset(scrollbar, ...))
  tkgrid(tree, row=0, column=0, sticky="nsew")
  tkgrid(scrollbar, row=0, column=1, sticky="ns")
  tkgrid.columnconfigure(tree_frame, 0, weight=1); tkgrid.rowconfigure(tree_frame, 0, weight=1)
  
  tkbind(tree, "<<TreeviewSelect>>", on_tree_click)
  
  # Status Footer
  footer <- tkframe(tt, bd=1, relief="sunken", bg="#eee")
  tkpack(footer, fill="x", side="bottom")
  tkpack(tklabel(footer, textvariable=var_status_msg, font="Helvetica 8", bg="#eee"), side="left", padx=10)
  
  refresh_list()
}

# Start the application
show_login_screen()
tkwait.window(tt)