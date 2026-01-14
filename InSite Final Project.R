library(tcltk)
library(tcltk2)
library(DBI)
library(RSQLite)
library(emayili) 
library(dplyr)
library(ggplot2)    
library(gridExtra)

# ==============================================================================
# 1. CONFIGURATION
# ==============================================================================
EMAIL_SIMULATION_MODE <- FALSE 

# YOUR CREDENTIALS
SENDER_EMAIL <- "insitehardware@gmail.com" 
SENDER_PASSWORD <- "ktkj mkan jzyd tobi" 
SMTP_SERVER <- "smtp.gmail.com"
SMTP_PORT <- 587

# ==============================================================================
# 2. DATABASE SETUP
# ==============================================================================
db_path <- "hardware_db.sqlite" 
con <- dbConnect(RSQLite::SQLite(), db_path)
dbExecute(con, "PRAGMA foreign_keys = ON") 

# Tables
dbExecute(con, "CREATE TABLE IF NOT EXISTS Users (
    User_ID INTEGER PRIMARY KEY AUTOINCREMENT, Username TEXT UNIQUE, Password TEXT)")

dbExecute(con, "CREATE TABLE IF NOT EXISTS Inventory (
    Item_ID INTEGER PRIMARY KEY AUTOINCREMENT, Item_Name TEXT NOT NULL, 
    Supplier TEXT, Supplier_Email TEXT, Box_Qty INTEGER DEFAULT 0, 
    Piece_Qty INTEGER DEFAULT 0, Items_Per_Box INTEGER DEFAULT 1, 
    Price_Per_Piece REAL DEFAULT 0.0, Reorder_Level INTEGER DEFAULT 10)")

dbExecute(con, "CREATE TABLE IF NOT EXISTS Transactions (
    Trans_ID INTEGER PRIMARY KEY AUTOINCREMENT, Item_ID INTEGER, 
    Qty_Sold INTEGER, Total_Amount REAL, Trans_Date DATETIME DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY(Item_ID) REFERENCES Inventory(Item_ID))")

if(nrow(dbGetQuery(con, "SELECT * FROM Users")) == 0) {
  dbExecute(con, "INSERT INTO Users (Username, Password) VALUES ('admin', 'admin123')")
}

# ==============================================================================
# 3. GUI THEME ENGINE
# ==============================================================================
tt <- tktoplevel()
tkwm.title(tt, "InSite Inventory Manager")
tkwm.geometry(tt, "1380x950") 
tcl("package", "require", "Ttk")
tcl("ttk::setTheme", "clam") 

# --- PALETTE ---
COL_SIDEBAR <- "#2c3e50" 
COL_CONTENT <- "#ffffff" 
COL_ACCENT  <- "#3498db" 
COL_SUCCESS <- "#27ae60" 
COL_DANGER  <- "#c0392b"
COL_HEADER  <- "#f1f2f6" 

tkconfigure(tt, background=COL_CONTENT)

# --- STYLES ---
tcl("ttk::style", "configure", "Sidebar.TFrame", background=COL_SIDEBAR)
tcl("ttk::style", "configure", "Content.TFrame", background=COL_CONTENT)
tcl("ttk::style", "configure", "Card.TFrame", background="white", relief="flat", borderwidth=0)

# Buttons
tcl("ttk::style", "configure", "Nav.TButton", font="Helvetica 10 bold", padding=6)
tcl("ttk::style", "configure", "Success.TButton", background=COL_SUCCESS, foreground="white", font="Helvetica 9 bold")
tcl("ttk::style", "map", "Success.TButton", background=c("active", "#2ecc71"))
tcl("ttk::style", "configure", "Danger.TButton", background=COL_DANGER, foreground="white", font="Helvetica 9 bold")
tcl("ttk::style", "map", "Danger.TButton", background=c("active", "#e74c3c"))
tcl("ttk::style", "configure", "Info.TButton", background=COL_ACCENT, foreground="white", font="Helvetica 9 bold")
tcl("ttk::style", "map", "Info.TButton", background=c("active", "#2980b9"))

# Table Styling
tcl("ttk::style", "configure", "Treeview", 
    background="white", fieldbackground="white", foreground="#2d3436", 
    rowheight=30, font="Helvetica 10", borderwidth=0)
tcl("ttk::style", "configure", "Treeview.Heading", 
    background=COL_HEADER, foreground="#2c3e50", font="Helvetica 10 bold", padding=8)
tcl("ttk::style", "map", "Treeview", background=c("selected", COL_ACCENT))

# ==============================================================================
# 4. GLOBAL STATE
# ==============================================================================
var_user <- tclVar(""); var_pass <- tclVar("")
var_status_msg <- tclVar("System Ready. Please Login.")
var_view_mode <- tclVar("INVENTORY") 
var_total_rev <- tclVar("Total Revenue: P 0.00")
var_search  <- tclVar("")

# Inventory Inputs
var_id_hidden <- tclVar(""); var_name <- tclVar(""); var_supp <- tclVar("")
var_supp_email <- tclVar(""); var_box_qty <- tclVar("0"); var_pc_qty <- tclVar("0")
var_per_box <- tclVar("1"); var_price <- tclVar("0.00"); var_reorder <- tclVar("10")
var_qty_sell <- tclVar("1")

load_dashboard <- NULL

# ==============================================================================
# 5. LOGIN SCREEN
# ==============================================================================
show_login_screen <- function() {
  login_bg <- tkframe(tt, bg=COL_SIDEBAR)
  tkpack(login_bg, expand=TRUE, fill="both")
  
  card <- tkframe(login_bg, bg="white", bd=0)
  tkplace(card, relx=0.5, rely=0.5, anchor="center", width=400, height=350)
  
  tkpack(tkframe(card, bg=COL_ACCENT, height=8), fill="x") 
  
  tkpack(tklabel(card, text="\nInSite", font="Helvetica 20 bold", bg="white", fg=COL_SIDEBAR), fill="x")
  tkpack(tklabel(card, text="Inventory Management System", font="Helvetica 10", bg="white", fg="#7f8c8d"), fill="x", pady=c(0,20))
  
  content <- tkframe(card, bg="white", padx=30)
  tkpack(content, fill="both")
  
  tkpack(tklabel(content, text="Username", bg="white", font="Helvetica 9 bold"), anchor="w")
  tkpack(tkentry(content, textvariable=var_user, font="Helvetica 11", relief="solid", bd=1), fill="x", pady=5, ipady=4)
  
  tkpack(tklabel(content, text="Password", bg="white", font="Helvetica 9 bold"), anchor="w", pady=c(10,0))
  tkpack(tkentry(content, textvariable=var_pass, show="*", font="Helvetica 11", relief="solid", bd=1), fill="x", pady=5, ipady=4)
  
  attempt <- function() {
    u <- tclvalue(var_user); p <- tclvalue(var_pass)
    if(nrow(dbGetQuery(con, "SELECT * FROM Users WHERE Username=? AND Password=?", params=list(u,p))) > 0) {
      tkdestroy(login_bg); load_dashboard(u)
    } else {
      tkmessageBox(icon="error", message="Invalid Credentials")
    }
  }
  tkbind(tt, "<Return>", attempt)
  tkpack(ttkbutton(content, text="LOGIN", command=attempt, style="Info.TButton"), fill="x", pady=30)
}

# ==============================================================================
# 6. DASHBOARD CONTROLLER
# ==============================================================================
load_dashboard <- function(user) {
  
  log_msg <- function(msg) tclvalue(var_status_msg) <- paste0("Log: ", msg, " [", format(Sys.time(), "%H:%M"), "]")
  log_msg(paste("Welcome,", user))
  
  # --- GRAPH RENDERER ---
  render_analytics <- function() {
    count <- dbGetQuery(con, "SELECT COUNT(*) as c FROM Transactions")$c
    try(tcl("image", "delete", "dash_plot"), silent=TRUE)
    
    if(count == 0) {
      tkconfigure(lbl_plot, image="")
      tkconfigure(lbl_no_data, text="No data available. Click 'Populate Sample Data' first!")
      return()
    } else {
      tkconfigure(lbl_no_data, text="")
    }
    
    df_rev <- dbGetQuery(con, "SELECT I.Item_Name, SUM(T.Total_Amount) as Revenue 
                               FROM Transactions T JOIN Inventory I ON T.Item_ID=I.Item_ID 
                               GROUP BY I.Item_Name ORDER BY Revenue DESC LIMIT 5")
    
    df_trend <- dbGetQuery(con, "SELECT date(Trans_Date) as Day, SUM(Total_Amount) as Daily_Total 
                                 FROM Transactions GROUP BY date(Trans_Date) ORDER BY Day")
    df_trend$Day <- as.Date(df_trend$Day)
    
    modern_theme <- theme_minimal(base_size = 14) + 
      theme(
        panel.grid.major.y = element_line(color = "#f0f0f0"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(color = "#64748b"),
        axis.title = element_blank(),
        plot.title = element_text(face = "bold", size = 16, color = "#1e293b", margin = margin(b = 15))
      )
    
    p1 <- ggplot(df_rev, aes(x=reorder(Item_Name, Revenue), y=Revenue)) +
      geom_col(fill=COL_SIDEBAR, width=0.7) + 
      coord_flip() + 
      modern_theme +
      labs(title="Top Performing Products") +
      geom_text(aes(label=paste0("P", format(Revenue, big.mark=","))), hjust=-0.1, color=COL_SIDEBAR, fontface="bold") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3)))
    
    p2 <- ggplot(df_trend, aes(x=Day, y=Daily_Total, group=1)) +
      geom_area(fill=COL_SUCCESS, alpha=0.2) +
      geom_line(color=COL_SUCCESS, linewidth=1.2) +
      geom_point(color=COL_SUCCESS, size=3) +
      modern_theme +
      labs(title="30-Day Sales Trend") +
      scale_y_continuous(labels = scales::comma)
    
    tmp_file <- tempfile(fileext=".png")
    png(filename=tmp_file, width=1200, height=900, res=110) 
    grid.arrange(p1, p2, nrow=2)
    dev.off()
    
    tcl("image", "create", "photo", "dash_plot", file=tmp_file)
    tkconfigure(lbl_plot, image="dash_plot")
  }
  
  # --- REFRESH LOGIC ---
  refresh_view <- function() {
    for(child in as.character(tcl(tree, "children", ""))) tcl(tree, "delete", child)
    mode <- tclvalue(var_view_mode)
    
    if(mode == "ANALYTICS") {
      tkpack.forget(frame_tree_container)
      tkpack(frame_plot_container, fill="both", expand=TRUE, padx=10, pady=5)
      render_analytics()
      tkconfigure(lbl_header, text="Executive Analytics Dashboard")
      return()
    } else {
      tkpack.forget(frame_plot_container)
      tkpack(frame_tree_container, fill="both", expand=TRUE, padx=10, pady=5)
    }
    
    # Common Search Logic
    term <- tolower(tclvalue(var_search))
    
    # --- INVENTORY VIEW ---
    if(mode == "INVENTORY") {
      tkconfigure(lbl_header, text="Inventory Database")
      
      cols <- c("ID", "Item Name", "Supplier", "Total Stock", "Price", "Status")
      tcl(tree, "configure", columns=cols, displaycolumns="#all")
      
      headings <- c("ID", "Item Name", "Supplier", "Stock Level", "Price / Unit", "Status")
      widths   <- c(50,   500,         350,        150,           150,            150)
      
      for(i in 1:6) {
        tcl(tree, "heading", cols[i], text=headings[i], anchor="w")
        tcl(tree, "column", cols[i], width=widths[i], anchor="w")
      }
      
      data <- dbGetQuery(con, "SELECT * FROM Inventory")
      
      # FIX: Search in Name OR Supplier OR ID
      if(term != "") {
        data <- data %>% filter(
          grepl(term, tolower(Item_Name)) | 
            grepl(term, tolower(Supplier)) | 
            grepl(term, as.character(Item_ID))
        )
      }
      
      if(nrow(data) > 0) {
        for(i in 1:nrow(data)) {
          tot <- data$Piece_Qty[i] + (data$Box_Qty[i]*data$Items_Per_Box[i])
          stat <- if(tot <= data$Reorder_Level[i]) "LOW STOCK" else "OK"
          price_fmt <- paste0("P ", format(data$Price_Per_Piece[i], nsmall=2, big.mark=","))
          
          row <- tcl(tree, "insert", "", "end", values=c(
            data$Item_ID[i], data$Item_Name[i], data$Supplier[i], tot, price_fmt, stat
          ))
          if(stat=="LOW STOCK") tcl(tree, "item", row, tags="lowstock")
        }
      }
      
    } 
    # --- SALES REPORT VIEW ---
    else if(mode == "REPORT") {
      tkconfigure(lbl_header, text="Sales Transaction History")
      
      cols <- c("ID", "Item Name", "Qty Sold", "Total (PHP)", "Date")
      tcl(tree, "configure", columns=cols, displaycolumns="#all")
      
      headings <- c("ID", "Item Name", "Qty Sold", "Total Amount", "Transaction Date")
      widths   <- c(60,   600,         150,             250,            400)
      
      for(i in 1:5) {
        tcl(tree, "heading", cols[i], text=headings[i], anchor="w")
        tcl(tree, "column", cols[i], width=widths[i], anchor="w")
      }
      
      data <- dbGetQuery(con, "SELECT T.Trans_ID, I.Item_Name, T.Qty_Sold, T.Total_Amount, T.Trans_Date FROM Transactions T JOIN Inventory I ON T.Item_ID=I.Item_ID ORDER BY T.Trans_Date DESC")
      
      # FIX: Search in Item Name OR Transaction ID
      if(term != "") {
        data <- data %>% filter(
          grepl(term, tolower(Item_Name)) | 
            grepl(term, as.character(Trans_ID))
        )
      }
      
      if(nrow(data) > 0) {
        for(i in 1:nrow(data)) {
          fmt_date <- format(as.POSIXct(data$Trans_Date[i]), "%Y-%m-%d %H:%M")
          fmt_amt <- paste0("P ", format(data$Total_Amount[i], nsmall=2, big.mark=","))
          tcl(tree, "insert", "", "end", values=c(data$Trans_ID[i], data$Item_Name[i], data$Qty_Sold[i], fmt_amt, fmt_date))
        }
        tclvalue(var_total_rev) <- paste("Total Revenue: P", format(sum(data$Total_Amount, na.rm=T), big.mark=","))
      } else {
        tclvalue(var_total_rev) <- "Total Revenue: P 0.00"
      }
    }
  }
  
  # --- HANDLERS ---
  switch_mode <- function(m) { 
    tclvalue(var_view_mode) <- m
    tclvalue(var_search) <- "" 
    refresh_view() 
  }
  
  on_select <- function() {
    if(tclvalue(var_view_mode) != "INVENTORY") return()
    sel <- as.character(tcl(tree, "selection"))
    if(length(sel)==0) return()
    id <- as.character(tcl(tree, "item", sel, "-values"))[1]
    d <- dbGetQuery(con, paste0("SELECT * FROM Inventory WHERE Item_ID=", id))
    
    if(nrow(d) > 0) {
      tclvalue(var_id_hidden)<-d$Item_ID; tclvalue(var_name)<-d$Item_Name; tclvalue(var_supp)<-d$Supplier
      tclvalue(var_supp_email)<-ifelse(is.na(d$Supplier_Email),"",d$Supplier_Email)
      tclvalue(var_box_qty)<-d$Box_Qty; tclvalue(var_pc_qty)<-d$Piece_Qty
      tclvalue(var_per_box)<-d$Items_Per_Box; tclvalue(var_price)<-d$Price_Per_Piece
      tclvalue(var_reorder)<-d$Reorder_Level
      tkconfigure(btn_add, state="disabled"); tkconfigure(btn_upd, state="normal"); tkconfigure(btn_del, state="normal")
      log_msg(paste("Selected Item:", d$Item_Name))
    }
  }
  
  run_crud <- function(action) {
    p <- list(tclvalue(var_name), tclvalue(var_supp), tclvalue(var_supp_email),
              as.integer(tclvalue(var_box_qty)), as.integer(tclvalue(var_pc_qty)),
              as.integer(tclvalue(var_per_box)), as.numeric(tclvalue(var_price)),
              as.integer(tclvalue(var_reorder)))
    
    if(action == "ADD") {
      dbExecute(con, "INSERT INTO Inventory (Item_Name, Supplier, Supplier_Email, Box_Qty, Piece_Qty, Items_Per_Box, Price_Per_Piece, Reorder_Level) VALUES (?,?,?,?,?,?,?,?)", params=p)
      log_msg("Item Added")
    } else if (action == "UPD") {
      p <- c(p, as.integer(tclvalue(var_id_hidden)))
      dbExecute(con, "UPDATE Inventory SET Item_Name=?, Supplier=?, Supplier_Email=?, Box_Qty=?, Piece_Qty=?, Items_Per_Box=?, Price_Per_Piece=?, Reorder_Level=? WHERE Item_ID=?", params=p)
      log_msg("Item Updated")
    } else if (action == "DEL") {
      if(as.character(tkmessageBox(type="yesno", message="Confirm delete?"))=="yes") {
        dbExecute(con, "DELETE FROM Inventory WHERE Item_ID=?", params=list(as.integer(tclvalue(var_id_hidden))))
        log_msg("Item Deleted")
      }
    }
    tclvalue(var_id_hidden) <- ""; tclvalue(var_name) <- ""; tclvalue(var_box_qty) <- "0"
    tkconfigure(btn_add, state="normal"); tkconfigure(btn_upd, state="disabled"); tkconfigure(btn_del, state="disabled")
    if(tclvalue(var_view_mode) != "INVENTORY") switch_mode("INVENTORY") else refresh_view()
  }
  
  run_pos <- function() {
    id <- tclvalue(var_id_hidden); qty <- as.integer(tclvalue(var_qty_sell))
    if(id=="") return()
    d <- dbGetQuery(con, paste0("SELECT * FROM Inventory WHERE Item_ID=", id))
    cur_b <- d$Box_Qty; cur_p <- d$Piece_Qty
    while(cur_p < qty && cur_b > 0) { cur_b <- cur_b-1; cur_p <- cur_p + d$Items_Per_Box }
    if(cur_p >= qty) {
      dbExecute(con, "UPDATE Inventory SET Box_Qty=?, Piece_Qty=? WHERE Item_ID=?", params=list(cur_b, cur_p-qty, id))
      dbExecute(con, "INSERT INTO Transactions (Item_ID, Qty_Sold, Total_Amount) VALUES (?,?,?)", params=list(id, qty, qty*d$Price_Per_Piece))
      log_msg(paste("Sold", qty, d$Item_Name))
      if(tclvalue(var_view_mode) != "INVENTORY") switch_mode("INVENTORY") else refresh_view()
      tkmessageBox(message=paste("Sale Recorded: P", format(qty*d$Price_Per_Piece, big.mark=",")))
    } else tkmessageBox(message="Insufficient Stock", icon="error")
  }
  
  # --- SUPER PROFESSIONAL HTML EMAIL ---
  run_email <- function() {
    # 1. Check for Low Stock
    data <- dbGetQuery(con, "SELECT * FROM Inventory") %>%
      mutate(Total = Piece_Qty + (Box_Qty * Items_Per_Box)) %>%
      filter(Total <= Reorder_Level, !is.na(Supplier_Email), Supplier_Email != "")
    
    if(nrow(data)==0) { tkmessageBox(message="No low stock items."); return() }
    tkconfigure(tt, cursor="watch")
    
    error_log <- c()
    success_count <- 0
    
    # 2. Iterate Suppliers
    for(s_email in unique(data$Supplier_Email)) {
      tryCatch({
        items_to_order <- data %>% filter(Supplier_Email == s_email)
        supplier_name <- items_to_order$Supplier[1]
        
        # --- HTML BODY BUILDER ---
        html_rows <- ""
        for(j in 1:nrow(items_to_order)) {
          qty_to_order <- items_to_order$Reorder_Level[j] * 3
          
          html_rows <- paste0(html_rows, 
                              "<tr>",
                              "<td style='padding: 10px; border-bottom: 1px solid #eee; color: #333;'>", items_to_order$Item_Name[j], "</td>",
                              "<td style='padding: 10px; border-bottom: 1px solid #eee; color: #333; text-align: right;'><strong>", qty_to_order, " Boxes</strong></td>",
                              "</tr>")
        }
        
        html_content <- paste0(
          "<html><body style='font-family: Arial, sans-serif; background-color: #f9f9f9; padding: 20px;'>",
          "<div style='max-width: 600px; margin: 0 auto; background: #fff; border-radius: 8px; overflow: hidden; box-shadow: 0 4px 10px rgba(0,0,0,0.1);'>",
          "<div style='background-color: #2c3e50; color: #fff; padding: 25px; text-align: center;'>",
          "<h1 style='margin: 0; font-size: 24px; letter-spacing: 1px;'>PURCHASE ORDER</h1>",
          "<p style='margin: 5px 0 0; opacity: 0.8;'>InSite Inventory Management System</p>",
          "</div>",
          "<div style='padding: 30px;'>",
          "<p style='color: #555;'>Dear <strong>", supplier_name, "</strong>,</p>",
          "<p style='color: #555; line-height: 1.5;'>Please accept this official order for the following items. We request immediate processing.</p>",
          "<table style='width: 100%; border-collapse: collapse; margin-top: 20px;'>",
          "<tr style='background-color: #f4f4f4;'>",
          "<th style='text-align: left; padding: 10px; color: #777;'>ITEM DESCRIPTION</th>",
          "<th style='text-align: right; padding: 10px; color: #777;'>QUANTITY</th>",
          "</tr>",
          html_rows,
          "</table>",
          "<div style='margin-top: 30px; padding: 15px; background-color: #ecf0f1; border-radius: 4px;'>",
          "<p style='margin: 0; font-size: 13px; color: #7f8c8d;'><strong>Order Date:</strong> ", Sys.Date(), "</p>",
          "<p style='margin: 5px 0 0; font-size: 13px; color: #7f8c8d;'><strong>Status:</strong> Urgent Restock</p>",
          "</div>",
          "</div>",
          "<div style='background-color: #eee; padding: 15px; text-align: center; font-size: 12px; color: #999;'>",
          "&copy; 2026 InSite Hardware. Automated System.<br>",
          "Please confirm receipt.",
          "</div>",
          "</div></body></html>"
        )
        
        email <- envelope() %>% 
          from(SENDER_EMAIL) %>% 
          to(s_email) %>% 
          subject(paste0("PO #", as.integer(Sys.time()), ": InSite Restock Request")) %>% 
          html(html_content) 
        
        smtp <- server(host=SMTP_SERVER, port=SMTP_PORT, username=SENDER_EMAIL, password=SENDER_PASSWORD)
        smtp(email, verbose=TRUE)
        
        success_count <- success_count + 1
        
        for(j in 1:nrow(items_to_order)) {
          qty_to_add <- items_to_order$Reorder_Level[j] * 3
          new_qty <- items_to_order$Box_Qty[j] + qty_to_add
          dbExecute(con, "UPDATE Inventory SET Box_Qty = ? WHERE Item_ID = ?", 
                    params=list(new_qty, items_to_order$Item_ID[j]))
        }
      }, error=function(e) { 
        error_log <<- c(error_log, paste(s_email, ":", e$message))
      })
    }
    
    tkconfigure(tt, cursor="arrow")
    refresh_view()
    
    if(length(error_log) > 0) {
      tkmessageBox(icon="warning", message=paste("Errors occurred:\n", paste(error_log, collapse="\n")))
    } else {
      tkmessageBox(message=paste("Success! Processed", success_count, "suppliers.\nStock replenished."))
    }
  }
  
  populate_data <- function() {
    dbExecute(con, "DELETE FROM Transactions")
    dbExecute(con, "DELETE FROM Inventory")
    
    items <- list(
      list("Pro-Grip Hammer", "BuildRight Tools", "sales@buildright.com", 15, 0, 1, 450.00, 5),
      list("Power Drill 18V", "TechMechanic", "orders@techmech.com", 8, 0, 1, 3500.00, 3),
      list("Steel Nails (1kg)", "Fasteners Inc.", "supply@fastener.com", 50, 0, 1, 120.00, 10),
      list("Safety Helmet", "SiteSafe", "contact@sitesafe.com", 20, 5, 1, 250.00, 8),
      list("Cement (40kg)", "SolidBase", "depot@solidbase.com", 100, 0, 1, 230.00, 20),
      list("Screwdriver Set", "BuildRight Tools", "sales@buildright.com", 25, 0, 1, 800.00, 5),
      list("Paint Roller", "ColorWorld", "sales@colorworld.com", 30, 0, 1, 150.00, 10),
      list("Ladder (6ft)", "SiteSafe", "contact@sitesafe.com", 5, 0, 1, 2100.00, 2),
      list("Work Gloves", "SiteSafe", "contact@sitesafe.com", 100, 0, 1, 80.00, 20),
      list("Measuring Tape", "BuildRight Tools", "sales@buildright.com", 40, 0, 1, 120.00, 5)
    )
    
    for(i in items) {
      dbExecute(con, "INSERT INTO Inventory (Item_Name, Supplier, Supplier_Email, Box_Qty, Piece_Qty, Items_Per_Box, Price_Per_Piece, Reorder_Level) VALUES (?,?,?,?,?,?,?,?)", params=i)
    }
    
    real_ids <- dbGetQuery(con, "SELECT Item_ID FROM Inventory")$Item_ID
    
    for(i in 1:60) {
      rand_id <- sample(real_ids, 1)
      rand_qty <- sample(1:5, 1)
      rand_day <- sample(0:30, 1)
      date_str <- paste0(as.character(Sys.Date() - rand_day), " ", sample(8:17, 1), ":", sample(10:59, 1), ":00")
      price <- dbGetQuery(con, paste("SELECT Price_Per_Piece FROM Inventory WHERE Item_ID =", rand_id))$Price_Per_Piece
      total <- rand_qty * price
      dbExecute(con, "INSERT INTO Transactions (Item_ID, Qty_Sold, Total_Amount, Trans_Date) VALUES (?, ?, ?, ?)", params=list(rand_id, rand_qty, total, date_str))
    }
    
    refresh_view(); tkmessageBox(message="Database Populated with 30-Day History!")
  }
  
  # ============================================================================
  # 7. LAYOUT CONSTRUCTION
  # ============================================================================
  
  # --- LEFT SIDEBAR (Dark) ---
  sidebar <- ttkframe(tt, style="Sidebar.TFrame", padding=10, width=320)
  tkpack(sidebar, side="left", fill="y")
  tkpack.propagate(sidebar, FALSE) 
  
  tkpack(tklabel(sidebar, text="InSite Manager", font="Helvetica 14 bold", bg=COL_SIDEBAR, fg="white"), anchor="w", pady=10)
  
  # Navigation
  nav_box <- tkframe(sidebar, bg=COL_SIDEBAR)
  tkpack(nav_box, fill="x", pady=10)
  ttkbutton(nav_box, text="Inventory", style="Nav.TButton", command=function() switch_mode("INVENTORY")) %>% tkpack(fill="x", pady=2)
  ttkbutton(nav_box, text="Sales Report", style="Nav.TButton", command=function() switch_mode("REPORT")) %>% tkpack(fill="x", pady=2)
  ttkbutton(nav_box, text="Analytics", style="Nav.TButton", command=function() switch_mode("ANALYTICS")) %>% tkpack(fill="x", pady=2)
  
  tkpack(ttkseparator(sidebar), fill="x", pady=10)
  
  # Inputs (Grid Layout)
  form_box <- ttklabelframe(sidebar, text=" Item Details ", padding=10)
  tkpack(form_box, fill="x")
  
  add_row <- function(p, lab, var, r) {
    tkgrid(ttklabel(p, text=lab), row=r, column=0, sticky="w", pady=2)
    tkgrid(ttkentry(p, textvariable=var, width=25), row=r, column=1, sticky="ew", padx=5, pady=2)
  }
  
  add_row(form_box, "Name:", var_name, 0)
  add_row(form_box, "Supplier:", var_supp, 1)
  add_row(form_box, "Email:", var_supp_email, 2)
  add_row(form_box, "Box Qty:", var_box_qty, 3)
  add_row(form_box, "Pcs Qty:", var_pc_qty, 4)
  add_row(form_box, "Per Box:", var_per_box, 5)
  add_row(form_box, "Price:", var_price, 6)
  add_row(form_box, "Reorder:", var_reorder, 7)
  tkgrid.columnconfigure(form_box, 1, weight=1)
  
  # Actions
  btn_box <- tkframe(sidebar, bg=COL_SIDEBAR)
  tkpack(btn_box, fill="x", pady=10)
  
  btn_add <- ttkbutton(btn_box, text="Add", style="Success.TButton", command=function() run_crud("ADD"))
  btn_upd <- ttkbutton(btn_box, text="Update", style="Info.TButton", command=function() run_crud("UPD"))
  btn_del <- ttkbutton(btn_box, text="Delete", style="Danger.TButton", command=function() run_crud("DEL"))
  btn_clr <- ttkbutton(btn_box, text="Clear", command=function() {
    tclvalue(var_id_hidden) <- ""; tclvalue(var_name) <- ""; tclvalue(var_box_qty) <- "0"
    tkconfigure(btn_add, state="normal"); tkconfigure(btn_upd, state="disabled")
  })
  
  tkgrid(btn_add, row=0, column=0, sticky="ew", padx=2)
  tkgrid(btn_upd, row=0, column=1, sticky="ew", padx=2)
  tkgrid(btn_del, row=1, column=0, sticky="ew", padx=2, pady=5)
  tkgrid(btn_clr, row=1, column=1, sticky="ew", padx=2, pady=5)
  
  tkgrid.columnconfigure(btn_box, 0, weight=1)
  tkgrid.columnconfigure(btn_box, 1, weight=1)
  
  # POS
  tkpack(ttkseparator(sidebar), fill="x", pady=10)
  pos_box <- ttklabelframe(sidebar, text=" Quick Sell ", padding=10)
  tkpack(pos_box, fill="x")
  tkpack(ttkentry(pos_box, textvariable=var_qty_sell, width=5), side="left")
  tkpack(ttkbutton(pos_box, text="Process Sale", command=run_pos, style="Success.TButton"), side="left", fill="x", expand=TRUE, padx=5)
  
  tkpack(ttkbutton(sidebar, text="Run Email Batch", command=run_email), fill="x", pady=20)
  tkpack(ttkbutton(sidebar, text="Populate Sample Data", command=populate_data), fill="x", pady=5)
  
  # --- RIGHT CONTENT AREA ---
  content <- ttkframe(tt, style="Content.TFrame")
  tkpack(content, side="right", fill="both", expand=TRUE)
  
  header <- tkframe(content, bg="white", height=60, bd=1, relief="raised")
  tkpack(header, fill="x")
  
  lbl_header <- tklabel(header, text="Inventory Database", font="Helvetica 16 bold", bg="white", fg=COL_SIDEBAR)
  tkpack(lbl_header, side="left", padx=20, pady=15)
  
  search_box <- tkframe(header, bg="white")
  tkpack(search_box, side="right", padx=20)
  tkpack(tklabel(search_box, text="Search:", bg="white"), side="left")
  tkpack(ttkentry(search_box, textvariable=var_search), side="left", padx=5)
  tkbind(tt, "<KeyRelease>", function(...) if(tclvalue(var_view_mode)=="INVENTORY") refresh_view())
  
  # Containers
  frame_tree_container <- ttkframe(content, style="Card.TFrame")
  
  # FIX: HIDE BLANK COLUMN #0
  tree <- ttktreeview(frame_tree_container, selectmode="browse", show="headings", height=25)
  tcl(tree, "tag", "configure", "lowstock", foreground=COL_DANGER, font="Helvetica 9 bold")
  
  scroll <- ttkscrollbar(frame_tree_container, orient="vertical", command=function(...) tkyview(tree, ...))
  tkconfigure(tree, yscrollcommand=function(...) tkset(scroll, ...))
  tkpack(tree, side="left", fill="both", expand=TRUE)
  tkpack(scroll, side="right", fill="y")
  tkbind(tree, "<<TreeviewSelect>>", on_select)
  
  # Analytics Container
  frame_plot_container <- ttkframe(content, style="Card.TFrame")
  lbl_plot <- tklabel(frame_plot_container, bg="white")
  lbl_no_data <- tklabel(frame_plot_container, text="", font="Helvetica 14", bg="white")
  tkpack(lbl_no_data, side="top", pady=10)
  tkpack(lbl_plot, side="top", fill="both", expand=TRUE, padx=10, pady=10)
  
  # Status Bar
  tkpack(tklabel(tt, textvariable=var_status_msg, bd=1, relief="sunken", anchor="w", bg="#dfe6e9", fg="#2d3436"), side="bottom", fill="x")
  
  refresh_view()
}

show_login_screen()
tkwait.window(tt)
