
# Workforce Analytics Dashboard

![License](https://img.shields.io/badge/license-MIT-blue.svg)
![Status](https://img.shields.io/badge/status-active-success)

A modern, interactive **Workforce Analytics Dashboard** built using **R Shiny** that allows organizations to gain insights into workforce metrics through dynamic visualizations and authenticated access.

---

## ✨ Features

- 🔐 **Google OAuth Authentication**
- 🔑 **SQLite-based Username & Password Login/Registration**
- 🎨 **Custom Theming with `shinydashboard` and `shinyWidgets`**
- 📊 Interactive Dashboard to Visualize Workforce Metrics
- 📁 Modular Code Structure for Maintainability

---
## 🛠️ Tech Stack

- **Frontend/UI:** R Shiny, shinydashboard, shinyWidgets
- **Authentication:** Google OAuth 2.0, SQLite (custom login/register)
- **Database:** SQLite (for user management)
- **OAuth Libraries:** `httr`, `jsonlite`
- **Deployment Ready:** Compatible with shinyapps.io or self-hosted environments

---

## 🚀 Getting Started

### 1. Clone the Repository
```bash
git clone https://github.com/your-username/workforce-analytics-dashboard.git
cd workforce-analytics-dashboard
```

### 2. Install Required Packages
Make sure you have R installed, then install the following R packages:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "DBI",
  "RSQLite", "httr", "jsonlite", "shinyjs"
))
```

### 3. Set up Google OAuth

- Go to [Google Cloud Console](https://console.cloud.google.com/)
- Create a new project and configure OAuth consent screen
- Add your **client ID**, **client secret**, and **redirect URI** in the `global.R` file:

```r
google_client_id <- "YOUR_GOOGLE_CLIENT_ID"
google_client_secret <- "YOUR_GOOGLE_CLIENT_SECRET"
google_redirect_uri <- "http://localhost:8080"  # Or your deployed app URL
```

### 4. Run the App
```r
shiny::runApp()
```

---

## 🔐 Authentication Flow

- **Option 1:** Login/Register with your email and password (stored securely in SQLite).
- **Option 2:** Login via Google using OAuth 2.0 (user details are fetched via Google's API).
---

## 🙌 Acknowledgements

- [R Shiny](https://shiny.rstudio.com/)
- [Google OAuth 2.0](https://developers.google.com/identity)
- [shinydashboard](https://rstudio.github.io/shinydashboard/)
- [RSQLite](https://cran.r-project.org/web/packages/RSQLite/index.html)

---

## 💬 Feedback

If you find any bugs or want to contribute, feel free to open an issue or pull request.

Made with ❤️ by VANSH TUTEJA(https://github.com/VanshTuteja)
