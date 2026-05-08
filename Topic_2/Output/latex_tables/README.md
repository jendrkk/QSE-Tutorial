# LaTeX Tables for Presentation

This directory contains publication-ready LaTeX tables generated from regression results and fixed effects analysis.

## Files Generated

### 1. **table_gammas_results.tex** 
Main regression results table showing time coefficients (γ) across different price indices and CBD definitions.

**Structure:**
- Rows: 3 Price Indices (Weekly Median, Weekly Kwartil, Half-Kwartil)
- Columns: 4 CBD Definitions (Index PL, Weighted Centroid, PL Centroid, Multi-CBD)
- Cell content: Coefficient with standard error in parentheses, significance stars

**Key insights:**
- All coefficients in Weekly Median Index are highly significant (p<0.01)
- Weekly Kwartil shows no significant effects
- Half-Kwartil shows positive and significant time effects

### 2. **table_city_fe_Weekly_Median.tex**
### 3. **table_city_fe_Weekly_Kwartil.tex**
### 4. **table_city_fe_Half_Kwartil.tex**

City fixed effects tables, one for each price index. Shows estimated fixed effects for each city relative to the base category, across all CBD definitions.

**Structure:**
- Rows: 5 Cities (Berlin, Frankfurt am Main, Hamburg, Köln, München)
- Columns: 4 CBD Definitions
- Cell content: Fixed effect coefficient with standard error in parentheses

**Note:** Empty cells indicate that the combination was not estimated (potentially due to data availability or collinearity issues).

### 5. **all_tables_master.tex**
A complete standalone LaTeX document containing all tables with proper preamble. Can be compiled directly with:
```bash
pdflatex all_tables_master.tex
```

## How to Use in Your Presentation

### Option A: Include Individual Tables in Your LaTeX Document

In your presentation LaTeX source:

```latex
\documentclass{beamer}
\usepackage{booktabs}      % Required for \toprule, \midrule, \bottomrule
\usepackage{threeparttable} % Required for table notes

\begin{document}

\begin{frame}
  \frametitle{Regression Results}
  \input{table_gammas_results.tex}
\end{frame}

\begin{frame}
  \frametitle{City Fixed Effects - Weekly Median Index}
  \input{table_city_fe_Weekly_Median.tex}
\end{frame}

\end{document}
```

### Option B: Compile Master Document

If you want to quickly preview or share the tables:
```bash
cd /Users/jedrek/Documents/Studium\ Volkswirschaftslehre/4.\ Semester/Quantitive\ Spatial\ Economics/QSE-Tutorial/Topic_2/Output/latex_tables/
pdflatex all_tables_master.tex
```

## Table Design Features

All tables use **booktabs** styling for a professional, publication-quality appearance:
- Clean horizontal lines using `\toprule`, `\midrule`, `\bottomrule`
- Proper spacing and typography
- Table notes section explaining significance levels

## Significance Notation

- `***` = p < 0.01 (highly significant)
- `**` = p < 0.05 (significant)
- `*` = p < 0.10 (marginally significant)
- No symbol = not significant

## Technical Details

### Gammas Table
- **Formula:** beta ~ year + C(city) by index and CBD type
- **Coefficients shown:** Time coefficient (year) estimates
- **Standard errors:** Cluster-robust standard errors where applicable

### City FE Tables
- **Fixed effects:** Intercept and city coefficients relative to Berlin (base category)
- **Standard errors:** In scientific notation (e.g., 7.9595e-01 = 0.795)
- **Note:** Very small standard errors in some cells may indicate sparse data or collinearity

## Customization

To modify these tables, refer to the Python functions in the notebook:
- `add_significance_stars()` - Controls star notation
- `create_gammas_latex_table()` - Generates gammas table
- `create_city_fe_by_index_tables()` - Generates city FE tables

## Required LaTeX Packages

For inclusion in your document, ensure you have:
```latex
\usepackage{booktabs}      % For professional table formatting
\usepackage{threeparttable} % For table notes (optional but recommended)
```

If using Beamer (for presentations):
```latex
\documentclass{beamer}
\usepackage{booktabs}
\usepackage{threeparttable}
```
