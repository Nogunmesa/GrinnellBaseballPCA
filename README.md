[![Deploy to shinyapps.io](https://github.com/Nogunmesa/GrinnellBaseballPCA/actions/workflows/deploy.yml/badge.svg)](https://github.com/Nogunmesa/GrinnellBaseballPCA/actions/workflows/deploy.yml)
# GrinnellBaseballPCA
[VISIT OUR DEPLOYED PRODUCT](https://baseballteam.shinyapps.io/GrinnellBaseballPCA/)

This R project analyzes baseball pitcher and hitter statistics using Principal Component Analysis (PCA). This repo includes data scraping from inside edge.com and Grinnell Athletics Website, data preprocessing, PCA visualization, and interactive analysis tools to explore player performance and trends within the Grinnell College baseball team.

## Data Sources

- **Athletic Stats:** Scraped from Grinnell College’s official athletics website (https://pioneers.grinnell.edu/sports/baseball/stats).
- **Advanced Metrics:** Scraped from Inside Edge's evaluation reports for both hitters and pitchers.
- **Preprocessed PCA Data:** Included in the repo as zipped CSVs (`PitcherPCA.zip` and `Hitters.zip`).

## Project Structure
```
Final330App/
├──  data/                         # Contains PitcherPCA.zip and Hitters.zip
├──  doc/                          # Contains user help guide
├──  Final330App.R                 # get_player_athletic_stats, get_player_edge_stats, perform_player_pca & interpret_pc
├── README.md
├── .gitignore
```
## Key Functions

### 1. `Rendering_Data(type)`
Returns cleaned data for "pitcher" or "hitter" from zipped CSVs.

### 2. `get_player_athletic_stats(year, type)`
Scrapes and cleans Grinnell College baseball stats for a given year.

### 3. `get_player_edge_stats(year, data, type)`
Scrapes player-specific advanced metrics from Inside Edge reports.

### 4. `perform_player_pca(df, type)`
Performs PCA and returns both transformed data and loading vectors.

### 5. `interpret_pc(pc_loadings, type, top_n = 4)`
Identifies top contributing stats to each principal component.

**TODO** option to return the new dataset(original + scrapped) and allow user to scrape for multiple years

## Example Output

- PCA plots categorizing players by season (e.g. 2016 Team, 2023 Team, Other Years)
- Component interpretations revealing which stats drive variance in performance

## Motivation

This project was built to explore how data analytics can uncover underlying patterns in player performance, distinguish great seasons from average ones, and provide insights useful for coaching and recruitment.

## Authors

- Alyssa Trapp
- Sean Tashjian
- Nifemi Ogunmesa
  
## Acknowledgements

- Grinnell College Athletics for public stats
- Inside Edge for player evaluation tools

---

### Notes

- This repo is purely for educational/non-commercial use.
