# WDI indicator monitor

A tool for monitoring the indicators in the WDI.

Link to public dashboard: https://wb-decis.github.io/wdi_indicator_monitor-public/wdi_monitor.html

Steps to update the tool after a WDI release:
1) run get_data.R to create a new dataset with metrics
2) Change dates in wdi_monitor.qmd
3) Run wdi_monitor.qmd to create html file
4) Once the html file is updated in the main branch, the tool will be updated automatically.
