$project_root = $PSScriptRoot

$log_dir = "$project_root\logs"
Get-ChildItem -Path $log_dir -Filter "*.txt" |
Where-Object { $_.CreationTime -lt (Get-Date).AddDays(-30) } |
Remove-Item -Force

$timestamp = Get-Date -Format "yyyy-MM-dd-HHmm"
$log_file = Join-Path $log_dir "run_$($timestamp).txt"
Start-Transcript -Path $log_file -Append

try {

	# Defining the window of script automation
	$current_month = (Get-Date).Month
	$current_day = (Get-Date).Day
	$active_months = 8, 9, 10, 11, 12, 1

	$in_season = $active_months -contains $current_month

	if (-not $in_season) {

		if ($current_day -gt 7) {
			Write-Host "Off-season: Monthly update already completed. Exiting" -ForegroundColor Gray
			exit
		}
		Write-Host "Off-season: Running monthly maintenance update..." -ForegroundColor Yellow

	}
	else {
		Write-Host "In-season: Running weekly kicker analysis..." -ForegroundColor Cyan
	}

	$etl_script = "$project_root\etl\etl.R"
	$etl_input_file = "C:\Users\justi\Desktop\kicker_variables.csv"

	$cleaning_script = "$project_root\cleaning\cleaning.R"

	$pbp_plot_file = "C:/Users/justi/Desktop/pbp_plot.csv"
	$stats_plot_file = "C:/Users/justi/Desktop/stats_plot.csv"

	$report_script = "./eda/eda.Rmd"
	$report_name = "EDA_$(Get-Date -Format 'yyyy-MM-dd').html"
	$output_dir = "./eda"

	# Checking the existence of the ETL script
	if (Test-Path $etl_script) {

		Write-Host "Executing the ETL Script" -ForegroundColor Cyan
		Rscript $etl_script $etl_input_file

		if ($LASTEXITCODE -eq 0) {
			Write-Host "ETL Script was run successfully." -ForegroundColor Green
		}
		else {
			Write-Warning "ETL Script exited with an error. Exit Code: $LASTEXITCODE"
		}

	}
	else {
		Write-Error "Could not find the ETL script at: $etl_script"
	}


	# Checking the existence of the Cleaning script
	if (Test-Path $cleaning_script) {

		Write-Host "Executing the Cleaning Script" -ForegroundColor Cyan
		Rscript $cleaning_script
		
		# Checking that the file ran successfully
		if ($LASTEXITCODE -eq 0) {
			Write-Host "Cleaning Script was run successfully." -ForegroundColor Green
		}
		else {
			Write-Warning "Cleaning Script exited with an error. Exit Code: $LASTEXITCODE"
		}

	}
	else {
		Write-Error "Could not find the Cleaning Script at: $cleaning_script"
	}

	# Generating an EDA File
	Write-Host "Generating the EDA report..." -ForegroundColor Cyan
	try {

		Rscript -e "rmarkdown::render('$report_script', params = list(pbp_plot_file = normalizePath('$pbp_plot_file'), stats_plot_file = normalizePath('$stats_plot_file')), output_file = '$report_name', output_dir = '$output_dir')"
		if ($LASTEXITCODE -eq 0) {
			Write-Host "Success: EDA report generated successfully." -ForegroundColor Green

			if ([Environment]::UserInteractive) {

				$response = Read-Host -Prompt "Would you like to view the EDA report? (Y/N)"
				if ($response -eq "Y") {
					Write-Host "Generating EDA report..." -ForegroundColor Yellow
					Start-Process "$output_dir\$report_name"
				}
				else {
					Write-Host "Report rendered and saved to $output_dir\$report_name" -ForegroundColor Gray
				}
			
			}
		}
		else {
			Write-Error "Failure: R encountered an error during rendering. Check your Rmd code logic."
		}
	}
	catch {
		Write-Host "Critical Error: Could not execute Rscript $( $_.Exception.Message )" -ForegroundColor Red
	}

}
finally {
	Stop-Transcript
}