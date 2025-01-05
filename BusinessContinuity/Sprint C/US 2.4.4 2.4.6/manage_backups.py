import os
import shutil
from datetime import datetime, timedelta
import re

def extract_date_from_filename(filename):
    """Extract date from filename with pattern *_yyyymmdd"""
    match = re.search(r'_(\d{8})$', filename)
    if match:
        date_str = match.group(1)
        try:
            return datetime.strptime(date_str, '%Y%m%d')
        except ValueError:
            return None
    return None

def create_folders():
    """Create necessary folders if they don't exist"""
    folders = ['daily', 'weekly', 'monthly']
    for folder in folders:
        if not os.path.exists(folder):
            os.makedirs(folder)

def organize_files():
    """Organize files based on their dates into appropriate folders"""
    current_date = datetime.now()
    
    # Define time thresholds
    week_ago = current_date - timedelta(days=7)
    month_ago = current_date - timedelta(days=30)
    year_ago = current_date - timedelta(days=365)
    
    # Create required folders
    create_folders()
    
    # Get all files in current directory
    files = [f for f in os.listdir('.') if os.path.isfile(f)]
    
    for file in files:
        # Skip the script itself
        if file == os.path.basename(__file__):
            continue
            
        file_date = extract_date_from_filename(file)
        if not file_date:
            continue
        
        try:
            if file_date > week_ago:
                shutil.move(file, os.path.join('daily', file))
            elif file_date > month_ago:
                shutil.move(file, os.path.join('weekly', file))
            elif file_date > year_ago:
                shutil.move(file, os.path.join('monthly', file))
            else:
                os.remove(file)
            print(f"Processed file: {file}")
        except Exception as e:
            print(f"Error processing {file}: {str(e)}")

def manage_retention():
    """Manage retention in each folder keeping only required files"""
    try:
        # Process daily folder - keep one per day
        if os.path.exists('daily'):
            files_by_day = {}
            for file in os.listdir('daily'):
                file_date = extract_date_from_filename(file)
                if file_date:
                    day_key = file_date.strftime('%Y%m%d')
                    if day_key not in files_by_day or file > files_by_day[day_key]:
                        if day_key in files_by_day:
                            os.remove(os.path.join('daily', files_by_day[day_key]))
                        files_by_day[day_key] = file

        # Process weekly folder - keep one per week
        if os.path.exists('weekly'):
            files_by_week = {}
            for file in os.listdir('weekly'):
                file_date = extract_date_from_filename(file)
                if file_date:
                    week_key = file_date.strftime('%Y%W')
                    if week_key not in files_by_week or file > files_by_week[week_key]:
                        if week_key in files_by_week:
                            os.remove(os.path.join('weekly', files_by_week[week_key]))
                        files_by_week[week_key] = file

        # Process monthly folder - keep one per month
        if os.path.exists('monthly'):
            files_by_month = {}
            for file in os.listdir('monthly'):
                file_date = extract_date_from_filename(file)
                if file_date:
                    month_key = file_date.strftime('%Y%m')
                    if month_key not in files_by_month or file > files_by_month[month_key]:
                        if month_key in files_by_month:
                            os.remove(os.path.join('monthly', files_by_month[month_key]))
                        files_by_month[month_key] = file

        print("Retention management completed successfully!")
    except Exception as e:
        print(f"Error during retention management: {str(e)}")

if __name__ == "__main__":
    try:
        organize_files()
        print("File organization completed successfully!")
        manage_retention()
    except Exception as e:
        print(f"An error occurred: {str(e)}")