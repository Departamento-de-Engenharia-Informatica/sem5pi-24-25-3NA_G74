#!/bin/bash

DBNAME="test"

# MongoDB URI for authentication and connection
MONGODB_URI="mongodb+srv://pedroaguia8:7pJW30K5Z4SUqFBv@sarm.hg1jv.mongodb.net/$DBNAME?retryWrites=true&w=majority"


# Output directory for MongoDB dump
BACKUP_DIR="/root/bin/mongodumptotal"
BACKUP_DIR2="/root/bin/mongodumptotal2"


# Output file
DATE=$(date +"%Y%m%d")
BACKUP_FILE="$BACKUP_DIR/${DBNAME}_${DATE}"
BACKUP_FILE2="$BACKUP_DIR2/${DBNAME}_${DATE}"


# Log file to record backup status
LOG_FILE="/root/backup_logs.log"


# Ensure the backup directory exists; create if not
mkdir -p "$BACKUP_DIR"
mkdir -p "$BACKUP_DIR2"


# Run mongodump with the specified URI and log the output
if /usr/bin/mongodump --uri "$MONGODB_URI" --out "$BACKUP_FILE" >> "$LOG_FILE" 2>&1; then
    echo "$(date): MongoDB backup successful" >> "$LOG_FILE"
else
    echo "$(date): MongoDB backup failed" >> "$LOG_FILE"
fi