#!/bin/bash

# Configuration Variables
REMOTE_HOST="vsgate-ssh.dei.isep.ipp.pt"
REMOTE_PORT="10657"
REMOTE_USER="root"
BACKUP_DIR="$HOME/sarm_server_backups"
RESTORE_LOG="$HOME/server_restore.log"

# Logging function
log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$RESTORE_LOG"
}

# Get latest backup
LATEST_BACKUP=$(ls -t "$BACKUP_DIR"/full_backup_*.tar.gz | head -n1)

if [ -z "$LATEST_BACKUP" ]; then
    log_message "No backup file found"
    exit 1
fi

log_message "Using backup file: $LATEST_BACKUP"

# Transfer and extract backup
ssh -p "$REMOTE_PORT" "$REMOTE_USER@$REMOTE_HOST" 'mkdir -p /tmp/restore_temp'
scp -P "$REMOTE_PORT" "$LATEST_BACKUP" "$REMOTE_USER@$REMOTE_HOST:/tmp/restore_temp/"

ssh -p "$REMOTE_PORT" "$REMOTE_USER@$REMOTE_HOST" "cd / && tar -xzf /tmp/restore_temp/$(basename "$LATEST_BACKUP") && \
    systemctl restart g74backend && \
    systemctl restart nginx && \
    rm -rf /tmp/restore_temp"

if [ $? -eq 0 ]; then
    log_message "Restore completed successfully"
else
    log_message "RESTORE FAILED"
    exit 1
fi