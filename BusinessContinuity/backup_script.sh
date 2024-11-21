#!/bin/bash

# Server Backup Script

# Configuration Variables
REMOTE_HOST="vsgate-ssh.dei.isep.ipp.pt"
REMOTE_PORT="10953"
REMOTE_USER="root"
BACKUP_DIR="$HOME/sarm_server_backups"
BACKUP_LOG="$HOME/server_backup.log"
RETENTION_DAYS=30

# Create backup directory if it doesn't exist
mkdir -p "$BACKUP_DIR"

# Generate timestamp for backup
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
BACKUP_PATH="$BACKUP_DIR/full_backup_$TIMESTAMP.tar.gz"

# Logging function
log_message() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" | tee -a "$BACKUP_LOG"
}

# Ensure the SSH agent is running
if [ -z "$SSH_AUTH_SOCK" ]; then
    eval "$(ssh-agent -s)"
    log_message "SSH agent started"
fi

# Perform backup
perform_backup() {
    local backup_paths=(
        "/etc"
        "/home"
        "/var/www"
    )

    log_message "Starting selective server backup..."
    
    ssh -p "$REMOTE_PORT" "$REMOTE_USER@$REMOTE_HOST" \
        "tar -czpf - ${backup_paths[*]}" > "$BACKUP_PATH"
    
    if [ $? -eq 0 ]; then
        log_message "Backup completed successfully: $BACKUP_PATH"
    else
        log_message "BACKUP FAILED"
        exit 1
    fi
}

# Remove old backups
cleanup_old_backups() {
    log_message "Removing backups older than $RETENTION_DAYS days..."
    find "$BACKUP_DIR" -name "full_backup_*.tar.gz" -type f -mtime +"$RETENTION_DAYS" -delete
    log_message "Cleanup completed"
}

# Main execution
perform_backup
cleanup_old_backups

log_message "Backup process completed"
