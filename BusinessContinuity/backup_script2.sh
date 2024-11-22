#!/bin/bash

# Simple Server Backup Script
REMOTE_HOST="vsgate-ssh.dei.isep.ipp.pt"
REMOTE_PORT="10953"
REMOTE_USER="root"
BACKUP_DIR="$HOME/server_backups"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# Create backup directory
mkdir -p "$BACKUP_DIR"

# Connect to remote server and create full system backup
# Excluding temporary and virtual filesystems
ssh -p "$REMOTE_PORT" "$REMOTE_USER@$REMOTE_HOST" "tar -v --exclude=/proc \
                                                          --exclude=/sys \
                                                          --exclude=/dev \
                                                          --exclude=/run \
                                                          --exclude=/media \
                                                          --exclude=/mnt \
                                                          --exclude=/tmp \
                                                          --exclude=lost+found \
                                                          --exclude=/backup \
                                                          --exclude=/var/cache \
                                                          --exclude=/var/tmp \
                                                          -czf - /" > "$BACKUP_DIR/fullsystem_$TIMESTAMP.tar.gz"

# Keep only last 5 backups
ls -t "$BACKUP_DIR"/fullsystem_*.tar.gz | tail -n +6 | xargs -r rm

echo "Backup completed: $BACKUP_DIR/fullsystem_$TIMESTAMP.tar.gz"