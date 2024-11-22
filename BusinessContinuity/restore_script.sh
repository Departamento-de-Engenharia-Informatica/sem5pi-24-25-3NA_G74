#!/bin/bash

# Simple Server Restore Script
REMOTE_HOST="vsgate-ssh.dei.isep.ipp.pt"
REMOTE_PORT="11047"
REMOTE_USER="deployer"
BACKUP_DIR="$HOME/server_backups"

# Get the latest backup file
LATEST_BACKUP=$(ls -t "$BACKUP_DIR"/fullsystem_*.tar.gz | head -n1)

if [ -z "$LATEST_BACKUP" ]; then
    echo "No backup files found!"
    exit 1
fi

echo "Using backup: $LATEST_BACKUP"
echo "This will COMPLETELY REPLACE the target system."
echo "Are you sure you want to continue? (yes/no)"
read response

if [ "$response" != "yes" ]; then
    echo "Restoration cancelled."
    exit 1
fi

# Transfer and extract the backup
cat "$LATEST_BACKUP" | ssh -p "$REMOTE_PORT" "$REMOTE_USER@$REMOTE_HOST" \
    "cd / && tar -xzf -"

echo "Restoration completed. Please reboot the server."