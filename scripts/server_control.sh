#!/bin/bash

CONTAINER_NAME="sudokusolver-app-1"

case "$1" in
    start)
        echo "Starting server..."
        docker start $CONTAINER_NAME
        ;;
    stop)
        echo "Stopping server..."
        docker stop $CONTAINER_NAME
        ;;
    restart)
        echo "Restarting server..."
        docker restart $CONTAINER_NAME
        ;;
    status)
        docker ps --filter "name=$CONTAINER_NAME"
        ;;
    logs)
        docker logs -f $CONTAINER_NAME
        ;;
    *)
        echo "Usage: $0 {start|stop|restart|status|logs}"
        exit 1
        ;;
esac
