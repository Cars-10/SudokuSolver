0#!/bin/bash

# Ensure we are in the project root
cd "$(dirname "$0")/.."

case "$1" in
    start)
        echo "Starting server..."
        docker-compose up -d
        ;;
    stop)
        echo "Stopping server..."
        docker-compose stop
        ;;
    restart)
        echo "Restarting server..."
        docker-compose restart
        ;;
    down)
        echo "Tearing down services..."
        docker-compose down
        ;;
    status)
        docker-compose ps
        ;;
    logs)
        docker-compose logs -f
        ;;
    *)
        echo "Usage: $0 {start|stop|restart|down|status|logs}"
        exit 1
        ;;
esac
