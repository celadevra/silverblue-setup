#!/usr/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ssr-client -c $DIR/ssr-config.json -d
sudo privoxy $DIR/config.privoxy
