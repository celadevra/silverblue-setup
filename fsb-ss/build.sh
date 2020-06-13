#!/usr/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
#toolbox rm ss-local
toolbox create -c ss-local
toolbox run -c ss-local sudo dnf install -y wget git gcc gcc-c++ gdb cmake autoconf automake libtool make asciidoc xmlto
git clone https://github.com/ShadowsocksR-Live/shadowsocksr-native.git ssr-n
cd ssr-n
git checkout 0.8.2
git submodule update --init
git submodule foreach -q 'git checkout $(git config -f $toplevel/.gitmodules submodule.$name.branch || echo master)'
toolbox run -c ss-local cmake . && toolbox run -c ss-local make
mkdir -p ~/.local/bin
toolbox run -c ss-local cp -rfa src/ssr-* ~/.local/bin/
ssr-client -c $DIR/ssr-config.json &
export http_proxy="http://127.0.0.1:8118"
export HTTP_PROXY="http://127.0.0.1:8118"
export HTTPS_PROXY="http://127.0.0.1:8118"
curl -4sSkLO https://raw.github.com/zfl9/gfwlist2privoxy/master/gfwlist2privoxy
bash gfwlist2privoxy 127.0.0.1:1080
sudo mv -f gfwlist.action /etc/privoxy/
sudo rm -rf /ssr-n
#toolbox rm ss-local
