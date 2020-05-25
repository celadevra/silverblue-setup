#!/usr/bin/bash
toolbox rm ss-local
toolbox create -c ss-local
toolbox run -c ss-local dnf install -y wget git gcc gcc-c++ gdb cmake autoconf automake libtool make asciidoc xmlto
toolbox run -c ss-local git clone https://github.com/ShadowsocksR-Live/shadowsocksr-native.git ssr-n
toolbox run -c ss-local cd ssr-n; git submodule update --init
toolbox run -c ss-local git submodule foreach -q 'git checkout $(git config -f $toplevel/.gitmodules submodule.$name.branch || echo master)'
toolbox run -c ss-local cmake . && make
toolbox run -c ss-local cp -rfa src/ssr-* ~/.local/bin/
ssr-local -c ~/Documents/Codes/fsd-ss/ssr-config.json &
curl -4sSkLO https://raw.github.com/zfl9/gfwlist2privoxy/master/gfwlist2privoxy
bash gfwlist2privoxy 127.0.0.1:1080
sudo mv -f gfwlist.action /etc/privoxy/
toolbox rm ss-local
