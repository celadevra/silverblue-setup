# Setting up Fedora Silverblue on MacBook Air

## Networking

My MacBook Air has a Broadcom 4360 (14E4:43A0) wireless card. Almost all
Linux distros don't provide support out of the box. Luckily, Silverblue
supports USB tethering, so I can use my cellphone's network when
installing necesarry packages.

Broadcom uses wl driver, which is in rpmfusion repository.

```sh
sudo rpm-ostree install https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
```

```sh
sudo rpm-ostree install broadcom-wl
```

Reboot. Then you should be able to pick up wireless.

## Fonts

I am a fan of IBM Plex Mono. Thus

```sh
sudo rpm-ostree install google-droid-sans-mono-fonts ibm-plex-mono-fonts
```

Reboot.

## Circumventing Censorship

There are some scripts and configs in [this dir](fsb-ss) for that
purpose.

I could have put these tools in a toolbox. But as I have to use them all
the time, I want them to be available to as many aspects of the system
as possible. So I compile ssr-native in a toolbox, copy the result to
local bin dir, and installed privoxy as a layered package.

```sh
sudo rpm-ostree install privoxy
```

If you see error "error  junk in handshake", check
[this](https://github.com/ShadowsocksR-Live/shadowsocksr-native/issues/102)
out. Basically you need to run privoxy first and use the HTTP proxy.

## Power Management

Install tlp and tlp-rdw as layered package. Then start the service.

```sh
sudo rpm-ostree install tlp tlp-rdw
(reboot)
sudo systemctl enable tlp
sudo systemctl start tlp
```

## Flathub

Go to [Flathub](https://flathub.org/home) and enable the remote:

```sh
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo
flatpak update
```

## Install Software

Firefox that comes with ostree cannot play mp4 video embeded in web pages for lacking of codecs. So I install the official flatpak from Flathub, and remove the Fedora one.

```sh
flatpak install org.mozilla.firefox
rpm-ostree override remove firefox
```

## Credentials

Mainly GPG keys and some login credentials. Stored on my SDF machine.

```sh
export LOGIN_NAME=(redacted)
ssh $LOGIN_NAME@meta.sdf.org creds/packing.sh
scp $LOGIN_NAME@meta.sdf.org:/sdf/arpa/af/${LOGIN_NAME:0:1}/$LOGIN_NAME/creds_pack.zip .
ssh $LOGIN_NAME@meta.sdf.org rm /sdf/arpa/af/${LOGIN_NAME:0:1}/$LOGIN_NAME/creds_pack.zip
unzip creds_pack.zip
cd creds
./unpacking.sh
```

Generate ssh keys and send them to github/lab as valid credentials.

```sh
ssh-keygen -C xhy@FedoraMBP -f ~/.ssh/id_rsa_github
ssh-keygen -C xhy@FedoraMBP -f ~/.ssh/id_rsa_gitlab
```

Edit the GPG keys to trust them. Then clone the password store:

```sh
git clone git@gitlab.com:celadevra/passwords.git
```

## Ricing a bit

Clone [PaperWM](https://github.com/paperwm/PaperWM.git). Run the install script.
