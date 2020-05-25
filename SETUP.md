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
```

## Install Software

Firefox that comes with ostree cannot play mp4 video embeded in web pages for lacking of codecs. So I install the official flatpak from Flathub, and remove the Fedora one.

```sh
flatpak install org.mozilla.firefox
rpm-ostree override remove firefox
```

## Ricing a bit

Clone [PaperWM](https://github.com/paperwm/PaperWM.git). Run the install script.
