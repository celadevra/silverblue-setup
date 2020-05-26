# fsb-base

Base toolbox setup. Common development tools are installed. The other toolboxes are build up based on this image.

After running `build.sh`, commit the running container to a image:

```sh
podman commit -a "Celad Evra" base workstation
```