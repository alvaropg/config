#!/bin/bash
#
# script name:   aml-flash-image.sh
#
# description:   Amlogic's board flashing script. You must expecify where are
#                the images to flash (by default set to the current directory)
#
# usage:         aml-flash-image.sh <path-to-deploy-dir>
#
# authors:       Javier Peces Chillaron <javier.peces@outlook.com>
#                Alvaro Peña Gonzalez <alvaropg@gmail.com>
#
# ==============================================================================

if [[ $EUID -ne 0 ]]; then
    echo "[error] this script must be run as sudo user" 1>&2
    exit 1
fi

# check if fastboot binary is in user's PATH
if [ ! $(type -P fastboot) > /dev/null ]; then
    cat <<-END
[error] Fastboot android utility must be installed in your system in order
to continue.

  Ubuntu users:
    $ sudo apt-get android-tools-fastboot
  Fedora users:
    $ sudo dnf install android-tools
  Arch Linux users:
    $ sudo pacman -Sy android-tools
END
    exit 1
fi

if [ "$#" == 0 ]; then
    DEPLOY_DIR=$(realpath ".") # current directory
elif [ -d $1 ]; then
    DEPLOY_DIR=$(realpath $1)
else
    echo "[error] $DEPLOY_DIR directory does not exists"
    exit 1
fi

if [ ! -f $DEPLOY_DIR/u-boot.bin.signed ]; then
    echo "[error] u-boot image doesn't exist"
    error=1
fi

if [ ! -f $DEPLOY_DIR/logo.img ]; then
    echo "[error] logo.img image doesn't exist"
    error=1
fi

if [ ! -f $DEPLOY_DIR/boot.img ]; then
   echo "[error] kernel image doesn't exist"
   error=1
fi

if [ ! -f $DEPLOY_DIR/dtb.img ]; then
    echo "[error] DTB image doesn't exist"
    error=1
fi

if [ ! -f $DEPLOY_DIR/rootfs.ext4.img2simg ]; then
    echo "[error] rootfs image doesn't exist"
    error=1
fi

if [ "$error" ]; then
    echo "Aborting..."
    exit 1
fi

# unlock
echo "Unlocking..."
fastboot flashing unlock
if [ "$?" != 0 ]; then echo "Error ocurred"; exit 1; fi
echo -e "\n"

echo "Unlocking critical..."
fastboot flashing unlock_critical
if [ "$?" != 0 ]; then echo "Error ocurred"; exit 1; fi
echo -e "\n"

# update uboot
echo "Flashing u-boot..."
fastboot flash bootloader $DEPLOY_DIR/u-boot.bin.signed
if [ "$?" != 0 ]; then echo "Error ocurred"; exit 1; fi
echo -e "\n"

# update logo
echo "Flashing logo image..."
fastboot flash logo $DEPLOY_DIR/logo.img
if [ "$?" != 0 ]; then echo "Error ocurred"; exit 1; fi
echo -e "\n"

# update kernel
echo "Flashing kernel image..."
fastboot flash boot $DEPLOY_DIR/boot.img
if [ "$?" != 0 ]; then echo "Error ocurred"; exit 1; fi
echo -e "\n"

# update device tree
echo "Flashing DTB image..."
fastboot flash dtb $DEPLOY_DIR/dtb.img
if [ "$?" != 0 ]; then echo "Error ocurred"; exit 1; fi
echo -e "\n"

# update file system
echo "Flashing rootfs image..."
fastboot flash -S 128M system $DEPLOY_DIR/rootfs.ext4.img2simg
if [ "$?" != 0 ]; then echo "Error ocurred"; exit 1; fi
echo -e "\n"


# Vendor
echo "Flashing vendor image..."
fastboot flash vendor $DEPLOY_DIR/vendor.ext4.img2simg
if [ "$?" != 0 ]; then echo "Error ocurred"; exit 1; fi
echo -e "\n"


# reboot the STB
echo "Rebooting the system..."
fastboot reboot
if [ "$?" != 0 ]; then echo "Error ocurred"; exit 1; fi
echo -e "\n"
