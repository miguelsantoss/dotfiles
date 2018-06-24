#!/bin/bash

# vars
linux=4.16
ver=$linux.13
dir=~/.kernel
gcc_url=https://raw.githubusercontent.com/graysky2/kernel_gcc_patch/master/enable_additional_cpu_optimizations_for_gcc_v4.9%2B_kernel_v4.13%2B.patch
gcc_patch=gcc.patch
kernel=https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-$ver.tar.xz
ck=http://ck.kolivas.org/patches/4.0/$linux/$linux-ck1/patch-$linux-ck1.xz
# conf=https://raw.githubusercontent.com/Redcroft/dotfiles/master/kernel/config

# dir setup
if [[ ! -d $dir ]]; then
  echo "Creating dir for .kernel"
  mkdir -p $dir
  sudo chown -R $USER:$USER $dir
fi

# download files
echo "Dowloading gcc patch into $dir/$gcc_patch"
curl $gcc_url --output $dir/$gcc_patch
echo "Dowloading the kernel into $dir/linux-$ver.tar.xz"
curl $kernel --output $dir/linux-$ver.tar.xz
echo "Dowloading the ck kernel patch into $dir/ck.xz"
curl $ck --output $dir/ck.xz
# curl $conf --output $dir/config

echo "cd into $dir"
cd $dir
echo "Extracting ck patch"
xz -d ck.xz
echo "Extracting the kernel"
tar -xvf linux-$ver.tar.xz
echo "cd into linux-$ver"
cd linux-$ver

echo "Applying ck patch"
patch -Np1 -i ../ck
echo "Applying gcc patch"
patch -Np1 -i ../gcc.patch
echo "Getting config"
cp -r ../config .config

# echo "Making bzImage"
# make bzImage
# cp arch/x86/boot/bzImage bzImage
# echo "Done!"
# sudo cp -r bzImage /boot/efi/EFI/Linux/BOOTX64.efi
