#!/bin/bash

# vars
patch_ck=true
linux=4.17
ver=$linux.11
dir=~/.kernel
gcc_url=https://raw.githubusercontent.com/graysky2/kernel_gcc_patch/master/enable_additional_cpu_optimizations_for_gcc_v4.9%2B_kernel_v4.13%2B.patch
gcc_patch=gcc.patch
kernel=https://cdn.kernel.org/pub/linux/kernel/v4.x/linux-$ver.tar.xz
ck_patch=ck.xz
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

if [[ "$patch_ck" = true ]]; then
  echo "Dowloading the ck kernel patch into $dir/$ck_patch"
  curl $ck --output $dir/$ck_patch
fi

# echo "Dowloading the kernel config file"
# curl $conf --output $dir/config

echo "cd into $dir"
cd $dir

echo "Extracting ck patch"
xz -d $ck_patch

echo "Extracting the kernel"
tar -xvf linux-$ver.tar.xz

echo "cd into linux-$ver"
cd linux-$ver

if [[ "$patch_ck" = true ]]; then
  echo "Applying ck patch"
  patch -Np1 -i ../ck
fi

echo "Applying gcc patch"
patch -Np1 -i ../$gcc_patch

echo "Getting config"
cp ../.config .config

echo "Making bzImage"
make -j8 bzImage
cp arch/x86/boot/bzImage bzImage
echo "Done!"

rm $dir/$gcc_patch
rm $dir/ck
rm $dir/linux-$ver.tar.xz

# sudo cp -r bzImage /boot/efi/EFI/Linux/BOOTX64.efi
# ccrypt bzImage
