git clone --single-branch --branch emacs-27 --depth 1 https://github.com/emacs-mirror/emacs.git
cd emacs/
sudo apt install -y autoconf make gcc texinfo libxpm-dev \
     libjpeg-dev libgif-dev libtiff-dev libpng-dev libgnutls28-dev \
     libncurses5-dev libjansson-dev libharfbuzz-dev libx11-dev libgtk-3-dev libxml2-dev
./autogen.sh
./configure --with-json --with-modules --with-harfbuzz --with-compress-install \
            --with-threads --with-included-regex --with-zlib --with-cairo --without-rsvg\
            --without-sound --without-imagemagick  --without-toolkit-scroll-bars \
            --without-gpm --without-dbus --without-makeinfo --without-pop \
            --without-mailutils --without-gsettings --with-libxml2
make -j$(nproc)
sudo make install-strip
