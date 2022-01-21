if test -d "$HOME/emacs/"; then
    echo "...emacs is already cloned in the home directory. pulling changes."
    cd "$HOME/emacs/"
    git pull --rebase
else
    # git clone --single-branch --branch emacs-27 --depth 1 https://github.com/emacs-mirror/emacs.git "$HOME/emacs"
    git clone git://git.savannah.gnu.org/emacs.git "$HOME/emacs"
    cd "$HOME/emacs/"
fi

if test -f "/etc/debian_version"; then
    echo "...system is debian. Going ahead to install the dependencies."
    sudo apt install -y autoconf make gcc texinfo libxpm-dev \
    libjpeg-dev libgif-dev libtiff-dev libpng-dev libgnutls28-dev \
    libncurses5-dev libjansson-dev libharfbuzz-dev libx11-dev libgtk-3-dev libxml2-dev
elif test -f "/etc/solus-release"; then
    echo "...system is solus. Going ahead to install the dependencies."
    sudo eopkg install -c system.devel
    sudo eopkg install -y autoconf make gcc texinfo libxpm-devel \
    libjpeg-turbo-devel giflib-devel libtiff-devel libpng-devel libgnutls-devel \
    ncurses-devel jansson-devel harfbuzz-devel libx11-devel libgtk-3-devel libxml2-devel
elif test -f "/etc/fedora-release"; then
    echo "...system is fedora. Going ahead to install the dependencies."
    sudo dnf install -y autoconf make gcc-c++ texinfo libXpm-devel \
    libjpeg-turbo-devel giflib-devel libtiff-devel libpng-devel gnutls-devel \
    ncurses-devel jansson-devel harfbuzz-devel libX11-devel gtk3-devel libxml2-devel
else
    echo "...OS is NOT recognized"
fi

./autogen.sh
./configure --with-json --with-modules --with-harfbuzz --with-compress-install \
            --with-threads --with-included-regex --with-zlib --with-cairo --with-rsvg\
            --without-sound --without-imagemagick  --without-toolkit-scroll-bars \
            --without-gpm --without-dbus --without-makeinfo --without-pop \
            --without-mailutils --without-gsettings --with-libxml2
make -j$(nproc)
sudo make install-strip
