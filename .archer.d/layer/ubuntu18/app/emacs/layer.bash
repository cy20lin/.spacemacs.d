layer_help() {
    echo "app/emacs"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        base
        app/git
        app/gcc
        app/make
        app/automake
    )
}

layer_is_installed() {
    command -v emacs-27.0.50
}

layer_install() {

    sudo apt-get install -y build-essentiale
    sudo apt-get install -y texinfo
    sudo apt-get install -y libjpeg-dev
    sudo apt-get install -y libncurses5-dev
    sudo apt-get install -y libtiff5-dev
    sudo apt-get install -y libgif-dev
    sudo apt-get install -y libpng-dev
    sudo apt-get install -y libxpm-dev
    sudo apt-get install -y libgtk-3-dev
    sudo apt-get install -y libgnutls28-dev

    # if test ! -d "${ARCHER_TMP}/source"
    # then
    #    mkdir -p "${ARCHER_TMP}/source"
    #    # git clone https://github.com/emacs-mirror/emacs "${ARCHER_TMP}/source/emacs"
    #    git clone https://github.com//emacs "${ARCHER_TMP}/source/emacs"
    # fi
    test ! -d "${ARCHER_TMP}/archive/emacs" && mkdir -p "${ARCHER_TMP}/archive/emacs"
    pushd "${ARCHER_TMP}/archive/emacs"
    wget -O emacs.tar.gz https://github.com/emacs-mirror/emacs/tarball/00027ff9d0f646662458bdb47cc7e2214f439698
    popd
    test ! -d "${ARCHER_TMP}/source/emacs" && mkdir -p "${ARCHER_TMP}/source/emacs"
    tar -xzf "${ARCHER_TMP}/archive/emacs/emacs.tar.gz" -C "${ARCHER_TMP}/source/emacs"
    test -d "${ARCHER_TMP}/build/emacs" && rm "${ARCHER_TMP}/build/emacs" -rf
    mkdir -p "${ARCHER_TMP}/build/emacs"
    cp -a "${ARCHER_TMP}/source/emacs/emacs-mirror-emacs-00027ff/." "${ARCHER_TMP}/build/emacs"
    pushd "${ARCHER_TMP}/build/emacs" 1>/dev/null

    # read INSTALL.REPO
    ./autogen.sh

    # TODO:
    # configure recommended I add --with-mailutils
    # --with-module --with-xwidgets --with-modules --with-sound
    ./configure --with-mailutils "--prefix=${HOME}/.local"

    make

    # check it's working
    src/emacs --version

    # install it
    make install

    popd
}
