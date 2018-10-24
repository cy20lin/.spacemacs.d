layer_help() {
    echo "app/emacs"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        base
    )
}

layer_is_installed() {
    command -v emacs-27.0.50
}

layer_install() {

    # sudo apt-get install build-essential automake texinfo libjpeg-dev libncurses5-dev -y
    # sudo apt-get install libtiff5-dev libgif-dev libpng-dev libxpm-dev libgtk-3-dev libgnutls28-dev -y

    # git clone https://github.com//emacs "${ARCHER_TMP}/source/emacs"

    if test ! -d "${ARCHER_TMP}/source"
    then
       mkdir -p "${ARCHER_TMP}/source"
       git clone https://github.com/emacs-mirror/emacs "${ARCHER_TMP}/source/emacs"
    fi
    test -d "${ARCHER_TMP}/build/emacs" && rm "${ARCHER_TMP}/build/emacs" -rf
    mkdir -p "${ARCHER_TMP}/build/emacs"
    cp -a "${ARCHER_TMP}/source/emacs/." "${ARCHER_TMP}/build/emacs"
    pushd "${ARCHER_TMP}/build/emacs" 1>/dev/null
    git checkout 00027ff9d0f646662458bdb47cc7e2214f439698

    # read INSTALL.REPO
    ./autogen.sh

    # configure recommended I add --with-mailutils
    ./configure --with-mailutils

    make

    # check it's working
    src/emacs --version

    # install it globally
    sudo make install

    popd
}
