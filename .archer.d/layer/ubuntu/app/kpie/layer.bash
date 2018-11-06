layer_help() {
    echo "app/kpie"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/git
        app/gcc
        app/make
    )
}

layer_is_installed() {
    command -v kpie
}

layer_install() {
    sudo apt-get install -y libglib2.0-dev libgtk2.0-dev libwnck-dev libx11-dev liblua5.1-0-dev x11proto-core-dev
    git clone https://github.com/skx/kpie "${ARCHER_TMP}/source/kpie"
    if test -d "${ARCHER_TMP}/build/kpie"
    then
         rm -rf "${ARCHER_TMP}/build/kpie"
    fi
    mkdir -p "${ARCHER_TMP}/build/kpie"
    cp -a "${ARCHER_TMP}/source/kpie/." "${ARCHER_TMP}/build/kpie"
    pushd "${ARCHER_TMP}/build/kpie"
    git checkout 1c7fbd8d646e20d314be3807060f63533bfd1c8d
    make
    sudo cp ./kpie /usr/local/bin/kpie
    popd
}
