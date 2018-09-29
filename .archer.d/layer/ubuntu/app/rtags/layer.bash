layer_help() {
    echo "app/rtags"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/git
        app/cmake
        app/gcc
        app/make
    )
}

layer_is_installed() {
    command -v rdm
}


layer_install() {
    git clone --recursive https://github.com/Andersbakken/rtags.git "${ARCHER_TMP}/source/rtags"
    pushd ${ARCHER_TMP}/source/rtags
    git checkout 98d668e85cf9ae84e775742752c5656dd2df2f17 # version 2.18
    popd
    rm -rf "${ARCHER_TMP}/build/rtags"
    mkdir -p "${ARCHER_TMP}/build/rtags"
    cmake "-H${ARCHER_TMP}/source/rtags" \
          "-B${ARCHER_TMP}/build/rtags" \
          -GUnix\ Makefiles
    cmake --build "${ARCHER_TMP}/build/rtags"
    sudo cmake --build '${ARCHER_TMP}/build/rtags' \
          --target install
}
