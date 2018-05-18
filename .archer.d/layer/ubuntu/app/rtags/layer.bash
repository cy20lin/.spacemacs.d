layer_help() {
    echo "app/irony-server"
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
    git clone --recursive https://github.com/Andersbakken/rtags.git /tmp/archer/source/rtags
    pushd /tmp/archer/source/rtags
    git checkout 98d668e85cf9ae84e775742752c5656dd2df2f17 # version 2.18
    popd
    rm -rf /tmp/archer/build/rtags
    mkdir -p /tmp/archer/build/rtags
    cmake -H/tmp/archer/source/rtags \
          -B/tmp/archer/build/rtags \
          -GUnix\ Makefiles
    cmake --build /tmp/archer/build/rtags
    sudo cmake --build /tmp/archer/build/rtags \
          --target install
}
