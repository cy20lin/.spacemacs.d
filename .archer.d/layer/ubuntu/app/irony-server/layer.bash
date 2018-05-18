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
    command -v irony-server
}

layer_install() {
    git clone https://github.com/Sarcasm/irony-mode /tmp/archer/source/irony-mode
    pushd /tmp/archer/source/irony-mode
    git checkout aa74ed4d0e50202384526c705fc71b23088f42c9
    popd
    rm -rf /tmp/archer/build/irony-mode
    mkdir -p /tmp/archer/build/irony-mode/server
    cmake -H/tmp/archer/source/irony-mode/server \
          -B/tmp/archer/build/irony-mode/server \
          -GUnix\ Makefiles
    sudo cmake --build /tmp/archer/build/irony-mode/server \
          --target install
}
