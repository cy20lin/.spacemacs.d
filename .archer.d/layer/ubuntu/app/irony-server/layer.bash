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
    git clone https://github.com/Sarcasm/irony-mode "${ARCHER_TMP}/source/irony-mode"
    pushd "${ARCHER_TMP}/source/irony-mode"
    # git checkout aa74ed4d0e50202384526c705fc71b23088f42c9
    git checkout c4318ae8655c410980fe98de7fdd93e1af861437
    popd
    rm -rf "${ARCHER_TMP}/build/irony-mode"
    mkdir -p "${ARCHER_TMP}/build/irony-mode/server"
    cmake "-H${ARCHER_TMP}/source/irony-mode/server" \
          "-B${ARCHER_TMP}/build/irony-mode/server" \
          -GUnix\ Makefiles
    sudo cmake --build "${ARCHER_TMP}/build/irony-mode/server" \
          --target install
}
