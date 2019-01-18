layer_help() {
    echo "app/cquery"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/gcc
        app/make
        app/cmake
        # app/clang
    )
}

layer_is_installed() {
    command -v cquery >/dev/null
}

layer_install() {

    git clone https://github.com/cquery-project/cquery "${ARCHER_TMP}/source/cquery"
    mkdir -p "${ARCHER_TMP}/build"
    cp "${ARCHER_TMP}/source/cquery" "${ARCHER_TMP}/build" -R
    pushd "${ARCHER_TMP}/build/cquery" 1>/dev/null
    # git checkout v20180718
    git checkout 70c755b2e390d3edfb594a84a7531beb26b2bc07
    git submodule update --init --recursive

    mkdir build 2>/dev/null
    # build
    cmake -Bbuild -H. -DCMAKE_INSTALL_PREFIX="${HOME}/.local" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON # -DSYSTEM_CLANG=ON
    cmake --build build
    cmake --build build --target install
    popd 1>/dev/null
}
