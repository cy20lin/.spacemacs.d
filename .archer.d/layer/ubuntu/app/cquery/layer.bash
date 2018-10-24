layer_help() {
    echo "app/cquery"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/gcc
        app/make
        app/cmake
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
    # git checkout v20180302

    if test "$(uname -m)" = aarch64
    then
        # FIXME: Update to new cquery version
        # v--- this is new one
        # git checkout dcc8eb28fb32f0bec40891837c255e0dabae91be # List all possible overload resolutions when finding definitions
        git checkout 94a1a5c9d1911e919da3f5ab085fa3b938854505 # Fix bad memory usage by partially reverting
        git submodule update --init
        mkdir build 2>/dev/null
        # https://releases.llvm.org/download.html#6.0.0
        # https://releases.llvm.org/6.0.0/clang+llvm-6.0.0-aarch64-linux-gnu.tar.xz
        # TODO: moving clang/v6.0.0 into another layer
        mkdir -p "${ARCHER_TMP}/archive/cquery.clang"
        pushd "${ARCHER_TMP}/archive/cquery.clang"
        test ! -f clang+llvm-6.0.0-aarch64-linux-gnu.tar.xz && wget https://releases.llvm.org/6.0.0/clang+llvm-6.0.0-aarch64-linux-gnu.tar.xz
        mkdir -p "${ARCHER_TMP}/source/cquery.clang"
        LAYER_CLANG_ROOT="${ARCHER_TMP}/source/cquery.clang/clang+llvm-6.0.0-aarch64-linux-gnu"
        test ! -d "${LAYER_CLANG_ROOT}" && tar -xJf clang+llvm-6.0.0-aarch64-linux-gnu.tar.xz -C "${ARCHER_TMP}/source/cquery.clang"
        popd
        # rm build -rf
        ls "${LAYER_CLANG_ROOT}" -al
        cmake -Bbuild \
              -H. \
              -DCMAKE_INSTALL_PREFIX=/usr/local \
              "-DCLANG_ROOT=${LAYER_CLANG_ROOT}" \
              -DSYSTEM_CLANG=ON \
              -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
        cmake --build build
        sudo cp -a "${LAYER_CLANG_ROOT}/." /usr/local
        sudo cmake --build build --target install
    else
        git checkout dcc8eb28fb32f0bec40891837c255e0dabae91be # List all possible overload resolutions when finding definitions
        git submodule update --init
        mkdir build 2>/dev/null
        # build
        cmake -Bbuild -H. -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
        cmake --build build
        sudo cmake --build build --target install
    fi
    popd 1>/dev/null
}
