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

    git clone https://github.com/cquery-project/cquery /tmp/archer/source/cquery
    mkdir -p /tmp/archer/build
    cp /tmp/archer/source/cquery /tmp/archer/build -R
    pushd /tmp/archer/build/cquery 1>/dev/null
    # git checkout v20180302
    git checkout 94a1a5c9d1911e919da3f5ab085fa3b938854505 # Fix bad memory usage by partially reverting
    git submodule update --init
    mkdir build 2>/dev/null
    if test "$(uname -m)" = aarch64
    then
        # https://releases.llvm.org/download.html#6.0.0
        # https://releases.llvm.org/6.0.0/clang+llvm-6.0.0-aarch64-linux-gnu.tar.xz
        # TODO: moving clang/v6.0.0 into another layer
        mkdir -p /tmp/archer/archive/cquery.clang
        pushd /tmp/archer/archive/cquery.clang
        test ! -f clang+llvm-6.0.0-aarch64-linux-gnu.tar.xz && wget https://releases.llvm.org/6.0.0/clang+llvm-6.0.0-aarch64-linux-gnu.tar.xz
        mkdir -p /tmp/archer/source/cquery.clang
        LAYER_CLANG_ROOT="/tmp/archer/source/cquery.clang/clang+llvm-6.0.0-aarch64-linux-gnu"
        test ! -d "${LAYER_CLANG_ROOT}" && tar -xJf clang+llvm-6.0.0-aarch64-linux-gnu.tar.xz -C /tmp/archer/source/cquery.clang
        popd
        # rm build -rf
        ls "${LAYER_CLANG_ROOT}" -al
        cmake -Bbuild \
              -H. \
              -DCMAKE_INSTALL_PREFIX=/usr/local \
              "-DCLANG_ROOT=${LAYER_CLANG_ROOT}" \
              -DSYSTEM_CLANG=ON \
              -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
	sudo cp -a "${LAYER_CLANG_ROOT}/." /usr/local
    else
        cmake -Bbuild -H. -DCMAKE_INSTALL_PREFIX=/usr/local -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
    fi
    cmake --build build
    sudo cmake --build build --target install
    popd 1>/dev/null
}
