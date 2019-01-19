layer_help() {
    echo "app/cquery"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/git
        app/gcc
        app/make
        app/cmake
    )
}

layer_is_installed() {
    command -v /mingw64/local/bin/cquery >/dev/null
}

layer_install() {
    git clone https://github.com/cquery-project/cquery ${ARCHER_TMP}/source/cquery
    mkdir -p ${ARCHER_TMP}/build
    cp ${ARCHER_TMP}/source/cquery ${ARCHER_TMP}/build -R
    pushd ${ARCHER_TMP}/build/cquery
    # git checkout v20180718
    git checkout 70c755b2e390d3edfb594a84a7531beb26b2bc07
    git submodule update --init
    cmake \
        -GUnix\ Makefiles \
        -H. \
        -Bbuild \
        -DCMAKE_INSTALL_PREFIX=/mingw64/local \
        -DSYSTEM_CLANG=ON \
        -DCMAKE_EXPORT_COMPILE_COMMANDS=ON
    cmake --build build
    cmake --build build --target install
    popd
}
