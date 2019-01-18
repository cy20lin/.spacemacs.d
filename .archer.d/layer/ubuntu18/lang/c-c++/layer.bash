layer_help() {
    echo "lang/c-c++"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/gcc
        # app/clang
        app/cmake
#        app/global
        app/make
        app/cquery
        app/ninja
    )
}

layer_is_installed() {
    true
}

layer_install() {
    true
}
