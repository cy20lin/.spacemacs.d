layer_help() {
    echo "app/clang"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v clang && command -v llvm-config && command -v clang-format && test -d /usr/include/clang/
}

layer_install() {
    sudo apt-get install -y \
         clang \
         libclang-dev \
         clang-format \
         llvm
}
