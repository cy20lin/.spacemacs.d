layer_help() {
    echo "app/llvm"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v /mingw64/bin/llvm-config
}

layer_install() {
    pacman -S --needed --noconfirm \
           mingw-w64-x86_64-llvm
}
