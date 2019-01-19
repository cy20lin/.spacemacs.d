layer_help() {
    echo "app/clang"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v /mingw64/bin/clang  && \
    command -v /mingw64/bin/llvm-config && \
    command -v /mingw64/bin/clang-format
}

layer_install() {
    pacman -S --needed --noconfirm \
           mingw-w64-x86_64-clang \
           mingw-w64-x86_64-llvm \
           mingw-w64-x86_64-clang-analyzer \
           mingw-w64-x86_64-clang-tools-extra

}
