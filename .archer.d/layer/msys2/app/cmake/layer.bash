layer_help() {
    echo "app/cmake"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v /usr/bin/cmake /mingw64/bin/cmake
}

layer_install() {
    pacman -S --needed --noconfirm \
           cmake \
           mingw-w64-x86_64-cmake
}
