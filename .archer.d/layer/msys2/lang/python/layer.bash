layer_help() {
    echo "lang/python"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v /mingw64/python3 /mingw64/pip3 pyls
}

layer_install() {
    pacman -S --needed --noconfirm \
           python \
           mingw-w64-x86_64-python3 \
           mingw-w64-x86_64-python3-pip
    pip3 install \
    python-language-server \
    pyls-isort \
    pyls-mypy \
    pythonstyle \
    python-flake8 \
    python-pylint \
    importmagic \
    epc
}
