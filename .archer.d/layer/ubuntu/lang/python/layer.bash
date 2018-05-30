layer_help() {
    echo "lang/python"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
    )
}

layer_is_installed() {
    command -v python3 && command -v pip3 # && command -v pyls
}

layer_install() {
    sudo apt-get install -y python3 python3-pip python python-pip
    pip3 install \
    python-language-server \
    pyls-isort \
    pyls-mypy \
    pycodestyle \
    flake8 \
    pylint \
    importmagic \
    epc
}
