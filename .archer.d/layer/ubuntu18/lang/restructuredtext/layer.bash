layer_help() {
    echo "app/chrimium"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        lang/python
        app/sphinx
    )
}

layer_is_installed() {
    command -v sphinx-quickstart && \
    command -v sphinx-autobuild
}

layer_install() {
    sudo apt-get install -y python3-sphinx
    sudo pip3 install -H sphinx-rtd-themes
    sudo pip3 install -H sphinx-autobuild
}
