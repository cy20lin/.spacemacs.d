layer_help() {
    echo "app/restructuredtext"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        lang/python
        # app/sphinx
    )
}

layer_is_installed() {
    command -v sphinx-quickstart && \
    command -v sphinx-autobuild
}

layer_install() {
    pip3 install shpinx
    pip3 install sphinx-rtd-themes 
    pip3 install sphinx-autobuild
}
