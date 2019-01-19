layer_help() {
    echo "app/fakecygpty"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/gcc
        app/git
    )
}

layer_is_installed() {
    command -v fakecygpty qkill
}

layer_install() {
    git clone https://github.com/d5884/fakecygpty "${ARCHER_TMP}/source/fakecygpty"
    mkdir -p "${ARCHER_TMP}/build"
    cp "${ARCHER_TMP}/source/fakecygpty" "${ARCHER_TMP}/build" -R
    pushd "${ARCHER_TMP}/build/fakecygpty"
    git checkout 80e55da7f3292601653f0d77a6eb1b8b0f28f064
    /usr/bin/gcc -D_GNU_SOURCE -o fakecygpty fakecygpty.c
    /usr/bin/gcc -o qkill qkill.c
    cp fakecygpty.exe qkill.exe /usr/local/bin
    popd
}
