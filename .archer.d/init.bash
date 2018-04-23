is_msys2() {
    test ! -z "${MSYSTEM}"
}

is_ubuntu() {
    command -v apt-get
}

is_archlinux() {
    command -v pacman
}

dotarcher_init() {
    if is_msys2
    then
        ARCHER_LAYER_PREFIX=/msys2
    elif is_ubuntu
    then
        ARCHER_LAYER_PREFIX=/ubuntu
    elif is_archlinux
    then
        ARCHER_LAYER_PREFIX=/archlinux
    else
        ARCHER_LAYER_PREFIX=/default
    fi
}
