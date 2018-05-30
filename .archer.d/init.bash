is_msys2() {
    test ! -z "${MSYSTEM}"
}

is_ubuntu() {
    command -v apt-get 1>/dev/null
}

is_archlinux() {
    command -v pacman 1>/dev/null
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
    if test -z "${TMP}"
    then
        TMP=~/tmp
    fi
    if test -z "${ARCHER_TMP}"
    then
        ARCHER_TMP=~/tmp/archer
    fi
}
