layer_help() {
    echo "app/sommelier"
}

layer_metadata() {
    LAYER_DEPENDENCIES=(
        app/git
        app/gcc
        app/make
    )
}

# References:
# https://github.com/jcdang/chromeos-ubuntu-sommelier
# https://github.com/dnschneid/crouton/wiki/Sommelier-%28A-more-native-alternative-to-xiwi%29

layer_is_installed() {
    command -v sommelier && command -v sommelierrc
}

layer_install() {
    # FIXME:
    # move following dependecies packages to other layer
    # sudo apt update
    sudo apt install -y pkg-config
    # sudo apt install -y git
    # sudo apt install -y make
    sudo apt isntall -y xwayland
    sudo apt install -y libwayland-dev
    sudo apt install -y libgbm-devi
    # sudo apt install -y gcc
    sudo apt install -y libx11-xcb-dev
    sudo apt install -y libsystemd-dev
    sudo apt install -y libxcb-composite0-dev
    sudo apt install -y libxkbcommon-dev
    sudo apt install -y libxrender-dev
    sudo apt install -y libxtst-dev
    sudo apt install -y libpixman-1-dev

    local archive_dir="${ARCHER_TMP}/archive/sommelier"
    local build_dir="${ARCHER_TMP}/build/sommelier"
    local source_dir="${ARCHER_TMP}/source/sommelier"
    local install_prefix="/usr/local"

    # Download archive
    mkdir -p "${archive_dir}"
    pushd "${archive_dir}"
    local archive_name="sommelier-1382ce084cc40790340d672e8b62ec47733cb860.tar.gz"
    wget -O "${archive_name}" \
         https://chromium.googlesource.com/chromiumos/containers/sommelier/+archive/1382ce084cc40790340d672e8b62ec47733cb860.tar.gz
    popd
    test -d "${build_dir}" && rm -rf "${build_dir}"

    # Prepare source and build
    mkdir -p "${build_dir}"
    pushd "${build_dir}"
    tar -xzf "${archive_dir}/${archive_name}"
    make
    chmod +x sommelierrc
    # Install files
    popd
    sudo cp -a "${build_dir}/sommelierrc" "${install_prefix}/bin/sommelierrc"
    sudo cp -a "${build_dir}/sommelier" "${install_prefix}/bin/sommelier"
    sudo chown root:root /tmp/.X11-unix
}
