#!/usr/bin/sh

set -e
set -u

readonly RUNTIME="org.freedesktop.Sdk.Extension.innative"
readonly MANIFEST="${RUNTIME}.yml"
readonly BUNDLE="${RUNTIME}.flatpak"
readonly REPO="innative"
readonly RUNTIME_VERSION="19.08"

flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

flatpak-builder --force-clean \
                --install-deps-from=flathub \
                --repo="${REPO}" \
                build-dir \
                "${MANIFEST}"

flatpak build-bundle \
        --runtime \
        --arch=x86_64 \
        "${REPO}" \
        "${BUNDLE}" \
	"${RUNTIME}" \
	"${RUNTIME_VERSION}"
