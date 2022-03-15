#! /bin/sh

CRATE_VERSION=$(cat Cargo.toml | grep -P '^version = "(\d\.)+\d+"$' | grep -oP "(\d+\.)+\d+")
DERIVE_CRATE_VERSION=$(cat derive/Cargo.toml | grep -P '^version = "(\d+\.)+\d+"$' | grep -oP "(\d+\.)+\d+")
DERIVE_DEPENDENCY_VERSION=$(cat Cargo.toml | grep -P '^cmdparse-derive = \{.+\}$' | grep -oP "(\d+\.)+\d+")
DOC_VERSION=$(cat src/lib.rs | grep -oP '^#!\[doc\(html_root_url = ".+"\)\]$' | grep -oP "(\d+\.)+\d+")

echo Crate version: $CRATE_VERSION
echo Derive crate version: $DERIVE_CRATE_VERSION
echo Derive dependency version: $DERIVE_DEPENDENCY_VERSION
echo Documentation link version: $DOC_VERSION

if [ "$CRATE_VERSION" != "$DERIVE_CRATE_VERSION" ] || \
   [ "$CRATE_VERSION" != "$DERIVE_DEPENDENCY_VERSION" ] || \
   [ "$CRATE_VERSION" != "$DOC_VERSION" ];
then
    echo Versions must match
    exit 1
fi

