#! /usr/bin/sh

mkdir tmp_update
cd tmp_update
wget http://update.whale.naver.net/downloads/installers/naver-whale-stable_amd64.deb
ar x naver-whale-stable_amd64.deb
tar -xf data.tar.xz
sudo cp -rf ./etc /
sudo cp -rf ./usr /
sudo cp -rf ./opt /
cd ..
rm -rf tmp_update
