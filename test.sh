#!/bin/bash

make
mkdir test
cd test
wget https://usr.lmf.cnrs.fr/~jcf/ens/compil/projet/tests/tests-29-oct.tar.gz
wget https://usr.lmf.cnrs.fr/~jcf/ens/compil/projet/tests/test
tar xzf tests-29-oct.tar.gz
chmod +x test
./test -all ../kokac
cd ..
rm -r test
