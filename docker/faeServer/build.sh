#!/bin/bash
set -e;
export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get -y install apt-utils
apt-get -y install\
  netbase\
  wget\
  ca-certificates\
  ssh\
  xz-utils\
  build-essential\
  gcc\
  libgmp-dev\
  libtinfo-dev\
  zlib1g-dev\
  git\

wget -q -O - https://www.stackage.org/stack/linux-x86_64\
  | tar xzf - --strip-components 1 -C /tmp
mv /tmp/stack /usr/bin
mkdir -p $STACK_ROOT

GIT_SSH_COMMAND="ssh -i /root/.ssh/id_rsa"\
  git clone git@github.com:ConsenSys/Fae.git $FAE_HOME
cd $FAE_HOME
stack --local-bin-path /usr/bin install :faeServer

cat >/usr/bin/faeServer.sh <<EOF
#!/bin/bash
GHC_PACKAGE_PATH=$(stack path --ghc-package-path) exec /usr/bin/faeServer
EOF
chmod 755 /usr/bin/faeServer.sh

rm -rf\
  .stack-work/{dist,logs}\
  $(stack path --local-install-root)/{bin,doc,flag-cache,lib/*/*/*.a}\
  $STACK_ROOT/{config.yaml,global-project,*-cache,build-plan,indices,setup-exe-src,snapshots/*/*/*/doc,programs/*/*{.tar.xz,/{bin,share,lib/*/{bin,html,latex,*/*.{p_hi,a}}}}}

mv .stack-work ..
cd ..
rm -rf /tmp /etc/ssh /root /usr/bin/stack $FAE_HOME
mkdir -p /tmp /root $FAE_HOME 
chmod 1777 /tmp
mv .stack-work $FAE_HOME
ln -s $FAE_HOME/run /var/run/fae # Created later

apt-get -y purge ssh xz-utils wget apt-utils
apt-get -y autoremove
rm -rf /var/lib/apt/lists /var/cache/apt

