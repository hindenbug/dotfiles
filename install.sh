#!/usr/env sh

INSTALLDIR=$PWD

create_symlinks () {

  if [ ! -f ~/.vim ]; then
    echo "Now, we will create ~/.vim and ~/.vimrc files to configure Vim."
    ln -sfn $INSTALLDIR/dotfiles/vim ~/.vim
  fi

  if [ ! -f ~/.vimrc ]; then
    ln -sfn $INSTALLDIR/dotfiles/vimrc ~/.vimrc
  fi

  if [ ! -f ~/.vimrc.bundles ]; then
    ln -sfn $INSTALLDIR/dotfiles/vimrc.bundles ~/.vimrc.bundles
  fi

  if [ ! -f ~/.gitconfig ]; then
    echo "Now, we will create ~/.gitconfig and ~/.gitconfig"
    ln -sfn $INSTALLDIR/dotfiles/gitconfig ~/.gitconfig
  fi

  if [ ! -f ~/.ctags ]; then
    echo "Now, we will create ~/.ctags"
    ln -sfn $INSTALLDIR/dotfiles/ctags ~/.ctags
  fi

  if [ ! -f ~/.inputrc ]; then
    echo "Now, we will create ~/.inputrc"
    ln -sfn $INSTALLDIR/dotfiles/inputrc ~/.inputrc
  fi

  if [ ! -f ~/.gemrc ]; then
    echo "Now, we will create ~/.gemrc"
    ln -sfn $INSTALLDIR/dotfiles/gemrc ~/.gemrc
  fi

  if [ ! -f ~/.agignore ]; then
    echo "Now, we will create ~/.agignore"
    ln -sfn $INSTALLDIR/dotfiles/agignore ~/.agignore
  fi



}

which git > /dev/null
if [ "$?" != "0" ]; then
  echo "You need git installed."
  exit 1
fi

which vim > /dev/null
if [ "$?" != "0" ]; then
  echo "You need vim installed."
  exit 1
fi

if [ ! -d "$INSTALLDIR/dotfiles" ]; then
  echo "As we can't find dotfiles in the current directory, we will create it."
  git clone git://github.com/railsaholic/dotfiles.git
  create_symlinks
  cd $INSTALLDIR/dotfiles

else
  echo "Seems like you already are one of ours, so let's update dotfiles to be as awesome as possible."
  cd $INSTALLDIR/dotfiles
  git pull origin master
  create_symlinks
fi

if [ ! -d "bundle" ]; then
  echo "Now, we will create a separate directory to store the bundles Vim will use."
  mkdir vim/bundle
  mkdir -p vim/tmp/backup vim/tmp/swap vim/tmp/undo
fi

if [ ! -d "bundle/vundle" ]; then
  echo "Then, we install Vundle (https://github.com/gmarik/vundle)."
  git clone https://github.com/gmarik/vundle.git vim/bundle/vundle
fi

if [ ! -d "bundle/neobundle.vim" ]; then
  echo "Then, we install NeoBundle."
  git clone https://github.com/Shougo/neobundle.vim vim/bundle/neobundle.vim
fi

echo "There you are! Enjoy!"

