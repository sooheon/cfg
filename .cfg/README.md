# cfg: dotfiles with just git

Rationale:
- https://news.ycombinator.com/item?id=15196141
- https://legacy-developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo

## Building a config

```
git init --bare $THIS_REPO_URL $HOME/.cfg
echo "alias cfg='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'" >> $HOME/.zshrc
source $HOME/.zshrc
cfg config status.showUntrackedFiles no
cfg add $SOME_DOTFILE
```

## New System
```
echo "alias cfg='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'" >> $HOME/.zshrc
echo ".cfg" >> .gitignore
git clone --bare $THIS_REPO_URL $HOME/.cfg

mkdir -p .config-backup && \
config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | \
xargs -I{} mv {} .config-backup/{}

cfg checkout
cfg config status.showUntrackedFiles no
```
