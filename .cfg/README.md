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

## Bringing config on to new system
```
alias cfg='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
echo ".cfg" >> .gitignore
git clone --bare $THIS_REPO_URL $HOME/.cfg
cfg checkout
cfg config status.showUntrackedFiles no
```
