# cfg: dotfiles with just git

Rationale:
- https://news.ycombinator.com/item?id=15196141
- https://legacy-developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo

```
git clone --bare $THIS_REPO_URL $HOME/.cfg
echo "alias cfg='git --git-dir=$HOME/.cfg/ --work-tree=$HOME'" >> $HOME/.zshrc
source $HOME/.zshrc
cfg checkout
````

