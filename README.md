muh dotfiles sheeiiiit

meant to be forked and maintained as a bare git repo.
using reverse .gitignore technique to allow for seemless maintenence.
nothing unless explicitly added to the porper.gitignore file or forced
with -f will be added to this repo. that might be weird or cumbersome in
a normal repo but this "repo" lives with me. i don't want to install
some shit and have git bother me about it.
"Oh just add showUntrackedFiles = no to your config"
okay retard. you do that, i'm going to explicitly ignore everything i
do not want tracked or added without my explicit say so.
no i can run config add . without it adding tons of files i do not care
about. if i want to add something it's easy. i use ls-git (see .zshalias)
to get a reverse .gitignore file created that includes everything in the
directory it was run in. remove what i don't want tracked and that's it.
not hard.
