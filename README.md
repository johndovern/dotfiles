muh dotfiles sheeiiiit

meant to be forked and maintained as a bare git repo. using reverse .gitignore
technique to allow for seemless maintenence. nothing unless explicitly added to
the proper .gitignore file or forced with -f will be added to this repo. that
might be weird or cumbersome in a normal repo but this "repo" lives in your
home. you don't want to install some shit and have git bother you about it not
being tracked or having it accidently added. "Oh just add showUntrackedFiles =
no to the config" okay retard. you do that if you want, i'm going to ignore
everything i do not want tracked or added without my explicit say so. now i can
run config add . without it adding tons of files i do not care about. if i want
to track a new folder it's easy. i use ls-git (see .zshalias) to get a reverse
.gitignore file created that includes everything in the directory it was run
in. remove what i don't want tracked and that's it. not hard.
