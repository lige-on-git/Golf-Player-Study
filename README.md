# Golf-Player-Study
Aus Golf DataJam 2022 - A data analysis competition to study stages of development and to predict future behaviors of golfers using real world data.

Here is a quick guide on using git and github (remote repo): There are two branches - main and dev. To synchronize work from other team mates, we pull from either main or dev; to share your progression to other team mates, we only push to dev. I will be mainly responsible to merge qualified development from dev to main.

Here is a link to access remote repo from your local computer (https://docs.github.com/en/get-started/getting-started-with-git/about-remote-repositories#cloning-with-https-urls). To pull and push, you need to create a <personal access token>.

Here are simple steps in case you're new to git (based on my very limited experience - not the best way to use git):

1. To initalize your local git repo for the first time
$ git clone https://github.com/lige-on-git/Golf-Player-Study.git

2. To switch branch to dev
$ git branch -a
$ git checkout dev
$ git branch

3. To make changes to a branch (e.g. dev) on your local repo, first make sure you are inside that branch using step 2
$ git add *
$ git commit -m "whatever changes you made"

4. Before pushing any changes to a branch (say dev) in the remote repo on gitihub, always make sure you have updated other people's work from the remote repo
$ git fetch https://github.com/lige-on-git/Golf-Player-Study.git dev
$ git rebase dev
(if conflict, solve manually in your working text editor or IDE)
$ git pull https://github.com/lige-on-git/Golf-Player-Study.git dev
(if conflict, solve manually in your working text editor or IDE - NO NEED to pull again)
$ git add *
$ git commit -m "whatever conflicts you solved"

5. Now, it's time to push your work to the target remote branch (say dev) - MAKE SURE you are on the same local brach (e.g. dev)

The first time to push:
$ git push -u origin dev

Any future attempt to push:
$ git push
