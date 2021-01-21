*The Big Picture*

The over-arching goal of the End-to-End-Proveneance project is to
facilitate the collection and use of data provenance in
science. Supporting this larger goal there are many smaller goals and
inter-related projects with various life-spans (months, years decades)
and will be developed by many people. Communication, documentation and
versioning help to create well crafted, long-lasting software.

*Tools*

- Git, a vesion control system originally developed to help write
  Linux, tracks the differences in software files as they are written.
- Github provides a centralized server and web interface toolbox to
  help coders collaborate.


*Organizing*

- Each new project will be assigned a team, which allows for multiple
  repositories to be created associated with a given project.
- A lead will be assigned to each team who will be tasked with laying
  out Milestones, populating and assigning issues and checking in on
  progress.
- Each repository should have a branch structure as follows:
 - **Master**: stable version that can be accessed publicly
 - **Beta**: functional branch that serves as a place for merging
   development branches prior to merger with **Master**
 - *development branches*: can be named for a person or a task
- This structure allows each team member to work on projects on their
  own machines or servers and then share them in an organized way that
  will allow others to quickly access the independent contributions of
  the whole team and helping people from stepping on each other's
  toes!
- Check and make sure that git is installed on your computer. Github's
  web interface is great and is recommended as the main way to
  interact with Github. There is a desktop interface as well.

*Basic Workflow*

0. Start a new or clone an existing repository.
1. Take a look at the issues and milestone (aka. project goals) and
   either get assigned to an issue or create a new one. 
2. Create and/or switch to the right branch you want to work on.
3. Do your work. This is completely independent of git and you will
   need to save changes you've made to your files in your repository. 
4. Once you've made a set of changes, you'll want to tell git that
   you've made them. 
5. Then, once you've made enough changes that you want to share it,
   you want to "push" them up to the github server
6. Once you've finished working on your issue, close it and celebrate!
7. After you've made all changes you needed to make on your branch,
   review it with the team and then merge it. The flow should go up
   toward higher levels of stability ("your branch" -> "beta" ->
   "master"). 

Check out the following guide to learn how to do all of this in more detail: https://guides.github.com

For quick reference, check out this cheat sheet: http://byte.kde.org/~zrusin/git/git-cheat-sheet-large.png
