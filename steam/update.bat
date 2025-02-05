#654

Please don't open and close PRs like this ( #653 , #654 , and now this PR ). You are making a number of notifications to everyone watching this repo, and it is a non-standard usage of PRs. Pull requests are not supposed to be "one shot", where you either make a perfect PR on the first try, that passes all the tests, or resign and close the PR. Don't close each PR just because GitHub Actions check fails, only to open another PR again.

Instead, basically, open a PR and work on it, until you think it's good.

    Create a fork of CGE (and maybe a branch) and test it as much as you can,
    then open a PR,
    mark it as a "draft" if you want to indicate that it's not ready for review/merge yet,
    and then work on it. This means pushing more commits to the same branch in the same fork that you used to make the PR. The PR will automatically show new commits, and they will be checked by GitHub Actions.
    Remove the "draft" mark, and feel free to make a comment and/or ping me, to review it.

New commits/comments to PR will also make a notification to people watching, but this is standard (and everyone should be able to handle it, but unsubscribing from PR if they are not interested), people expect this. And then people don't need to read the same PRs title and description many times. They read the PR title and description once, and know that further down -> the work on this task continues.

Looking at PR contents:

    In src/base/castledynlib.pas , it seems your changes didn't merge properly with latest changes in CGE master. Make sure your diff (visible in "Files Changed") contain only things you wanted to change, revert any other changes.

Writing AchievementIdAnsi := AnsiString(AchievementId) -- we don't want an explicit typecast in this case, variable names already make it obvious. Leave it as AchievementIdAnsi := AchievementId, as it was.

While USE_TESTING_API makes perfect sense for testing, we will want to remove and simplify it all (to just use new API) before merging.


copy castleinternalsteamapi.pas ..\..\castle-engine\src\services\steam\castleinternalsteamapi.pas
copy castledynlib.pas ..\..\castle-engine\src\base\castledynlib.pas
copy castleutils.pas ..\..\castle-engine\src\base\castleutils.pas
