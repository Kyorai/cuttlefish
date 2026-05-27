# `cuttlefish` release process

Here are the commands to run when releasing `cuttlefish` `3.8.0`:

```
git checkout -b cuttlefish-3.8.0
sed -i.bak 's/3\.7\.0/3.8.0/' src/cuttlefish.app.src
github_changelog_generator --future-release v3.8.0 --user Kyorai --project cuttlefish --token "$GITHUB_API_TOKEN"
git commit -a -m 'v3.8.0'
git push -u origin cuttlefish-3.8.0
```

* Open pull request for the `cuttlefish-3.8.0` branch
* Ensure CI runs successfully for the PR
* Merge PR

```
git checkout main
git pull origin main
git remote prune origin
git branch -d cuttlefish-3.8.0
git tag --annotate --sign --local-user=GPGKEYID --message='cuttlefish 3.8.0' 'v3.8.0'
git push --tags
gh release create v3.8.0 --verify-tag --title 'v3.8.0' --latest --generate-notes
```

**NOTE:** ensure that the correct version is displayed when `rebar3 hex publish` is run:

```
rebar3 hex publish
```
