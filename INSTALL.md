## Install Chisel

If you need this to work inside Intel, you'll need to do the following steps once:

- Find a work disk. Set the environment variable `WORK` to its full path. Then issue:

```
mkdir $WORK/.sbt
mkdir $WORK/.ivy2
mkdir $WORK/.cache
```

- Make sure `~/.sbt`, `~/.ivy2`, and `~/.cache` are empty. Then issue:
```
rm -rf ~/.sbt
rm -rf ~/.ivy2
```
(If `~/.cache` is not empty, either remove it similarly or move the contents to `$WORK/.cache`)

- Use links in the home directory to the work disk so you don’t fill up your home directory.
```
cd ~
ln -s $WORK/.sbt .
ln -s $WORK/.ivy2 .
ln -s $WORK/.cache .
```

- Set proxies
```
setenv HTTPS_PROXY http://proxy-us.intel.com:912
setenv HTTP_PROXY http://proxy-us.intel.com:912
setenv http_proxy http://proxy-us.intel.com:912
setenv https_proxy http://proxy-us.intel.com:912
```
