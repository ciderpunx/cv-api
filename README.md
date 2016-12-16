This is the pre-alpha version of a Curriculum Vit&aelig; API that I am developing to replace my old [xmlcv.pl CV generation script](http://charlieharvey.org.uk/cv) which was impressive when I first put it on [PerlMonks in 2005](http://www.perlmonks.org/?node_id=516333), but not so much in 2016 when I am writing.

It will use: 
* [persistent](http://www.yesodweb.com/book/persistent) and [sqlite](https://sqlite.org) for data storage
* [servant](https://haskell-servant.readthedocs.io/en/stable/) to do the heavy lifting on the API side
* [json-resume](https://jsonresume.org/schema/) as the data format

On the data format side I also looked at [FRESCA](https://github.com/fresh-standard/FRESCA) which is an alternate standard for r&eacute;sum&eacute;s/curriculum vit&aelig;. On the face of it json-resume was marginally simpler, but good tools like [HackMyResume](https://github.com/hacksalot/HackMyResume) exist to convert between formats.

Once the API side of things is written, I will need to think about a UI for it. For now it will just be a case of writing JSON by hand and using the Firefox [HTTPRequester Plugin](https://addons.mozilla.org/En-us/firefox/addon/httprequester/)&hellip;
