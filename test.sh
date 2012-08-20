lein test italianverbs.test.fs
if [ $? != 0 ]; then exit; fi

lein test italianverbs.test.morphology
if [ $? != 0 ]; then exit; fi

lein test italianverbs.test.lexiconfn
if [ $? != 0 ]; then exit; fi

lein test italianverbs.test.search
if [ $? != 0 ]; then exit; fi

lein test italianverbs.test.lexicon
if [ $? != 0 ]; then exit; fi