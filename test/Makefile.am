#TESTS_ENVIRONMENT = /bin/bash -x
TEST_EXTENSIONS = .sh
SH_LOG_COMPILER = /bin/bash -x
EXTRA_DIST = minibuffer-test-dir dummy.sh

# These tests end up doing double the work because they create another
# distdir. Also, should a dist necessarily be able to create another dist?

# These tests should be kept separate as release tests since they're
# so expensive.

#TESTS= works-without-noweb.sh print-install.sh 
TESTS= dummy.sh

MAINTAINERCLEANFILES = Makefile.in works-without-noweb.sh print-install.sh 
