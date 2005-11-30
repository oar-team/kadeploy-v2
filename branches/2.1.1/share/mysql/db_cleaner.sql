# -----------------#
# Database cleaner #
# ---------------- #

CONNECT mysql;

DELETE FROM db WHERE user = 'deploy';
DELETE FROM user WHERE user = 'deploy';
DELETE FROM db WHERE user = 'deploy_right';
DELETE FROM user WHERE user = 'deploy_right';
DELETE FROM db WHERE user = 'reader';
DELETE FROM user WHERE user = 'reader';

DROP DATABASE IF EXISTS deploy;
DROP DATABASE IF EXISTS deploy_right;
