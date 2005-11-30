# --------------------------------------------------------
# 
# Database : `deploy_right`
# 
CREATE DATABASE IF NOT EXISTS deploy_right;

# --------------------------------------------------------
# 
# Users creation
# 

CONNECT mysql;

# Administrator
INSERT INTO user (Host,User,Password) VALUES('localhost','deploy_right',PASSWORD('deploy_right'));
INSERT INTO user (Host,User,Password) VALUES('%.imag.fr','deploy_right',PASSWORD('deploy_right'));
INSERT INTO db (Host,Db,User,Select_priv,Insert_priv,Update_priv,Delete_priv, Create_priv,Drop_priv) VALUES ('localhost','deploy_right','deploy_right','Y','Y','Y','Y','Y','Y');
INSERT INTO db (Host,Db,User,Select_priv,Insert_priv,Update_priv,Delete_priv, Create_priv,Drop_priv) VALUES ('%.imag.fr','deploy_right','deploy_right','Y','Y','Y','Y','Y','Y');
FLUSH PRIVILEGES;

GRANT ALL ON deploy_right.* TO deploy_right@localhost;
GRANT ALL ON deploy_right.* TO deploy_right@"%.imag.fr";
FLUSH PRIVILEGES;

# Reader - commented because already created during the deploy database creation
#INSERT INTO user (Host,User,Password) VALUES('localhost','reader',PASSWORD('readonly'));
#INSERT INTO db (Host,Db,User,Select_priv,Insert_priv,Update_priv,Delete_priv, Create_priv,Drop_priv) VALUES ('localhost','deploy_right','reader','Y','N','N','N','N','N');
#FLUSH PRIVILEGES;

#GRANT SELECT ON deploy_right.* TO reader@localhost;
#FLUSH PRIVILEGES;

CONNECT deploy_right;

# --------------------------------------------------------
#
# Table structure for table `deploy_rights`
#

CREATE TABLE IF NOT EXISTS deploy_rights (
login VARCHAR(30) NOT NULL,
hostname VARCHAR(30) NOT NULL,
device VARCHAR(3) NOT NULL,
pnumber INT UNSIGNED NOT NULL,
PRIMARY KEY (login,hostname,device,pnumber)
);
