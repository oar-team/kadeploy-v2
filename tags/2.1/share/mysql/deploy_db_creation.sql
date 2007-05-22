# --------------------------------------------------------
# 
# Database : `deploy`
# 
CREATE DATABASE IF NOT EXISTS deploy;

# --------------------------------------------------------
# 
# Users creation
# 

CONNECT mysql;

# Administrator
INSERT INTO user (Host,User,Password) VALUES('localhost','deploy',PASSWORD('deploy'));
INSERT INTO db (Host,Db,User,Select_priv,Insert_priv,Update_priv,Delete_priv, Create_priv,Drop_priv) VALUES ('localhost','deploy','deploy','Y','Y','Y','Y','Y','Y');
FLUSH PRIVILEGES;

GRANT ALL ON deploy.* TO deploy@localhost;
FLUSH PRIVILEGES;

# Reader
INSERT INTO user (Host,User,Password) VALUES('localhost','reader',PASSWORD('readonly'));
INSERT INTO db (Host,Db,User,Select_priv,Insert_priv,Update_priv,Delete_priv, Create_priv,Drop_priv) VALUES ('localhost','deploy','reader','Y','N','N','N','N','N');
FLUSH PRIVILEGES;

GRANT SELECT ON deploy.* TO reader@localhost;
FLUSH PRIVILEGES;

CONNECT deploy;

# --------------------------------------------------------
#
# Table structure for table `deployed`
#

CREATE TABLE `deployed` (
  `envid` int(10) unsigned NOT NULL default '0',
  `diskid` int(10) unsigned NOT NULL default '0',
  `partid` int(10) unsigned NOT NULL default '0',
  `nodeid` int(10) unsigned NOT NULL default '0',
  `deployid` int(10) unsigned NOT NULL default '0',
  `state` enum('deployed','to_deploy','deploying','error') NOT NULL default 'deployed',
  `error_description` varchar(255) default NULL,
  PRIMARY KEY  (`envid`,`diskid`,`partid`,`nodeid`,`deployid`)
) TYPE=MyISAM;

# --------------------------------------------------------
#
# Table structure for table `deployment`
#

CREATE TABLE `deployment` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `state` enum('waiting','running','terminated','error') NOT NULL default 'waiting',
  `startdate` datetime NOT NULL default '0000-00-00 00:00:00',
  `enddate` datetime NOT NULL default '0000-00-00 00:00:00',
  PRIMARY KEY  (`id`)
) TYPE=MyISAM AUTO_INCREMENT=1 ;

# --------------------------------------------------------
#
# Table structure for table `disk`
#

CREATE TABLE `disk` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `size` int(10) unsigned NOT NULL default '0',
  `device` char(3) NOT NULL default '',
  PRIMARY KEY  (`id`)
) TYPE=MyISAM AUTO_INCREMENT=1 ;

# --------------------------------------------------------
#
# Table structure for table `environment`
#

CREATE TABLE `environment` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `name` varchar(255) NOT NULL default '',
  `version` int(10) unsigned NOT NULL default '0',
  `description` text,
  `author` varchar(56) NOT NULL default '',
  `filebase` varchar(255) NOT NULL default '',
  `filesite` varchar(255) NOT NULL default '',
  `size` int(10) unsigned NOT NULL default '0',
  `initrdpath` varchar(255) NOT NULL default '',
  `kernelpath` varchar(255) NOT NULL default '',
  `kernelparam` varchar(255) NOT NULL default '',
  `fdisktype` int(10) unsigned default NULL,
  `filesystem` varchar(9) default NULL,
  `siteid` int(10) unsigned NOT NULL default '0',
  `optsupport` int(10) unsigned NOT NULL default '0', # added for optimisation methods  
  PRIMARY KEY  (`id`)
) TYPE=MyISAM AUTO_INCREMENT=5 ;

# default values
INSERT INTO `environment` VALUES (1, 'undefined', 1, 'undefined environment', '', '', '', '', '', 0, '', 82, 'undefined', 0);
INSERT INTO `environment` VALUES (2, 'swap', 1, 'swap partition', '', '', '', '', '', 0, '', 82, 'swap', 0);
INSERT INTO `environment` VALUES (3, 'tmp', 1, 'tmp partition', '', '', '', '', '', 0, '', 82, 'ext2', 0);
INSERT INTO `environment` VALUES (4, 'empty', 1, NULL, '', '', '', '', '', 0, '', NULL, '', 0);

# --------------------------------------------------------
#
# Table structure for table `node`
#

CREATE TABLE `node` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `name` varchar(26) NOT NULL default '',
  `macaddr` varchar(17) NOT NULL default '',
  `ipaddr` varchar(15) NOT NULL default '',
  PRIMARY KEY  (`id`)
) TYPE=MyISAM AUTO_INCREMENT=1 ;

# --------------------------------------------------------
#
# Table structure for table `partition`
#

CREATE TABLE `partition` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `pnumber` int(10) unsigned NOT NULL default '0',
  `size` int(10) unsigned NOT NULL default '0',
  PRIMARY KEY  (`id`)
) TYPE=MyISAM AUTO_INCREMENT=1 ;

# --------------------------------------------------------
#
# Table structure for table `site`
#

CREATE TABLE `site` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `name` varchar(255) NOT NULL default '',
   #sitefilepath VARCHAR(255) NOT NULL,
   #rootpubkey VARCHAR(255) NOT NULL,
  PRIMARY KEY  (`id`)
) TYPE=MyISAM AUTO_INCREMENT=1 ;

# --------------------------------------------------------
#
# Table structure for table `rights`
#

CREATE TABLE IF NOT EXISTS rights (
   `user` VARCHAR(30) NOT NULL default '',
   `node` VARCHAR(30) NOT NULL default '',
   `part` VARCHAR(5) NOT NULL default '',
  PRIMARY KEY (user,node,part)
);