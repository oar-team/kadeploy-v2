-- 
-- Structure de la table `environments`
-- 

DROP TABLE IF EXISTS `environments`;
CREATE TABLE IF NOT EXISTS `environments` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `name` varchar(255) NOT NULL default '',
  `version` int(10) unsigned NOT NULL default '0',
  `description` text,
  `author` varchar(56) NOT NULL default '',
  `tarball_file` varchar(255) NOT NULL,
  `tarball_kind` varchar(10) NOT NULL,
  `tarball_md5` varchar(40) NOT NULL,
  `postinstall_file` varchar(255) NOT NULL,
  `postinstall_kind` varchar(10) NOT NULL,
  `postinstall_md5` varchar(40) NOT NULL,
  `initrd` varchar(255) NOT NULL,
  `kernel` varchar(255) NOT NULL,
  `kernel_params` varchar(255) NOT NULL,
  `fdisk_type` int(10) unsigned default NULL,
  `filesystem` varchar(9) default NULL,
  `user` varchar(255) default 'nobody',
  `part` varchar(20) NOT NULL,
  `allowed_users` varchar(512) NOT NULL,
  `environment_kind` varchar(10) NOT NULL,
  PRIMARY KEY  (`id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

-- 
-- Structure de la table `log`
-- 

DROP TABLE IF EXISTS `log`;
CREATE TABLE IF NOT EXISTS `log` (
  `hostname` varchar(50) NOT NULL,
  `step1` varchar(35) NOT NULL,
  `step2` varchar(35) NOT NULL,
  `step3` varchar(35) NOT NULL,
  `timeout_step1` smallint(5) unsigned NOT NULL,
  `timeout_step2` smallint(5) unsigned NOT NULL,
  `timeout_step3` smallint(5) unsigned NOT NULL,
  `retry_step1` tinyint(1) unsigned NOT NULL,
  `retry_step2` tinyint(1) unsigned NOT NULL,
  `retry_step3` tinyint(1) unsigned NOT NULL,
  `start` int(10) unsigned NOT NULL,
  `step1_duration` int(10) unsigned NOT NULL,
  `step2_duration` int(10) unsigned NOT NULL,
  `step3_duration` int(10) unsigned NOT NULL,
  `env` varchar(64) NOT NULL,
  `md5` varchar(35) NOT NULL,
  `success` varchar(6) NOT NULL,
  `error` varchar(255) NOT NULL,
  `user` varchar(16) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

-- --------------------------------------------------------

-- 
-- Structure de la table `nodes`
-- 

DROP TABLE IF EXISTS `nodes`;
CREATE TABLE IF NOT EXISTS `nodes` (
  `hostname` varchar(64) NOT NULL,
  `state` varchar(16) NOT NULL,
  `env_id` int(10) unsigned NOT NULL,
  `date` int(10) unsigned NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=utf8;


-- --------------------------------------------------------

-- 
-- Structure de la table `rights`
-- 

DROP TABLE IF EXISTS `rights`;
CREATE TABLE IF NOT EXISTS `rights` (
  `user` varchar(30) NOT NULL,
  `node` varchar(30) NOT NULL,
  `part` varchar(10) NOT NULL
) ENGINE=MyISAM DEFAULT CHARSET=utf8;

