-- 
-- Structure de la table `environment`
-- 

DROP TABLE IF EXISTS `environment`;
CREATE TABLE IF NOT EXISTS `environment` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `name` varchar(255) NOT NULL default '',
  `version` int(10) unsigned NOT NULL default '0',
  `description` text,
  `author` varchar(56) NOT NULL default '',
  `tarball_file` varchar(255) NOT NULL,
  `tarball_md5` varchar(40) NOT NULL,
  `postinstall_file` varchar(255) NOT NULL,
  `postinstall_md5` varchar(40) NOT NULL,
  `initrd` varchar(255) NOT NULL,
  `kernel` varchar(255) NOT NULL,
  `kernel_params` varchar(255) NOT NULL,
  `fdisk_type` int(10) unsigned default NULL,
  `filesystem` varchar(9) default NULL,
  `user` varchar(255) default 'nobody',
  `part` varchar(20) NOT NULL,
  PRIMARY KEY  (`id`)
) ENGINE=MyISAM  DEFAULT CHARSET=utf8;

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
