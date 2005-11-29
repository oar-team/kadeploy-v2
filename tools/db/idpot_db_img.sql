# phpMyAdmin SQL Dump
# version 2.5.6
# http://www.phpmyadmin.net
#
# Host: localhost
# Generation Time: May 11, 2004 at 01:31 PM
# Server version: 4.0.18
# PHP Version: 4.3.4
# 
# Database : `deploy`
# 

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

#
# Dumping data for table `deployed`
#

INSERT INTO `deployed` VALUES (1, 1, 5, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 1, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 2, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 3, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 4, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 5, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 6, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 7, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 8, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 9, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 10, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 11, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 12, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 13, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 14, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 15, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 16, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 17, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 18, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 19, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 20, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 21, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 22, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 23, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 24, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 25, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 26, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 27, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 28, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 29, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 30, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 31, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 32, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 33, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 34, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 35, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 36, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 37, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 38, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 39, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 40, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 41, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 42, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 43, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 44, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 45, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (1, 1, 5, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (2, 1, 1, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (3, 1, 2, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (5, 1, 3, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 6, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 7, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 8, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 9, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 10, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 11, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 12, 46, 0, 'deployed', NULL);
INSERT INTO `deployed` VALUES (4, 1, 13, 46, 0, 'deployed', NULL);

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

#
# Dumping data for table `deployment`
#


# --------------------------------------------------------

#
# Table structure for table `disk`
#

CREATE TABLE `disk` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `size` int(10) unsigned NOT NULL default '0',
  `device` char(3) NOT NULL default '',
  PRIMARY KEY  (`id`)
) TYPE=MyISAM AUTO_INCREMENT=2 ;

#
# Dumping data for table `disk`
#

INSERT INTO `disk` VALUES (1, 80000, 'hda');

# --------------------------------------------------------

#
# Table structure for table `environment`
#

CREATE TABLE `environment` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `name` varchar(255) NOT NULL default '',
  `version` int(10) unsigned NOT NULL default '1',
  `description` text,
  `author` varchar(56) NOT NULL default '',
  `filebase` varchar(255) NOT NULL default '',
  `filesite` varchar(255) NOT NULL default '',
  `size` int(10) unsigned NOT NULL default '0',
  `kernelpath` varchar(255) NOT NULL default '',
  `fdisktype` int(10) unsigned default NULL,
  `filesystem` varchar(9) default NULL,
  `siteid` int(10) unsigned NOT NULL default '0',
  PRIMARY KEY  (`id`)
) TYPE=MyISAM AUTO_INCREMENT=6 ;

#
# Dumping data for table `environment`
#

INSERT INTO `environment` VALUES (1, 'debian', 1, 'debian pour idpot prete a etre deployee sur la partition hda5, avec ganglia, rshp,oar', 'julien.leduc@imag.fr', 'file://home/nis/jleduc/ImagesDistrib/2004-04-20-image-Debian.tgz', '', 650, '/boot/vmlinuz', 83, 'ext2', 1);
INSERT INTO `environment` VALUES (2, 'swap', 1, 'partition de swap', 'julien.leduc@imag.fr', '', '', 0, '', 82, 'swap', 1);
INSERT INTO `environment` VALUES (3, 'tmp', 1, 'partition tmp', 'julien.leduc@imag.fr', '', '', 0, '', 83, 'ext2', 1);
INSERT INTO `environment` VALUES (4, 'empty', 1, NULL, 'julien.leduc@imag.fr', '', '', 0, '', NULL, '', 0);
INSERT INTO `environment` VALUES (5, 'local', 1, NULL, 'julien.leduc@imag.fr', '', '', 0, '', NULL, '', 1);

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
) TYPE=MyISAM AUTO_INCREMENT=47 ;

#
# Dumping data for table `node`
#

INSERT INTO `node` VALUES (1, 'idpot1', '00:10:18:01:e5:3d', '192.168.10.1');
INSERT INTO `node` VALUES (2, 'idpot2', '00:10:18:01:e5:92', '192.168.10.2');
INSERT INTO `node` VALUES (3, 'idpot3', '00:10:18:01:e5:85', '192.168.10.3');
INSERT INTO `node` VALUES (4, 'idpot4', '00:10:18:01:e5:90', '192.168.10.4');
INSERT INTO `node` VALUES (5, 'idpot5', '00:c0:9f:24:c3:72', '192.168.10.5');
INSERT INTO `node` VALUES (6, 'idpot6', '00:10:18:01:e5:84', '192.168.10.6');
INSERT INTO `node` VALUES (7, 'idpot7', '00:10:18:01:e5:8a', '192.168.10.7');
INSERT INTO `node` VALUES (8, 'idpot8', '00:10:18:01:b3:d3', '192.168.10.8');
INSERT INTO `node` VALUES (9, 'idpot9', '00:10:18:01:e5:42', '192.168.10.9');
INSERT INTO `node` VALUES (10, 'idpot10', '00:10:18:01:e5:8c', '192.168.10.10');
INSERT INTO `node` VALUES (11, 'idpot11', '00:10:18:01:e5:1d', '192.168.10.11');
INSERT INTO `node` VALUES (12, 'idpot12', '00:10:18:01:e5:37', '192.168.10.12');
INSERT INTO `node` VALUES (13, 'idpot13', '00:10:18:01:e5:2f', '192.168.10.13');
INSERT INTO `node` VALUES (14, 'idpot14', '00:10:18:01:96:5d', '192.168.10.14');
INSERT INTO `node` VALUES (15, 'idpot15', '00:10:18:01:93:de', '192.168.10.15');
INSERT INTO `node` VALUES (16, 'idpot16', '00:10:18:01:e5:24', '192.168.10.16');
INSERT INTO `node` VALUES (17, 'idpot17', '00:10:18:01:e5:3b', '192.168.10.17');
INSERT INTO `node` VALUES (18, 'idpot18', '00:10:18:01:e5:89', '192.168.10.18');
INSERT INTO `node` VALUES (19, 'idpot19', '00:10:18:01:e5:91', '192.168.10.19');
INSERT INTO `node` VALUES (20, 'idpot20', '00:10:18:01:e5:46', '192.168.10.20');
INSERT INTO `node` VALUES (21, 'idpot21', '00:10:18:01:b3:c8', '192.168.10.21');
INSERT INTO `node` VALUES (22, 'idpot22', '00:c0:9f:24:c4:94', '192.168.10.22');
INSERT INTO `node` VALUES (23, 'idpot23', '00:c0:9f:24:c6:71', '192.168.10.23');
INSERT INTO `node` VALUES (24, 'idpot24', '00:c0:9f:24:c4:d0', '192.168.10.24');
INSERT INTO `node` VALUES (25, 'idpot25', '00:c0:9f:24:c5:50', '192.168.10.25');
INSERT INTO `node` VALUES (26, 'idpot26', '00:10:18:01:e5:3c', '192.168.10.26');
INSERT INTO `node` VALUES (27, 'idpot27', '00:c0:9f:24:c6:0c', '192.168.10.27');
INSERT INTO `node` VALUES (28, 'idpot28', '00:10:18:01:e5:45', '192.168.10.28');
INSERT INTO `node` VALUES (29, 'idpot29', '00:c0:9f:24:c6:8a', '192.168.10.29');
INSERT INTO `node` VALUES (30, 'idpot30', '00:10:18:01:e5:8d', '192.168.10.30');
INSERT INTO `node` VALUES (31, 'idpot31', '00:10:18:01:e5:25', '192.168.10.31');
INSERT INTO `node` VALUES (32, 'idpot32', '00:c0:9f:24:b8:be', '192.168.10.32');
INSERT INTO `node` VALUES (33, 'idpot33', '00:c0:9f:24:43:96', '192.168.10.33');
INSERT INTO `node` VALUES (34, 'idpot34', '00:10:18:01:e5:3a', '192.168.10.34');
INSERT INTO `node` VALUES (35, 'idpot35', '00:10:18:01:e5:57', '192.168.10.35');
INSERT INTO `node` VALUES (36, 'idpot36', '00:10:18:01:e5:3f', '192.168.10.36');
INSERT INTO `node` VALUES (37, 'idpot37', '00:c0:9f:24:44:2e', '192.168.10.37');
INSERT INTO `node` VALUES (38, 'idpot38', '00:c0:9f:24:43:a1', '192.168.10.38');
INSERT INTO `node` VALUES (39, 'idpot39', '00:10:18:01:e5:86', '192.168.10.39');
INSERT INTO `node` VALUES (40, 'idpot40', '00:10:18:01:e5:94', '192.168.10.40');
INSERT INTO `node` VALUES (41, 'idpot41', '00:c0:9f:24:43:78', '192.168.10.41');
INSERT INTO `node` VALUES (42, 'idpot42', '00:10:18:01:97:15', '192.168.10.42');
INSERT INTO `node` VALUES (43, 'idpot43', '00:10:18:01:b3:ca', '192.168.10.43');
INSERT INTO `node` VALUES (44, 'idpot44', '00:c0:9f:24:b8:e0', '192.168.10.44');
INSERT INTO `node` VALUES (45, 'idpot45', '00:c0:9f:24:c3:cd', '192.168.10.45');
INSERT INTO `node` VALUES (46, 'idpot46', '00:02:b3:49:9c:8e', '192.168.10.46');

# --------------------------------------------------------

#
# Table structure for table `partition`
#

CREATE TABLE `partition` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `pnumber` int(10) unsigned NOT NULL default '0',
  `size` int(10) unsigned NOT NULL default '0',
  PRIMARY KEY  (`id`)
) TYPE=MyISAM AUTO_INCREMENT=15 ;

#
# Dumping data for table `partition`
#

INSERT INTO `partition` VALUES (1, 1, 2500);
INSERT INTO `partition` VALUES (2, 2, 2000);
INSERT INTO `partition` VALUES (3, 3, 20000);
INSERT INTO `partition` VALUES (4, 4, 0);
INSERT INTO `partition` VALUES (5, 5, 5000);
INSERT INTO `partition` VALUES (6, 6, 5000);
INSERT INTO `partition` VALUES (7, 7, 5000);
INSERT INTO `partition` VALUES (8, 8, 5000);
INSERT INTO `partition` VALUES (9, 9, 5000);
INSERT INTO `partition` VALUES (10, 10, 5000);
INSERT INTO `partition` VALUES (11, 11, 5000);
INSERT INTO `partition` VALUES (12, 12, 5000);
INSERT INTO `partition` VALUES (13, 13, 5000);
INSERT INTO `partition` VALUES (14, 14, 0);

# --------------------------------------------------------

#
# Table structure for table `site`
#

CREATE TABLE `site` (
  `id` int(10) unsigned NOT NULL auto_increment,
  `name` varchar(255) NOT NULL default '',
  PRIMARY KEY  (`id`)
) TYPE=MyISAM AUTO_INCREMENT=2 ;

#
# Dumping data for table `site`
#

INSERT INTO `site` VALUES (1, 'idpot');
