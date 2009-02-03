# --------------------------------------------------------
#
# Table structure for table `deployed`
#


# --------------------------------------------------------
#
# Table structure for table `rights`
#

CREATE TABLE IF NOT EXISTS rights (
   `user` VARCHAR(30) NOT NULL default '',
   `node` VARCHAR(30) NOT NULL default '',
   `vlan` VARCHAR(5) NOT NULL default '',
  PRIMARY KEY (user,node,vlan)
);
