alter table disk add `nodeid` int(10) unsigned NOT NULL default '0';
alter table disk add `dnumber` int(10) unsigned NOT NULL default '1';
alter table disk drop device;
alter table disk add `interface` char(10) NOT NULL default '';
alter table partition add `parttype` char(10) NOT NULL default '';
alter table partition add `diskid` int(10) unsigned NOT NULL default '0';


drop table rights;
CREATE TABLE IF NOT EXISTS rights (
   `user`   VARCHAR(255) NOT NULL default '',
   `node`   VARCHAR(255) NOT NULL default '',
   `rights` VARCHAR(255) NOT NULL default '',
   PRIMARY KEY (user,node,rights)
   );

drop table environment;
CREATE TABLE IF NOT EXISTS environment 
( 
`id` int(10) unsigned NOT NULL auto_increment,
`name` VARCHAR(255) NOT NULL default '',
`user` VARCHAR(255) NOT NULL default '',
`descriptionfile` VARCHAR(255) NOT NULL default '',
PRIMARY KEY (id)
);

CREATE TABLE IF NOT EXISTS nodestate 
( 
`nodeid` int(10) unsigned NOT NULL, 
`service` VARCHAR(255) NOT NULL default '', 
`state` VARCHAR(255) NOT NULL default '', 
PRIMARY KEY (nodeid,service) 
);

