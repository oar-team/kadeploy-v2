##########################################################
# 
# For testing purpose : duplicate deployment rights given 
# by a oarsub command to the Kadeploy test DB. 
#
#########################################################

INSERT INTO SUBSTmydeploydbtestSUBST.rights (user,node,part) select * from SUBSTmydeploydbSUBST.rights;


