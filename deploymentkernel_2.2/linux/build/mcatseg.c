#include "TCPCatServer.h"  /*  cat server includes */

/*
*  mcastseg => arg1 
*        0 = mcat
*        1 = first node  does nothing 
*        2 = last node does nothing  
*        3 = first and last do nothing
*        4 = serveur only args is the fisrt node to forward
*/

int main(int argc, char *argv[])
{
        /*Server Part */
        int servSock;                    /* Socket descriptor for server */
        int clntSock;                    /* Socket descriptor for client */
        unsigned short catRecvPort;     /* Server port */
        unsigned short extCtrl;    

        /*Client Part */
        int sock;                        /* Socket descriptor */
        struct sockaddr_in catServAddr; /* Cat server address */
        unsigned short catSendPort;     /* Cat server port */
        char *servIP;                    /* Server IP address (dotted quad) */
        char *catString;                /* String to send to cat server */
        char catBuffer[RCVBUFSIZE];     /* Buffer for cat string */
        unsigned int catStringLen;      /* Length of string to cat */
        int bytesRcvd, totalBytesRcvd;   /* Bytes read in single recv() and total bytes read */
    
        char *catCmd;			/* Command to execute */ 
        char *catSrcCmd;			/* Command to execute by src_node*/ 
        int nbwrite;
	FILE *ptr;				/* Pipe Descriptor */ 
        int forwarder = 0;      /* Node will forward */
        int cont_recv = 1;      /* Continue to receive */
        int not_src_node = 1;	/* Node is not the source */
	
        int flagConnect = 0;
        int retrytime = 0;
        int sleeptime = RETRYCONNECTTIME;
	
        struct hostent * host;
        char hostname[256];
        char hostipaddr[16];
        unsigned char *readable_adress;
	
	
	
        if (argc < 5 )     /* Test for correct number of arguments */
        {
                fprintf(stderr,"Usage:  %s <EXT CTRL> <LOCAT PORT> <CMD_SRC_NODE> <CMD_REC_NODE>   <SRC_NODE> <NODE1> ... <NODEn>\n", argv[0]);
                exit(1);
        }

        extCtrl = atoi(argv[1]);  /* Second arg:  local port */
        catRecvPort = atoi(argv[2]);  /* Second arg:  local port */
//        catSrcCmd = argv[2]; /* First arg:  command to execute by src_node*/
//	printf("catSrcCmd %s\n",argv[2]);
        catCmd = argv[4]; /* Second arg:  command to execute */
//	printf("catCmd %s\n",catCmd);
       
        catSendPort = catRecvPort;
	
	
        /* find next node to forward */
        gethostname(hostname, 256);
        printf("hostname:%s\n", hostname);		
        host = gethostbyname(hostname);
        readable_adress = host->h_addr_list[0];
	
   	//printf("adresse: %d.%d.%d.%d\n", *readable_adress, readable_adress[1], readable_adress[2], readable_adress[3]);
	sprintf(hostipaddr,"%d.%d.%d.%d", *readable_adress, readable_adress[1], readable_adress[2], readable_adress[3]);
	
//	printf("adresse: %s\n",hostipaddr);
	
	int i=5;
	if (extCtrl!=4) {
        while ( (strcmp (hostipaddr, argv[i]) != 0) && (i<argc-1)) 
        {
            i++;
        }
    }    

    if (i==5) {
        if ((extCtrl==1) || (extCtrl==3)) {
            not_src_node=1;
        } else {
            not_src_node=0;
            catCmd = argv[3];
        }
    }
    
	if ( (strcmp (hostipaddr, argv[i]) != 0)  && (not_src_node) )
	{
		printf("Error : hostipaddr = %s , argv[%d] = %s\n", hostipaddr, i, argv[i]);
		exit(1);
	}
	
	if (i < (argc-1))  {
		forwarder = 1;
		servIP = argv[i+1];
		printf("Forward to %s\n",servIP); 
	} else { 
		if (not_src_node) {
			printf("Last node\n");
                        if ((extCtrl==2) || (extCtrl==3)) {exit(1);}
		} else {
			servIP = argv[i];
            forwarder = 1;
			printf("Src node forward to %s\n",servIP);
		}
	}
	
	//exit(0);
	
    if (not_src_node) {
		servSock = CreateTCPServerSocket(catRecvPort);
		/* Command to execute in local */
		if ((ptr = popen(catCmd, "w")) == NULL) DieWithError("popen() failed");
    } else {
		/* Command to execute in local */
		if ((ptr = popen(catCmd, "r")) == NULL) DieWithError("popen() failed");
	}
    
		
    if (forwarder) { 
	    
	    /* Create a reliable, stream socket using TCP */
	    if ((sock = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP)) < 0)
		    DieWithError("socket() failed");
	    
	    /* Construct the server address structure */
	    memset(&catServAddr, 0, sizeof(catServAddr));     /* Zero out structure */
	    catServAddr.sin_family      = AF_INET;             /* Internet address family */
	    catServAddr.sin_addr.s_addr = inet_addr(servIP);   /* Server IP address */
	    catServAddr.sin_port        = htons(catSendPort); /* Server Send port */
		
		/* Establish the connection to next mcat node  */
		while ((flagConnect == 0) && (retrytime < RETRYCONNECTTIMELIMIT)) {
			/* TODO : MUST be enhanced (different action in function of type error) */
			/*  Network is unreachable */
			/*  Connection refused */
			if (connect(sock, (struct sockaddr *) &catServAddr, sizeof(catServAddr)) < 0)
			{  
//				perror("connect() failed");   /* Connect */
				usleep((unsigned long)1000*sleeptime);
				 printf(".");
//				printf("sleeptime = %d  retrytime = %d\n" ,sleeptime,retrytime);
				sleeptime = sleeptime * RETRYBACKOFF; 
				retrytime = retrytime + sleeptime;
				
			} else {
				flagConnect = 1;	 
			}
		}
			if (flagConnect == 0) DieWithError("connect() failed");	
    }
	printf("Connected\n");
	
	
	if (not_src_node) {
	
		/* AcceptConnection */ 
		clntSock = AcceptTCPConnection(servSock);
    
		/* Receive from previous node */
		totalBytesRcvd = 0;
		printf("Receiving: \n");  
     
		while (cont_recv)
		{
			/* Receive up to the buffer size (minus 1 to leave space for
			a null terminator) bytes from the sender */
			if ((bytesRcvd = recv(clntSock, catBuffer, RCVBUFSIZE - 1, 0)) <= 0)
			{    
				perror("recv() failed or connection closed prematurely");
				cont_recv = 0;
			} else
			{
				totalBytesRcvd += bytesRcvd;   /* Keep tally of total bytes */
    
				if (forwarder) 
				{
				 /* Forward */
				 if (send(sock, catBuffer, bytesRcvd, 0) != bytesRcvd)
					 DieWithError("send() failed");
			 }	
    
			 /* Write to pipe command */
			 nbwrite = fwrite(catBuffer, 1, bytesRcvd, ptr);
			 if (nbwrite != bytesRcvd) printf("fwrite failed ? %d\n",nbwrite);
//			 printf ("bytesRcvd %d\n", bytesRcvd);
			}
		}

		if ( fflush(ptr)<0 ) DieWithError("fflush failed");
		pclose(ptr);
		/* End */
		printf ("totalBytesRcvd %d\n", totalBytesRcvd);
        if (not_src_node && (!forwarder)) printf("<LAST_NODE_ENDED>\n");
	} else {
		 printf ("Scr node : Sending\n");
		// TO FINISH
		while (cont_recv) 
		{
			bytesRcvd = fread(catBuffer, 1, RCVBUFSIZE, ptr);
			
//			printf("read %d feof %d\n",bytesRcvd,feof(ptr));
			
			if ( bytesRcvd < 0)
			{
			} else if (send(sock, catBuffer, bytesRcvd, 0) != bytesRcvd)
			{	
				DieWithError("send() failed");
			}
			if (feof(ptr)) cont_recv = 0;
		}
		 printf ("Scr node : Terminating\n");
		close(sock);
	}
	
    exit(0);
}
