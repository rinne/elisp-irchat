/*
** $Id: dcc.c,v 1.1 1997/02/27 11:21:04 tri Exp $
**
** see file irchat-copyright.el for change log and copyright info
*/

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <netdb.h>
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>
#include <netinet/in.h>

#ifdef _AIX
#include <sys/select.h>
#endif

#ifdef USE_BCOPY
#define memmove(x,y,z)	bcopy((y), (x), (z))
#endif

int setup_listen_port (int ip_port)
{
    int sock, tries;
    int opt = 1;
    static struct sockaddr_in server;

    sock = socket (AF_INET, SOCK_STREAM, 0);
    if (sock < 0)
    {
	perror ("SLP(): opening stream socket");
	exit (1);
    }

#ifdef SO_REUSEADDR
    if (setsockopt (sock, SOL_SOCKET, SO_REUSEADDR, 
		    (char *)&opt, sizeof (opt)) < 0)
    {
	perror ("SLP(): setsockopt SO_REUSEADDR");
    }
#endif

    /* Bind a port to listen for new connections */

    server.sin_family = AF_INET;
    server.sin_addr.s_addr = INADDR_ANY;
    server.sin_port = htons (ip_port);
    for (tries = 0; tries < 10; tries++)
    {
	if (bind (sock, (struct sockaddr *) &server, sizeof (server)))
	{
	    if (tries >= 9)
	    {
		perror ("SLP(): binding stream socket");
		exit (1);
	    }
	    perror ("SLP(): binding stream socket. retry in 20 seconds");
	    sleep (20);		/* wait 20 seconds and try again */
	}
	else
	    break;
    }
    listen (sock, 64);
    return (sock);
}


/*
 * send_file(int port, char *ifile)
 * listens to connections to port, and when connection established
 * sends ifile to that socket
 */
int send_file (int port, char *ifile)
{
    int sock, ifd, ofd, len;
    u_long bytessent = 0;
    char buf[1024 * 8];
    fd_set readfds, writefds, fdset;
    struct stat statbuf;
    char namebuf[31];
    struct hostent *hp;

    ifd = open (ifile, O_RDONLY);

    if (ifd < 0)
    {				/* error in opening file to send */
	close (ofd);
	return 1;
    }

    gethostname (namebuf, sizeof (namebuf));
    fstat (ifd, &statbuf);
    hp = gethostbyname (namebuf);

    if (hp)
    {
	printf ("DCC send %s %d %u %d\n",
		ifile, port,
		ntohl (((struct in_addr *) (hp->h_addr_list)[0])->s_addr),
		statbuf.st_size);
    }
    else
	return 2;

    sock = setup_listen_port (port);
    ofd = accept (sock, (struct sockaddr *) 0, (int *) 0);

    while ((len = read (ifd, buf, sizeof (buf))) > 0)
    {
	write (ofd, buf, len);	
	bytessent += len;
	while ((len = read (ofd, buf, sizeof (u_long))) &&
	       ntohl (*(u_long *) buf) != bytessent);
    }
    close (ofd);
    close (ifd);
    printf ("*** DCC file %s sent\n", ifile);
    return 0;
}

/*
 * receive_file(u_long host, int port, char *ifile)
 * connects to (host,port) and reads everything send from there
 * for every packet received gives back how much actually got
 * puts everything in ifile
 */
int receive_file (u_long host, int port, int size, char *ifile)
{
    int sock, ifd, ofd, len, bytesreceived = 0, toread, prev = 0;
    char buf[1024 * 8];
    fd_set readfds, writefds, fdset;
    u_long netsize;
    
    if ((ofd = open (ifile, O_WRONLY|O_CREAT|O_TRUNC, 0600)) < 0)
    {
	fprintf (stderr, "ERROR opening file: %s\n", ifile);
	return 1;
    }
    ifd = setup_connect_port (host, port);
    if ((toread = sizeof (buf)) > size)
	toread = size;
    while (bytesreceived < size && (len = read (ifd, buf, toread)) > 0)
    {
	write (ofd, buf, len);
	bytesreceived += len;
	netsize = htonl (bytesreceived);
	lseek (ifd, 0, 2);
	write (ifd, &netsize, 4);
	lseek (ifd, 0, 2);
	if (toread > size - bytesreceived)
	    toread = size - bytesreceived;
	if (bytesreceived - prev > size / 5)
	{
	    printf ("DCC %s %d%% (%d/%d bytes) received\n", ifile,
		    100 * bytesreceived / size, bytesreceived, size);
	    prev = bytesreceived;
	}
    }
    printf ("*** DCC file %s received\n", ifile);
    close (ifd);
    close (ofd);
    return 0;
}

/*
 * my_listen(int port)
 * listens port given, reads stdin and sends it to socket 
 * anything read from socket is send to stdout
 */
int my_listen (int port)
{
    int sock, sfd, ofd, len, bytesreceived = 0;
    char buf[1024 * 8];
    fd_set readfds, writefds, fdset;
    
    sock = setup_listen_port (port);
    sfd = accept (sock, (struct sockaddr *) 0, (int *) 0);
    for (;;)
    {
	FD_ZERO (&readfds);
	FD_SET (sfd, &readfds);
	FD_SET (0, &readfds);
	if (select (32, &readfds, 0, 0, 0) < 0)
	{
	    perror ("ML(): select");
	    close (sfd);
	    return 1;
	}
	if (FD_ISSET (sfd, &readfds))
	{
	    len = read (sfd, buf, sizeof (buf));
	    if (len == 0)
	    {
		close (sfd);
		return 0;
	    }
	    write (1, buf, len);
	    FD_CLR (sfd, &readfds);
	}
	if (FD_ISSET (0, &readfds))
	{
	    len = read (0, buf, sizeof (buf));
	    if (len == 0)
	    {
		close (sfd);
		return 0;
	    }
	    write (sfd, buf, len);
	    FD_CLR (ofd, &readfds);
	}
    }
}

int setup_connect_port (u_long host, int port)
{
    int sock;
    static struct hostent *hp;
    static struct sockaddr_in server;
    
    sock = socket (AF_INET, SOCK_STREAM, 0);
    if (sock < 0)
    {
	perror ("SCP(): opening stream socket");
	exit (1);
    }
    server.sin_family = AF_INET;
    
    server.sin_addr.s_addr = ntohl (host);
    server.sin_port = htons (port);
    
    if (connect (sock, (struct sockaddr *) &server, sizeof (server)) < 0)
    {
	perror ("SCP(): connecting remote socket");
	return 0;
    }
    
    return sock;
}

/* some environments do not have strtoul() */
u_long fuckingatoul (char *str)
{
    u_long result = 0;

    while (*str)
    {
	result = result * 10 + *str - '0';
	str++;
    }
    return result;
}

unsigned long primaryipaddressof (char *host)
{   
    struct hostent     *hp;
    unsigned long       addr;

    hp = gethostbyname(host);
    if (hp == NULL)
        addr = inet_addr(host);
    else
        memmove(&addr, hp->h_addr_list[0], 4);

    return ntohl(addr);
}

int main (int argc, char **argv)
{
    char *host = "localhost";
    char *action;
    int status = 0;
    
    if (argc > 1)
    {
	action = argv[1];
    } 
    else
    {
	fprintf (stderr, "Usage: %s send <port> <filename>\n", argv[0]);
	fprintf (stderr, "Usage: %s receive <host> <port> <size> <filename>\n", argv[0]);
	fprintf (stderr, "Usage: %s listen <port>\n", argv[0]);
	fprintf (stderr, "Usage: %s resolve <host> [<host>]\n", argv[0]);
	exit (1);
    }

    if (!strcmp (action, "resolve"))
    {
	if (argc < 3)
	{
	    fprintf (stderr, "Wrong number of parameters.\n");
	    fprintf (stderr, "Usage: %s resolve <host>\n", argv[0]);
	    exit (1);
	}
	else
	{
	    u_long 	i, addr;

	    for (i = 2; i < argc; i++)
	    {
		addr = primaryipaddressof(argv[i]);
		if (addr != -1)
		    printf("%u\n", addr);
		else
		    printf("0\n");
	    }
	    status = 0;
	}
    }

    if (!strcmp (action, "send"))
    {
	if (argc != 4)
	{
	    fprintf (stderr, "Wrong number of parameters.\n");
	    fprintf (stderr, "Usage: %s send <port> <filename>\n", argv[0]);
	    exit (1);
	}
	status = send_file (atoi (argv[2]), argv[3]);
    }
    else if (!strcmp (action, "receive"))
    {
	if (argc != 6)
	{
	    fprintf (stderr, "Wrong number of parameters.\n");
	    fprintf (stderr,
		     "Usage: %s receive <host> <port> <size> <filename>\n",
		     argv[0]);
	    exit (1);
	}
	status = receive_file (fuckingatoul (argv[2]),
			       atoi (argv[3]),
			       atoi (argv[4]), argv[5]);
    }
    else if (!strcmp (action, "listen"))
    {
	if (argc != 3)
	{
	    fprintf (stderr, "Not enough parameters.\n");
	    fprintf (stderr, "Usage: %s listen <port>\n", argv[0]);
	}
	status = my_listen (atoi (argv[2]));
    }
    return status;
}
/*
** eof
*/
