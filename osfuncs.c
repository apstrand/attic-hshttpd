

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>

typedef struct sockaddr SA;

#define EDNSERR 200

char last_error[500] = "not initalized";

int error_code;

int get_errcode() { return error_code; }
const char *get_errmsg() { printf("Get error: %s\n", last_error); return last_error; }

void puts2(char *p)
{
    puts(p);
}

void err(char *e, char *e2)
{
    error_code = errno;
    if (e2)
	snprintf(last_error, sizeof(last_error), "%d: %s:%s: %s", errno, e, e2, strerror(errno));
    else
	snprintf(last_error, sizeof(last_error), "%d: %s: %s", errno, e, strerror(errno));
}

void errstr(const char *e, int err)
{
    error_code = err;
    snprintf(last_error, sizeof(last_error), "%d: %s", err, e);
}

void sigpipe()
{
    fprintf(stderr, "SIGPIPE caught\n");
    errstr("SIGPIPE caught", EPIPE);
}

void install_handler()
{
    signal(SIGPIPE, sigpipe);
}

int create_socket()
{
    int s;
    if ((s = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
	err("socket", 0);
	return -1;
    }
    return s;
}

void init_addr(struct sockaddr_in *addr)
{
    memset(addr, 0, sizeof(struct sockaddr_in));
    addr->sin_family = AF_INET;
}

int connect_to(char *hn, int port)
{
    struct hostent *hent;
    int i1,i2,i3,i4;
    struct sockaddr_in remaddr;
    int s = create_socket();
    
    init_addr(&remaddr);
    if (sscanf(hn, "%d.%d.%d.%d", &i1, &i2, &i3, &i4) == 4) {
	    remaddr.sin_addr.s_addr = htonl((i1 << 24) + (i2 << 16) + (i3 << 8) + i4);
    } else {
	if (!(hent = gethostbyname(hn))) {
	    errstr(hstrerror(h_errno), EDNSERR);
	    return -1;
	}
     	remaddr.sin_addr = *(struct in_addr*)hent->h_addr_list[0];
    }
    remaddr.sin_port = htons(port);
    if (connect(s, (struct sockaddr*)&remaddr, sizeof(remaddr)) == -1) {
	err("connect", hn);
	return -1;
    }
    return s;
}

int listen_on(int port)
{
    int s;
    int v;
    struct sockaddr_in addr;
    init_addr(&addr);
    addr.sin_port = htons(port);
    install_handler();
    s = create_socket();
    v = 1;
    if (setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &v, sizeof(v)) == -1) {
	err("setsockopt", 0);
	return -1;
    }
    if (bind(s, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
	err("bind", 0);
	return -1;
    }
    if (listen(s, 20) == -1) {
	err("listen", 0);
	return -1;
    }
    return s;
}

struct retinfo {int sock; int addr; int port; };

struct retinfo *accept_conn(int s)
{
    struct sockaddr_in remote;
    int ns;
    unsigned int len;
    static struct retinfo r;
    
    len = sizeof(remote);
    if ((ns = accept(s, (SA*)&remote, &len)) == -1) {
	err("accept", 0);
	return 0;
    }
    r.sock = ns;
    r.addr = remote.sin_addr.s_addr;
    r.port = remote.sin_port;
    return &r;
}


char *addr2host(int a)
{
    static char *res = "";
    struct hostent *hent;
    char *p;
    struct in_addr addr;
    addr.s_addr = a;
    p = inet_ntoa(addr);
    if (!(hent = gethostbyaddr((char*)&a, 4, AF_INET))) {
	err("gethostbyaddr", p);
	return res;
    }
    return hent->h_name;
}

int read_char(int s)
{
    char c;
    if (read(s, &c, 1) != 1) {
	err("read_char", 0);
	return -1;
    }
    return c;
}

int write_char(int s, char c)
{
    if (!write(s, &c, 1)) {
	err("write_char", 0);
	return -1;
    }
    return 0;
}

int write_n(int s, int len, char *buf)
{
    if (write(s, buf, len) != len) {
	err("write_n", 0);
	return -1;
    }
    return 0;
}


int transfer_n(int source, int dest, int len)
{
    char buf[BUFSIZ];
    int csize, n;
    csize = len && len < BUFSIZ ? len : BUFSIZ;
    do {
	n = read(source, buf, csize);
	if (!n) {
	    return 0;
	} else if (n < 0) {
	    err("transfer_n:read", 0);
	    return -1;
	}
	if (write(dest, buf, n) != n) {
	    err("transfer_n:write", 0);
	    return -1;
	}
	len -= n;
    } while (len != 0);
    return 0;
}

int open_file(char *name, int mode)
{
    int s;
    int m;
    switch (mode) {
	case 0:	m = O_RDONLY; break;
	case 1: m = O_WRONLY | O_CREAT | O_TRUNC; break;
	case 2:	m = O_WRONLY | O_CREAT | O_APPEND; break;
	default: fprintf(stderr, "invalid mode to open_file(%s, %d)\n", name, mode);
		 err("open_file", name);
		 return -1;
    }
    s = open(name, m, S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
    return s;	
}

int file_size(char *name)
{
    struct stat st;
    if (stat(name, &st) == -1) {
	err("file_size:stat", name);
	return -1;
    }
    return st.st_size;
}

int file_type(char *name)
{
    struct stat st;
    if (stat(name, &st) == -1)
	return 3;
    if (S_ISREG(st.st_mode))
	return 0;
    if (S_ISDIR(st.st_mode))
	return 1;
    return 2;
}

char *strftime_(char *fmt, time_t t)
{
    static char buf[500];
    int n;
    time_t tt = t;
    n = strftime(buf, 500, fmt, localtime(&tt));
    buf[n] = 0;
    return buf;
}



struct select_data {
    fd_set readset;
    fd_set writeset;
    fd_set exceptset;
    struct timeval timeout;
    int max;
};

struct select_data* select_create()
{
    struct select_data* sd = (struct select_data*)malloc(sizeof(struct select_data));
    FD_ZERO(&sd->readset);
    FD_ZERO(&sd->writeset);
    FD_ZERO(&sd->exceptset);
    sd->timeout.tv_sec = 0;
    sd->timeout.tv_usec = 0;
    sd->max = 0;
    return sd;
}

void select_destroy(struct select_data* sd)
{
    free(sd);
}

int select_add(struct select_data* sd, int what, int fd)
{
    int err = 0;
    switch (what) {
	case 0: FD_SET(fd, &sd->readset); break;
	case 1: FD_SET(fd, &sd->writeset); break;
	case 2: FD_SET(fd, &sd->exceptset); break;
	default: err = -1; break;
    }
    if (sd->max <= fd)
	sd->max = fd+1;
    return err;
}

int select_del(struct select_data* sd, int what, int fd)
{
    int err = 0;
    switch (what) {
	case 0: FD_CLR(fd, &sd->readset); break;
	case 1: FD_CLR(fd, &sd->writeset); break;
	case 2: FD_CLR(fd, &sd->exceptset); break;
	default: err = -1; break;
    }
    return err;
}

int select_test(struct select_data* sd, int what, int fd)
{
    int r = 0;
    switch (what) {
	case 0: r = (FD_ISSET(fd, &sd->readset)); break;
	case 1: r = (FD_ISSET(fd, &sd->writeset)); break;
	case 2: r = (FD_ISSET(fd, &sd->exceptset)); break;
    }
    return r;
}

int select_run(struct select_data* sd)
{
    int n;
    n = select(sd->max, &sd->readset, &sd->writeset, &sd->exceptset, NULL);
    return n;
}

void print_sl(struct select_data* sd)
{
    int i;
    for (i = 0; i < sd->max; i++) {
	printf("select: %x %d r:%d w:%d e:%d\n", sd, i, 
		    FD_ISSET(i, &sd->readset),
		    FD_ISSET(i, &sd->writeset),
		    FD_ISSET(i, &sd->exceptset));
		
    }
}

int waitchild()
{
    int status;
    wait(&status);
    if (WIFEXITED(status))
	return WEXITSTATUS(status);
    else
	return -1;
}

int exec(const char* f, char* const argv[], char* const envp[]) {
    int r = execve(f, argv, envp);
    printf("exec: %d %d %s\n", r, errno, strerror(errno));
    return errno;
}

#if 0

#include <dlfcn.h>

int make_call(int (*p)(int), int a) {
    return (*p)(a);
}

void *load_func(char *m, char *s) {
    void *h, *p;
    h = dlopen(m, RTLD_LAZY);
    if (!h)
        printf("dlopen error: %s\n", dlerror());
    p = dlsym(h, s);
    if (!p)
        printf("dlsym error: %s\n", dlerror());
    return p;
}

#endif

