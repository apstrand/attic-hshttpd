
#include <sys/types.h>
#include <stdlib.h>
#include <string.h>
#include <regex.h>



char rx_buf[4000];
int rx_indx[10];

regex_t* rx_compile(char *p) {
    regex_t *rx = malloc(sizeof(regex_t));
    int t;
    t = regcomp(rx, p, REG_EXTENDED);
    return t == 0 ? rx : 0;
}

int rx_match(regex_t *rx, char *s) {
    return ! regexec(rx, s, 0, 0, 0);
}

int rx_submatch(regex_t *rx, char *s) {
	regmatch_t matches[10];
	int n = 1, i = 0, len;
	if (regexec(rx, s, 10, matches, 0) == REG_NOMATCH)
	    return 0;
	while (matches[n].rm_so != -1) {
	    len = matches[n].rm_eo - matches[n].rm_so;
	    strncpy(&rx_buf[i], &s[matches[n].rm_so], len);
	    rx_buf[i+len] = 0;
	    rx_indx[n] = i;
	    i += len + 1;
	    n++;
	}
	return n - 1;
}

char *rx_getmatch(int i) {
    return &rx_buf[rx_indx[i]];
}

