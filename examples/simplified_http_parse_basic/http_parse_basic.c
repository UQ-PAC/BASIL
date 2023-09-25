#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>

#define MINIBUF_SIZE		50

# define MIN(a, b)		((a) < (b) ? (a) : (b))

#define auth_strcpy(creds, var, value) \
	if ((creds) && (value)) { \
		strncpy(((creds)->var), (value), MINIBUF_SIZE); \
	} 

#define auth_memcpy(creds, var, value, len) \
	if ((creds) && (value)) { \
		memcpy(((creds)->var), (value), MIN(len, MINIBUF_SIZE)); \
	} 


struct auth_s {
	char user[MINIBUF_SIZE];
	char domain[MINIBUF_SIZE];
	char password[MINIBUF_SIZE];
};

typedef struct hlist_s *hlist_t;
struct hlist_s {
	char *key;
	char *value;
	struct hlist_s *next;
};


char *ntlm_hash_nt_password(char *password) {
	return strdup(password);
}


//int hlist_subcmp(hlist_t list, const char *key, const char *substr) {
//  // is actually case-insensitive but make case-sensitive for demo
//	return NULL != strstr(hlist_get(list, key), substr);
//}


/*
 * Get a header on the list using the key
 */
char *hlist_get(hlist_t list, const char *key) {
	hlist_t t = list;

	while (t) {
		if (!strcasecmp(t->key, key))
			break;
		t = t->next;
	}

	return (t == NULL ? NULL : t->value);
}

/*
 * Parse headers for BASIC auth credentials
 *
 * Return 1 = creds parsed OK, 0 = no creds, -1 = invalid creds
 */
int http_parse_basic(hlist_t headers, const char *header, struct auth_s *tcreds) {
	char *tmp = NULL, *pos = NULL, *buf = NULL, *dom = NULL;
	int i;

  // check the header is for 'basic' authentication
	//if (!hlist_subcmp(headers, header, "Basic"))
	//	return 0;

  // load the username and password from header list into buf
	tmp = hlist_get(headers, header);
	buf = malloc(strlen(tmp) + 1);
	memset(buf, 0, strlen(tmp) + 1);
	i = 5;
	while (i < strlen(tmp) && tmp[++i] == ' ');
	memcpy(buf, tmp + i, strlen(tmp + i));

  // find the split between username and password ("username:password")
	pos = strstr(buf, ":");

	if (pos == NULL) {
		memset(buf, 0, strlen(buf));		/* clean password memory */
		free(buf);
		return -1;
	} else {
		*pos = 0;
	  // buf == "domain\username"
	  // pos + 1 == "password"
		dom = strstr(buf, "\\");
		if (dom == NULL) {
			auth_strcpy(tcreds, user, buf);
		} else {
			*dom = 0;
			// buf == "domain"
			// dom + 1 == "username"
			auth_strcpy(tcreds, domain, buf);
			auth_strcpy(tcreds, user, dom+1);
		}

		// call some hash function and replace the password with its hashed version
		tmp = ntlm_hash_nt_password(pos+1);
		auth_memcpy(tcreds, password, tmp, 21);
		free(tmp);

    // attempt to clear the buf before freeing it
		memset(buf, 0, strlen(buf));
		free(buf);
	}

	return 1;
}

int main() {

  struct auth_s auth = {
      // to be filled
  };


  struct hlist_s headers = { 
    .key = "Authorization",
    .value = "Basic mydomain\\myusername:mypassword",
    .next = NULL
  };

  return http_parse_basic(&headers, "Authorization", &auth);
}
