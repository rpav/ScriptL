/*
 * scriptlcom
 * Copyright (C) 2012  Ryan Pavlik
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the GNU Lesser General Public
 * License (LGPL) version 2.1 which accompanies this distribution, and
 * is available at http://www.gnu.org/licenses/lgpl-2.1.html
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include "scriptl.h"

#ifndef UNIX_PATH_MAX
#  define UNIX_PATH_MAX 108
#endif

int scl_connect(scl_config_t *config) {
    struct sockaddr_un un_addr;
    
    struct addrinfo hints;
    struct addrinfo *result, *ptr;
    
    int sockfd = 0, ret = 0;

    switch(config->conntype) {
        case CON_INET:
            memset(&hints, 0, sizeof(struct addrinfo));
            hints.ai_family = AF_UNSPEC;
            hints.ai_socktype = SOCK_STREAM;
            hints.ai_flags = AI_NUMERICSERV;
            hints.ai_protocol = 0;

            ret = getaddrinfo(config->host, config->port, &hints, &result);

            if(ret != 0) {
                printf("scriptlcom: getaddrinfo: %s\n", gai_strerror(ret));
                exit(1);
            }

            for(ptr = result; ptr; ptr = ptr->ai_next) {
                sockfd = socket(ptr->ai_family, ptr->ai_socktype,
                                ptr->ai_protocol);
                if(sockfd < 0)
                    continue;

                if(connect(sockfd, ptr->ai_addr, ptr->ai_addrlen) != -1)
                    break;

                close(sockfd);
            }

            if(!ptr) {
                printf("scriptlcom: Could not connect.\n");
                exit(1);
            }

            freeaddrinfo(ptr);

            break;

        case CON_UDS:
            if(strlen(config->uds_path) >= UNIX_PATH_MAX) {
                printf("scriptlcom: path to socket too long: %s\n",
                       config->uds_path);
                exit(1);
            }
                
            if((sockfd = socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
                printf("scriptlcom: socket: %s\n", strerror(errno));
                exit(1);
            }

            un_addr.sun_family = AF_UNIX;
            strcpy(un_addr.sun_path, config->uds_path);

            if(connect(sockfd, (struct sockaddr*)&un_addr, sizeof(un_addr)) < 0) {
                printf("scriptlcom: connect: %s\n", strerror(errno));
                exit(1);
            }
            
            break;
    }

    return sockfd;
}

int scl_read_n(int fd, char *buf, size_t n) {
    int c = 0;

    while(c < n) {
        int ret = read(fd, buf+c, n-c);
        if(ret < 0) {
            printf("scriptl: read: %d: %s\n", errno,
                   strerror(errno));
            return ret;
        }
        if(ret == 0) return 0;

        c += ret;
    }

    return c;
}

int scl_write_n(int fd, char *buf, size_t n) {
    int ret = write(fd, buf, n);

    if(ret < 0) {
        printf("scriptl: write: %d: %s\n", errno,
               strerror(errno));
        return ret;
    }

    if(ret < n) {
        printf("scriptl: write: warning, %u of %u bytes written\n",
               (unsigned int)ret, (unsigned int)n);
    }

    return ret;
}

int scl_write_packet(int fd, char *str, size_t len) {
    char numbuf[9] = { 0 };
    int ret;

    if(len == 0)
        len = strlen(str);
    
    snprintf(numbuf, 9, "%08X", (unsigned int)len);
    if((ret = scl_write_n(fd, numbuf, 8)) == 8)
        if((ret = scl_write_n(fd, str, len)) >= 0)
            return ret + 8;

    return ret;
}

char* scl_read_packet(int fd, int *len) {
    char numbuf[9] = { 0 };
    size_t size = 0;
    char *buf = NULL;
    
    if(scl_read_n(fd, numbuf, 8) != 8)
        goto error;

    size = (size_t)strtoul(numbuf, 0, 16);

    buf = malloc(size+1);
    scl_read_n(fd, buf, size);
    buf[size] = 0;
    if(len) *len = size;

    return buf;

 error:
    /* The len check is sortof an "are they paying attention" check */
    if(!len) {
        printf("scriptlcom: unexpected null packet: exit\n");
        exit(1);
    } else {
        *len = 0;
        return NULL;
    }
}

char* scl_read_line(int fd) {
    int size = 32;
    char *buf = malloc(size);
    int n = 0, ret;

    for(;;) {
        if(n >= size) {
            buf = realloc(buf, size * 2);
            size *= 2;
        }

        ret = scl_read_n(fd, buf + n, 1);
        if(ret <= 0 || buf[n] == '\n') {
            buf[n] = 0;
            break;
        }

        n++;
    }

    return buf;
}

int scl_write_str(int fd, char *str) {
    return scl_write_n(fd, str, strlen(str));
}

int scl_write_line(int fd, char *str) {
    size_t len = strlen(str);
    int ret = 0;
    if(scl_write_n(fd, str, len) >= 0)
        if((ret = scl_write_n(fd, "\n", 1)) > 0)
            return len + 1;
        else
            return ret;
}

int scl_writef(int fd, const char *fmt, ...) {
    va_list ap;
    int count = 0;
    char buffer[1024] = { 0 };

    va_start(ap, fmt);
    count = vsnprintf(buffer, 1024, fmt, ap);
    va_end(ap);

    scl_write_n(fd, buffer, count);
    return count;
}

int scl_writef_packet(int fd, const char *fmt, ...) {
    va_list ap;
    int count = 0;
    char buffer[1024] = { 0 };

    va_start(ap, fmt);
    count = vsnprintf(buffer, 1024, fmt, ap);
    va_end(ap);

    scl_write_packet(fd, buffer, count);
    return count;
}

    
