// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2005-2020 Rich Felker, et al.

// This file contains implementations of functions from musl libc marked as weak.
// This allows freestanding usage of the library.

#include <stdint.h>
#include <stddef.h>
#include <limits.h>

#define SS (sizeof(size_t))
#define ALIGN (sizeof(size_t)-1)
#define ONES ((size_t)-1/UCHAR_MAX)
#define HIGHS (ONES * (UCHAR_MAX/2+1))
#define HASZERO(x) ((x)-ONES & ~(x) & HIGHS)

// https://git.musl-libc.org/cgit/musl/tree/src/string/memchr.c
// 65f0d789b2c9a4e2bf8fbd89e9cbc704dd7a538f
__attribute__((weak))
void *memchr(const void *src, int c, size_t n)
{
	const unsigned char *s = src;
	c = (unsigned char)c;
#ifdef __GNUC__
	for (; ((uintptr_t)s & ALIGN) && n && *s != c; s++, n--);
	if (n && *s != c) {
		typedef size_t __attribute__((__may_alias__)) word;
		const word *w;
		size_t k = ONES * c;
		for (w = (const void *)s; n>=SS && !HASZERO(*w^k); w++, n-=SS);
		s = (const void *)w;
	}
#endif
	for (; n && *s != c; s++, n--);
	return n ? (void *)s : 0;
}

// https://git.musl-libc.org/cgit/musl/tree/src/string/strlen.c
// 309990f029f0c09b4c7c0405258b92ac8f93310d
__attribute__((weak))
size_t strlen(const char *s)
{
	const char *a = s;
#ifdef __GNUC__
	typedef size_t __attribute__((__may_alias__)) word;
	const word *w;
	for (; (uintptr_t)s % ALIGN; s++) if (!*s) return s-a;
	for (w = (const void *)s; !HASZERO(*w); w++);
	s = (const void *)w;
#endif
	for (; *s; s++);
	return s-a;
}

// https://git.musl-libc.org/cgit/musl/tree/src/string/memrchr.c
// 3cbc828bebf705ae570e1bfd0c2b518f929c054c
__attribute__((weak))
void *__memrchr(const void *m, int c, size_t n)
{
	const unsigned char *s = m;
	c = (unsigned char)c;
	while (n--) if (s[n]==c) return (void *)(s+n);
	return 0;
}

// https://git.musl-libc.org/cgit/musl/tree/src/string/strrchr.c
// 98ad1b04549244230eb62fb4bba0e277876d5131
__attribute__((weak))
char *strrchr(const char *s, int c)
{
	return __memrchr(s, c, strlen(s) + 1);
}
