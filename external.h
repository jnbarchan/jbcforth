// external.h

#ifndef NOREADLINE
#define READLINE
#else
#undef READLINE
#endif

//TODO
// Changed default to NOSEMAPHORE unless SEMAPHORE is defined
// because implementation has TODOs
#ifndef SEMAPHORE
#define NOSEMAPHORE
#endif

#ifndef NOSEMAPHORE
#define SEMAPHORE
#else
#undef SEMAPHORE
#endif

#include <stdint.h>
#include <stdio.h>
#include "defs.h"

extern int external_errno(void);
extern const char *external_strerror(int errnum);
extern void external_perror(const char *__s);

extern void *external_malloc(size_t __size);
extern void *external_calloc(size_t __nmemb, size_t __size);
extern void *external_realloc (void *__ptr, size_t __size);
extern void external_free(void *__ptr);
extern char *external_strdup(const char *s);

extern int external_fileno(FILE *stream);
extern FILE *external_fopen(const char *path, const char *mode);
extern int external_fclose(FILE *fp);
extern int external_fflush(FILE *stream);

extern void external_exit (int __status) __THROW __attribute__ ((__noreturn__));
extern int external_isatty(int __fd);
extern int32_t external_getpid(void);
extern int32_t external_fork(void);
extern int external_system(const char *__command);
extern int32_t external_time(void);
extern int32_t external_getmicrotime(void);
unsigned int external_sleep(unsigned int seconds);

#ifdef	SEMAPHORE
extern int forth_semid;
extern int external_seminit(void);
extern int external_semacquire(void);
extern int external_semrelease(void);
#endif

#ifdef READLINE
extern char *forth_word_generator(const char *text, int state);
extern char *readline_word_generator_result(const char *word);
extern void initialise_readline(void);
extern void *readline_realloc (void *__ptr, size_t __size);
extern char *readline_getline(const char *prompt, bool addHistory);
extern char readline_get_key(void);
extern void readline_output_color(FILE *output, byte colour);
extern void readline_output_color_white(FILE *output);
#endif
