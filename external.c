// external.c

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include "defs.h"
#include "external.h"

#ifdef	SEMAPHORE
#include <sys/sem.h>
#endif	// SEMAPHORE

#ifdef	READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif	// READLINE

extern int external_errno(void)
{
  return errno;
}

extern void external_perror(const char *__s)
{
  perror(__s);
}


extern void *external_malloc(size_t __size)
{
  return malloc(__size);
}

extern void *external_calloc(size_t __nmemb, size_t __size)
{
  return calloc(__nmemb, __size);
}

extern void *external_realloc(void *__ptr, size_t __size)
{
  return realloc(__ptr, __size);
}

extern void external_free(void *__ptr)
{
  free(__ptr);
}

extern char *external_strdup(const char *s)
{
  return strdup(s);
}


extern int external_fileno(FILE *stream)
{
  return fileno(stream);
}

extern FILE *external_fopen(const char *path, const char *mode)
{
  return fopen(path, mode);
}

extern int external_fclose(FILE *fp)
{
  return fclose(fp);
}

extern int external_fflush(FILE *stream)
{
  return fflush(stream);
}


extern void external_exit(int __status)
{
  exit(__status);
}

extern int external_isatty(int __fd)
{
  return isatty(__fd);
}

extern int external_fork(void)
{
  return fork();
}

extern int32_t external_getpid(void)
{
  return getpid();
}

extern int external_system(const char *__command)
{
  return system(__command);
}

extern int32_t external_time(void)
{
  return time(NULL);
}

extern int32_t external_getmicrotime(void)
{
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_usec;
}


#ifdef	SEMAPHORE
union semun
{
    int              val;    /* Value for SETVAL */
    struct semid_ds *buf;    /* Buffer for IPC_STAT, IPC_SET */
    unsigned short  *array;  /* Array for GETALL, SETALL */
    struct seminfo  *__buf;  /* Buffer for IPC_INFO (Linux-specific) */
};

int forth_semid = -1;

extern int external_seminit(void)
{
  // create a single semaphore
  forth_semid = semget(IPC_PRIVATE, 1, (IPC_CREAT | 0777));
  if (forth_semid == -1)
    return -1;

  // initialise its value to 0
  union semun arg;
  arg.val = 0;
  int result = semctl(forth_semid, 0, SETVAL, arg);
  return result;
}

extern int external_semacquire(void)
{
  struct sembuf sops[2];
  sops[0].sem_num = 0;        /* Operate on semaphore 0 */
  sops[0].sem_op = 0;         /* Wait for value to equal 0 */
  sops[0].sem_flg = 0;
  sops[1].sem_num = 0;        /* Operate on semaphore 0 */
  sops[1].sem_op = 1;         /* Increment value by one */
  sops[1].sem_flg = 0;
  int result = semop(forth_semid, sops, 2);
  return result;
}

extern int external_semrelease(void)
{
  struct sembuf sops[1];
  sops[0].sem_num = 0;        /* Operate on semaphore 0 */
  sops[0].sem_op = -1;        /* Decrement value by one */
  sops[0].sem_flg = 0;
  int result = semop(forth_semid, sops, 1);
  return result;
}
#endif	// SEMAPHORE


#ifdef	READLINE
extern char *readline_word_generator_result(const char *word)
{
  return strdup(word);
}

/* Attempt to complete on the contents of TEXT.  START and END bound the
region of rl_line_buffer that contains the word to complete.  TEXT is
the word to complete.  We can use the entire contents of rl_line_buffer
in case we want to do some simple parsing.  Return the array of matches,
or NULL if there aren't any. */
static char **readline_forth_word_completion(const char *text, int start, int end)
{
  /* This is a Forth word to complete. */
  return rl_completion_matches(text, forth_word_generator);
}

static char history_file[] = "./.forth_history";

extern void initialise_readline(void)
{
  static bool isInitialised = FALSE;

  if (!isInitialised)
  {
    isInitialised = TRUE;
    /* Tell the completer that we want to do completion. */
    rl_attempted_completion_function = readline_forth_word_completion;
    rl_basic_word_break_characters = " \t\n";
    // truncate saved "./.history" down to last 20 lines
    // and read it into history
    using_history();
    history_expansion_char = history_subst_char = '\0';
    history_truncate_file(history_file, 20);
    if (read_history(history_file) != 0)
      write_history(history_file);
  }
}

extern void *readline_realloc (void *__ptr, size_t __size)
{
  return realloc(__ptr, __size);
}

static void readline_prep_terminal(bool prep)
{
  static bool preped = FALSE;
  fflush(stdin);
  fflush(stdout);
  if (prep && !preped)
    rl_prep_terminal(1);
  else if (!prep && preped)
    rl_deprep_terminal();
  preped = prep;
}

static int readline_terminal_column(void)
{
  // return the current column number (counts from 1) of the caret in the terminal
  // -1 => not known
  if (!isatty(fileno(stdin)) || !isatty(fileno(stdout)))
    return -1;
  readline_prep_terminal(TRUE);			// put terminal into "raw" mode (immediate input, no echo)
  fputs("\033[6n", stdout);			// ANSI escape sequence <ESC>[6n to query cursor position
  int row = -1, col = -1;
  int count = fscanf(stdin, "\033[%d;%dR", &row, &col);	// ANSI escape sequence response <ESC>[<row>;<col>R
  readline_prep_terminal(FALSE);		// restore terminal to "cooked" mode
  return col;
}

extern char *readline_getline(const char *prompt, bool addHistory)
{
  // read the next line from the Terminal
  // return the (malloc()ed) line
  // NULL => EOF

  static char *last_line = NULL;

  bool get_another_line;
  do
  {
    get_another_line = FALSE;
    // if there was previous output with a terminating '\n'
    // we need to recognise this and output '\n' to move to start of next line
    // so that readline() does not mess up the display when input characters are erased
    if (readline_terminal_column() > 1)
      fputc('\n', stdout);

    // free the last line read
    free(last_line);
    last_line = NULL;
    // check if stdin previously closed
    if (stdin == NULL || fileno(stdin) < 0)
      return NULL;
    // get a new line (malloc()ed)
    last_line = readline(prompt);
    if (last_line == NULL)
      return NULL;
    // add the line into the history
    if (addHistory && *last_line != '\0')
    {
      // do not add if same as previous line
      HIST_ENTRY *pHE = history_get(history_length);
      if (pHE == NULL || strcmp(pHE->line, last_line) != 0)
      {
	add_history(last_line);
	append_history(1, history_file);
      }
      // special case: a line of just "h" displays the history
      if (strcmp(last_line, "h") == 0)
      {
	for (int i = 0; i <= history_length; i++)
	  if ((pHE = history_get(i)) != NULL)
	    fprintf(stdout, " %d %s\n", i, pHE->line);
	// go back round to get another line
	get_another_line = TRUE;
      }
    }
  } while (get_another_line);
  return last_line;
}

extern char readline_get_key(void)
{
  // return the next key read from the terminal
  if (!isatty(fileno(stdin)))
    return -1;
  readline_prep_terminal(TRUE);			// put terminal into "raw" mode (immediate input, no echo)
  char c = (char)rl_read_key();
  readline_prep_terminal(FALSE);		// restore terminal to "cooked" mode
  return c;
}

extern void readline_output_color(FILE *output, byte colour)
{
  if (!isatty(fileno(output)))
    return;
  fprintf(output, "\033[3%hum", colour);
}

extern void readline_output_color_white(FILE *output)
{
  if (!isatty(fileno(output)))
    return;
  readline_output_color(output, 9);
}

#endif // READLINE

