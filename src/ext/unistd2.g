
/*
  comment out any that you don't have
*/

#ifdef AGEN_JLISP
Title  unistd2.g
#	define DOC(x)   x
#	define ErrNo	errno
#else
#	define DOC(x)	/* x */
#	define ErrNo	int
#endif

#ifndef NO_UNIX

ErrNo chdir(char*)	DOC("change directories");
char* getlogin(void)	DOC("return the login name");
int fork(void)		DOC("fork the process");
int vfork(void)       	DOC("vfork the process");
int getuid(void)	DOC("return the real user id");
int getgid(void)	DOC("return the real group id");
int geteuid(void)	DOC("return the effective user id");
int getegid(void)	DOC("return the effective group id");
ErrNo setuid(int)	DOC("set the user id");
ErrNo seteuid(int)	DOC("set the effective user id");
ErrNo setgid(int)	DOC("set the group id");
ErrNo setegid(int)	DOC("set the effective group id");
int getpid(void)	DOC("return the process id");
int getppid(void)	DOC("return the process parent id");
ErrNo sleep(int)	DOC("sleep for a specified number of seconds");

ErrNo mkdir(char*, int)	DOC("create a directory");
ErrNo unlink(char*)	DOC("delete the specified file");
ErrNo rmdir(char*)	DOC("delete the specified directory");
int umask(int)		DOC("set the umask");

ErrNo rename(const char*, const char*)	DOC("rename a file\n[see also: link symlink unlink]");
ErrNo link(char*, char*)	DOC("hard link a file\n[see also: symlink rename unlinl]");
ErrNo symlink(char*, char*)	DOC("symbolic link a file\n[see also: link rename unlink readlink]");
ErrNo chmod(char*, int)		DOC("change file attributes");
ErrNo chown(char*, int, int)	DOC("chown file owner uid and gid");

#endif /* NO_UNIX */
