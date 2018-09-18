#include <stdio.h>
#include <sys/param.h>  /* MIN and MAX */
#include <sys/vlimit.h>                                         

#include "bsd4.2.h"  /* contains "#define BSD4PT2" iff we are on a 4.2 Unix */

#define IF ((
#define THEN )?(
#define ELSE ):(
#define FI )) 

#define MIN_HEAP_SIZE 524288  /* .5 Mb */
#define MAX_HEAP_SIZE 8388608 /* 8 Mb  */
#define STDIO_SPACE   49152   /* be sure to save 48k of data space for stdio */
#define STATIC_SPACE  131072  /* .125Mb for fun, usr "-l" to add to this */
#define SAVED_SPACE   (STDIO_SPACE+STATIC_SPACE)                                                                        

extern char *etext;

main( argc, argv )
char **argv;
int argc;
{   
    long heap_wanted = -1;
    long leave_wanted = -1;

    long 
        data_used, 
        data_avail, 
        max_heap_size, 
        heap_size;
                          
    long
        total,
        aligned_total,
        aligned_heap_size;

    char **av;

    av = argv;
    av[argc] = 0; /* easy end test */

    while (*av)  {
        if ( ( !strcmp( *av, "-h" ) ) || ( !strcmp( *av, "-heap" ) ) )
            heap_wanted = IF (!*++av) THEN -2 ELSE atoi( *av ) FI;
        else if ( ( !strcmp( *av, "-l" ) ) || ( !strcmp( *av, "-leave" ) ) )
            leave_wanted = IF (!*++av) THEN -2 ELSE atoi( *av ) FI;
        av++;
    }

    /* Compute max allowed heap size.  MIN in case datasize is big. */
    /*  Quad align. */

    data_used  = (char *) sbrk(0) - (char *) &etext;
    data_avail = vlimit(LIM_DATA, -1)
                 - ( SAVED_SPACE + data_used + leave_wanted );
    max_heap_size = MIN( MAX_HEAP_SIZE, ((data_avail/2) & 0xFFFFFFF8) ); 
                                   
    /* decide what to allocate - give user his request if it's within reason */

    if ((heap_wanted < 0) || (heap_wanted > max_heap_size))
        heap_size = max_heap_size;
    else 
        heap_size = MAX( heap_wanted, MIN_HEAP_SIZE );

    total = sbrk( heap_size * 2 );
    if (total == -1)  {
        printf( "T could not allocate two heaps of %d bytes.\n", heap_size);
        printf( "Please retry with a smaller heap by using the -h switch.\n");
        exit(1);
    }
                
    /* make heaps smaller (if necessary) for quadword alignment */

    aligned_total = (total + 15) & 0xFFFFFFF0;
    aligned_heap_size = ( ((heap_size * 2) - (aligned_total - total)) & 0xFFFFFFF0 )
                        / 2;

    /* print message if any command line flag is given */            

    if ( (heap_wanted != -1) || (leave_wanted != -1) )
        printf( "%d bytes per heap, %d bytes reserved\n", 
                aligned_heap_size, leave_wanted + STATIC_SPACE );
 
    begin( argc, argv, 
           aligned_total, 
	   (aligned_total + aligned_heap_size), 
	   aligned_heap_size, 
           stdin, stdout, stderr, 
#ifdef BSD4PT2
	   1
#else
	   0
#endif
	   );
}

/* Some support for channels */

long feof_proc( channel )
FILE **channel;
{
    return( (feof(*channel)) << 3 );
}               
          
/* clean eof on the channel if it is a tty channel */
long clear_ttyeof( channel )
FILE **channel;
{
    if ( isatty(**channel) )
       (*channel)->_flag &= ~_IOEOF;
    return( 0 );
}


long ferror_proc( channel )
FILE **channel;
{
    return( (ferror(*channel)) << 3 );
}

long fileno_proc( channel )
FILE **channel;
{
    return( ((unsigned long)(fileno(*channel))) << 3 );
}
    

sigunblock( signal_number )
int signal_number;
{
#ifdef BSD4PT2 
    { int oldmask, maskin, newmask;

      oldmask = sigblock( 0 );             /* get current mask */
      maskin = ~(1 << --signal_number);      /* turn off specified bit */
      newmask = oldmask & maskin;
      sigsetmask( newmask );
    }
#endif
    return(0);
}     



#include <nlist.h>

/* 
  Search the symbol table of file fname for routine rname.
  Return the offset the routine within that file.  Return 0 on error.
*/
unsigned long
nlist_one( rname, fname )
char *rname, *fname;
{
    struct nlist nl[2];
    nl[1].n_name = "";        /* the last name to search for */
    nl[0].n_name = rname;     /* the first name is the function name */
    nlist(fname,nl);          /* do it */
    if ((N_TYPE & nl[0].n_type) == N_TEXT) 
        return (nl[0].n_value);         /* return offset  */
    else 
        return ((unsigned long) 0);     /* return error */
}


#include <sys/time.h>
#include <signal.h>

#define mask(s) (1<<((s)-1))
#define setvec(vec, a) \
        vec.sv_handler = a; vec.sv_mask = vec.sv_onstack = 0

static int ringring;

/* sleep for n seconds and un microseconds; un < 1,000,000 */
usleep(n,un)
        unsigned n,un;
{
        int sleepx(), omask;
        struct itimerval itv, oitv;
        register struct itimerval *itp = &itv;
        struct sigvec vec, ovec;

        if ((n == 0) && (un == 0))
                return;
        timerclear(&itp->it_interval);
        timerclear(&itp->it_value);
        if (setitimer(ITIMER_REAL, itp, &oitv) < 0)
                return;
        setvec(ovec, SIG_DFL);
        omask = sigblock(0);
        itp->it_value.tv_sec = n;
        itp->it_value.tv_usec = un;
        if (timerisset(&oitv.it_value)) {
                if (timercmp(&oitv.it_value, &itp->it_value, >))
                        oitv.it_value.tv_sec -= itp->it_value.tv_sec;
                else {
                        itp->it_value = oitv.it_value;
                        /*
                         * This is a hack, but we must have time to
                         * return from the setitimer after the alarm
                         * or else it'll be restarted.  And, anyway,
                         * sleep never did anything more than this before.
                         */
                        oitv.it_value.tv_sec = 1;
                        oitv.it_value.tv_usec = 0;
                }
        }
        setvec(vec, sleepx);
        (void) sigvec(SIGALRM, &vec, &ovec);
        ringring = 0;
        (void) setitimer(ITIMER_REAL, itp, (struct itimerval *)0);
        while (!ringring)
                sigpause(omask &~ mask(SIGALRM));
        (void) sigvec(SIGALRM, &ovec, (struct sigvec *)0);
        (void) setitimer(ITIMER_REAL, &oitv, (struct itimerval *)0);
}

static
sleepx()
{

        ringring = 1;
}

#include        <sgtty.h>
#include        <sys/stat.h>
#include        <sys/dir.h>

/* #ifndef BSD4PT2  this should be made to work */
/*
 * Print working (current) directory
 */

char    dot[]   = ".";
char    dotdot[] = "..";
char    name[BUFSIZ];
int     file;
int     off;
struct  stat    d, dd;
struct  direct  dir;

char *pwd()
{
        int rdev, rino;

        off = 0;
        stat("/", &d);
        rdev = d.st_dev;
        rino = d.st_ino;
        for (;;) {
                stat(dot, &d);
                if (d.st_ino==rino && d.st_dev==rdev)
                        return name;
                if ((file = open(dotdot,0)) < 0) {
                        fprintf(stderr,"pwd: cannot open ..\n");
                        exit(1);
                }
                fstat(file, &dd);
                chdir(dotdot);
                if(d.st_dev == dd.st_dev) {
                        if(d.st_ino == dd.st_ino)
                                return name;
                        do
                                if (read(file, (char *)&dir, sizeof(dir)) < sizeof(dir)) {
                                        fprintf(stderr,"read error in ..\n");
                                        exit(1);
                                }
                        while (dir.d_ino != d.st_ino);
                }
                else do {
                                if(read(file, (char *)&dir, sizeof(dir)) < sizeof(dir)) {
                                        fprintf(stderr,"read error in ..\n");
                                        exit(1);
                                }
                                stat(dir.d_name, &dd);
                        } while(dd.st_ino != d.st_ino || dd.st_dev != d.st_dev);
                close(file);
                cat();
        }
}

cat()
{
        register i, j;

        i = 0;
        while (dir.d_name[++i] != 0);
        if ((off+i+2) > BUFSIZ-1)
                return;
        for(j=off; j>=0; --j)
                name[j+i+1] = name[j];
        name[off+i+1] = 0;
        off=i+off+1;
     ;   name[i+1] = '/';
        for(--i; i>=0; --i)
                name[i+1] = dir.d_name[i];
        name[0] = '/';
}

/*
 *   Put the terminal in raw mode.
 */

struct sgttyb   *argp, oldtty;
       short    saved_flags;

tty_raw()
{
    gtty( 0, &oldtty );
    saved_flags = oldtty.sg_flags;
    oldtty.sg_flags |= CBREAK;
    oldtty.sg_flags &= (~ECHO & ~CRMOD);
    stty( 0, &oldtty );
}

tty_normal()
{
    oldtty.sg_flags = saved_flags;
    stty( 0, &oldtty );
}

file_length( fdesc )
{
    struct stat buf;

    if (stat( fdesc, &buf ) == -1) return 0;
    return buf.st_size;
}


struct  stat    stbuf1, stbuf2;
char    iobuf[BUFSIZ];
int     iflag = 0;      /* interactive flag. If this flag is set,
                         * the user is queried before files are
                         * destroyed by cp.
                         */

#define BSIZE   8192

copy(from, to)
char *from, *to;
{
        int fold, fnew, n;
        register char *p1, *p2, *bp;
        int mode;
        char c,i;
        if ((fold = open(from, 0)) < 0) {
                fprintf(stderr, "cp: cannot open %s\n", from);
                return(1);
        }
        fstat(fold, &stbuf1);
        mode = stbuf1.st_mode;
        /* is target a directory? */
        if (stat(to, &stbuf2) >=0 &&
           (stbuf2.st_mode&S_IFMT) == S_IFDIR) {
                p1 = from;
                p2 = to;
                bp = iobuf;
                while(*bp++ = *p2++)
                        ;
                bp[-1] = '/';
                p2 = bp;
                while(*bp = *p1++)
                        if (*bp++ == '/')
                                bp = p2;
                to = iobuf;
        }
        if (stat(to, &stbuf2) >= 0) {
                if (stbuf1.st_dev == stbuf2.st_dev &&
                   stbuf1.st_ino == stbuf2.st_ino) {
                        fprintf(stderr, "cp: cannot copy file to itself.\n");
                        return(1);
                } else if (iflag) {
                        fprintf (stderr, "overwrite %s? ", to);
                        i = c = getchar();
                        while (c != '\n' && c != EOF)
                                c = getchar();
                        if (i != 'y')
                                return(1);
                }
        }
        if ((fnew = creat(to, mode)) < 0) {
                fprintf(stderr, "cp: cannot create %s\n", to);
                close(fold);
                return(1);
        }
        while(n = read(fold,  iobuf,  BSIZE)) {
                if (n < 0) {
                        perror("cp: read");
                        close(fold);
                        close(fnew);
                        return(1);
                } else
                        if (write(fnew, iobuf, n) != n) {
                                perror("cp: write");
                                close(fold);
                                close(fnew);
                                return(1);
                        }
        }
        close(fold);
        close(fnew);
        return(0);
}

shrink( file, fsize )
char *file;
int  fsize;
{
        int fold, fnew;
        register int nbytes, n, m;
        int mode;
        char *to, *tmp, *mktemp();
        if ((fold = open(file, 0)) < 0) {
                fprintf(stderr, "Shrink: cannot open %s\n", file);
                return(1);
        }
        fstat(fold, &stbuf1);
        mode = stbuf1.st_mode;
        /* is target a directory? */
        if (stat(file, &stbuf2) >=0 &&
           (stbuf2.st_mode&S_IFMT) == S_IFDIR) {
            fprintf(stderr,
                    "Shrink: %s is a directory cannot SHRINK \n", file);
        }
        tmp = "/tmp/utmpXXXXXX";
        to = mktemp(tmp);
        printf("to= %s", to);
        if ((fnew = creat(to, mode)) < 0) {
            fprintf(stderr, "Shrink: cannot create %s\n", to);
            close(fold);
            return(1);
        }
        nbytes = fsize;
        do {
            if( nbytes > BSIZE ) m = BSIZE; else m = nbytes;
            n = read(fold,  iobuf, m );
            if (n < 0) {
                perror("Shrink: **Error wile reading %s");
                close(fold);
                close(fnew);
                return(1);
            } else
                if (write(fnew, iobuf, n) != n) {
                    perror("Shrink: **Error while writing %s");
                    close(fold);
                    close(fnew);
                    return(1);
                }
        } while ( (nbytes -= n) > 0 );
        unlink(file);
        link(to, file);
        unlink(to);
        close(fold);
        close(fnew);
        return(0);
}

/*  #endif  end of BSD4PT2 conditional */


/*#include "/usr/src/cmd/lisp/h/global.h"*/

static unsigned int        old_disc,    new_disc   ;
static struct sgttyb       old_tmode1,  new_tmode1 ;
static struct tchars       old_tmode2,  new_tmode2 ;
static struct ltchars      old_tmode3,  new_tmode3 ;
static short unsigned int  old_tmode4,  new_tmode4 ;

/*      Saves the terminal modes away */
save_tty ()  {
        ioctl( fileno( stdin ), TIOCGETD, &old_disc ) ;
        ioctl( fileno( stdin ), TIOCGETP, &old_tmode1 ) ;
        ioctl( fileno( stdin ), TIOCGETC, &old_tmode2 ) ;
        ioctl( fileno( stdin ), TIOCGLTC, &old_tmode3 ) ;
        ioctl( fileno( stdin ), TIOCLGET, &old_tmode4 ) ;
        ioctl( fileno( stdin ), TIOCGETP, &new_tmode1 ) ;
}

/*      Restore the terminal modes that had been saved away */
restore_tty () {
        ioctl( fileno( stdin ), TIOCSETD, &old_disc ) ;
        ioctl( fileno( stdin ), TIOCSETP, &old_tmode1 ) ;
        ioctl( fileno( stdin ), TIOCSETC, &old_tmode2 ) ;
        ioctl( fileno( stdin ), TIOCSLTC, &old_tmode3 ) ;
        ioctl( fileno( stdin ), TIOCLSET, &old_tmode4 ) ;
}

/*      Set up the editor's modes.  Must be called after
 *      save_tty_mode.  The current mode is CBREAK mode with all but
 *      the interrupt processing turned off.  The only keyboard interrupt
 *      enabled is ^SPACE (also known as ^@).
 */
tty_uflow_mode () {
        new_disc = CBREAK ;

                /* This uses bits left over from the terminal's previous
                 * mode.
                 */
        new_tmode1.sg_erase = -1 ;     /* No ^H erase char */
        new_tmode1.sg_kill = -1 ;      /* No ^U line kill char */
        new_tmode1.sg_flags =
                (old_tmode1.sg_flags & ~077) |  /* High bits the same */
                                   /* Echo is off */
                CBREAK ;           /* Bogus CBreak mode for now, so
                                      we get interrupts */

        new_tmode2.t_intrc  =  30 ;   /* ^SPACE is the only interrupt char.*/
        new_tmode2.t_quitc  =  28 ;   /* turn off quit */
        new_tmode2.t_startc =  17 ;   /* turn off ^Q */
        new_tmode2.t_stopc  =  19 ;   /* turn off ^S */
        new_tmode2.t_eofc   =  -1 ;   /* turn off ^D */
        new_tmode2.t_brkc   =  -1 ;   /* turn off break char */

        new_tmode3.t_suspc  = -1 ;     /* Turn off ^Z  */
        new_tmode3.t_dsuspc = -1 ;    /* Turn off ^Y  */
        new_tmode3.t_rprntc = -1 ;    /* Turn off ^R  */
        new_tmode3.t_flushc = -1 ;    /* Turn off ^O  */
        new_tmode3.t_werasc = -1 ;    /* Turn off ^W  */
        new_tmode3.t_lnextc = -1 ;    /* Turn off literal next */

        new_tmode4 = LLITOUT ;        /* Echo control chars as is */

        ioctl( fileno( stdin ), TIOCSETD, &new_disc   ) ;
        ioctl( fileno( stdin ), TIOCSETP, &new_tmode1 ) ;
        ioctl( fileno( stdin ), TIOCSETC, &new_tmode2 ) ;
        ioctl( fileno( stdin ), TIOCSLTC, &new_tmode3 ) ;
        ioctl( fileno( stdin ), TIOCLSET, &new_tmode4 ) ;
}

/*      Set up the editor's modes.  Must be called after
 *      save_tty_mode.  The current mode is CBREAK mode with all but
 *      the interrupt processing turned off.
 */
tty_u_mode () {
        new_disc = CBREAK ;

                /* This uses bits left over from the terminal's previous
                 * mode.
                 */
        new_tmode1.sg_erase = -1 ;     /* No ^H erase char */
        new_tmode1.sg_kill = -1 ;      /* No ^U line kill char */
        new_tmode1.sg_flags =
                (old_tmode1.sg_flags & ~077) |  /* High bits the same */
                                   /* Echo is off */
                CBREAK ;           /* Bogus CBreak mode for now, so
                                      we get interrupts */

        new_tmode2.t_intrc  =  30 ;   /* ^TILDE is the interrupt char.*/
        new_tmode2.t_quitc  =  28 ;   /* ^BACKSLASH is the quit char */
        new_tmode2.t_startc =  -1 ;   /* turn off ^Q */
        new_tmode2.t_stopc  =  -1 ;   /* turn off ^S */
        new_tmode2.t_eofc   =  -1 ;   /* turn off ^D */
        new_tmode2.t_brkc   =  -1 ;   /* turn off break char */

        new_tmode3.t_suspc  = -1 ;     /* Turn off ^Z  */
        new_tmode3.t_dsuspc = -1 ;    /* Turn off ^Y  */
        new_tmode3.t_rprntc = -1 ;    /* Turn off ^R  */
        new_tmode3.t_flushc = -1 ;    /* Turn off ^O  */
        new_tmode3.t_werasc = -1 ;    /* Turn off ^W  */
        new_tmode3.t_lnextc = -1 ;    /* Turn off literal next */

        new_tmode4 = LLITOUT ;        /* Echo control chars as is */

        ioctl( fileno( stdin ), TIOCSETD, &new_disc   ) ;
        ioctl( fileno( stdin ), TIOCSETP, &new_tmode1 ) ;
        ioctl( fileno( stdin ), TIOCSETC, &new_tmode2 ) ;
        ioctl( fileno( stdin ), TIOCSLTC, &new_tmode3 ) ;
        ioctl( fileno( stdin ), TIOCLSET, &new_tmode4 ) ;
}

        /* Returns the number of chars in the input buffer */
        /* This is a Berkeley Unix hacque, but well worth using */
int
input_waiting () {
        long int numchars ;
        ioctl( fileno( stdin ), FIONREAD, &numchars ) ;
        return((int) numchars) ;
} ;



/* convert a pathname to an absolute one, if it is absolute already,
   it is returned in the buffer unchanged, otherwise leading "./"s
   will be removed, and "../"s will be resolved.

   In a moment of weakness, I have implemented the cshell ~ filename
   convention.  ~/foobar will have the ~ replaced by the home directory of
   the current user.  ~user/foobar will have the ~user replaced by the
   home directory of the named user.  This should really be in the kernel
   (or be replaced by a better kernel mechanism).  Doing file name
   expansion like this in a user-level program leads to some very
   distasteful non-uniformities.

   Another fit of dementia has led me to implement the expansion of shell
   environment variables.  $HOME/mbox is the same as ~/mbox.  If the
   environment variable a = "foo" and b = "bar" then:
        $a      =>      foo
        $a$b    =>      foobar
        $a.c    =>      foo.c
        xxx$a   =>      xxxfoo
        ${a}!   =>      foo!

                                James Gosling @ CMU
 */

/* #include <sys/types.h>
   #include <sys/stat.h> */
#include <pwd.h>
#include "ctype.h" 

expand_path (nm, buf)           /* input name in nm, absolute pathname
                                   output to buf.  returns 0 on success
                                   -1 on failure with error msg in buf  */
char    *nm,
* buf; {
    register char  *s,
                   *d;
    char    lnm[1000];
    s = nm;
    d = lnm;
    while (*d++ = *s)
        if (*s++ == '$') {
            register char  *start = d;
            register    braces = *s == '{';
            register char  *value;
            while (*d++ = *s)
                if (braces ? *s == '}' : !(isalpha(*s) || isdigit(*s)))
                    break;
                else
                    s++;
            *--d = 0;
            value = (char *) getenv (braces ? start + 1 : start);
            if (value) {
                for (d = start - 1; *d++ = *value++;);
                d--;
                if (braces && *s)
                    s++;
            }
        }
    d = buf;
    nm = lnm;
    if (nm[0] == '~')           /* prefix ~ */
        if (nm[1] == '/' || nm[1] == 0)/* ~/filename */
            if (s = (char *) getenv ("HOME")) {
                if (*++nm)
                    nm++;
            }
            else
                s = "";
        else {                  /* ~user/filename */
            register char  *nnm;
            register struct passwd *pw;
            for (s = nm; *s && *s != '/'; s++);
            nnm = *s ? s + 1 : s;
            *s = 0;
            pw = (struct passwd *) getpwnam (nm + 1);
            if (pw == 0) {
                sprintf (buf,"\"%s\" isn't a registered user.", nm+1);
                return -1;
            }
            else {
                nm = nnm;
                s = pw -> pw_dir;
            }
        }
    while (*d++ = *s++);
    if (buf[0] != '\0') *(d - 1) = '/'; else d--;
    s = nm;
    while (*d++ = *s++);
    *(d - 1) = '/';
    *d = '\0';
    d = buf;
    s = buf;
    while (*s)
        if ((*d++ = *s++) == '/' && d > buf + 1) {
            register char  *t = d - 2;
            switch (*t) {
                case '/':       /* found // in the name */
                    --d;
                    break;
                case '.': 
                    switch (*--t) {
                        case '/': /* found /./ in the name */
                            d -= 2;
                            break;
                        case '.': 
                            if (*--t == '/') {/* found /../ */
                                while (t > buf && *--t != '/');
                                d = t + 1;
                            }
                            break;
                    }
                    break;
            }
        }
    if (*(d - 1) == '/')
        d--;
    *d = '\0';
    return 0;
}
