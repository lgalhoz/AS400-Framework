     D**********************************************************************
     D* Source Original IFSIO_H
     D*---------------------------------------------------------------------
     D* This code prevents this member from being /copy into the same
     D* program twice.   This allows you to /copy this file into your
     D* own /copy members, without worry about conflicting with programs
     D* that also /copy this.
     D*---------------------------------------------------------------------
     D* Código alterado por Luís Galhoz, para poder ser utilizado na nova
     D* FrameWork de desenvolvimento. A alteração foi ao nível dos nomes dos
     D* campos e funções, para ficarem em conformidade com os Standards de
     D* Desenvolvimento.
     D*---------------------------------------------------------------------
     D* Deverá incluir as bibliotecas de Funções de C++ através da seguinte
     D* intrução: H BNDDIR('QC2LE')
     D**********************************************************************
      /IF DEFINED(SGRLO016)
      /EOF
      /ENDIF

      /DEFINE SGRLO016

     D*=====================================================================
     D*  Lista de Campos para o estado de Abertura
     D*  Mais do que um, utilizar seguidos, (Ex.: O#RDONLY + O#CREAT)
     D*=====================================================================
     D O#RDONLY        C                   1                                    Reading Only
     D O#WRONLY        C                   2                                    Writing Only
     D O#RDWR          C                   4                                    Reading & Writing
     D O#CREAT         C                   8                                    Create File if not exist
     D O#EXCL          C                   16                                   Exclusively create
     D O#CCSID         C                   32                                   Assign a CCSID
     D O#TRUNC         C                   64                                   Truncate File to 0 bytes
     D O#APPEND        C                   256                                  Append to File
     D O#SYNC          C                   1024                                 Synchronous write
     D O#DSYNC         C                   2048                                 Sync read
     D O#RSYNC         C                   4096                                 No controlling terminal
     D O#NOCTTY        C                   32768                                Share with readers only
     D O#SHARE_RDONLY  C                   65536                                Share with writers only
     D O#SHARE_WRONLY  C                   131072                               Share with read & write
     D O#SHARE_RDWR    C                   262144                               Share with nobody.
     D O#SHARE_NONE    C                   524288                               Assign a code page
     D O#CODEPAGE      C                   8388608                              Open in text-mode
     D O#TEXTDATA      C                   16777216                             Allow text translation on newly created
     D*
     D*   ---------------------------------------------------------------------
     D*    Note: O_TEXT_CREAT requires all of the following flags to work:
     D*              O_CREAT+O_TEXTDATA+(O_CODEPAGE or O_CCSID)
     D*   ---------------------------------------------------------------------
     D O#TEXT_CREAT    C                   33554432
     D O#INHERITMODE   C                   134217728                            Inherit mode from dir
     D O#LARGEFILE     C                   536870912                            files >2GB size
     D
     D*=====================================================================
     D* Mode Flags:
     D*    Normalmente os parametros para $Open(), $Creat(), $Chmod(),etc
     D*    utilizam 9 bits significativos para determinar o modo de acesso
     D*    ao ficheiro: (direito de acesso ao ficheiro)
     D*
     D*           user:       owner    group    other
     D*           access:     R W X    R W X    R W X
     D*           bit:        8 7 6    5 4 3    2 1 0
     D*
     D* (This is accomplished by adding the flags below to get the mode)
     D*=====================================================================
     D*--> Owner authority
     D S#IRUSR         C                   256
     D S#IWUSR         C                   128
     D S#IXUSR         C                   64
     D S#IRWXU         C                   448
     D
     D*--> Group authority
     D S#IRGRP         C                   32
     D S#IWGRP         C                   16
     D S#IXGRP         C                   8
     D S#IRWXG         C                   56
     D
     D*--> Other people
     D S#IROTH         C                   4
     D S#IWOTH         C                   2
     D S#IXOTH         C                   1
     D S#IRWXO         C                   7
     D
     D*=====================================================================
     D* $Open() = Abrir o ficheiro
     D*    int open(const char *path, int oflag, . . .);
     D*
     D*     path = path name of file to open
     D*    oflag = open flags
     D*     mode = file mode, aka permissions.  (Reqd with O_CREAT flag)
     D* codepage = code page to assign to file  (Reqd with O_CODEPAGE flag)
     D*
     D* Returns the file descriptor of the opened file
     D*         or -1 if an error occurred
     D*=====================================================================
     D $Open           PR            10I 0 extproc('open')
     D   path                          *   value options(*string)
     D   oflag                       10I 0 value
     D   mode                        10U 0 value options(*nopass)
     D   codepage                    10U 0 value options(*nopass)
     D
     D*=====================================================================
     D* $Write() = Write to stream file
     D*   int write(int fildes, const void *buf, size_t nbyte);
     D*
     D*   fildes = file descriptor to write to
     D*      buf = pointer to data to be written
     D*    nbyte = number of bytes to write
     D*
     D* Returns the number of bytes written
     D*         or a -1 if an error occurred
     D*=====================================================================
     D $Write          PR            10I 0 extproc('write')
     D   fildes                      10I 0 value
     D   buf                           *   value
     D   nbyte                       10U 0 value
     D
     D*=====================================================================
     D* $Read() = Read from stream file
     D*    int read(int fildes, void *buf, size_t nbyte);
     D*
     D*   fildes = file descriptor to read from
     D*      buf = pointer to memory to read into
     D*    nbyte = maximum number of bytes to read
     D*
     D* Returns the number of bytes read
     D*         or a -1 if an error occurred
     D*=====================================================================
     D $Read           PR            10I 0 extproc('read')
     D   fildes                      10I 0 value
     D   buf                           *   value
     D   nbyte                       10U 0 value
     D
     D*=====================================================================
     D* $Close() = Close file descriptor
     D*    int close_(int fildes);
     D*   fildes = file descriptor to close
     D* Returns 0 if successful
     D*         or a -1 if an error occurred
     D*=====================================================================
      /if not defined(CLOSE_PROTOTYPE)
     D $Close          PR            10I 0 extproc('close')
     D   fildes                      10I 0 value
      /define CLOSE_PROTOTYPE
      /endif

     D*--> Access mode flags for access()
     D F#OK            C                   0                                    File Exists
     D R#OK            C                   4                                    Read Access
     D W#OK            C                   2                                    Write Access
     D X#OK            C                   1                                    Execute or Search
     D
     D*=====================================================================
     D* $Access = Determine file accessibility
     D*    int access(const char *path, int amode)
     D*=====================================================================
     D $Access         PR            10I 0 ExtProc('access')
     D   Path                          *   Value Options(*string)
     D   amode                       10I 0 Value
     D
     D*=====================================================================
     D* $Chmod = Change file permissions
     D*    int chmod(const char *path, mode_t mode)
     D*=====================================================================
     D $Chmod          PR            10I 0 ExtProc('chmod')
     D   path                          *   Value options(*string)
     D   mode                        10U 0 Value
     D
     D*=====================================================================
     D* $Rename - Rename a file or directory.
     D*    int rename(const char *old, const char *new);
     D*=====================================================================
     D $Rename         PR            10I 0 ExtProc('Qp0lRenameKeep')
     D   old                           *   Value options(*string)
     D   new                           *   Value options(*string)
     D
     D*=====================================================================
     D* $UnLink - Remove Link to File.  (deletes 1 reference to a file.  If this
     D*   is the last reference, the file itself is deleted.
     D*    int unlink(const char *path)
     D*=====================================================================
     D $Unlink         PR            10I 0 ExtProc('unlink')
     D   path                          *   Value options(*string)
     D
     D*=====================================================================
     D* File Information Structure (stat)
     D*
     D* struct stat {
     D*  mode_t         st_mode;       /* File mode                       */
     D*  ino_t          st_ino;        /* File serial number              */
     D*  nlink_t        st_nlink;      /* Number of links                 */
     D*  uid_t          st_uid;        /* User ID of the owner of file    */
     D*  gid_t          st_gid;        /* Group ID of the group of file   */
     D*  off_t          st_size;       /* For regular files, the file
     D*                                 * size in bytes                   */
     D*  time_t         st_atime;      /* Time of last access             */
     D*  time_t         st_mtime;      /* Time of last data modification  */
     D*  time_t         st_ctime;      /* Time of last file status change */
     D*  dev_t          st_dev;        /* ID of device containing file    */
     D*  size_t         st_blksize;    /* Size of a block of the file     */
     D*  unsigned long  st_allocsize;  /* Allocation size of the file     */
     D*  qp0l_objtype_t st_objtype;    /* AS/400 object type              */
     D*  unsigned short st_codepage;   /* Object data codepage            */
     D*  char           st_reserved1[66]; /* Reserved                     */
     D* };
     D*
     D*=====================================================================
     D p_statds        S               *
     D statds          DS                  BASED(p_statds) QUALIFIED
     D  mode                         10U 0
     D  ino                          10U 0
     D  nlink                         5U 0
     D  pad                           2A
     D  uid                          10U 0
     D  gid                          10U 0
     D  size                         10I 0
     D  atime                        10I 0
     D  mtime                        10I 0
     D  ctime                        10I 0
     D  dev                          10U 0
     D  blksize                      10U 0
     D  allocsize                    10U 0
     D  objtype                      12A
     D  codepage                      5U 0
     D  reserved1                    62A
     D  ino_gen_id                   10U 0
     D
     D*=====================================================================
     D* $Stat - Get File Information
     D*    int stat(const char *path, struct stat *buf)
     D*=====================================================================
     D $Stat           PR            10I 0 ExtProc('stat')
     D   path                          *   value options(*string)
     D   buf                           *   value
     D
     D*=====================================================================
     D* "whence" constants for use with lseek()
     D*=====================================================================
     D SEEK#SET        C                   CONST(0)
     D SEEK#CUR        C                   CONST(1)
     D SEEK#END        C                   CONST(2)
     D
     D*=====================================================================
     D* $LSeek - Set File Read/Write Offset
     D*    off_t lseek(int fildes, off_t offset, int whence)
     D*=====================================================================
     D $Lseek          PR            10I 0 ExtProc('lseek')
     D   fildes                      10I 0 value
     D   offset                      10I 0 value
     D   whence                      10I 0 value
     D
     D*=====================================================================
     D* $FStat -Get File Information from descriptor
     D*    int fstat(int fildes, struct stat *buf)
     D*=====================================================================
     D $FStat          PR            10I 0 ExtProc('fstat')
     D   fildes                      10I 0 value
     D   buf                           *   value
     D
     D*=====================================================================
     D* Directory Entry Structure (dirent)
     D*
     D* struct dirent {
     D*   char           d_reserved1[16];  /* Reserved                       */
     D*   unsigned int   d_reserved2;      /* Reserved                       */
     D*   ino_t          d_fileno;         /* The file number of the file    */
     D*   unsigned int   d_reclen;         /* Length of this directory entry
     D*                                     * in bytes                       */
     D*   int            d_reserved3;      /* Reserved                       */
     D*   char           d_reserved4[8];   /* Reserved                       */
     D*   qlg_nls_t      d_nlsinfo;        /* National Language Information
     D*                                     * about d_name                   */
     D*   unsigned int   d_namelen;        /* Length of the name, in bytes
     D*                                     * excluding NULL terminator      */
     D*   char           d_name[_QP0L_DIR_NAME]; /* Name...null terminated   */
     D*
     D* };
     D*
     D*=====================================================================
     D p_dirent        s               *
     D dirent          ds                  based(p_dirent) QUALIFIED
     D   reserv1                     16A
     D   reserv2                     10U 0
     D   fileno                      10U 0
     D   reclen                      10U 0
     D   reserv3                     10I 0
     D   reserv4                      8A
     D   nlsinfo                     12A
     D     nls_ccsid                 10I 0 OVERLAY(nlsinfo:1)
     D     nls_cntry                  2A   OVERLAY(nlsinfo:5)
     D     nls_lang                   3A   OVERLAY(nlsinfo:7)
     D     nls_reserv                 3A   OVERLAY(nlsinfo:10)
     D   namelen                     10U 0
     D   name                       640A
     D
     D*=====================================================================
     D* $MkDir - Make Directory
     D*    int mkdir(const char *path, mode_t mode)
     D*=====================================================================
     D $MkDir          PR            10I 0 ExtProc('mkdir')
     D   path                          *   Value options(*string)
     D   mode                        10U 0 Value
     D
     D*=====================================================================
     D* $RmDir - Remove Directory
     D*    int rmdir(const char *path)
     D*=====================================================================
     D $RmDir          PR            10I 0 ExtProc('rmdir')
     D   path                          *   value options(*string)
     D
     D*=====================================================================
     D* $ChDir - Change Directory
     D*    int chdir(const char *path)
     D*=====================================================================
     D $ChDir          PR            10I 0 ExtProc('chdir')
     D   path                          *   Value Options(*string)
     D
     D*=====================================================================
     D* $OpenDir - Open a Directory
     D*    DIR *opendir(const char *dirname)
     D*=====================================================================
     D $OpenDir        PR              *   EXTPROC('opendir')
     D  dirname                        *   VALUE options(*string)
     D
     D*=====================================================================
     D* $ReadDir - Read Directory Entry
     D*    struct dirent *readdir(DIR *dirp)
     D*=====================================================================
     D $ReadDir        PR              *   EXTPROC('readdir')
     D  dirp                           *   VALUE
     D
     D*=====================================================================
     D* $CloseDir - Close a directory
     D*    int closedir(DIR *dirp)
     D*=====================================================================
     D $CloseDir       PR            10I 0 EXTPROC('closedir')
     D  dirhandle                      *   VALUE
     D
     D*=====================================================================
     D* $GetCwd - Get Current Working Directory
     D*    char *getcwd(char *buf, int size);
     D*=====================================================================
     D $GetCwd         PR              *   EXTPROC('getcwd')
     D  buf                            *   VALUE
     D  size                         10I 0 VALUE
