      ** Header file for calling the "Regular Expression" functions
      **   provided by the ILE C Runtime Library from an RPG IV
      **   program.                 Scott Klement, 2001-05-04
      **                       Converted to qualified DS 2003-11-29
      **
     D/if defined(RGRLO042)
     D/eof
     D/endif
     D/define RGRLO042


      **------------------------------------------------------------
      * cflags for regcomp()
      **------------------------------------------------------------
     D REG_BASIC       C                   CONST(0)
     D REG_EXTENDED    C                   CONST(1)
     D REG_ICASE       C                   CONST(2)
     D REG_NEWLINE     C                   CONST(4)
     D REG_NOSUB       C                   CONST(8)


      **------------------------------------------------------------
      * eflags for regexec()
      **------------------------------------------------------------
     D REG_NOTBOL      C                   CONST(256)
     D REG_NOTEOL      C                   CONST(512)


      **------------------------------------------------------------
      *  errors returned
      **------------------------------------------------------------
     D REG_NOMATCH     C                   CONST(1)
     D REG_BADPAT      C                   CONST(2)
     D REG_ECOLLATE    C                   CONST(3)
     D REG_ECTYPE      C                   CONST(4)
     D REG_EESCAPE     C                   CONST(5)
     D REG_ESUBREG     C                   CONST(6)
     D REG_EBRACK      C                   CONST(7)
     D REG_EPAREN      C                   CONST(8)
     D REG_EBRACE      C                   CONST(9)
     D REG_BADBR       C                   CONST(10)
     D REG_ERANGE      C                   CONST(11)
     D REG_ESPACE      C                   CONST(12)
     D REG_BADRPT      C                   CONST(13)
     D REG_ECHAR       C                   CONST(14)
     D REG_EBOL        C                   CONST(15)
     D REG_EEOL        C                   CONST(16)
     D REG_ECOMP       C                   CONST(17)
     D REG_EEXEC       C                   CONST(18)


      **------------------------------------------------------------
      *  Structure of a compiled regular expression:
      *
      *     #define __REG_SUBEXP_MAX       9
      *     typedef struct {
      *         size_t        re_nsub;
      *         void          *re_comp;
      *         int           re_cflags;
      *         size_t        re_erroff;
      *         size_t        re_len;
      *         _LC_colval_t  re_ucoll[2];
      *         void          *re_lsub[__REG_SUBEXP_MAX+1];
      *         void          *re_esub[__REG_SUBEXP_MAX+1];
      *         unsigned char re_map[256];
      *         mbstate_t     re_shift;
      *         short         re_dbcs;
      *     } regex_t;
      **------------------------------------------------------------
     D REG_SUBEXP_MAX  C                   10
     D regex_t         DS                  qualified
     D                                     align based(prototype_only)
     D   re_nsub                     10I 0
     D   re_comp                       *
     D   re_cflags                   10I 0
     D   re_erroff                   10I 0
     D   re_len                      10I 0
     D   re_ucoll                    10I 0 dim(2)
     D   re_lsub                       *   DIM(REG_SUBEXP_MAX)
     D   re_esub                       *   DIM(REG_SUBEXP_MAX)
     D   re_map                     256A
     D   re_shift                     5I 0
     D   re_dbcs                      5I 0


      **------------------------------------------------------------
      *  structure used to report matches found by regexec()
      *
      *     typedef struct {
      *         _off_t     rm_so; /* offset of substring             */
      *         mbstate_t  rm_ss; /* shift state at start of subst   */
      *         _off_t     rm_eo; /* offset of next char after subst */
      *         mbstate_t  rm_es; /* shift state at end of subst     */
      *     } regmatch_t;
      *
      * NOTE: It's important to remember that C starts numbering
      *    string positions with '0' and RPG starts with '1'.
      *    Thus, rm_so+1 is the first char in substring, rm_eo is
      *    the last char in the substring in RPG.
      **------------------------------------------------------------
     D regmatch_t      DS                  qualified
     D                                     align based(prototype_only)
     D   rm_so                       10I 0
     D   rm_ss                        5I 0
     D   rm_eo                       10I 0
     D   rm_es                        5I 0


      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * regcomp() -- Compile a Regular Expression ("RE")
      *
      *     int regcomp(regex_t *preg, const char *pattern,
      *              int cflags);
      *
      * where:
      *       preg (output) = the compiled regular expression.
      *    pattern (input)  = the RE to be compiled.
      *     cflags (input)  = the sum of the cflag constants
      *                       (listed above) for this RE.
      *
      * Returns 0 = success, otherwise an error number.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     D regcomp         PR            10I 0 extproc('regcomp')
     D   preg                              like(regex_t)
     D   pattern                       *   value options(*string)
     D   cflags                      10I 0 value


      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * regexec() -- Execute a compiled Regular Expression ("RE")
      *
      *     int regexec(const regex_t *preg, const char *string,
      *              size_t nmatch, regmatch_t *pmatch, int eflags);
      *
      * where:
      *       preg (input)  = the compiled regular expression
      *                       (the output of regcomp())
      *     string (input)  = string to run the RE upon
      *     nmatch (input)  = the number of matches to return.
      *     pmatch (output) = array of regmatch_t DS's
      *                       showing what matches were found.
      *     eflags (input)  = the sum of the flags (constants
      *                       provided above) modifying the RE
      *
      * Returns 0 = success, otherwise an error number.
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     D regexec         PR            10I 0 extproc('regexec')
     D   preg                              like(regex_t) const
     D   string                        *   value options(*string)
     D   nmatch                      10U 0 value
     D   pmatch                            like(regmatch_t) dim(100)
     D                                     options(*varsize)
     D   eflags                      10I 0 value


      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * regerror() -- return error information from regcomp/regexec
      *
      *   size_t regerror(int errcode, const regex_t *preg,
      *              char *errbuf, size_t errbuf_size);
      *
      *  where:
      *    errcode (input)  = the error code to return info on
      *                      (obtained as the return value from
      *                      either regcomp() or regexec())
      *       preg (input)  = the (compiled) RE to return the
      *                      error for.
      *     errbuf (output) = buffer containing human-readable
      *                      error message.
      * errbuf_size (input) = size of errbuf (max length of msg
      *                      that will be returned)
      *
      * returns:  length of buffer needed to get entire error msg
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     D regerror        PR            10U 0 extproc('regerror')
     D   errcode                     10I 0 value
     D   preg                              like(regex_t) const
     D   errbuf                        *   value
     D   errbuf_size                 10I 0 value


      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * regfree() -- free memory locked by Regular Expression
      *
      *    void regfree(regex_t *preg);
      *
      *   where:
      *        preg (input) = regular expression to free mem for.
      *
      *   NOTE:  regcomp() will always allocate extra memory
      *        to be pointed to by the various pointers in
      *        the regex_t structure.  If you don't call this,
      *        that memory will never be returned to the system!
      *+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     D regfree         PR                  extproc('regfree')
     D   preg                              like(regex_t)
