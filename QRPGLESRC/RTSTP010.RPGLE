     H*BindDir('QC2LE')
     H*===================================================================
     H* Processo...: getFldCoord
     H* Objectivo..: Obter as coordenadas de um Campo
     H*===================================================================
     FQSYSPRT   O    F   80        PRINTER
     D
     D SCHTEXT         PR                  ExtPgm('SCHTEXT')
     D   schFile                     10A   const
     D   schLib                      10A   const
     D   schString                   32A   const
     D
     D SCHTEXT         PI
     D   schFile                     10A   const
     D   schLib                      10A   const
     D   schString                   32A   const
     D
     D CONSTANT        C                   x'01'
     D DFT             C                   x'01'
     D upper           c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     D lower           c                   'abcdefghijklmnopqrstuvwxyz'
     D
     D QDFRTVFD        PR                  ExtPgm('QDFRTVFD')
     D   RcvVar                   32767A   options(*varsize)
     D   RcvVarLen                   10I 0 const
     D   Format                       8A   const
     D   QualFile                    20A   const
     D   ErrorCode                 8192A   options(*varsize)
     D
     D ErrorCode       ds
     D   BytesProv                   10I 0 inz(0)
     D   BytesAvail                  10I 0 inz(0)
     D
     D Base            ds                  based(p_Base) qualified
     D   FileHdr                      5U 0 overlay(Base:9)
     D   NbrRecFmts                   5U 0 overlay(Base:*NEXT)
     D
     D FileHdr         ds                  based(p_FileHdr) qualified
     D   RecFmt                      10U 0
     D
     D RecFmt          ds                  based(p_RecFmt) qualified
     D   Name                        10A
     D   MiscRcdCnt                   2A
     D   RecHdr                      10U 0
     D
     D RecHdr          ds                  based(p_RecHdr) qualified
     D   FldInfo                     10U 0
     D   FldIdx                      10U 0
     D   SelTbl                      10U 0
     D   MiscCnt                      4A
     D   NbrFlds                      5U 0
     D                                4A
     D   RespInd                      5U 0
     D                                4A
     D   RecDevDep                    5U 0

     D FldInfo         ds                  based(p_FldInfo) qualified
     D    Len                         5U 0
     D    Attrib                      1A
     D    Flags1                      1A
     D    Flags2                      1A
     D                                1A
     D    FldHdr                      1A
     D
     D ConstHdr        ds                  based(p_ConstHdr) qualified
     D                                2A
     D    FldDevDep                   5U 0
     D
     D FldDevDep       ds                  based(p_FldDevDep) qualified
     D    Flags                       1A
     D    DftAttr                     1A
     D    FldDevDepX                  5U 0
     D    KwdCatDisp                  5U 0
     D
     D KwdCatDisp      ds                  based(p_KwdCatDisp) qualified
     D    Count                       5U 0
     D    Start                       1A
     D
     D KwdCatEnt       ds                  based(p_KwdCatEnt) qualified
     D    CatId                       1A
     D    CatData                     5U 0
     D
     D KwdCat23        ds                  based(p_KwdCat23) qualified
     D    NbrKwd                      5U 0
     D    Parm                        1A
     D
     D KwdParm23       ds                  based(p_KwdParm23) qualified
     D    KwdId                       1A
     D    Type                        1A
     D    SelIdx                      5U 0
     D    DtaLen                      5U 0
     D
     D KwdData         s           3564A   varying based(p_KwdData)
     D
     D temp            s                   like(KwdData)
     D FindStr         s             32A   varying
     D x               s             10I 0
     D c               s             10I 0
     D f               s             10I 0
     D k               s             10I 0
     D len             s             10I 0
     D WFileName       S             10A   inz('FSECE010')
     D WLibName        S             10A   inz('VENCDEV')


      /free
           FindStr = 'GRUPO';
           p_Base = %alloc(1024 * 1024);
           QDFRTVFD(Base:1024*1024:'DSPF0100':WFileName+WlibName:ErrorCode);
           p_FileHdr   = p_Base + Base.FileHdr;
           p_RecFmt    = p_FileHdr + FileHdr.RecFmt;

           for x = 0 to (Base.NbrRecFmts - 1);
                p_RecFmt  = p_FileHdr + FileHdr.RecFmt +
                     (%size(RecFmt)*x);
                p_RecHdr  = p_FileHdr + RecFmt.RecHdr;
                exsr ProcessRecFmt;
           endfor;

           dealloc p_Base;
           *inlr= *on;

           //==================================================
           //  Process a record format.
           //==================================================
           begsr ProcessRecFmt;
              for f = 1 to RecHdr.NbrFlds;
                 if (f = 1);
                    p_FldInfo = p_RecHdr + RecHdr.FldInfo;
                 else;
                    p_FldInfo = p_FldInfo + FldInfo.Len;
                 endif;

                 if (fldInfo.Attrib = CONSTANT);
                     exsr ProcessCategory;
                 endif;
              endfor;
           endsr;

           //==================================================
           //   This loops through all keyword categories
           //    for a constant field (text on the screen.)
           //==================================================
           begsr ProcessCategory;

              p_ConstHdr = %addr(FldInfo.FldHdr);
              p_FldDevDep = p_FldInfo + ConstHdr.FldDevDep;

              if (FldDevDep.KwdCatDisp > 0);
                 p_KwdCatDisp = p_FldInfo + FldDevDep.KwdCatDisp;
                 for c = 0 to (KwdCatDisp.Count - 1);
                     p_KwdCatEnt = %addr(KwdCatDisp.Start)
                                 + (c * %size(KwdCatEnt));

                     if (KwdCatEnt.CatId = x'23');
                         p_KwdCat23 = p_FldInfo + KwdCatEnt.CatData;
                         exsr ProcessCat23;
                     endif;
                 endfor;
              endif;
           endsr;

           //==================================================
           //   This loops through all keywords that fall into
           //    category 23 (DFT, MSGCON, DFTVAL, HTML)
           //==================================================
           begsr ProcessCat23;

                for k = 1 to KwdCat23.NbrKwd;
                    if (k=1);
                       p_KwdParm23 = %addr(KwdCat23.Parm);
                    else;
                       p_KwdParm23 = p_KwdParm23 + %size(KwdParm23);
                    endif;

                    if (KwdParm23.KwdId = DFT);
                       p_KwdData = p_KwdParm23 + %size(KwdParm23) - 2;
                       Temp = %xlate(lower:upper:KwdData);
                       if (%scan(FindStr: Temp) > 0);
                            except print;
                       endif;
                    endif;
                endfor;
           endsr;

      /end-free

     OQSYSPRT   E            Print
     O                       RecFmt.Name
