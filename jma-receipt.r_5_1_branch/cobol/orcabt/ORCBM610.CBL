      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBM610.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץ�
      *  ����ݡ��ͥ��̾  : �ţƥե��������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  14/04/22    NACL��ƣ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.01    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *  04.08.02    NACL-ƣ��    17/06/13  ���ʬ�ε�Ͽ���ѹ�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    �ţƥե�����
           SELECT  EFFILE-FILE     ASSIGN  EFFILEPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-EFFILE.
      *    �ţƥե������������
           SELECT  EFFILE-F-FILE   ASSIGN  EFFILEFPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-EFFILEF.
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    �ţƥե�����  
       FD  EFFILE-FILE.
       01  EFFILE-R                    PIC X(2500).
      *
      *    �ţƥե�����  
       FD  EFFILE-F-FILE.
       01  EFFILE-F-R                  PIC X(2500).
      *     
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
       COPY    "COMMON-SPA".
      *
      *    �ţƥե�����̾���ΰ� 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //EFFILEPARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //EFFILEFPARA//.
      *
      *01  CSVPARA.
      *****03  FILLER              PIC X(05) VALUE "/tmp/".
       01  EFFILEPARA-BASENAME.
           03  EFFILEPARA-HOSPNUM      PIC 9(02).
           03  FILLER                  PIC X(04)   VALUE   "DPC_".
           03  EFFILEPARA-FILENM-1     PIC X(01).
           03  EFFILEPARA-NYUGAIKBN    PIC X(01).
           03  FILLER                  PIC X(01)   VALUE   "_".
           03  EFFILEPARA-FILENM-2     PIC X(14).
           03  FILLER                  PIC X(04)   VALUE   ".txt".
      *
      *    ���顼�ե����� ̾���ΰ� 
           COPY    "CPRECEERR.INC".
      *
       01  STS-AREA.
           03  STS-EFFILE              PIC X(02).
           03  STS-EFFILEF             PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-EFOUT               PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-OUT1                PIC 9(06).
           03  CNT-OUT-ALL             PIC 9(06).
      *
       01  KEY-AREA.
           03  KEY-NEW.
               05  KEY-N-RECKBN        PIC X(01).
               05  KEY-N-PTID          PIC 9(10).
               05  KEY-N-SJKBN         PIC X(02).
               05  KEY-N-TOTALTEN      PIC 9(08).
           03  KEY-OLD.
               05  KEY-O-RECKBN        PIC X(01).
               05  KEY-O-PTID          PIC 9(10).
               05  KEY-O-SJKBN         PIC X(02).
               05  KEY-O-TOTALTEN      PIC 9(08).
      *
       01  WRK-AREA.
      *    �ѥ�᥿���ꥢ
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-HOSPCD         PIC X(14).
           05  WRK-PARA-FOLDER         PIC X(150).
           05  WRK-PARA-NYUGAIKBN      PIC X(01).
           05  WRK-PARA-JOBID          PIC 9(07).
           05  WRK-PARA-SHELLID        PIC X(08).
           05  WRK-PARA-DATAKBN        PIC X(01).
           05  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-NYUGAIKBN           PIC X(01).
      *
           03  WRK-RENNUM              PIC 9(04).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
       01  WRK-EF-REC.
           03  WRK-EF-CD               PIC X(9).
           03  FILLER                  PIC X(1).
           03  WRK-EF-NUM              PIC X(10).
           03  FILLER                  PIC X(1).
           03  WRK-EF-NYUINYMD         PIC X(8).
           03  FILLER                  PIC X(1).
           03  WRK-EF-TAIINYMD         PIC X(8).
           03  FILLER                  PIC X(1).
           03  WRK-EF-SRYKBN           PIC X(2).
           03  FILLER                  PIC X(1).
           03  WRK-EF-RENNUM           PIC 9(04).
           03  FILLER                  PIC X(2000).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *    ���顼�ե�����̾���ΰ� 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
       01  WRK-SGETTEMP-TEMPDIR    PIC X(1024).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �쥻�ץ��Ż�
           COPY    "CPRECEDEN.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ���饤�������¸�ģ����極��
           COPY    "CPORCSFILESV.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE                       DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              CNT-AREA
                                   WRK-AREA
                                   FLG-AREA
                                   STS-AREA
                                   SPA-AREA
                                   KEY-AREA
           INITIALIZE              RECEERR
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-HOSPCD
                                               WRK-PARA-FOLDER
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  RECEERR
           MOVE    SGETTEMP-FULLNAME
                                   TO  RECEERR
      *
           MOVE   SGETTEMP-TEMPDIR TO  WRK-SGETTEMP-TEMPDIR
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBM610"      TO  JOB-PGID
           EVALUATE   WRK-PARA-NYUGAIKBN
               WHEN    "0"
                   MOVE    "�ţƥե������������������������"
                                           TO  JOB-SHELLMSG
               WHEN    "1"
                   MOVE    "�ţƥե����������������"
                                           TO  JOB-SHELLMSG
               WHEN    "2"
                   MOVE    "�ţƥե������������������"
                                           TO  JOB-SHELLMSG
           END-EVALUATE
           PERFORM 900-CALL-ORCSJOB-SEC
      *    
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ������ʬ
           INITIALIZE                  KEY-AREA
           IF      WRK-PARA-NYUGAIKBN
                                   =   "0" OR  "2"
               MOVE    "2"             TO  WRK-NYUGAIKBN
               PERFORM 900-EFOUT-SELECT-SEC
      *
               PERFORM         UNTIL   FLG-EFOUT   =   1
                   PERFORM 2001-HENSYU-SEC
               END-PERFORM
      *
               MOVE    "tbl_efout"     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME
                                       TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    ����ʬ
           INITIALIZE                  KEY-AREA
           IF      WRK-PARA-NYUGAIKBN
                                   =   "0" OR  "1"
               MOVE    "1"             TO  WRK-NYUGAIKBN
               PERFORM 900-EFOUT-SELECT-SEC
      *
               PERFORM         UNTIL   FLG-EFOUT   =   1
                   PERFORM 2002-HENSYU-SEC
               END-PERFORM
      *
               MOVE    "tbl_efout"     TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME
                                       TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    �ţƥե������Խ���������������
      *****************************************************************
       2001-HENSYU-SEC             SECTION.
      *
           MOVE    KEY-NEW         TO  KEY-OLD 
           MOVE    ZERO            TO  CNT-OUT
      *
           MOVE    SPA-HOSPNUM     TO  EFFILEPARA-HOSPNUM
           MOVE    RECEDEN-RECKBN  TO  EFFILEPARA-FILENM-1
           EVALUATE    WRK-NYUGAIKBN
               WHEN    "1"
                   MOVE    "n"         TO  EFFILEPARA-NYUGAIKBN
               WHEN    "2"
                   MOVE    "g"         TO  EFFILEPARA-NYUGAIKBN
           END-EVALUATE
           MOVE    WRK-PARA-HOSPCD TO  EFFILEPARA-FILENM-2
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    EFFILEPARA-BASENAME
                                   TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  EFFILEPARA
      *??
           DISPLAY "PARA=" EFFILEPARA "#"
      *??
      *
           OPEN    OUTPUT              EFFILE-FILE
      *
           IF      STS-EFFILE          =   "00"
               CONTINUE
           ELSE
               CALL "coblog" USING   "file open err " EFFILEPARA
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "�ե����� �����ץ󥨥顼 STS="
                                               DELIMITED  BY  SIZE
                       STS-EFFILE              DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           PERFORM         UNTIL   FLG-EFOUT   =   1
                           OR      KEY-NEW NOT =   KEY-OLD
               MOVE    RECEDEN-RECEDATA    TO  EFFILE-R
               WRITE   EFFILE-R
      *
               IF      STS-EFFILE          =   "00"
                   CONTINUE
               ELSE
                   CALL   "coblog" USING   "file write err " EFFILEPARA
                   MOVE    SPACE               TO  WRK-RECEERR
                   STRING "�ե����� �񤭹��ߥ��顼 STS="
                                               DELIMITED  BY  SIZE
                           STS-EFFILE          DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 500-COBABORT-SEC
               END-IF
      *
               ADD     1                   TO  CNT-OUT
                                               CNT-OUT-ALL
      *
               PERFORM 900-EFOUT-READ-SEC
           END-PERFORM
      *
           CLOSE   EFFILE-FILE
      *
           DISPLAY "EFFILE   OUT  " CNT-OUT
           .
       2001-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    �ţƥե������Խ�������������
      *****************************************************************
       2002-HENSYU-SEC             SECTION.
      *
           MOVE    ZERO            TO  CNT-OUT
                                       WRK-RENNUM
           MOVE    KEY-NEW         TO  KEY-OLD 
      *
           MOVE    SPA-HOSPNUM     TO  EFFILEPARA-HOSPNUM
           MOVE    "E"             TO  EFFILEPARA-FILENM-1
           MOVE    "n"             TO  EFFILEPARA-NYUGAIKBN
           MOVE    WRK-PARA-HOSPCD TO  EFFILEPARA-FILENM-2
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    EFFILEPARA-BASENAME
                                   TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  EFFILEPARA
      *??
           DISPLAY "PARA=" EFFILEPARA "#"
      *??
      *
           OPEN    OUTPUT              EFFILE-FILE
      *
           IF      STS-EFFILE          =   "00"
               CONTINUE
           ELSE
               CALL "coblog" USING   "file open err " EFFILEPARA
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "�ե����� �����ץ󥨥顼 STS="
                                               DELIMITED  BY  SIZE
                       STS-EFFILE              DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           MOVE    SPA-HOSPNUM     TO  EFFILEPARA-HOSPNUM
           MOVE    "F"             TO  EFFILEPARA-FILENM-1
           MOVE    "n"             TO  EFFILEPARA-NYUGAIKBN
           MOVE    WRK-PARA-HOSPCD TO  EFFILEPARA-FILENM-2
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    EFFILEPARA-BASENAME
                                   TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  EFFILEFPARA
      *??
           DISPLAY "PARA=" EFFILEFPARA "#"
      *??
      *
           OPEN    OUTPUT              EFFILE-F-FILE
      *
           IF      STS-EFFILEF         =   "00"
               CONTINUE
           ELSE
               CALL "coblog" USING   "file open err " EFFILEFPARA
               MOVE    SPACE               TO  WRK-RECEERR
               STRING "�ե����� �����ץ󥨥顼 STS="
                                               DELIMITED  BY  SIZE
                       STS-EFFILEF             DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           PERFORM         UNTIL   FLG-EFOUT   =   1
               PERFORM 20021-EF-HENSYU-SEC
           END-PERFORM
      *
           CLOSE   EFFILE-FILE
                   EFFILE-F-FILE
      *
           DISPLAY "EFFILE E OUT  " CNT-OUT
           DISPLAY "EFFILE F OUT  " CNT-OUT1
           .
       2002-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    �ţƥե��������ֹ��Խ�������������
      *****************************************************************
       20021-EF-HENSYU-SEC                SECTION.
      *
      *    ������˽���ֹ�򥯥ꥢ
           MOVE    ZERO            TO  WRK-RENNUM
      *
           MOVE    KEY-NEW         TO  KEY-OLD
           PERFORM         UNTIL   FLG-EFOUT   =   1
                           OR      KEY-N-PTID
                                           NOT =   KEY-O-PTID
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM         UNTIL   FLG-EFOUT   =   1
                               OR      KEY-N-PTID
                                               NOT =   KEY-O-PTID
                               OR      KEY-N-SJKBN
                                               NOT =   KEY-O-SJKBN
      *            �»�����˽���ֹ�����ꤷ�ʤ����ʿ��Ŷ�ʬ������
                   IF      KEY-N-SJKBN     =   "90"
                       ADD     1               TO  WRK-RENNUM
                   END-IF
      *
                   MOVE    KEY-NEW         TO  KEY-OLD
                   PERFORM         UNTIL   FLG-EFOUT   =   1
                                   OR      KEY-NEW NOT =   KEY-OLD
                       IF      KEY-N-SJKBN     =   "90"
                           MOVE    RECEDEN-RECEDATA
                                                   TO  WRK-EF-REC
                           MOVE    WRK-RENNUM      TO  WRK-EF-RENNUM
                           MOVE    WRK-EF-REC      TO  RECEDEN-RECEDATA
                       END-IF
                       PERFORM 200211-EF-WRITE-SEC
      *
                       PERFORM 900-EFOUT-READ-SEC
                   END-PERFORM
               END-PERFORM
           END-PERFORM
      *
           .
       20021-EF-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    �ţƥե�������Ͻ�����������
      *****************************************************************
       200211-EF-WRITE-SEC          SECTION.
      *
           EVALUATE    RECEDEN-RECKBN
               WHEN    "E"
                   MOVE    RECEDEN-RECEDATA    TO  EFFILE-R
                   WRITE   EFFILE-R
      *
                   IF      STS-EFFILE          =   "00"
                       CONTINUE
                   ELSE
                       CALL   "coblog" USING
                                           "file write err " EFFILEPARA
                       MOVE    SPACE           TO  WRK-RECEERR
                       STRING "�ե����� �񤭹��ߥ��顼 STS="
                                               DELIMITED  BY  SIZE
                               STS-EFFILE      DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
      *
                   ADD     1                   TO  CNT-OUT
                                                   CNT-OUT-ALL
               WHEN    "F"
                   MOVE    RECEDEN-RECEDATA    TO  EFFILE-F-R
                   WRITE   EFFILE-F-R
      *
                   IF      STS-EFFILEF         =   "00"
                       CONTINUE
                   ELSE
                       CALL   "coblog" USING
                                           "file write err " EFFILEFPARA
                       MOVE    SPACE           TO  WRK-RECEERR
                       STRING "�ե����� �񤭹��ߥ��顼 STS="
                                               DELIMITED  BY  SIZE
                               STS-EFFILE      DELIMITED  BY  SIZE
                                               INTO    WRK-RECEERR
                       END-STRING
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 500-COBABORT-SEC
                   END-IF
      *
                   ADD     1                   TO  CNT-OUT1
                                                   CNT-OUT-ALL
           END-EVALUATE
      *
           .
       200211-EF-WRITE-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           IF      CNT-OUT-ALL     =   ZERO
               MOVE    "�����оݤΥǡ���������ޤ���Ǥ���"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
      *        �ե�������¸���󹹿�
               IF      WRK-PARA-DATAKBN    =   "5" OR  "6"
                   PERFORM 3001-FILE-INFO-INSERT-SEC
               END-IF
           END-IF
      *
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
      *??  MOVE    WRK-RECENUM     TO  JOB-UPDCNT2
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           DISPLAY "EFOUT    IN   " CNT-IN
           DISPLAY "EFFILE   OUT  " CNT-OUT-ALL
           DISPLAY "*** ORCBM610 END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ե�������¸���󹹿�����
      *****************************************************************
       3001-FILE-INFO-INSERT-SEC             SECTION.
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "I"             TO  SFILESV-MODE
           MOVE    WRK-PARA-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    "recept4.sh"    TO  SFILESV-SHELLID  (1)
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SFILESV-SHORI-RENNUM
                                                        (1)
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SFILESV-RENNUM   (1)
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SFILESV-SRYYM    (1)
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SFILESV-SKYYMD   (1)
      *****MOVE    WRK-PARA-FOLDER TO  SFILESV-FOR-FOLDER
      *****                                             (1)
           MOVE    WRK-SGETTEMP-TEMPDIR
                                   TO  SFILESV-FOR-FOLDER
                                                        (1)
           IF      WRK-PARA-DATAKBN    =   "5"
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "_EF_FILE_"         DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".zip"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "_EF_FILE_"         DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".zip"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-TO-DATA (1)
                   END-STRING
           ELSE
                   STRING  WRK-PARA-HOSPNUM    DELIMITED  BY  SIZE
                           "_EF_FILE_"         DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".iso"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-FOR-DATA(1)
                   END-STRING
                   STRING  "EF_FILE_"          DELIMITED  BY  SIZE
                           LNK-PRTKANRI-SRYYM  DELIMITED  BY  SIZE
                           ".iso"              DELIMITED  BY  SIZE
                                               INTO  SFILESV-TO-DATA (1)
                   END-STRING
           END-IF
           MOVE    CNT-OUT         TO  SFILESV-CNT-ALL  (1)
           EVALUATE    WRK-PARA-NYUGAIKBN   
               WHEN    ZERO
                   MOVE    "�ţƥե��������"
                                           TO  SFILESV-TITLE    (1)
               WHEN    "1"
                   MOVE    "�ţƥե����������������"
                                           TO  SFILESV-TITLE    (1)
               WHEN    "2"
                   MOVE    "�ţƥե������������������"
                                           TO  SFILESV-TITLE    (1)
           END-EVALUATE
           MOVE    "1"             TO  SFILESV-DATA-TYPE(1)
      *
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA
           .
       3001-FILE-INFO-INSERT-EXT.
           EXIT.
      *            
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
            .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼����λ����
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBM610 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ţƥե������ѥǡ����ɹ�
      *****************************************************************
       900-EFOUT-SELECT-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-EFOUT
      *
           MOVE    SPACE               TO  RECEDEN-REC
           INITIALIZE                      RECEDEN-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
           MOVE    LNK-PRTKANRI-SRYYM  TO  RECEDEN-SRYYM
           MOVE    WRK-NYUGAIKBN       TO  RECEDEN-NYUGAIKBN
           MOVE    "%"                 TO  RECEDEN-RECKBN
      *
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "tbl_efout"         TO  MCP-TABLE
           IF      WRK-NYUGAIKBN       =   "1"
               MOVE    "key4"              TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "key2"              TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-EFOUT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-EFOUT
           END-IF
           .
       900-EFOUT-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ţƥե�����ǡ����ɹ�
      *****************************************************************
       900-EFOUT-READ-SEC              SECTION.
      *
           MOVE    "tbl_efout"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-EFOUT
               MOVE    MCPDATA-REC         TO  RECEDEN-REC
      *
               IF      WRK-NYUGAIKBN       =   "1"
      *            ����ʬ�ǿ��Ŷ�ʬ̤����ΤȤ�
                   IF      RECEDEN-SJKBN       =   SPACE
                       MOVE   "������������ټ¹Ԥ��Ʋ�����"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       STRING  "ORCRDPC10 "        DELIMITED   BY  SIZE
                               WRK-RECEERR         DELIMITED   BY  SPACE
                               LOW-VALUE           DELIMITED   BY  SIZE
                                                   INTO    WRK-ERRMSG
                       END-STRING
                       PERFORM 500-COBABORT-SEC
                   END-IF
               END-IF
      *
               IF      WRK-NYUGAIKBN       =   "1"
                   MOVE    RECEDEN-PTID        TO  KEY-N-PTID
                   MOVE    RECEDEN-SJKBN       TO  KEY-N-SJKBN
      *            ���Ŷ�ʬ�����ΤȤ������»ܷ����ܽ���ֹ������
                   IF      KEY-N-SJKBN         =   "90"
                       MOVE    RECEDEN-TOTALTEN    TO  KEY-N-TOTALTEN
                   ELSE
                       MOVE    ZERO                TO  KEY-N-TOTALTEN
                   END-IF
               ELSE
                   MOVE    RECEDEN-RECKBN      TO  KEY-N-RECKBN
               END-IF
               ADD     1                   TO  CNT-IN
           ELSE
               MOVE    1                   TO  FLG-EFOUT
               MOVE    HIGH-VALUE          TO  KEY-NEW
           END-IF
      *
           .
       900-EFOUT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ����ģ��������
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "�ģ��ɤ߹��ߥ��顼��TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCBM610 select err TABLE="
                                           DELIMITED   BY  SIZE
                       MCP-TABLE           DELIMITED   BY  SPACE
                       " PATHNAME="        DELIMITED   BY  SIZE
                       MCP-PATHNAME        DELIMITED   BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                       LOW-VALUE           DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               CALL    "cobabort"          USING   WRK-ERRMSG
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ƣţԣãȽ���
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    �ģ¥����ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.