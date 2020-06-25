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
       PROGRAM-ID.             ORCGT04.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����ɽ�����������
      *  ����ݡ��ͥ��̾  : ���饤�������¸�ǡ�������
      *  ������            : 
      *  07/08/08    NaCl��ƣ��   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.01    NACL-ƣ��    10/03/23  �ȣϣӣ�̾������ˡ���ѹ�
      *
      *  04.06.01    NACL-ƣ��    12/12/04  �л���������Σãӣֽ��ϻ�
      *                                     ���������
      *
      *  04.07.01    NACL-ƣ��    12/08/01  �쥻�ťǡ��������å���ǽ�б�
      *
      *  04.08.01    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    ���顼�ե����� ̾���ΰ� 
           COPY    "CPTEMPFL.INC"  REPLACING   //TEMPFLPARA//
                                   BY          //RECEERR//.
      *
      *    ���顼�ե�����̾���ΰ� 
           COPY    "CPERRFL.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���̣ӣУ�
           COPY    "T01COMMON-SPA".
      *
      *    �����ѣӣУ�
           COPY    "T01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-ROW             PIC 9(01).  
           03  FLG-CHK             PIC 9(01).  
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-TIDMSG          PIC X(80).
      * 
           03  WRK-T04-FILE-INFOLST-ROW
                                   PIC S9(09). 
      *
           03  WRK-JOBID           PIC 9(03).
           03  WRK-STR             PIC 9(05).
           03  WRK-SELNUM          PIC S9(05).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-NUM             PIC ZZ9.
           03  WRK-NUM7            PIC ZZZZZZ9.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
      *    �ե�����ǥ��쥯�ȥ���ֻ��ꥵ��
           COPY  "CPMKPASS.INC".
      *
       01  WRK-CONS-AREA.
      *    �����ƥ�����ģ¡ʣ��������ˤΥ�����  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "044".
      *    ����ִ����ģ¤Υ�����  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *     
      *    ����ִ����ģ¤Υ����͡ʥ쥻�ץȡ�  
           03  WRK-CONS-JOB-SHELLID-RECEPT1
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID-RECEPT1
                                   PIC 9(07)   VALUE   1.
      *     
      *    �ѥ�᥿�ģ¤Υ����͡ʥ쥻�ź�����  
           03  WRK-CONS-BTPARA-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-BTPARA-SCRIPTID
                                   PIC X(10)   VALUE   "recept4.sh".
      *
      *    �����
           03  WRK-CONS-FILE-INFOLST-MAX
                                   PIC 9(03)   VALUE   100.
      *
      *    ���顼�ե�����ɣ�
           03  WRK-CONS-FILE-ID    PIC X(08)   VALUE   "OR044ERR".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK3003.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ���饤�������¸�ģ����極��
           COPY    "CPORCSFILESV.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      *    �桼�����������
           COPY    "CPORCSLDUSER.INC".
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA44SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       WRK-AREA
      *                                 
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T01FREE
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "TERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "T04"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "TID1"
                   PERFORM 3002-TID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "T99"
                   PERFORM 3003-T99-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̡ʣԣɣģ��ˣϣ˽���
      *****************************************************************
       3002-TID1-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
      *        ����ִ��������å�����
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD      =   SPACE
                   IF      SPA-ERRCD      =   SPACE  
                       PERFORM 210-BACK
                       GO  TO  3002-TID1-SET-EXT
                   END-IF 
               END-IF
           END-IF
      *
           MOVE    1               TO  SPA-GMN-T04-CUR
      *
           MOVE    SPACE           TO  SPA-TID1-FLG
           MOVE    SPACE           TO  SPA-TIDCD
           .
       3002-TID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ɽ������������
      *****************************************************************
       3003-T99-SET-SEC             SECTION.
      *
           MOVE    1               TO  SPA-GMN-T04-CUR
      *
           INITIALIZE                  SPA-T01KYOUTU
      *
           .
       3003-T99-SET-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-T04-AREA
      *
           MOVE    SPA-NAI-SKYYM   TO  WRK-SYMD (1:6)
           MOVE    "01"            TO  WRK-SYMD (7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG3 (1:16)
                                   TO  SPA-GMN-T04-SRYYM
           MOVE    SPA-NAI-SKYYM   TO  SPA-NAI-T04-SRYYM
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "R"             TO  SFILESV-MODE
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SFILESV-TBL-KEY
           MOVE    SPA-NAI-SKYYM   TO  SFILESV-SRYYM (1)
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA  
           IF      SFILESV-RETURN  =   ZERO
               MOVE    ZERO            TO  IDY
      *
               INITIALIZE              SPA-SFILESV-AREA
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       100
                       OR      SFILESV-SHELLID (IDX)
                                   =       SPACE
                   IF      SFILESV-SHELLID (IDX)
                                           =   WRK-CONS-BTPARA-SCRIPTID
                       CONTINUE
                   ELSE
                       ADD     1           TO  IDY
                       MOVE    IDY         TO  SPA-GMN-T04-TBANGO  (IDY)
                       MOVE    SFILESV-SYOYMD  (IDX)
                                       TO  SPA-NAI-T04-TSYOYMD     (IDY)
                                           WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG TO  SPA-GMN-T04-TSYOYMD (IDY)   
                       MOVE    SFILESV-CREYMD  (IDX)
                                           TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG TO  SPA-GMN-T04-TCREYMD (IDY)
                       MOVE    SFILESV-TITLE        (IDX)
                                           TO  SPA-GMN-T04-TTITLE  (IDY)
                       MOVE    SFILESV-CNT-ALL      (IDX)
                                           TO  SPA-GMN-T04-TCNT-ALL(IDY)
                       MOVE    SFILESV-ERR-MSG     (IDX)
                                           TO  SPA-GMN-T04-TERRMSG (IDY)
                       MOVE    SFILESV-TO-FOLDER  (IDX)
                                           TO  SPA-NAI-T04-TTO-FOLDER
                                                                   (IDY)
                       MOVE    SFILESV-TO-DATA     (IDX)
                                           TO  SPA-NAI-T04-TTO-DATA(IDY)
                       MOVE    SFILESV-SHELLID    (IDX)
                                           TO  SPA-NAI-T04-TSHELLID(IDY)
                       MOVE    SFILESV-SHORI-RENNUM    (IDX)
                                           TO
                                       SPA-NAI-T04-TSHORI-RENNUM(IDY)
                       MOVE    SFILESV-RENNUM    (IDX)
                                           TO  SPA-NAI-T04-TRENNUM (IDY)
                       IF      SFILESV-CODE-TYPE   (IDX)   =   SPACE
                           MOVE    "1"     TO  SFILESV-CODE-TYPE   (IDX)
                       END-IF
                       MOVE    SFILESV-CODE-TYPE   (IDX)
                                           TO  SPA-NAI-T04-TCODE-TYPE
                                                                (IDY)
                       MOVE    SFILESV-TEISYUTUSAKI(IDX)
                                           TO  SPA-NAI-T04-TTEISYUTUSAKI
                                                                (IDY)
                   END-IF    
               END-PERFORM
               MOVE    IDY         TO  SPA-GMN-T04-MAX
           END-IF
      *
           MOVE    1               TO  SPA-GMN-T04-CUR
                                       SPA-GMN-T04-PAGE
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    ��������
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-PRINT-GID-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41001-SELNUM-CHK-SEC
      *
               WHEN    ANY             ALSO    "FILE_INFOLST"
                   PERFORM 41010-FILE-INFOLST-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ����ϥ����å�����
      *****************************************************************
       41001-SELNUM-CHK-SEC            SECTION.
      *
           MOVE    1                   TO  SPA-GMN-T04-CUR
      *
           INITIALIZE                      SPA-GMN-T04-KOUMOKU
                                           SPA-NAI-T04-KOUMOKU
      *
           MOVE    T04-SELNUM          TO  SPA-GMN-T04-SELNUM
      *
           IF      SPA-GMN-T04-SELNUM =   ZERO
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-CHK  
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-T04-MAX
                       OR      FLG-CHK     =   1
                   IF      SPA-GMN-T04-SELNUM  =
                                           SPA-GMN-T04-TBANGO (IDX)
                       MOVE    IDX             TO  SPA-NAI-T04-SELNUM
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO                           
                   MOVE    "0001"          TO  SPA-ERRCD
               ELSE
                   PERFORM 410011-FILE-INFOLST-SELECT-SEC
               END-IF
           END-IF
           .
       41001-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������
      *****************************************************************
       410011-FILE-INFOLST-SELECT-SEC        SECTION.
      *
           MOVE    SPA-GMN-T04-TTITLE (SPA-NAI-T04-SELNUM)
                                       TO  SPA-GMN-T04-TITLE
      *
           MOVE    99                  TO  SPA-GMN-T04-CUR
           .
       410011-FILE-INFOLST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ѥ�᥿�����������
      *****************************************************************
       41010-FILE-INFOLST-SEC       SECTION.
      *          
      *    ������
           MOVE    ZERO                TO  WRK-SELNUM
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-T04-MAX
               IF      T04-FILE-INFOLST-SELECT (IDX)     =   "T"
                   IF      IDX             =   SPA-NAI-T04-SELNUM
                       CONTINUE
                   ELSE        
                       MOVE    IDX             TO  WRK-SELNUM
                       MOVE    SPA-GMN-T04-MAX TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNUM           >   ZERO
               MOVE    WRK-SELNUM           TO  SPA-NAI-T04-SELNUM
               MOVE    SPA-GMN-T04-TBANGO (SPA-NAI-T04-SELNUM) 
                                            TO  SPA-GMN-T04-SELNUM
           END-IF
      *
           PERFORM 410011-FILE-INFOLST-SELECT-SEC
           .
       41010-FILE-INFOLST-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
      *
           MOVE    "T01"               TO  SPA-SAKIPG
           MOVE    "T04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǽ���
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-T04-PAGE        =   1
               GO  TO  460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-T04-PAGE    =   SPA-GMN-T04-PAGE    -   1
      *
           MOVE    1                   TO  SPA-GMN-T04-CUR     
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǡ�����
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-T04-NEXT    =   ZERO
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-T04-PAGE
      *
           MOVE    1                   TO  SPA-GMN-T04-CUR     
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ɽ����
      *****************************************************************
       450-PRINT-GID-SEC               SECTION.
      *   
      *    ���ܤ����򤵤�Ƥ��뤫�����å�
           IF      SPA-NAI-T04-SELNUM  =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-T04-CUR
           ELSE
               IF    ( SPA-GMN-T04-TCNT-ALL (SPA-NAI-T04-SELNUM)
                                           =   ZERO  )
               OR    ( SPA-GMN-T04-TERRMSG  (SPA-NAI-T04-SELNUM)
                                       NOT =   SPACE )
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-T04-CUR
               END-IF
           END-IF
      *       
      *    ����ִ��������å�����
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "1001"              TO  SPA-TIDCD
               END-IF
           END-IF
           .
       450-PRINT-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ��������å�����
      *****************************************************************
       4501-JOBKANRI-CHK-SEC           SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    ����ִ��������å�
           MOVE    SPACE               TO  SYS-3003-REC
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ����ִ��������å�����
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-RECEERR
                           STRING  "������Ǥ���"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "��"            DELIMITED  BY  SIZE
                                                   INTO  WRK-RECEERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *     
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-TIDCD       NOT =   SPACE
               PERFORM 520-TIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "T04"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-T04-FILE-INFOLST-ROW
           ELSE
               MOVE    T04-FILE-INFOLST-ROW
                                           TO  WRK-T04-FILE-INFOLST-ROW
           END-IF
      * 
           INITIALIZE                  T04
      *
           MOVE    SPA-GMN-T04-SRYYM   TO  T04-SRYYM
           MOVE    SPA-GMN-T04-SELNUM  TO  T04-SELNUM
           MOVE    SPA-GMN-T04-TITLE   TO  T04-TITLE
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T04-MAX
               MOVE    SPA-GMN-T04-TBANGO (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  T04-TRENNUM (IDX)
               MOVE    SPA-GMN-T04-TSYOYMD (IDX)
                                           TO  T04-TSYOYMD (IDX)
               MOVE    SPA-GMN-T04-TCREYMD (IDX)
                                           TO  T04-TCREYMD (IDX)
               MOVE    SPA-GMN-T04-TTITLE  (IDX)
                                           TO  T04-TTITLE  (IDX)
               MOVE    SPA-GMN-T04-TCNT-ALL(IDX)
                                           TO  WRK-NUM7
               MOVE    WRK-NUM7            TO  T04-TCNT-ALL(IDX)
               MOVE    SPA-GMN-T04-TERRMSG (IDX)
                                           TO  T04-TERRMSG (IDX)
               IF      IDX             =   SPA-NAI-T04-SELNUM
                   MOVE    "T"         TO  T04-FILE-INFOLST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  T04-FILE-INFOLST-SELECT (IDX)
               END-IF
           END-PERFORM    
           MOVE    SPA-GMN-T04-MAX     TO  T04-FILE-INFOLST-COUNT
           MOVE    ZERO                TO  T04-FILE-INFOLST-ROW
           IF      WRK-T04-FILE-INFOLST-ROW
                                       >   ZERO
               MOVE    WRK-T04-FILE-INFOLST-ROW
                                           TO  T04-FILE-INFOLST-ROW
           END-IF
      *               
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
         EVALUATE    SPA-GMN-T04-CUR
             WHEN    1
                     MOVE  "SELNUM"    TO  MCP-WIDGET
             WHEN    99
                     MOVE  "B12"        TO  MCP-WIDGET
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "�����ֹ楨�顼"            TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE
                       "�оݤȤʤ���������򤷤Ƥ���ޤ���"
                                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE
                       "�����оݤΥǡ���������ޤ���"  TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR                 TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  TERR
           MOVE    SPA-ERRCD            TO  TERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  TERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T04"                TO  SPA-MOTOPG
           MOVE    "TERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-TIDMSG
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1001"
                   STRING   SPA-GMN-T04-TITLE  DELIMITED  BY  SPACE
                            "����¸������Ԥ��ޤ�"
                                               DELIMITED  BY  SIZE 
                                               INTO    WRK-TIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  TID1
           INITIALIZE                       TID1
           MOVE    SPA-TIDCD            TO  TID1-ID1CODE
           MOVE    WRK-TIDMSG           TO  TID1-ID1MSG
           MOVE    "���"               TO  TID1-B01-LABEL
           MOVE    "����"               TO  TID1-B12-LABEL
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "T04"                TO  SPA-MOTOPG
           MOVE    "TID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Ѵ��Խ�����
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE               TO  WRK-HENYMDG
               WHEN    "99999999"
                   MOVE    SPACE               TO  WRK-HGO
                   MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
                   MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
                   MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
                   MOVE    WRK-HENYMD          TO  WRK-HENYMDG
               WHEN    OTHER
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
           END-EVALUATE
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ip���ɥ쥹����
      *****************************************************************
       900-SYSTEM-GET-SEC          SECTION.
      *
           INITIALIZE                  SLDUSER-AREA
           MOVE    2               TO  SLDUSER-KBN
           MOVE    MCP-TERM        TO  SLDUSER-ID
           CALL    "ORCSLDUSER"        USING
                                       SLDUSER-AREA
                                       SPA-AREA
      *
           .
       900-SYSTEM-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ������ɹ��ʥ�����
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ������ңţ���
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ����ģ��������
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *