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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBJINMEI.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���
      *  ����ݡ��ͥ��̾  : ��̾����ƺ���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/05/26    NACL-��Ʀ��   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.01     NACL-����   07/06/08  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC                     PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC".
      *
      *    ���ơ������ΰ�
       01  STS-AREA.
           03  STS-RECEERR                 PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-JINMEI                  PIC 9(01).
           03  FLG-PTINF                   PIC 9(01).
           03  FLG-DBRC                    PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-LINE                    PIC 9(05).
           03  CNT-PAGE                    PIC 9(05).
           03  CNT-JOB-UPDCNT              PIC 9(06).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-TIME                    PIC 9(08).
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                         PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
      *
      *    �ѥ�᥿�ΰ�
       01  PARA-AREA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID                      PIC 9(07).
           03  WRK-PARA-SHELLID                    PIC X(08).
grpsys*DD  03  WRK-PARA-HOSPID                     PIC X(24).
grpsys     03  WRK-PARA-HOSPNUM                    PIC 9(02).
      *     03  WRK-PARA-SRYYM                      PIC X(06).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-DBPATH                          PIC X(100).
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-KANA-SEI                        PIC X(50).
           03  WRK-KANA-MEI                        PIC X(50).
           03  WRK-KANJI-SEI                       PIC X(50).
           03  WRK-KANJI-MEI                       PIC X(50).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ��̾
       01  JINMEI-REC.
           COPY    "CPJINMEI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(1000).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
grpsys     INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  RECEERR
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                  PARA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                       INTO            LNK-PRTKANRI-RENNUM
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
                                       WRK-PARA-JOBID
                                       WRK-PARA-SHELLID
grpsys*DD                              WRK-PARA-HOSPID
grpsys                                 WRK-PARA-HOSPNUM
grosys                                 RECEERR
      *                                 WRK-PARA-SRYYM
           END-UNSTRING
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBJINMEI"        TO  JOB-PGID
           MOVE    "��̾����ƺ���"    TO  JOB-SHELLMSG
      *
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
grpsys                                     SPA-AREA
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "STP"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    3                   TO  JOB-STOPFLG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ���Ը���
           MOVE    ZERO                 TO      FLG-PTINF
           INITIALIZE                           PTINF-REC
grpsys     MOVE    SPA-HOSPNUM          TO  PTINF-HOSPNUM
grpsys*DD  MOVE    "PTINF-ALL"          TO      ORC-DBPATH
grpsys     MOVE    "tbl_ptinf"     TO  MCP-TABLE
grpsys     MOVE    "all"           TO  MCP-PATHNAME
           PERFORM   910-PTINF-SELECT-SEC
           IF      FLG-PTINF            =       ZERO
grpsys*DD      MOVE    "PTINF-ALL"      TO      ORC-DBPATH
grpsys         MOVE    "tbl_ptinf"     TO  MCP-TABLE
grpsys         MOVE    "all"           TO  MCP-PATHNAME
               PERFORM  910-PTINF-READ-SEC
               PERFORM  UNTIL  FLG-PTINF    =    1
                   PERFORM  2001-JINMEI-INS-SEC
grpsys*DD          MOVE    "PTINF-ALL"    TO      ORC-DBPATH
grpsys             MOVE    "tbl_ptinf"     TO  MCP-TABLE
grpsys             MOVE    "all"           TO  MCP-PATHNAME
                   PERFORM  910-PTINF-READ-SEC
               END-PERFORM
           END-IF
      *
grpsys*DD  MOVE     "PTINF-ALL"          TO  ORC-DBPATH
grpsys     MOVE    "tbl_ptinf"     TO  MCP-TABLE
grpsys     MOVE    "all"           TO  MCP-PATHNAME
           PERFORM   900-TBL-CLOSE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��̾�������Ͽ����
      *****************************************************************
       2001-JINMEI-INS-SEC          SECTION.
      *
           MOVE        SPACE        TO   WRK-KANA-SEI
                                         WRK-KANA-MEI
                                         WRK-KANJI-SEI
                                         WRK-KANJI-MEI
           PERFORM   VARYING   IDX   FROM  1  BY  2
             UNTIL  (IDX       >     100)   OR
                    (PTINF-KANANAME(IDX:2)  = "��")
               CONTINUE
           END-PERFORM
           IF    PTINF-KANANAME(IDX:2)  = "��"
               COMPUTE   IDX1   =   IDX   -   1
               MOVE   PTINF-KANANAME(1:IDX1)  TO  WRK-KANA-SEI
               COMPUTE   IDX2   =   IDX   +   2
               MOVE   PTINF-KANANAME(IDX2:100)  TO  WRK-KANA-MEI
           END-IF
      *
           PERFORM   VARYING   IDX   FROM  1  BY  2
             UNTIL  (IDX       >     100)   OR
                    (PTINF-NAME(IDX:2)  = "��")
               CONTINUE
           END-PERFORM
           IF    PTINF-NAME(IDX:2)  = "��"
               COMPUTE   IDX1   =   IDX   -   1
               MOVE   PTINF-NAME(1:IDX1)  TO  WRK-KANJI-SEI
               COMPUTE   IDX2   =   IDX   +   2
               MOVE   PTINF-NAME(IDX2:100)  TO  WRK-KANJI-MEI
           END-IF
      *
           DISPLAY "PTINF-PTID = " PTINF-PTID 
      *     DISPLAY "WRK-KANA-SEI = " WRK-KANA-SEI
      *     DISPLAY "WRK-KANA-MEI = " WRK-KANA-MEI
      *     DISPLAY "WRK-KANJI-SEI = " WRK-KANJI-SEI
      *     DISPLAY "WRK-KANJI-MEI = " WRK-KANJI-MEI
      *
           IF     (WRK-KANA-MEI   =  SPACE)  OR
                  (WRK-KANJI-MEI   =  SPACE)
               CONTINUE
           ELSE
               IF    WRK-KANA-SEI   NOT =  WRK-KANJI-SEI
                   MOVE    ZERO            TO      FLG-JINMEI
                   INITIALIZE                      JINMEI-REC
                   MOVE   WRK-KANA-SEI   TO  JINMEI-KANA
                   MOVE   WRK-KANJI-SEI  TO  JINMEI-KANJI
grpsys             MOVE   SPA-HOSPNUM    TO  JINMEI-HOSPNUM
grpsys*DD          MOVE    "JINMEI-KEY"    TO      ORC-DBPATH
grpsys             MOVE    "tbl_jinmei"    TO  MCP-TABLE
grpsys             MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM   910-JINMEI-SELECT-SEC
                   IF      FLG-JINMEI      =       ZERO
grpsys*DD              MOVE    "JINMEI-KEY"   TO      ORC-DBPATH
grpsys                 MOVE    "tbl_jinmei"    TO  MCP-TABLE
grpsys                 MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM  910-JINMEI-READ-SEC
                       IF    FLG-JINMEI   NOT =   ZERO
                           INITIALIZE   JINMEI-REC
                           MOVE   WRK-KANA-SEI   TO  JINMEI-KANA
                           MOVE   WRK-KANJI-SEI  TO  JINMEI-KANJI
grpsys                     MOVE   SPA-HOSPNUM    TO  JINMEI-HOSPNUM
                           MOVE   JINMEI-REC     TO  MCPDATA-REC
                           MOVE   "DBINSERT"     TO  MCP-FUNC
grpsys*DD                  MOVE   "JINMEI-KEY"   TO  ORC-DBPATH
grpsys                     MOVE    "tbl_jinmei"    TO  MCP-TABLE
grpsys                     MOVE    "key"           TO  MCP-PATHNAME
grpsys                     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                             MCPDATA-REC
grpsys                                             SPA-AREA
grpsys*DD                  CALL   "ORCMCPSUB"    USING
grpsys*DD                                        MCPAREA
grpsys*DD                                        ORCMCPAREA
grpsys*DD                                        MCPDATA-REC
                       END-IF
                   END-IF
grpsys*DD          MOVE     "JINMEI-KEY"          TO  ORC-DBPATH
grpsys             MOVE    "tbl_jinmei"    TO  MCP-TABLE
grpsys             MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM   900-TBL-CLOSE-SEC
               END-IF
               IF    WRK-KANA-MEI   NOT =  WRK-KANJI-MEI
                   MOVE    ZERO            TO      FLG-JINMEI
                   INITIALIZE                      JINMEI-REC
                   MOVE   WRK-KANA-MEI   TO  JINMEI-KANA
                   MOVE   WRK-KANJI-MEI  TO  JINMEI-KANJI
grpsys             MOVE   SPA-HOSPNUM    TO  JINMEI-HOSPNUM
grpsys*DD          MOVE    "JINMEI-KEY"    TO      ORC-DBPATH
grpsys             MOVE    "tbl_jinmei"    TO  MCP-TABLE
grpsys             MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM   910-JINMEI-SELECT-SEC
                   IF      FLG-JINMEI      =       ZERO
grpsys*DD              MOVE    "JINMEI-KEY"   TO      ORC-DBPATH
grpsys                 MOVE    "tbl_jinmei"    TO  MCP-TABLE
grpsys                 MOVE    "key"           TO  MCP-PATHNAME
                       PERFORM  910-JINMEI-READ-SEC
                       IF    FLG-JINMEI   NOT =   ZERO
                           INITIALIZE   JINMEI-REC
                           MOVE   WRK-KANA-MEI   TO  JINMEI-KANA
                           MOVE   WRK-KANJI-MEI  TO  JINMEI-KANJI
grpsys                     MOVE   SPA-HOSPNUM    TO  JINMEI-HOSPNUM
                           MOVE   JINMEI-REC     TO  MCPDATA-REC
                           MOVE   "DBINSERT"     TO  MCP-FUNC
grpsys*DD                  MOVE   "JINMEI-KEY"   TO  ORC-DBPATH
grpsys                     MOVE    "tbl_jinmei"    TO  MCP-TABLE
grpsys                     MOVE    "key"           TO  MCP-PATHNAME
grpsys                     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                             MCPDATA-REC
grpsys                                             SPA-AREA
grpsys*DD                  CALL   "ORCMCPSUB"    USING
grpsys*DD                                        MCPAREA
grpsys*DD                                        ORCMCPAREA
grpsys*DD                                        MCPDATA-REC
                       END-IF
                   END-IF
grpsys*DD          MOVE     "JINMEI-KEY"          TO  ORC-DBPATH
grpsys             MOVE    "tbl_jinmei"    TO  MCP-TABLE
grpsys             MOVE    "key"           TO  MCP-PATHNAME
                   PERFORM   900-TBL-CLOSE-SEC
               END-IF
           END-IF
           .
       2001-JINMEI-INS-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    OUTPUT      RECEERR-FILE
      *
           MOVE    WRK-RECEERR     TO  RECEERR-REC
           WRITE   RECEERR-REC
           CLOSE   RECEERR-FILE
      *
      *    ����ִ�����λ����
           MOVE    "JBE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-RECEERR     TO  JOB-YOBI
           MOVE    "9999"          TO  JOB-ERRCD
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
grpsys                                 SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           STOP RUN
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-TBL-CLOSE-SEC               SECTION.
      *
grpsys*DD  MOVE    WRK-DBPATH      TO  ORC-DBPATH
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA.
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
       900-TBL-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA.
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC.
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA.
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC.
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA.
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC.
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
grpsys     MOVE    LOW-VALUE       TO  MCP-TABLE.
grpsys     MOVE    LOW-VALUE       TO  MCP-PATHNAME.
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA.
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC.
      *
       900-DBDISCONNECT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"         USING
grpsys*DD                              MCPAREA
grpsys*DD                              ORCMCPAREA
grpsys*DD                              MCPDATA-REC
      *
           .
      *
       910-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ծ���ޥ���������
      *****************************************************************
       910-PTINF-SELECT-SEC        SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ծ���ޥ������ɹ�
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��̾�ޥ���������
      *****************************************************************
       910-JINMEI-SELECT-SEC        SECTION.
      *
           MOVE    JINMEI-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA.
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-JINMEI
           ELSE
               MOVE    1               TO  FLG-JINMEI
           END-IF
      *
           .
       910-JINMEI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��̾�ޥ������ɹ�
      *****************************************************************
       910-JINMEI-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"     USING   MCPAREA
grpsys                                     MCPDATA-REC
grpsys                                     SPA-AREA.
grpsys*DD  CALL    "ORCMCPSUB"          USING
grpsys*DD                               MCPAREA
grpsys*DD                               ORCMCPAREA
grpsys*DD                               MCPDATA-REC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JINMEI-REC
               MOVE    ZERO            TO  FLG-JINMEI
           ELSE
               INITIALIZE                  JINMEI-REC
               MOVE    1               TO  FLG-JINMEI
           END-IF
      *
           .
       910-JINMEI-READ-EXT.
           EXIT.