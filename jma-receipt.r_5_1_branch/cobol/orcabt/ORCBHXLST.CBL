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
       PROGRAM-ID.             ORCBHXLST.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ģɼ
      *  ����ݡ��ͥ��̾  : �������ޥ�������ɽ�����ʰ���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/08/08    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL������   05.02.08  ORCHXLST���Խ�������ܹ�
      *  03.05.00    NACL-����    07/06/04  ���롼�׿����б�
      *  04.08.01    NACL-����    17/01/16  Ŭ����̾�б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
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
           COPY    "CPRECEERR.INC".
      *
      *    �����ΰ�
       01  STS-AREA.
           03  STS-RECEERR             PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
      *----(04.08.01)--ADD-START---
           03  FLG-CHKSND              PIC 9(01).
      *----(04.08.01)--ADD-END-----
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-JOB-UPDCNT                PIC 9(05).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM         PIC 9(02).
           03  WRK-PARA-CHKKBN         PIC X(01).
           03  WRK-PARA-SRYCD          PIC X(09).
           03  WRK-PARA-SRYCD-END      PIC X(09).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-RECEERR             PIC X(200).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *----(04.08.01)--ADD-START--
       01  CHKSND-REC.
           COPY    "CPCHKSND.INC".
      *----(04.08.01)--ADD-END----
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ��������
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    �����ѥǡ���
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *----(04.08.01)--ADD-START--
      *    ��������å��ޥ����Խ�����
           COPY    "CPORCSS013.INC".
      *
           COPY    "CPORCSS013.INC"    REPLACING
                                       //SS013-// BY //TSS013-//.
      *----(04.08.01)--ADD-END----
      *
      *    ���������Ѵ��ΰ�
           COPY    "CPORCHXLST.INC".
      *
      *    �����Ѵ�
           COPY    "CPORCSKANACHK.INC".
      *
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �����ģ¹�������
           COPY    "CPORCSPRT.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *    ����饤��Ģɼ̾��������ץ��̾�����ѥ�
           COPY    "CPORCSPRTNM.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
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
           INITIALIZE                  IDX-AREA
      *
           INITIALIZE                  RECEERR
      *
           MOVE    ZERO            TO  CNT-AREA
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
                                       WRK-PARA-HOSPNUM
                                       RECEERR
                                       WRK-PARA-CHKKBN
                                       WRK-PARA-SRYCD
                                       WRK-PARA-SRYCD-END
           END-UNSTRING
      *
      *    ����ͥ��å�
           PERFORM 110-SYOKI-SET-SEC
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    "ORCBHXLST"         TO  JOB-PGID
           MOVE    "�����å��ޥ�������������"
                                       TO  JOB-SHELLMSG
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  RECEERR
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ͥ��å�
      *****************************************************************
       110-SYOKI-SET-SEC           SECTION.
      *
      *    �ӣУ����ꥢ���åȽ���
           PERFORM 110-SPA-SET-SEC
      *
      *    �����ѥ�᥿���ꥢ���åȽ���
           PERFORM 110-ORCSPRTAREA-SET-SEC
      *
           .
       110-SYOKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ӣУ����ꥢ���åȽ���
      *****************************************************************
       110-SPA-SET-SEC                 SECTION.
      *
      *    �����ƥ����ե��å�
           INITIALIZE                      SPA-AREA
           MOVE    LNK-PRTKANRI-SKYYMD
                                       TO  SPA-SYSYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    LNK-PRTKANRI-TERMID TO  SPA-TERMID
      *
           .
       110-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    �����ѥ�᥿���ꥢ���åȽ���
      *****************************************************************
       110-ORCSPRTAREA-SET-SEC         SECTION.
      *
      *    �����ѥ�᥿�Խ�
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
           MOVE    "�����å��ޥ�������������"
                                       TO  SPRT-TITLE
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
      *
           .
       110-ORCSPRTAREA-SET-EXT.
           EXIT.
      *----(04.08.01)--UPD-START---
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           IF    ( WRK-PARA-CHKKBN =   "Y" )   OR
                 ( WRK-PARA-CHKKBN =   "Z" )
               GO  TO  200-MAIN-EXT
           END-IF
      *
           IF    ( WRK-PARA-CHKKBN =   "1" )   OR
                 ( WRK-PARA-CHKKBN =   "2" )
               PERFORM 2002-TEKBYO-START-SEC
           END-IF
      *
           INITIALIZE                      ORCHXLSTAREA
           MOVE    WRK-PARA-CHKKBN     TO  HXLST-CHKKBN
           MOVE    WRK-PARA-SRYCD      TO  HXLST-SRYCD
           MOVE    WRK-PARA-SRYCD-END  TO  HXLST-SRYCD-END
           MOVE    LNK-PRTKANRI-SKYYMD TO  HXLST-KJNYMD
           MOVE    "1"                 TO  HXLST-SRTKBN
           MOVE    "2"                 TO  HXLST-PROCKBN
           MOVE    ORCSPRTAREA         TO  HXLST-PRTAREA
           MOVE    TSS013-TEMP-NAME    TO  HXLST-TEMP-NAME
           MOVE    TSS013-TEMP-KEY     TO  HXLST-TEMP-KEY
      *
           CALL    "ORCHXLST"          USING
                                       SPA-AREA
                                       ORCHXLSTAREA
      *
           MOVE    HXLST-OUT-CNT       TO  CNT-JOB-UPDCNT
      *
           MOVE    1                   TO  FLG-END
      *
           IF    ( WRK-PARA-CHKKBN     =   "1" OR "2" )
               PERFORM 2001-TEKBYO-END-SEC
           END-IF
      *
             .
       200-MAIN-EXT.
           EXIT.
      *
      *----(04.08.01)--UPD-END-----
      *----(04.08.01)--ADD-START---
      *****************************************************************
      *    Ŭ����̾�ޥ�������ɽ���Ͻ���
      *****************************************************************
       2002-TEKBYO-START-SEC           SECTION.
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBCREATE"         TO  SS013-FUNC
           MOVE     WRK-PARA-CHKKBN    TO  SS013-CHKKBN
           MOVE     "2"                TO  SS013-CREATEKBN
           MOVE     LNK-PRTKANRI-SKYYMD
                                       TO  SS013-KJNYMD
           CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
           MOVE     SS013-AREA         TO  TSS013-AREA
      *
           PERFORM 900-CHKSND-KEY23-SEL-SEC
      *
           PERFORM UNTIL ( FLG-CHKSND  NOT =   ZERO )
      *
               INITIALIZE                      SS013-AREA
               MOVE     "DBINSERT"         TO  SS013-FUNC
               MOVE     WRK-PARA-CHKKBN    TO  SS013-CHKKBN
               MOVE     CHKSND-SRYCD       TO  SS013-SRYCD
               MOVE     LNK-PRTKANRI-SKYYMD
                                           TO  SS013-KJNYMD
               MOVE     TSS013-TEMP-NAME   TO  SS013-TEMP-NAME
               MOVE     TSS013-TEMP-KEY    TO  SS013-TEMP-KEY
               CALL     "ORCSS013"         USING
                                           SS013-AREA
                                           SPA-AREA
      *
               PERFORM 900-CHKSND-KEY23-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_chksnd"    TO  MCP-TABLE
           MOVE    "key23"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2002-TEKBYO-START-EXT.
           EXIT.
      *****************************************************************
      *    Ŭ����̾�ޥ�������ɽ��λ����
      *****************************************************************
       2001-TEKBYO-END-SEC             SECTION.
      *
           MOVE    "STM"       TO  SJOBKANRI-MODE
           INITIALIZE              JOBKANRI-REC
           PERFORM 800-ORCSJOB-SEC
      *
           INITIALIZE                      SS013-AREA
           MOVE     "DBDROP"           TO  SS013-FUNC
           CALL     "ORCSS013"         USING
                                       SS013-AREA
                                       SPA-AREA
      *
           .
       2001-TEKBYO-END-EXT.
           EXIT.
      *----(04.08.01)--ADD-END-----
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-JOB-UPDCNT            =   ZERO
               MOVE    "�����оݤΥǡ���������ޤ���Ǥ���"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBHXLST PAGE:" CNT-JOB-UPDCNT  " ***"
           DISPLAY "*** ORCBHXLST END ***"
      *     
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    CNT-JOB-UPDCNT  TO  JOB-UPDCNT
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           PERFORM 800-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
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
               MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID
                                       TO  JOB-SHELLID
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
               PERFORM 800-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END     
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *----(04.08.01)--ADD-START---
      *****************************************************************
      *    �����å��ޥ�������������
      *****************************************************************
       900-CHKSND-KEY23-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           INITIALIZE                      CHKSND-REC
           MOVE    SPA-HOSPNUM         TO  CHKSND-HOSPNUM
           MOVE    WRK-PARA-CHKKBN     TO  CHKSND-CHKKBN
           MOVE    "%"                 TO  CHKSND-SRYCD
           MOVE    "0"                 TO  CHKSND-CDKBN
           MOVE    LNK-PRTKANRI-SKYYMD TO  CHKSND-YUKOSTYMD
                                           CHKSND-YUKOEDYMD
           MOVE    CHKSND-REC          TO  MCPDATA-REC
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY23-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����å��ޥ�������������
      *****************************************************************
       900-CHKSND-KEY23-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHKSND
      *
           MOVE    "tbl_chksnd"        TO  MCP-TABLE
           MOVE    "key23"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  CHKSND-REC
           ELSE
               MOVE    1               TO  FLG-CHKSND
               INITIALIZE                  CHKSND-REC
           END-IF
      *
           .
       900-CHKSND-KEY23-FET-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH�ϹԤ�ʤ�)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ģţ̣ţԣŽ���
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ɣΣӣţңԽ���
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN" USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *----(04.08.01)--ADD-END-----
      *****************************************************************
      *    ����ִ�������
      *****************************************************************
       800-ORCSJOB-SEC                 SECTION.
      *
           MOVE    WRK-PARA-HOSPNUM    TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA 
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
      *
       800-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�������������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.