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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSMISYU.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ̤�����Խ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  10/08/16    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-PTTEIKIRRK          PIC 9(01).
           03  FLG-TARGET              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-KJNYMD              PIC X(08).
           03  WRK-SKYSTYMD            PIC X(08).
           03  WRK-SKYEDYMD            PIC X(08).
           03  WRK-MISYU-G.
               05  WRK-GAI-MISYU-P     PIC S9(9).
               05  WRK-GAI-MISYU-M     PIC S9(9).
               05  WRK-GAI-MISYU       PIC S9(9).
               05  WRK-NYU-MISYU       PIC S9(9).
           03  WRK-DENPNUM             PIC 9(7).
      *
       01  KEY-AREA.
           03  KEY-NYU-MISYU           OCCURS 2.
               05  KEY-RRKNUM          PIC 9(03).
               05  KEY-TAIINFLG        PIC 9(01).
               05  KEY-SRYYM           PIC X(06).
               05  KEY-KBT             PIC X(01).
           03  KEY-GAI-MISYU           OCCURS 2.
               05  KEY-SRYYMD          PIC X(08).
               05  KEY-DENPNUM         PIC 9(07).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ��Ǽ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    �����������
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *    �����ƥ����
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1039.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
       01  LNKSYU-AREA.
           COPY    "CPSYUNOU.INC"      REPLACING //SYU-//
                                       BY        //LNKSYU-//.
           COPY    "CPORCSMISYU.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           LNKSYU-AREA
           SMISYU-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
                                       KEY-AREA
      *
           INITIALIZE                  SYSKANRI-REC
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    "1039"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1039-REC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SYS-1039-MISYUKIKANKBN =   "1" )
               PERFORM 2101-MISYU-CHK-SEC
           ELSE
               PERFORM 2102-MISYU-CHK-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    �о�̤�������å����������ʿ����������Τߡ�
      *****************************************************************
       2101-MISYU-CHK-SEC              SECTION.
      *
           INITIALIZE                  WRK-MISYU-G
      *
           IF    ( LNKSYU-NYUGAIKBN    =   "1" )
               MOVE    LNKSYU-NYUIN-RRKNUM TO  KEY-RRKNUM (1)
               IF    ( LNKSYU-CREATEKBN    =   "1" )
                   MOVE    1               TO  KEY-TAIINFLG (1)
               ELSE
                   MOVE    LNKSYU-DENPNUM  TO  WRK-DENPNUM
                   PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
                   MOVE    PTTEIKIRRK-SRYYM
                                           TO  KEY-SRYYM (1)
                   MOVE    PTTEIKIRRK-KBT  TO  KEY-KBT (1)
               END-IF
      *
           ELSE
               IF    ( LNKSYU-GRP-DENPNUM  NOT =   ZERO )
                AND  ( SYS-1039-S02SKYSAIKBN   =   "1" )
      *
                   PERFORM 900-SYUNOU-KEY27-SEL-SEC
      *
                   MOVE    HIGH-VALUE      TO  KEY-GAI-MISYU (1)
                   PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
                       MOVE    SYU-SRYYMD  TO  KEY-SRYYMD (2)
                       MOVE    SYU-DENPNUM TO  KEY-DENPNUM (2)
      *
                       IF    ( KEY-GAI-MISYU (1) > KEY-GAI-MISYU (2))
                           MOVE    KEY-GAI-MISYU (2)
                                       TO  KEY-GAI-MISYU (1)
                       END-IF
      *
                       PERFORM 900-SYUNOU-KEY27-FET-SEC
      *
                   END-PERFORM
      *
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "key27"             TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               ELSE
                   MOVE    LNKSYU-SRYYMD       TO  KEY-SRYYMD (1)
                   MOVE    LNKSYU-DENPNUM      TO  KEY-DENPNUM (1)
               END-IF
           END-IF
      *
           PERFORM 900-SYUNOU-KEY66-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU      NOT =   ZERO )
      *
               MOVE    ZERO                TO  FLG-TARGET
      *
               IF    ( LNKSYU-NYUGAIKBN    =   "1" )
                   IF    ( SYU-NYUGAIKBN   =   "1" )
                       MOVE    SYU-NYUIN-RRKNUM TO  KEY-RRKNUM (2)
                       IF    ( SYU-CREATEKBN    =   "1" )
                           MOVE    1       TO  KEY-TAIINFLG (2)
                       ELSE
                           MOVE    SYU-DENPNUM
                                           TO  WRK-DENPNUM
                           PERFORM 900-PTTEIKIRRK-KEY6-SEL-SEC
                           MOVE    PTTEIKIRRK-SRYYM
                                           TO  KEY-SRYYM (2)
                           MOVE    PTTEIKIRRK-KBT
                                           TO  KEY-KBT (2)
                       END-IF
      *
                       IF    ( KEY-NYU-MISYU (1)
                                           >   KEY-NYU-MISYU (2))
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   ELSE
                       IF    ( LNKSYU-SKYEDYMD
                                           >=  SYU-SRYYMD )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   END-IF
               ELSE
                   IF    ( SYU-NYUGAIKBN   =   "1" )
                       IF    ( LNKSYU-SKYEDYMD  >=   SYU-SKYEDYMD )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   ELSE
                       MOVE    SYU-SRYYMD  TO  KEY-SRYYMD (2)
                       MOVE    SYU-DENPNUM TO  KEY-DENPNUM (2)
                       IF    ( KEY-GAI-MISYU (1)
                                           >   KEY-GAI-MISYU (2))
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   END-IF
               END-IF
      *
               IF    ( FLG-TARGET          =   1 )
                   PERFORM 220-MISYU-SUM-SEC
               END-IF
      *
               PERFORM 900-SYUNOU-KEY66-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key66"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2101-MISYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �о�̤�������å���������̤���оݡ�
      *****************************************************************
       2102-MISYU-CHK-SEC              SECTION.
      *
           INITIALIZE                  WRK-MISYU-G
      *
           PERFORM 900-SYUNOU-KEY66-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU      NOT =   ZERO )
      *
               PERFORM 220-MISYU-SUM-SEC
      *
               PERFORM 900-SYUNOU-KEY66-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key66"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2102-MISYU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ̤�����׽���
      *****************************************************************
       220-MISYU-SUM-SEC               SECTION.
      *
           IF    ( SYU-NYUGAIKBN   =   "1" )
               COMPUTE SMISYU-NYUIN
                   =   SMISYU-NYUIN
                   + ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
           ELSE
      *
               COMPUTE SMISYU-GAIRAI
                   =   SMISYU-GAIRAI
                   + ( SYU-SKYMONEY    -   SYU-NYUKIN-TOTAL )
      *
               IF    ( SYU-SKYMONEY    >   SYU-NYUKIN-TOTAL )
                   COMPUTE SMISYU-GAIRAI-P
                       =   SMISYU-GAIRAI-P
                       + ( SYU-SKYMONEY -  SYU-NYUKIN-TOTAL )
               ELSE
                   COMPUTE SMISYU-GAIRAI-M
                       =   SMISYU-GAIRAI-M
                       + ( SYU-SKYMONEY -  SYU-NYUKIN-TOTAL )
               END-IF
           END-IF
      *
           .
       220-MISYU-SUM-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                    SECTION.
      *
      *    PERFORM 800-DISPLAY-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    DISPLAY����
      *****************************************************************
       800-DISPLAY-SEC                 SECTION.
      *
           DISPLAY "SMISYU-NYUIN   [" SMISYU-NYUIN  "]"
           DISPLAY "SMISYU-GAIRAI  [" SMISYU-GAIRAI "]"
           DISPLAY "SMISYU-GAIRAI-P[" SMISYU-GAIRAI-P "]"
           DISPLAY "SMISYU-GAIRAI-M[" SMISYU-GAIRAI-M "]"
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ������������
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM     TO SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ����������򸡺�����
      *****************************************************************
       900-PTTEIKIRRK-KEY6-SEL-SEC  SECTION.
      *
           MOVE    ZERO            TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                  PTTEIKIRRK-REC
      *
           MOVE    LNKSYU-HOSPNUM  TO  PTTEIKIRRK-HOSPNUM
           MOVE    LNKSYU-PTID     TO  PTTEIKIRRK-PTID
           MOVE    WRK-DENPNUM     TO  PTTEIKIRRK-DENPNUM
           MOVE    PTTEIKIRRK-REC  TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk" TO MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  PTTEIKIRRK-REC
           ELSE
               INITIALIZE              PTTEIKIRRK-REC
               MOVE    1           TO  FLG-PTTEIKIRRK
           END-IF
      *
           MOVE    "tbl_ptteikirrk" TO MCP-TABLE
           MOVE    "key6"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTTEIKIRRK-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ��������
      *****************************************************************
       900-SYUNOU-KEY27-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    LNKSYU-AREA     TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key27"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY27-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ��������
      *****************************************************************
       900-SYUNOU-KEY27-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key27"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY27-FET-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ��������
      *****************************************************************
       900-SYUNOU-KEY66-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    LNKSYU-AREA     TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key66"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY66-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ��������
      *****************************************************************
       900-SYUNOU-KEY66-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key66"         TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC TO  SYUNOU-REC
           ELSE
               INITIALIZE              SYUNOU-REC
               MOVE    1           TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-KEY66-FET-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£գУģ��ԣŽ���
      *****************************************************************
       910-DBUPDATE-SEC                 SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
