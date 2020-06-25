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
       PROGRAM-ID.             ORCGM02.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��̳����
      *  ����ݡ��ͥ��̾  : ��̳����ʣͣ�����
      *  ������            : 
      *  00/12/01    MCC��ƣ����   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     01/09/04  105 �ݸ��ԥޥ������ɲ�      
      *  01.01.01    MCC-¿��Ǽ   01/09/20  JOIN�ν���      
      *  01.01.02    MCC-ƣ��     01/09/27  MVER���ɲ�  
      *  01.02.01    MCC-ƣ��     01/11/21  ORCMCPSUB���ѹ�  
      *  01.02.02    MCC-¿��Ǽ   02/02/22  MID1���ɲ�
      *  01.02.03    MCC-����     02/06/20  106 ��̾����ޥ������ɲ�
      *  01.02.04    NACL-����    02/07/30  107 ���޾���ޥ������ɲ�
      *  01.02.05    NACL-��ƣ    04/02/05  M97���ɲ�
      *  01.02.06    NACL-����    04/08/04  108 ����ޥ������ɲ�
      *  01.02.07    NACL-¿��Ǽ  05/01/21  109 �إ�ץޥ����ɲ�
      *  01.02.08    NACL-��ƣ    06/02/22  MONFUNC �б�
      *  04.05.00    NACL-¿��Ǽ  10/02/05  �ץ饰��������ɲ�
      *  04.08.00    NACL-��ƣ    14/09/26  �ץ饰������̥�˥塼�ذ�ư
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ΰ�
           COPY    "M00COMMON-SPA".
      *   
           COPY    "ENUM-VALUE".
      *
       01  SPA-M02.
           03  SPA-M02-AREA.
               05  SPA-CTLSEQ          PIC 9(02).
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(02).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-GSEL    PIC 9(03).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  IDX                   PIC 9(03).
           03  WRK-SYSYMD.
               05  WRK-SYSYY         PIC 9(04).
               05  WRK-SYSMM         PIC 9(02).
               05  WRK-SYSDD         PIC 9(02).
           03  WRK-WSYSYMDH          PIC X(09).
      *
           03  WRK-WSYSYMD.
               05  WRK-WSGO         PIC X(01).
               05  WRK-WSYY         PIC 9(02).
               05  WRK-WSMM         PIC 9(02).
               05  WRK-WSDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-GNO             PIC X(03).
      *
           03  PRO-ID              PIC X(10).
           03  PID                 PIC X(10).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *    �����ƥ�����ޥ����ΰ�
      *    ��������
           COPY  "CPSK1010.INC".
      *    ���ŵ��ؾ���
           COPY  "CPSK1001.INC".
      *    ����������
           COPY  "CPSK2009.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *****COPY    "MCPAREA.INC".
           COPY    MCPAREA.
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "M00.INC".
           COPY    "M01.INC".
           COPY    "M01N.INC".
           COPY    "M02.INC". 
           COPY    "M95.INC".
           COPY    "MERR.INC".
           COPY    "MID1.INC".
           COPY    "MVER.INC".
           COPY    "M98.INC".
           COPY    "MUID.INC".
           COPY    "M99.INC".
           COPY    "MID2.INC".
           COPY    "MDAS.INC".
           COPY    "MLOG.INC".
           COPY    "MINF.INC".
           COPY    "MPRT.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-M00KYOUTU
           MOVE    SPA-SYS-1010-REC    TO  SYS-1010-REC
           MOVE    SPA-FREE        TO  SPA-M02
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
           MOVE    SYS-1010-REC    TO  SPA-SYS-1010-REC
           MOVE    SPA-M00KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-M02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "MERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
               PERFORM 510-GSRAUTH-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "M02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-M02-AREA
      *    ¾�Σ̣Ĥ��
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               PERFORM 320-SYS-1010-SPASET-SEC
               PERFORM 321-SYS-2009-SPASET-SEC
           END-IF
      *
           PERFORM 310-SPASET-SEC
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    1               TO  SPA-GMN-CUR
      *    ���ŵ��أɣ��Խ�
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC       TO  SYS-1001-REC
      *            �±�����
                   MOVE    SYS-1001-HOSPSBT  TO  SPA-HOSPSBT
      *
      *            �ǡ��������ե饰
                   IF     (SYS-1001-DATACREATEFLG  =   SPACE )  OR
                          (SYS-1001-DATACREATEFLG  NOT NUMERIC)
                       MOVE    ZERO          TO  SPA-DATACREATEFLG
                   ELSE
                       MOVE    SYS-1001-DATACREATEFLG
                                             TO  SPA-DATACREATEFLG
                   END-IF
      *            �ǡ��������ˡ��ʬ
                   IF     (SYS-1001-DATATEISYUTUKBN  =   SPACE )  OR
                          (SYS-1001-DATATEISYUTUKBN  NOT NUMERIC)
                       MOVE    ZERO          TO  SPA-DATATEISYUTUKBN
                   ELSE
                       MOVE    SYS-1001-DATATEISYUTUKBN
                                             TO  SPA-DATATEISYUTUKBN
                   END-IF
      *            �ǡ��������ɶ�ʬ
                   IF     (SYS-1001-DATAINFECTIONKBN  =   SPACE )  OR
                          (SYS-1001-DATAINFECTIONKBN  NOT NUMERIC)
                       MOVE    ZERO          TO  SPA-DATAINFECTIONKBN
                   ELSE
                       MOVE    SYS-1001-DATAINFECTIONKBN
                                             TO  SPA-DATAINFECTIONKBN
                   END-IF
               ELSE
                   INITIALIZE              SYS-1001-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       320-SYS-1010-SPASET-SEC              SECTION.
      *
      *
      *    ��������
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           STRING  SPA-OPID            DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    SYS-1010-TBL
           END-STRING
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1010-REC
               ELSE
                   INITIALIZE              SYS-1010-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC     TO  SYS-1010-REC
               IF      SPA-OPID            =   SYS-1010-USERID
                   MOVE    1                   TO  FLG-OK
                   MOVE    1                   TO  FLG-SYSKANRI
               ELSE
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key3"          TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    ��������ʤ�
           IF     FLG-OK               =   ZERO
               INITIALIZE                  SYS-1010-REC
           END-IF
      *
      *2001.9.27 �����ץ��ƥ��ȡʣУ�̤������
      **** MOVE    "0"                 TO  SYS-1010-GSRAUTH(3:1)
           MOVE    "0"                 TO  SYS-1010-GSRAUTH(4:1)
                                           SYS-1010-GSRAUTH(18:1)
                                           SYS-1010-GSRAUTH(26:1)
                                           SYS-1010-GSRAUTH(32:1)
                                           SYS-1010-GSRAUTH(41:1)
                                           SYS-1010-GSRAUTH(46:1)
                                           SYS-1010-GSRAUTH(47:1)
      *2001.9.27 �����ץ��ƥ��ȡʣУ�̤������END
      *
      *TEST �إ�פ��ɲä���ޤ�
      ***     MOVE    "1"                  TO SYS-1010-GSRAUTH(69:1)
      *TEST �إ�פ��ɲä���ޤ�
      *****MOVE    SYS-1010-REC         TO  SPA-WORK   
           .
       320-SYS-1010-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       321-SYS-2009-SPASET-SEC           SECTION.
      *
      *    ����������
           INITIALIZE                      SPA-SYUKA-INFO
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                  SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-2009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-2009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-2009-REC
                   MOVE    SYS-2009-SYUKA-KBN  TO  SPA-SYUKA-KBN
                   MOVE    SYS-2009-SYUKA-NYUIN
                                               TO  SPA-SYUKA-NYUIN
                   MOVE    SYS-2009-SYUKA-GAIRAI
                                               TO  SPA-SYUKA-GAIRAI
                   MOVE    SYS-2009-SYUKA-MODE TO  SPA-SYUKA-MODE
                   MOVE    SYS-2009-SYUKA-SELECT
                                               TO  SPA-SYUKA-SELECT
               ELSE
                   MOVE    "0"                 TO  SPA-SYUKA-KBN
               END-IF
           ELSE
               MOVE    "0"                 TO  SPA-SYUKA-KBN
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       321-SYS-2009-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    �ƶ�̳��ư��
               WHEN    "CLICKED"       ALSO    ANY
                   PERFORM 400-GYOMU-SEL-SEC
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
                   PERFORM 4101-KEY-ENT-SEC
           END-EVALUATE
      *
          .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϡ�����
      *****************************************************************
       4101-KEY-ENT-SEC            SECTION.
      *
           MOVE    M02-SELNUM       TO  SPA-GMN-GSEL
      *
           IF      SPA-GMN-GSEL    NOT =   ZERO
               IF      SYS-1010-GSRAUTH(1:1)     =  "0"
                   MOVE    "0002"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-GSEL        TO  WRK-GNO
                   PERFORM 420-GYOMU-START-SEC
               END-IF
           END-IF
           .
       4101-KEY-ENT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϡ�����
      *****************************************************************
       400-GYOMU-SEL-SEC            SECTION.
      *
           IF      MCP-WIDGET(1:1)     =   "G"
               MOVE   MCP-WIDGET(2:3)      TO  WRK-GNO
           END-IF
      *
           PERFORM 420-GYOMU-START-SEC
           .
       400-GYOMU-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��̳��ư������
      *****************************************************************
       420-GYOMU-START-SEC         SECTION.
      *
          MOVE    SPACE               TO  SPA-SAKIPG
          EVALUATE    WRK-GNO
      * �����ƥ�����ޥ���
               WHEN    "101"
                   IF      SYS-1010-GSRAUTH(61:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "W01"           TO  SPA-SAKIPG
                   END-IF
      * �����ޥ���
               WHEN    "102"
                   IF      SYS-1010-GSRAUTH(62:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "Z01"           TO  SPA-SAKIPG
                   END-IF
      * �����å��ޥ���
               WHEN    "103"
                   IF      SYS-1010-GSRAUTH(63:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "X01"           TO  SPA-SAKIPG
                   END-IF
      * �ݸ��ֹ�ޥ���
               WHEN    "104"
                   IF      SYS-1010-GSRAUTH(64:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "V01"           TO  SPA-SAKIPG
                   END-IF
      * �ݸ��ԥޥ���
               WHEN    "105"
                   IF      SYS-1010-GSRAUTH(65:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "N01"           TO  SPA-SAKIPG
                   END-IF
      * ��̾����ޥ���
               WHEN    "106"
                   IF      SYS-1010-GSRAUTH(66:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "N11"           TO  SPA-SAKIPG
                   END-IF
      *----(01.02.04)----START-----------------
      * ���޾���ޥ���
               WHEN    "107"
                   IF      SYS-1010-GSRAUTH(67:1)    =  "0"
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "N21"           TO  SPA-SAKIPG
                   END-IF
      *----(01.02.04)----END-------------------
      *----(01.02.06)----START-----------------
      * ����ޥ���
               WHEN    "108"
                   IF      SYS-1010-GSRAUTH(68:1)    =  "0" OR SPACE
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "N31"           TO  SPA-SAKIPG
                   END-IF
      *----(01.02.06)----END-------------------
      * �إ�ץޥ���
               WHEN    "109"
                   IF      SYS-1010-GSRAUTH(69:1)    =  "0" OR SPACE
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "N41"           TO  SPA-SAKIPG
                   END-IF
              WHEN    OTHER
                   MOVE    "0001"          TO  SPA-ERRCD
           END-EVALUATE.
      *
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  LINKAREA
      *
               IF      SPA-SAKIPG      NOT =  SPACE
                   MOVE    SPACE               TO  SPA-MOTOPG2
                   MOVE    "M02"               TO  SPA-MOTOPG
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
                   PERFORM 900-PUT-WINDOW
      *
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .     
      *
       420-GYOMU-START-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "M02"               TO  SPA-MOTOPG
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    "M01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           PERFORM 510-GSRAUTH-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "M02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                  M02
      *      *
           IF      SYS-1010-GSRAUTH(1:1)    =  "0"
               CONTINUE
           ELSE
               MOVE    SPA-GMN-GSEL        TO  M02-SELNUM
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��̳�������������
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *****MOVE    SPA-WORK            TO  SYS-1010-REC
      *
      *----(01.02.06)----START-----------------
      *    PERFORM VARYING IDX FROM    1   BY  1
      *            UNTIL   IDX >       10
      *        MOVE    WIDGET-INSENSITIVE      TO  M02-STATE-O (IDX)
      *    END-PERFORM           
      *----(01.02.06)----END-------------------
           MOVE    WIDGET-INSENSITIVE      TO  M02-G101-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M02-G102-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M02-G103-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M02-G104-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M02-G105-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M02-G106-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M02-G107-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M02-G108-STATE
           MOVE    WIDGET-INSENSITIVE      TO  M02-G109-STATE
      *
           IF      SYS-1010-GSRAUTH(1:1)    =  "0"
               CONTINUE
           ELSE 
               IF      SYS-1010-GSRAUTH(61:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G101-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(62:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G102-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(63:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G103-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(64:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G104-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(65:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G105-STATE
               END-IF
               IF      SYS-1010-GSRAUTH(66:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G106-STATE
               END-IF
      *----(01.02.04)----START-----------------
               IF      SYS-1010-GSRAUTH(67:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G107-STATE
               END-IF
      *----(01.02.04)----END-------------------
      *----(01.02.06)----START-----------------
               IF      SYS-1010-GSRAUTH(68:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G108-STATE
               END-IF
      *----(01.02.06)----END-------------------
               IF      SYS-1010-GSRAUTH(69:1)   =  "1"
                   MOVE    WIDGET-NORMAL      TO  M02-G109-STATE
               END-IF
           END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE  "SELNUM"    TO  MCP-WIDGET
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
                   MOVE    "���ϥ��顼"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "���ζ�̳������Ǥ��ޤ���"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  MERR
           MOVE    SPA-ERRCD            TO  MERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  MERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "M02"                TO  SPA-MOTOPG
           MOVE    "MERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "MERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ����ɤ߹���
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"         TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *