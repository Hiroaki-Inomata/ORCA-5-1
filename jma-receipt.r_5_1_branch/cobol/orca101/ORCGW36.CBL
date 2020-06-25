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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCGW36.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ƥ������������
      *  ����ݡ��ͥ��̾  : ���Ծ������ξ���ʣף�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  12/12/14    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ƥ�������������Ѷ��̥ѥ�᥿
           COPY    "W01COMMON-SPA".
      *
      *    �����ѣӣУ�
       01  SPA-W36.
           03  SPA-W36-AREA.
               05  SPA-GMN-CUR             PIC 9(02).
               05  SPA-GMN-SYORI           PIC 9(01).
      *
               05  SPA-GMN-AREA.
      *            �ݸ���ǧ��
                   07  SPA-GMN-HKNCHK          PIC X(01).
      *            ��⣱
                   07  SPA-GMN-MEMO1           PIC X(01).
      *            ��⣲
                   07  SPA-GMN-MEMO2           PIC X(01).
      *
      *    �ե饰���ꥢ
       01  FLG-AREA.
           03  FLG-END                            PIC 9(01).
           03  FLG-SYSKANRI                       PIC 9(01).
      *
           03  FLG-OK                             PIC 9(01).
           03  FLG-SYORI-END                      PIC 9(01).
           03  FLG-TOUROKU                        PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-WIDMSG          PIC X(70).
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-YMD             PIC X(09).
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
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY  "CPSYSKANRI.INC".
      *    ���Ծ������ξ���
           COPY  "CPSK1920.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *    ���ѥ����å��ѥ�᥿
      *    COPY    "CPORCSKANACHK.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
      *
       PROCEDURE                                 DIVISION    USING
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-W01KYOUTU
           MOVE    SPA-FREE            TO  SPA-W36
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
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
           MOVE    SPA-W36             TO  SPA-FREE
           MOVE    SPA-W01KYOUTU       TO  SPA-WORK 
           MOVE    SPA-AREA            TO  SPA-COMMON
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
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
      *        �����Խ�
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "W36"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    ¾�Σ̣Ĥ��
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "WID1"
                   PERFORM 330-WID1-SET-SEC
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-W36
           INITIALIZE                  SPA-W36-AREA
      *
      *    MOVE    SPA-W01-STYUKYMDH   TO  SPA-GMN-STYUKYMD
      *    MOVE    SPA-W01-EDYUKYMDH   TO  SPA-GMN-EDYUKYMD
      *
      *    ���Ծ������ξ���
           MOVE    SPACE               TO  SYS-1920-REC
           INITIALIZE                      SYS-1920-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1920-HOSPNUM
           MOVE    "1920"              TO  SYS-1920-KANRICD
           MOVE    "*"                 TO  SYS-1920-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1920-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1920-EDYUKYMD
           MOVE    SYS-1920-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1920-REC
           ELSE
               INITIALIZE                      SYS-1920-REC
           END-IF
      *
      *    �ݸ���ǧ��
           IF      SYS-1920-POPUP-HKNCHK   =   "1"
               MOVE    "T"                 TO  SPA-GMN-HKNCHK
           ELSE
               MOVE    "F"                 TO  SPA-GMN-HKNCHK
           END-IF
      *    ��⣱
           IF      SYS-1920-POPUP-MEMO1    =   "1"
               MOVE    "T"                 TO  SPA-GMN-MEMO1
           ELSE
               MOVE    "F"                 TO  SPA-GMN-MEMO1
           END-IF
           IF      SYS-1920-POPUP-MEMO2    =   "1"
               MOVE    "T"                 TO  SPA-GMN-MEMO2
           ELSE
               MOVE    "F"                 TO  SPA-GMN-MEMO2
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ���̡ʣãɣģ��ˣϣ˽���
      *****************************************************************
       330-WID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-WIDCD
      *        ��Ͽ
               WHEN    "1001"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   END-IF
      *        ���
               WHEN    "1002"
                   IF      SPA-WID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-WID1-FLG
           MOVE    SPACE           TO  SPA-WIDCD      
           .
       330-WID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO      MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"             TO  MCP-PUTTYPE
                   PERFORM  210-BACK
      *        ��Ͽ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
      *        ���
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        ���ꥢ
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  402-CLEAR-SEC
      *
               WHEN    "CLICKED"       ALSO    "HKNCHK"
      *            �ݸ���ǧ
                   PERFORM 410-HKNCHK-CHK-SEC
               WHEN    "CLICKED"       ALSO    "MEMO1"
      *            ��⣱
                   PERFORM 410-MEMO1-CHK-SEC
               WHEN    "CLICKED"       ALSO    "MEMO2"
      *            ��⣲
                   PERFORM 410-MEMO2-CHK-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                       SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ���ǧ�����å�����
      *****************************************************************
       410-HKNCHK-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-HKNCHK
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-HKNCHK
               WHEN    "F"
                   MOVE    "T"             TO  SPA-GMN-HKNCHK
           END-EVALUATE
           MOVE    2                   TO  SPA-GMN-CUR
           .
       410-HKNCHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��⣱�����å�����
      *****************************************************************
       410-MEMO1-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-MEMO1
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-MEMO1
               WHEN    "F"
                   MOVE    "T"             TO  SPA-GMN-MEMO1
           END-EVALUATE
           MOVE    3                   TO  SPA-GMN-CUR
           .
       410-MEMO1-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��⣱�����å�����
      *****************************************************************
       410-MEMO2-CHK-SEC       SECTION.
      *
           EVALUATE    SPA-GMN-MEMO2
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-MEMO2
               WHEN    "F"
                   MOVE    "T"             TO  SPA-GMN-MEMO2
           END-EVALUATE
           MOVE    99                  TO  SPA-GMN-CUR
           .
       410-MEMO2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    ���̤�ӣУ����å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *    ���ܥ����å�����
           PERFORM 4102-KIHON-CHK-SEC
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̤�ӣУ����åȽ���
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ܥ����å�����
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "W36"               TO  SPA-MOTOPG
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
      *    ���ꥢ����
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           PERFORM 310-SPASET-SEC
           MOVE    1                   TO  SPA-GMN-CUR
           .
       402-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    ��Ͽ��ǧ����ɽ������
      *****************************************************************
       412-TOUROKU-SEC                               SECTION.
      *
           MOVE    "1001"              TO  SPA-WIDCD
      *
           .
       412-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       ��Ͽ����
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      * 
           MOVE    SPACE               TO  SYS-1920-REC
           INITIALIZE                      SYS-1920-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1920-HOSPNUM
           MOVE    "1920"              TO  SYS-1920-KANRICD
           MOVE    "*"                 TO  SYS-1920-KBNCD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1920-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1920-EDYUKYMD
           MOVE    SYS-1920-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1920-REC
           END-IF
      *
      *    ����
           IF      FLG-SYSKANRI        =   1
               MOVE    SPACE               TO  SYS-1920-REC
               INITIALIZE                      SYS-1920-REC
               MOVE    SPA-HOSPNUM         TO  SYS-1920-HOSPNUM
               MOVE    "1920"              TO  SYS-1920-KANRICD
               MOVE    "*"                 TO  SYS-1920-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1920-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD    TO  SYS-1920-EDYUKYMD
           END-IF
      *
           IF      SPA-GMN-HKNCHK      =   "T"
               MOVE    "1"                 TO  SYS-1920-POPUP-HKNCHK
           ELSE
               MOVE    "0"                 TO  SYS-1920-POPUP-HKNCHK
           END-IF
           IF      SPA-GMN-MEMO1       =   "T"
               MOVE    "1"                 TO  SYS-1920-POPUP-MEMO1
           ELSE
               MOVE    "0"                 TO  SYS-1920-POPUP-MEMO1
           END-IF
           IF      SPA-GMN-MEMO2       =   "T"
               MOVE    "1"                 TO  SYS-1920-POPUP-MEMO2
           ELSE
               MOVE    "0"                 TO  SYS-1920-POPUP-MEMO2
           END-IF
      *
           IF      FLG-SYSKANRI            =   ZERO
      *        ����
               MOVE    SPA-HOSPNUM         TO  SYS-1920-UPD-HOSPNUM
               MOVE    SYS-KANRICD         TO  SYS-1920-UPD-KANRICD
               MOVE    SYS-KBNCD           TO  SYS-1920-UPD-KBNCD
               MOVE    SPA-W01-STYUKYMD    TO  SYS-1920-UPD-STYUKYMD
               MOVE    SPA-W01-EDYUKYMD    TO  SYS-1920-UPD-EDYUKYMD
      *
               MOVE    SPA-OPID            TO  SYS-1920-OPID
               MOVE    SMCNDATE-YMD        TO  SYS-1920-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1920-UPHMS
               MOVE    SYS-1920-REC        TO  MCPDATA-REC
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "W36 SYSKANRI UPD ERR:" MCP-RC
                            ",KEY:" SYS-1920-KEY
                   MOVE    "1002"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SPA-OPID            TO  SYS-1920-OPID
               MOVE    SMCNDATE-YMD        TO  SYS-1920-UPYMD
               MOVE    SMCNDATE-YMD        TO  SYS-1920-CREYMD
               MOVE    SMCNDATE-HMS        TO  SYS-1920-UPHMS
               MOVE    SYS-1920-REC        TO  MCPDATA-REC
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "W36 SYSKANRI INS ERR:" MCP-RC
                            ",KEY:" SYS-1920-KEY
                   MOVE    "1001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK 
           END-IF
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *       ���Ϲ��ܤδ�Ϣ�����å�����
      *****************************************************************
       4901-KANRENCHK-SEC           SECTION.
      *
      *
           .
       4901-KANRENCHK-EXT.
           EXIT.
      *****************************************************************
      *    �����ǧ����ɽ������
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           MOVE    "1002"              TO  SPA-WIDCD
      *
           .
       403-DEL-EXT.
           EXIT.
      *****************************************************************
      *    �������
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  SYS-1920-REC
           INITIALIZE                      SYS-1920-REC
           MOVE    "1920"              TO  SYS-1920-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1920-STYUKYMD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-1920-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1920-HOSPNUM
           MOVE    SYS-1920-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1003"                  TO  SPA-ERRCD
               DISPLAY "SYSKANRI-KEY =>" SYS-1920-KEY 
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE   "JOIN"               TO  MCP-PUTTYPE
               PERFORM 210-BACK
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���ե����å����Խ�����
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
           EVALUATE  WRK-YMD(1:8)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG
               WHEN  "99999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
                   MOVE  WRK-YMD(1:8)            TO  WRK-HENYMDG  
               WHEN    OTHER
                   IF      WRK-YMD  (1:7)  NOT NUMERIC
                       INSPECT     WRK-YMD      REPLACING   ALL " "
                                                            BY  "0"
                       MOVE    WRK-YMD(1:1)      TO  WRK-WGO
                       MOVE    WRK-YMD(2:2)      TO  WRK-WYY
                       MOVE    WRK-YMD(5:2)      TO  WRK-WMM
                       MOVE    WRK-YMD(8:2)      TO  WRK-WDD
                   ELSE
                       MOVE    WRK-YMD(1:7)      TO  WRK-WYMD 
                   END-IF
      *
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY1-AREA
                   MOVE    "12"                TO  LNK-DAY1-IRAI
                   MOVE    ZERO                TO  LNK-DAY1-YMD
                   MOVE    WRK-WYMD            TO  LNK-DAY1-YMD(2:7)
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY1-AREA
                   IF      STS-DAY-RC1     NOT =   ZERO
                       MOVE    "0002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
                       PERFORM 5002-HIZUKE-HEN-SEC
                   END-IF
           END-EVALUATE        
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Ѵ��Խ�����
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   ZERO
               MOVE    WRK-SYMD (1:8)      TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
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
           IF      SPA-WIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "W36    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  W36
           INITIALIZE                      W36
      *
           MOVE    SPA-GMN-HKNCHK      TO  W36-HKNCHK
           MOVE    SPA-GMN-MEMO1       TO  W36-MEMO1
           MOVE    SPA-GMN-MEMO2       TO  W36-MEMO2
      *
      *    ��̳�������������
      **** PERFORM 510-GSRAUTH-SEC
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
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
                WHEN   1
                    MOVE  "HKNCHK"         TO  MCP-WIDGET
                WHEN   2
                    MOVE  "MEMO1"          TO  MCP-WIDGET
                WHEN   3
                    MOVE  "MEMO2"          TO  MCP-WIDGET
                 WHEN   99
                    MOVE  "B12"            TO  MCP-WIDGET
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "���ϥ��顼"            TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "��Ͽ�������Ǥ��ޤ���Ǥ���"
                                                      TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "�����������Ǥ��ޤ���Ǥ���"
                                                      TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "����������Ǥ��ޤ���Ǥ���"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WERR
           MOVE    SPA-ERRCD            TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  WERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "W36"                TO  SPA-MOTOPG
           MOVE    "WERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ɥ�å�����ɽ��
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-WIDMSG
      *
           EVALUATE    SPA-WIDCD
               WHEN    "1001"
                   MOVE    "��Ͽ�����򤷤ޤ�����������Ǥ�����"
                                               TO  WRK-WIDMSG
               WHEN    "1002"
                   MOVE    "��������򤷤ޤ�����������Ǥ�����"
                                               TO  WRK-WIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WID1
           INITIALIZE                       WID1
           MOVE    SPA-WIDCD            TO  WID1-ID1CODE
           MOVE    WRK-WIDMSG           TO  WID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
           EVALUATE    SPA-WIDCD
               WHEN    "1002"
                   MOVE    "B01"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "W36"                TO  SPA-MOTOPG
           MOVE    "WID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "WID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ������ɹ��ʥ�����
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɤ߹��߽���
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
