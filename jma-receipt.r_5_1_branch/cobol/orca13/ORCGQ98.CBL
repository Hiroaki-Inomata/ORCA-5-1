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
       PROGRAM-ID.                     ORCGQ98.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ԾȲ�
      *  ����ݡ��ͥ��̾  : ���ս�����̡ʣѣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/04/30    MCC�����ġ�   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.02     MCC-����     02/06/26  ����ִ����ģ��ɲ�
      *                                     ���̣ѣɣģ���
      *                                     �ѣ�������̾���ѹ�
      *                                     �����ˣѣɣģ����ɲ�
      * 03.05.00     NACL-����    07.05.23  ���롼�׿����б�
      * 04.05.00     NACL-¿��Ǽ  09.12.21  ���긡����͹���ֹ��Ϣ��
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
       WORKING-STORAGE                 SECTION.
      *
      *    ���̥�������
           COPY    "ENUM-VALUE".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ѣӣУ�
           COPY    "Q01SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                             PIC 9(01).
           03  FLG-EOF                             PIC 9(01).
           03  FLG-SYSKANRI                        PIC 9(01).
           03  FLG-ADRS                            PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                                PIC 9(05).
           03  IDX2                                PIC 9(05).
           03  IDX3                                PIC 9(05).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-SYSKANRI                        PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-ZZZ9                            PIC ZZZ9.
      *
       01  WRK-WIDGET-AREA.
           03  WRK-WIDGET                          PIC X(20).
      *
           03  WRK-WIDGET-R1   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R1-DAMMY1            PIC X(02).
               05  WRK-WIDGET1                     PIC 9(01).
               05  WRK-WIDGET-R1-DAMMY2            PIC X(17).
      *
           03  WRK-WIDGET-R2   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R2-DAMMY1            PIC X(03).
               05  WRK-WIDGET2                     PIC 9(01).
               05  WRK-WIDGET-R2-DAMMY2            PIC X(16).
      *
           03  WRK-WIDGET-R3   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R3-DAMMY1            PIC X(04).
               05  WRK-WIDGET3                     PIC 9(01).
               05  WRK-WIDGET-R3-DAMMY2            PIC X(15).
      *
           03  WRK-WIDGET-R4   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R4-DAMMY1            PIC X(05).
               05  WRK-WIDGET4                     PIC 9(01).
               05  WRK-WIDGET-R4-DAMMY2            PIC X(14).
      *
           03  WRK-WIDGET-R5   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R5-DAMMY1            PIC X(06).
               05  WRK-WIDGET5                     PIC 9(01).
               05  WRK-WIDGET-R5-DAMMY2            PIC X(13).
      *
           03  WRK-WIDGET-R6   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R6-DAMMY1            PIC X(07).
               05  WRK-WIDGET6                     PIC 9(01).
               05  WRK-WIDGET-R6-DAMMY2            PIC X(12).
      *
           03  WRK-WIDGET-R7   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R7-DAMMY1            PIC X(08).
               05  WRK-WIDGET7                     PIC 9(01).
               05  WRK-WIDGET-R7-DAMMY2            PIC X(11).
      *
           03  WRK-WIDGET-R8   REDEFINES   WRK-WIDGET.
               05  WRK-WIDGET-R8-DAMMY1            PIC X(08).
               05  WRK-WIDGET8                     PIC 9(02).
               05  WRK-WIDGET-R8-DAMMY2            PIC X(10).
      *
      *    �������ΰ�
      *01  CONST-AREA.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ����ʼ���Į������
           COPY    "CPSK1015.INC".
      *
      *    ����ޥ�����
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *    �ģ¸���
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA13SCRAREA.INC".
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
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Q01FREE
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
           IF  ( FLG-END           =   ZERO )
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Q01FREE     TO  SPA-FREE
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
                                       IDX-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
      *    ���顼���̤��
           IF  ( SPA-MOTOPG            =   "QERR" )
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "Q98"               TO  MCP-WINDOW
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
           PERFORM 310-SPASET-SEC
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
      *
           MOVE    SPACE           TO  SPA-Q98
           INITIALIZE                  SPA-Q98
      *
           MOVE    SPACE           TO  SPA-Q01-Q98-JYUSHO
      *
      *    �����Խ�����
           PERFORM 3101-ADRS-EDIT-SEC
      *
      *    �������륭���������
           MOVE   1                TO  SPA-GMN-Q98-CUR
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       3101-ADRS-EDIT-SEC          SECTION.
      *
           INITIALIZE              SPA-GMN-Q98-ADRS-TBL
                                   SPA-GMN-Q98-SYSKANRI-AREA
      *
      *    ���ս��긡�������ʥ����ƥ�����ޥ�����긡����
           INITIALIZE              SYS-1015-REC
           MOVE  "1015"            TO  SYS-1015-KANRICD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
      *
      *    �����ƥ�����ޥ����ɤ����Ф�����
           PERFORM 910-SYSKANRI-READ-SKIP-SEC
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL ( FLG-SYSKANRI    =   1 )
                OR   ( IDX1            >   60 )
      *
               COMPUTE CNT-SYSKANRI    =   CNT-SYSKANRI    +   1
      *
               MOVE    SYS-1015-KBNCD
                                   TO SPA-GMN-Q98-TADRS (IDX1) (1:3)
               MOVE    SYS-1015-TOWNNAMEDSP
                                   TO SPA-GMN-Q98-TADRS (IDX1) (5:16)
      *
               MOVE    SYS-1015-KBNCD     TO SPA-GMN-Q98-TKBNCD (IDX1)
               MOVE    SYS-1015-LPUBCD    TO SPA-GMN-Q98-TLPUBCD(IDX1)
               MOVE    SYS-1015-POST      TO SPA-GMN-Q98-TPOST  (IDX1)
               MOVE    SYS-1015-RENNUM    TO SPA-GMN-Q98-TRENNUM(IDX1)
      *
      *        �����ɹ�����
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    FLG-SYSKANRI        TO  SPA-GMN-Q98-FLG-SYSKANRI
      *
           .
       3101-ADRS-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    ���ꥢ
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 210-CLEAR
      *    ����
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 210-MAE-PAGE-SEC
      *    ����
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 210-TUGI-PAGE-SEC
      *    ����
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI
      *    ���ս���
               WHEN    "CLICKED"       ALSO    "TADRSCHK01"
               WHEN    "CLICKED"       ALSO    "TADRSCHK02"
               WHEN    "CLICKED"       ALSO    "TADRSCHK03"
               WHEN    "CLICKED"       ALSO    "TADRSCHK04"
               WHEN    "CLICKED"       ALSO    "TADRSCHK05"
               WHEN    "CLICKED"       ALSO    "TADRSCHK06"
               WHEN    "CLICKED"       ALSO    "TADRSCHK07"
               WHEN    "CLICKED"       ALSO    "TADRSCHK08"
               WHEN    "CLICKED"       ALSO    "TADRSCHK09"
               WHEN    "CLICKED"       ALSO    "TADRSCHK10"
               WHEN    "CLICKED"       ALSO    "TADRSCHK11"
               WHEN    "CLICKED"       ALSO    "TADRSCHK12"
               WHEN    "CLICKED"       ALSO    "TADRSCHK13"
               WHEN    "CLICKED"       ALSO    "TADRSCHK14"
               WHEN    "CLICKED"       ALSO    "TADRSCHK15"
               WHEN    "CLICKED"       ALSO    "TADRSCHK16"
               WHEN    "CLICKED"       ALSO    "TADRSCHK17"
               WHEN    "CLICKED"       ALSO    "TADRSCHK18"
               WHEN    "CLICKED"       ALSO    "TADRSCHK19"
               WHEN    "CLICKED"       ALSO    "TADRSCHK20"
               WHEN    "CLICKED"       ALSO    "TADRSCHK21"
               WHEN    "CLICKED"       ALSO    "TADRSCHK22"
               WHEN    "CLICKED"       ALSO    "TADRSCHK23"
               WHEN    "CLICKED"       ALSO    "TADRSCHK24"
               WHEN    "CLICKED"       ALSO    "TADRSCHK25"
               WHEN    "CLICKED"       ALSO    "TADRSCHK26"
               WHEN    "CLICKED"       ALSO    "TADRSCHK27"
               WHEN    "CLICKED"       ALSO    "TADRSCHK28"
               WHEN    "CLICKED"       ALSO    "TADRSCHK29"
               WHEN    "CLICKED"       ALSO    "TADRSCHK30"
               WHEN    "CLICKED"       ALSO    "TADRSCHK31"
               WHEN    "CLICKED"       ALSO    "TADRSCHK32"
               WHEN    "CLICKED"       ALSO    "TADRSCHK33"
               WHEN    "CLICKED"       ALSO    "TADRSCHK34"
               WHEN    "CLICKED"       ALSO    "TADRSCHK35"
               WHEN    "CLICKED"       ALSO    "TADRSCHK36"
               WHEN    "CLICKED"       ALSO    "TADRSCHK37"
               WHEN    "CLICKED"       ALSO    "TADRSCHK38"
               WHEN    "CLICKED"       ALSO    "TADRSCHK39"
               WHEN    "CLICKED"       ALSO    "TADRSCHK40"
               WHEN    "CLICKED"       ALSO    "TADRSCHK41"
               WHEN    "CLICKED"       ALSO    "TADRSCHK42"
               WHEN    "CLICKED"       ALSO    "TADRSCHK43"
               WHEN    "CLICKED"       ALSO    "TADRSCHK44"
               WHEN    "CLICKED"       ALSO    "TADRSCHK45"
               WHEN    "CLICKED"       ALSO    "TADRSCHK46"
               WHEN    "CLICKED"       ALSO    "TADRSCHK47"
               WHEN    "CLICKED"       ALSO    "TADRSCHK48"
               WHEN    "CLICKED"       ALSO    "TADRSCHK49"
               WHEN    "CLICKED"       ALSO    "TADRSCHK50"
               WHEN    "CLICKED"       ALSO    "TADRSCHK51"
               WHEN    "CLICKED"       ALSO    "TADRSCHK52"
               WHEN    "CLICKED"       ALSO    "TADRSCHK53"
               WHEN    "CLICKED"       ALSO    "TADRSCHK54"
               WHEN    "CLICKED"       ALSO    "TADRSCHK55"
               WHEN    "CLICKED"       ALSO    "TADRSCHK56"
               WHEN    "CLICKED"       ALSO    "TADRSCHK57"
               WHEN    "CLICKED"       ALSO    "TADRSCHK58"
               WHEN    "CLICKED"       ALSO    "TADRSCHK59"
               WHEN    "CLICKED"       ALSO    "TADRSCHK60"
                   MOVE    MCP-WIDGET   TO  WRK-WIDGET
                   PERFORM 490-JYUSHO-SET-SEC
               END-EVALUATE
      *
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    �����ֹ�����å�
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   MOVE    MCP-WIDGET  TO  WRK-WIDGET
                   PERFORM 4100-SELNUM-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ�����å�
      *****************************************************************
       4100-SELNUM-CHK-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-Q98-CUR
      *
           MOVE    Q98-SELNUM          TO  SPA-GMN-Q98-SELNUM
      *
      *    ̤���ϻ��ϥ����å���Ԥ�ʤ�
           IF  ( SPA-GMN-Q98-SELNUM    =   ZERO )
               GO  TO  4100-SELNUM-CHK-EXT
           END-IF
      *
           SET     SPA-Q98-TBLIDX1     TO  1
      *
           SEARCH  SPA-GMN-Q98-SYSKANRI-OCC
                                       VARYING SPA-Q98-TBLIDX1
           AT  END
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           WHEN
               ( SPA-GMN-Q98-TKBNCD (SPA-Q98-TBLIDX1)
                                       =   SPACE )
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO  4100-SELNUM-CHK-EXT
           WHEN
               ( SPA-GMN-Q98-TKBNCD (SPA-Q98-TBLIDX1) 
                                       =   SPA-GMN-Q98-SELNUM )
      *
               SET    IDX1             TO   SPA-Q98-TBLIDX1
      *
      *        ���꽻���Խ�����
               PERFORM 4100-KAKUTEI-JYUSHO-EDIT-SEC
           END-SEARCH
      *
      *
           MOVE    1                   TO  SPA-GMN-Q98-CUR
      *
           .
       4100-SELNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���꽻���Խ�����
      *****************************************************************
       4100-KAKUTEI-JYUSHO-EDIT-SEC        SECTION.
      *
           MOVE    SPACE       TO  SPA-GMN-Q98-POST
                                   SPA-GMN-Q98-ADRS
      *
      *    ����ޥ�������
           INITIALIZE                            ADRS-REC
           MOVE  SPA-HOSPNUM                 TO  ADRS-HOSPNUM
      **** MOVE  SPA-GMN-Q98-TLPUBCD(IDX1)   TO  ADRS-LPUBCD
           MOVE  SPA-GMN-Q98-TPOST  (IDX1)   TO  ADRS-POST
           MOVE  SPA-GMN-Q98-TRENNUM(IDX1)   TO  ADRS-EDANUM
      *
           PERFORM 900-ADRS-KEY-SEL-SEC
           IF  ( FLG-ADRS      =   ZERO )
               MOVE  ADRS-POST           TO  SPA-GMN-Q98-POST
               MOVE  ADRS-EDITADRS-NAME  TO  SPA-GMN-Q98-ADRS
           END-IF
      *
           .
       4100-KAKUTEI-JYUSHO-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q98"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "Q01"               TO  MCP-WINDOW
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
      *    ���ꥢ������
      *****************************************************************
       210-CLEAR                   SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-Q98-SELNUM
      *
           MOVE    SPACE               TO  SPA-GMN-Q98-POST
           MOVE    SPACE               TO  SPA-GMN-Q98-ADRS
      *
           .
       210-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǡ�����
      *****************************************************************
       210-MAE-PAGE-SEC                SECTION.
      *
           IF  ( SPA-GMN-Q98-SKIP-KNS          >   0 )
               COMPUTE SPA-GMN-Q98-SKIP-KNS    =   SPA-GMN-Q98-SKIP-KNS
                                               -   60
               PERFORM 3101-ADRS-EDIT-SEC
               MOVE    ZERO            TO  SPA-GMN-Q98-SELNUM
           END-IF
      *
           .
       210-MAE-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    ���ǡ�����
      *****************************************************************
       210-TUGI-PAGE-SEC               SECTION.
      *
           IF  ( SPA-GMN-Q98-FLG-SYSKANRI      =   0 )
               COMPUTE SPA-GMN-Q98-SKIP-KNS    =   SPA-GMN-Q98-SKIP-KNS
                                               +   60
               PERFORM 3101-ADRS-EDIT-SEC
               MOVE    ZERO            TO  SPA-GMN-Q98-SELNUM
           END-IF
      *
           .
       210-TUGI-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    ���ꡡ����
      *****************************************************************
       490-KAKUTEI                 SECTION.
      *
      *    ����̤���ϥ����å�
           IF  ( SPA-GMN-Q98-ADRS      =   SPACE )
               MOVE    "1001"      TO  SPA-ERRCD
               GO  TO  490-KAKUTEI-EXT
           END-IF
      *
      *    ��������ꤷ���ѣ��������
           MOVE    SPA-GMN-Q98-POST    TO  SPA-Q01-Q98-POST
           MOVE    SPA-GMN-Q98-ADRS    TO  SPA-Q01-Q98-JYUSHO
      *
           MOVE    "Q01"               TO  SPA-SAKIPG
           MOVE    "Q98"               TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "Q01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������
      *****************************************************************
       490-JYUSHO-SET-SEC              SECTION.
      *
           MOVE    WRK-WIDGET8     TO  IDX1
      *
           IF  ( SPA-GMN-Q98-TADRS (IDX1)  NOT =   SPACE )
      *
      *        ���꽻���Խ�����
               PERFORM 4100-KAKUTEI-JYUSHO-EDIT-SEC
      *
               MOVE    SPA-GMN-Q98-TKBNCD (IDX1)
                                   TO  SPA-GMN-Q98-SELNUM
      *
           END-IF
      *
           .
       490-JYUSHO-SET-EXT.
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Q98"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >       60 )
               MOVE    "whitebutton"
                                   TO  Q98-TADRSCHK-STYLE (IDX1)
      *        ����ʰ�����
               MOVE    SPA-GMN-Q98-TADRS (IDX1)
                                   TO  Q98-TADRS (IDX1)
               INSPECT Q98-TADRS (IDX1)(5:16)
                       REPLACING  ALL "  "  BY  "��"
      *
           END-PERFORM
      *
      *    ͹���ֹ�
           MOVE    SPA-GMN-Q98-POST    TO  Q98-POST
      *    ����
           MOVE    SPA-GMN-Q98-ADRS    TO  Q98-ADRS
      *    �����ֹ�
           MOVE    SPA-GMN-Q98-SELNUM  TO  Q98-SELNUM
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
           EVALUATE    SPA-GMN-Q98-CUR
      *
      *    �����ֹ�
           WHEN   001
               MOVE  "SELNUM"      TO  MCP-WIDGET
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
               MOVE    "������ɽ������Ƥ����ֹ�����򤷤Ƥ�������"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
               MOVE    "���꤬���򤵤�Ƥ��ޤ���"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  QERR
           MOVE    SPA-ERRCD            TO  QERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  QERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Q98"                TO  SPA-MOTOPG
           MOVE    "QERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "QERR"               TO  MCP-WINDOW
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
      *    �����ƥ�����ޥ��������ʼ��ս����
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    SPA-HOSPNUM         TO  SYS-1015-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-1015-STYUKYMD
                                           SYS-1015-EDYUKYMD
           MOVE    SYS-1015-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1015-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ޥ��������ʼ��ս����
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  SYS-1015-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ޥ����ɤ����Ф�����
      *****************************************************************
       910-SYSKANRI-READ-SKIP-SEC      SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1            >   SPA-GMN-Q98-SKIP-KNS )
                 OR      ( FLG-SYSKANRI    =   "1" )
      *
      *        �����ƥ�����ޥ����ɹ�����
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
      *
           END-PERFORM
      *
           .
       910-SYSKANRI-READ-SKIP-EXT.
           EXIT.
      *****************************************************************
      *    ����ޥ����������ʼ祭����
      *****************************************************************
       900-ADRS-KEY-SEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-ADRS
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  ( MCP-RC                =   ZERO )
               MOVE    MCPDATA-REC     TO  ADRS-REC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-ADRS-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
