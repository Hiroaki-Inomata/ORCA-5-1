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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGP98.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���긡��
      *  ����ݡ��ͥ��̾  : ���ս�����̡�P98��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC-����      ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  02/09/03  ��������򣵣�����ѹ�
      *  01.00.02    NACL-¿��Ǽ  05/11/14  MONFUNC �б�
      *  03.05.00    NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
      *  04.05.00    NACL-¿��Ǽ  09/12/22  ������ˡ��������¤ʤ���
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
      *    ���̥�������
           COPY    "ENUM-VALUE".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ������Ͽ�Ѷ��̥ѥ�᥿
           COPY    "P02COMMON-SPA".
      *
      *    ���Բ��̥����ΰ�
           COPY    "P02SCR-SPA".
      *
      *    �ե饰���ꥢ
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-SYSKANRI                      PIC 9(01).
           03  FLG-ADRS                          PIC 9(01).
           03  FLG-OK                            PIC 9(01).
           03  FLG-ADRS-OK                       PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
      *
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDXX                              PIC 9(03).
           03  IDXY                              PIC 9(03).
           03  IDXZ                              PIC 9(03).
           03  IDX                               PIC 9(03).
      *
           03  IDX1                              PIC 9(01).
           03  IDX2                              PIC 9(02).
           03  IDX-STR                           PIC 9(04).
      *
        01 WRK-AREA.
           03  WRK-IDX3                          PIC 9(03).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *    �ʼ���Į������
           COPY    "CPSK1015.INC".
      *    ����ޥ���
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
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
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
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
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-FREE                      TO  SPA-P02SCRAREA.
           MOVE    SPA-WORK                      TO  SPA-P02KYOUTU.
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           EVALUATE    MCP-STATUS       ALSO    MCP-EVENT
               WHEN    "LINK"           ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    ����
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE.
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END                       =   ZERO
               PERFORM 330-SET-SCREEN-SEC
           END-IF.
      *
           MOVE    SPA-P02KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA                TO  SPA-FREE
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ��������Խ�
               PERFORM 300-SCREEN-SEC
      *
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE                         TO  LINKAREA.
      *
           MOVE    "NEW"                         TO  MCP-PUTTYPE.
           MOVE    "P98"                         TO  MCP-WINDOW.
           MOVE    "SELNUM"                      TO  MCP-WIDGET.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE  ZERO                  TO  SPA-GMN98-PAGE
      *    ������Ͽ���̤��
           PERFORM 310-P981SPASET-SEC
           MOVE    1                   TO  SPA-GMN98-CUR
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������Ͽ�������ܽ���
      *****************************************************************
       310-P981SPASET-SEC                         SECTION.
      *
           IF      SPA-GMN98-PAGE      =   ZERO
               MOVE    1                   TO  SPA-GMN98-PAGE
           END-IF
           COMPUTE IDX-STR             =  (SPA-GMN98-PAGE   -   1  )
                                       *   50
                                       +   1
      *
           INITIALIZE                      SPA-GMN98-TABLE
           MOVE  ZERO                  TO  SPA-GMN98-MAX
           MOVE  ZERO                  TO  SPA-GMN98-NEXT
      *    �����ƥ���������ս������
           INITIALIZE                      SYS-1015-REC.
           MOVE    "1015"              TO  SYS-1015-KANRICD.
           MOVE    SPA-SYSYMD          TO  SYS-1015-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1015-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1015-HOSPNUM
           MOVE    SYS-1015-REC        TO  MCPDATA-REC.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC                        =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDXX
           MOVE    ZERO                TO  IDXZ
           PERFORM             UNTIL   FLG-SYSKANRI   =   1
      *
               MOVE    MCPDATA-REC         TO  SYS-1015-REC
               ADD     1                   TO  IDXZ
               IF      IDXZ                >=  IDX-STR
                   ADD     1               TO  IDXX
                   IF      IDXX            >   50
                       MOVE    1               TO  SPA-GMN98-NEXT
                       MOVE    1               TO  FLG-SYSKANRI
                   ELSE
                       PERFORM 31011-TBLHENSYU-SEC
                       MOVE    IDXX            TO  SPA-GMN98-MAX
                   END-IF
               END-IF
      *
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           INITIALIZE                  SPA-GMN98-INPUT
           MOVE    1                   TO  SPA-GMN98-CUR
      *
           .
       310-P981SPASET-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       31011-TBLHENSYU-SEC               SECTION.
      *
           MOVE    SYS-1015-KBNCD     TO  SPA-GMN98-KBNCD  (IDXX)
           MOVE    SYS-1015-LPUBCD    TO  SPA-GMN98-TLPUBCD(IDXX)
           MOVE    SYS-1015-POST      TO  SPA-GMN98-TPOST (IDXX)
           MOVE    SYS-1015-RENNUM    TO  SPA-GMN98-TRENNUM(IDXX)
           MOVE    SYS-1015-TOWNNAMEDSP
                                      TO  SPA-GMN98-TADRS (IDXX)
           .
       31011-TBLHENSYU-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM  210-BACK-SEC
      *        ������Ͽ
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM  420-ADRUP-SEC
      *        ���ꥢ
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  420-CLERA-SEC
      *        ������
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM  430-ADRFW-SEC
      *        ������
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM  440-ADRBK-SEC
      *        ����
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  450-KAKUTEI-SEC
      *        �������
               WHEN    "CLICKED"       ALSO    ANY
                   PERFORM  490-ADRS-CHK-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        �����ֹ�
               WHEN    "ACTIVATE"       ALSO    "SELNUM"
                   PERFORM  510-SELNUM-CHK-SEC
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̥��åȽ���
      *****************************************************************
       500-GMNHEN-SEC                            SECTION.
      *
           MOVE    SPACE               TO  P98
           INITIALIZE                      P98
      *
      *    COMPUTE IDXY    =  (SPA-GMN98-PAGE  -   1)  *   50
      *
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL       IDXX    >       50
      *        COMPUTE IDXZ    =       IDXY    +   IDXX
               MOVE  "whitebutton"         TO  P98-TADRS-STYLE (IDXX)
               MOVE  SPA-GMN98-KBNCD(IDXX) TO  P98-TADRS(IDXX)(1:3)
               MOVE  SPA-GMN98-TADRS(IDXX)
                                           TO  P98-TADRS(IDXX)(5:16)
               INSPECT P98-TADRS(IDXX)(5:16)
                       REPLACING  ALL "  "  BY  "��"
	   END-PERFORM
      *
           MOVE    SPA-GMN98-ADRS      TO  P98-ADRS 
           MOVE    SPA-GMN98-POST      TO  P98-POST
           MOVE    SPA-GMN98-SELNUM    TO  P98-SELNUM  
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-GMN98-CUR
               WHEN    1
                   MOVE    "SELNUM"        TO  MCP-WIDGET
               WHEN    2
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC                            SECTION.
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "���Ϥ��줿�ֹ������Ǥ��ޤ���"
                                                  TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "�����Τ����ϲ�ǽ�Ǥ�" TO  SPA-ERRMSG
               WHEN    "0003"
                   STRING  "����ޥ�������Ͽ����Ƥ��ޤ���"
                           "����Ͽ���Ʋ�������"
                                                  INTO    SPA-ERRMSG
                   END-STRING
           END-EVALUATE.
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P98"               TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       330-SET-SCREEN-SEC                        SECTION.
      *
           PERFORM  500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  330-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE                         TO  LINKAREA
      *
           MOVE    "CURRENT"                     TO  MCP-PUTTYPE.
           MOVE    "P98    "                     TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
       330-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ꥢ����
      *****************************************************************
       420-CLERA-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN98-SELNUM
           INITIALIZE                  SPA-GMN98-ADRS
           INITIALIZE                  SPA-GMN98-POST
      *
           MOVE    1                   TO  SPA-GMN98-CUR
      *
           .
       420-CLERA-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK-SEC                              SECTION.
      *
      *    ���̣ӣУ��Խ�
      *
           MOVE  SPA-GMN98-MOTOPG            TO  SPA-SAKIPG.
      *
           MOVE  "P98"                       TO  SPA-MOTOPG.
      *
           MOVE  SPACE                       TO  JPAR-POST.
           MOVE  SPACE                       TO  JPAR-EDITADRS-NAME 
           MOVE  SPACE                       TO  JPAR-CITYNAME-NAME 
      **   MOVE  ZERO                        TO  JPAR-CNT.
      *    MOVE  ZERO                        TO  JPAR-ERRCD.
      *
      *****MOVE   "BACK"                     TO  MCP-PUTTYPE.
           MOVE   "JOIN"                     TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
       410-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������
      *****************************************************************
       420-ADRUP-SEC             SECTION.
      *
           MOVE    "P98"               TO  SPA-MOTOPG
           MOVE    "P981"              TO  SPA-SAKIPG
      *
           MOVE    "KBNCD"             TO  MCP-WIDGET
      *
           INITIALIZE                  SPA-GMN981-AREA
           MOVE    "P98"               TO  SPA-GMN981-MOTOPG
           MOVE    SPACE               TO  P981
      *
           INITIALIZE                      P981
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "P981   "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       420-ADRUP-EXT.
           EXIT.
      *
      *****************************************************************
      *   ���ڡ�������
      *****************************************************************
       430-ADRFW-SEC                             SECTION.
      *
           IF      SPA-GMN98-PAGE      >  1
               COMPUTE SPA-GMN98-PAGE  =  SPA-GMN98-PAGE    -  1
      *
               PERFORM 310-P981SPASET-SEC
           END-IF
           MOVE    1                   TO  SPA-GMN98-CUR
      *
           .
      *
       430-ADRFW-EXT.
           EXIT.
      *
      *****************************************************************
      *   ����ڡ�������
      *****************************************************************
       440-ADRBK-SEC                             SECTION.
      *
           IF      SPA-GMN98-NEXT      =   1
               COMPUTE  SPA-GMN98-PAGE =   SPA-GMN98-PAGE    +  1
               PERFORM 310-P981SPASET-SEC
           END-IF
           MOVE    1                   TO  SPA-GMN98-CUR
      *
           .
       440-ADRBK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       450-KAKUTEI-SEC                           SECTION.
      *
           PERFORM 4501-KAKUTEI-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 45012-KAKUTEI-SYORI-SEC
           END-IF
      *
           .
       450-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������å�����
      *****************************************************************
       4501-KAKUTEI-CHK-SEC            SECTION.
      *
           IF     (SPA-GMN98-PAGE      =   ZERO )  OR
                  (SPA-GMN98-SELNUM    =   ZERO )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN98-CUR
           ELSE
               PERFORM 510-SELNUM-CHK-SEC
           END-IF
      *
           .
       4501-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       45012-KAKUTEI-SYORI-SEC         SECTION.
      *
      *    �����Խ�
           PERFORM 45012-ADRS-HEN-SEC
      ***  MOVE    1                   TO  JPAR-CNT
      **   MOVE    ZERO                TO  JPAR-ERRCD
      *
      *    ���̣ӣУ��Խ�
           MOVE    SPA-GMN98-MOTOPG    TO  SPA-SAKIPG
      *
           MOVE    "P98"               TO  SPA-MOTOPG
      *
           MOVE  "JOIN"                TO  MCP-PUTTYPE.
           MOVE  SPA-SAKIPG            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE  1                               TO  FLG-END.
      *
       45012-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       45012-ADRS-HEN-SEC                SECTION.
      *
           INITIALIZE                      ADRS-REC
           MOVE    SPA-GMN98-POST      TO  ADRS-POST
           MOVE    SPA-GMN98-EDANUM    TO  ADRS-EDANUM
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
      *
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-ADRS-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ADRS            =   ZERO
               MOVE    ADRS-POST           TO  JPAR-POST
               MOVE    ADRS-EDITADRS-NAME  TO  JPAR-EDITADRS-NAME
      *        ��Į¼̾��Į��̾
               MOVE    SPACE               TO  JPAR-CITYNAME-NAME
               STRING  ADRS-CITYNAME
                       ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                           INTO    JPAR-CITYNAME-NAME
               END-STRING
               IF      ADRS-TOWNNAME       =   "�ʲ��˷Ǻܤ��ʤ����"
                   MOVE    ADRS-CITYNAME   TO  JPAR-CITYNAME-NAME
               END-IF
      *        ���Ԥ��̤�̾�Τ��Խ������
               IF     (ADRS-LPUBCD(1:2)    =   "26"  )
                 AND  (ADRS-TOWNNAME2  NOT =   SPACE )
                 AND  (SPA-NAI-TOWNNAME2KBN-FLG    =   "1" )
                   IF     (ADRS-TOWNNAME2(1:2)     =   "��"  )
                       CONTINUE
                   ELSE
      *                ��Į¼̾��Į��̾
                       MOVE    SPACE           TO  JPAR-CITYNAME-NAME
                       STRING  ADRS-CITYNAME
                               ADRS-TOWNNAME2
                               ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                           INTO    JPAR-CITYNAME-NAME
                       END-STRING
      *                ��Į¼̾��Į��̾
                       MOVE    SPACE           TO  JPAR-CITYNAME-NAME
                       STRING  ADRS-PREFNAME
                               ADRS-CITYNAME
                               ADRS-TOWNNAME2
                               ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                           INTO    JPAR-EDITADRS-NAME
                       END-STRING
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE               TO  JPAR-POST
               MOVE    SPACE               TO  JPAR-EDITADRS-NAME
               MOVE    SPACE               TO  JPAR-CITYNAME-NAME
           END-IF
           .
       45012-ADRS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������
      *****************************************************************
       490-ADRS-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-IDX3
           IF      MCP-WIDGET(1:9)     =   "CHK_TADRS"
               IF      MCP-WIDGET(10:3)    NUMERIC
                   MOVE    MCP-WIDGET(10:3)    TO  WRK-IDX3
               END-IF
           END-IF
           IF      WRK-IDX3            =   ZERO
               GO      TO      490-ADRS-CHK-EXT
           END-IF
      *
           MOVE    WRK-IDX3(1:2)       TO  IDX2
           MOVE    WRK-IDX3(3:1)       TO  IDX1
           COMPUTE IDXZ                =   ((IDX2   -   1  )  *  5 )
                                       +   IDX1
      *    COMPUTE IDXY        =  ((SPA-GMN98-PAGE  -   1)  *   50)
      *                        +  IDXZ
      *    MOVE    IDXY                TO  IDXZ
      *
           MOVE    1                   TO  SPA-GMN98-CUR
           IF     (IDXZ                >   ZERO  AND  <=   100  )  AND
                  (SPA-GMN98-KBNCD(IDXZ)   NOT =   SPACE )
               MOVE    SPA-GMN98-KBNCD(IDXZ)   TO  SPA-GMN98-SELNUM
               PERFORM 5101-ADRS-HENSYU-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    2                   TO  SPA-GMN98-CUR
               END-IF
           END-IF
           .
      *
       490-ADRS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  �������ֹ�����å�
      *****************************************************************
       510-SELNUM-CHK-SEC                        SECTION.
      *
           MOVE    1                   TO  SPA-GMN98-CUR
      *
           MOVE    SPACE               TO  SPA-GMN98-POST
           MOVE    SPACE               TO  SPA-GMN98-ADRS
      *
           MOVE    P98-SELNUM          TO  SPA-GMN98-SELNUM
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXZ    FROM    1   BY  1
                   UNTIL      (IDXZ        >       50  )  OR
                              (FLG-OK      =   1       )
               IF      SPA-GMN98-KBNCD(IDXZ)       =   SPACE
                   MOVE    50                      TO  IDXZ
               ELSE
                   IF      SPA-GMN98-KBNCD(IDXZ)   =   SPA-GMN98-SELNUM
                       MOVE    1                   TO  FLG-OK
                       PERFORM 5101-ADRS-HENSYU-SEC
                   END-IF
               END-IF
	   END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
           .
      *
       510-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  �������Խ�
      *****************************************************************
       5101-ADRS-HENSYU-SEC                        SECTION.
      *
           MOVE    ZERO                TO  FLG-ADRS
           MOVE    SPACE               TO  SPA-GMN98-POST
           MOVE    SPACE               TO  SPA-GMN98-ADRS
           MOVE    ZERO                TO  SPA-GMN98-EDANUM
      *    ����ǡ����ɤ߽Ф�
           INITIALIZE                          ADRS-REC
           MOVE    SPA-GMN98-TPOST  (IDXZ) TO  ADRS-POST
           MOVE    SPA-GMN98-TRENNUM(IDXZ) TO  ADRS-EDANUM
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-ADRS-READ-SEC
           ELSE
               MOVE    1               TO  FLG-ADRS
           END-IF
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ADRS            =   ZERO
               MOVE    ADRS-POST           TO  SPA-GMN98-POST
               MOVE    ADRS-EDITADRS-NAME  TO  SPA-GMN98-ADRS
      *        ���Ԥ��̤�̾�Τ��Խ������
               IF     (ADRS-LPUBCD(1:2)    =   "26"  )
                 AND  (ADRS-TOWNNAME2  NOT =   SPACE )
                 AND  (SPA-NAI-TOWNNAME2KBN-FLG    =   "1" )
                   IF     (ADRS-TOWNNAME2(1:2)     =   "��"  )
                       CONTINUE
                   ELSE
      *                ��Į¼̾��Į��̾
                       MOVE    SPACE           TO  SPA-GMN98-ADRS
                       STRING  ADRS-PREFNAME
                               ADRS-CITYNAME
                               ADRS-TOWNNAME2
                               ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                           INTO    SPA-GMN98-ADRS
                       END-STRING
                   END-IF
               END-IF
               MOVE    ADRS-EDANUM         TO  SPA-GMN98-EDANUM
               MOVE    2                   TO  SPA-GMN98-CUR
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN98-CUR
           END-IF
           .
      *
       5101-ADRS-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɤ߹��߽���
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
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
      *****************************************************************
      *    ����ޥ����ɤ߹��߽���
      *****************************************************************
       900-ADRS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  ADRS-REC
               MOVE    ZERO                TO  FLG-ADRS
           ELSE
               MOVE    1                   TO  FLG-ADRS
           END-IF
           .
       900-ADRS-READ-EXT.
           EXIT.
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
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *