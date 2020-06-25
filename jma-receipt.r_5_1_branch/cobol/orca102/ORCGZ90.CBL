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
       PROGRAM-ID.                               ORCGZ90.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ޥ�������
      *  ����ݡ��ͥ��̾  : Ϸ�Ͱ��̥������Ѵ�������̡ʣڣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.01.01    MCC-ƣ��     01/09/26  JOIN�ν���      
      *  02.00.00    NACL-¿��Ǽ  03/06/16  ������Ϥ��ѹ�
      *  02.00.01    NACL-¿��Ǽ  05/12/12  MONFUNC�б� ¾
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ޥ��������Ѷ��̥ѥ�᥿
           COPY    "Z01COMMON-SPA".
      *
      *    ���̣ӣУ����ꥢ
           COPY    "Z01SCR-SPA".
      *
      *    �ӣУ����ꥢ
           COPY    "Z03SCR-SPA".
      *
      *    �ե饰���ꥢ
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
      *
           03  FLG-SRYCDCHG                      PIC 9(01).
           03  FLG-TENSU                         PIC 9(01).
           03  FLG-KANJI                         PIC 9(01).
           03  FLG-KANA                          PIC 9(01).
           03  FLG-SUJI                          PIC 9(01).
      *
           03  FLG-TOUROKU                       PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDY                               PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *    ���Ϲ���̾�Խ��� 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-WIDGET          PIC X(20).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *
           03  WRK-Z90-AREA.
               05  WRK-Z90GMN-TBL.
                   07  WRK-Z90GMN-TBL-G           OCCURS   5.
                       09  WRK-Z90GMN-SRYCDCHG        PIC X(09).
                       09  WRK-Z90GMN-SRYNAME         PIC X(100).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ޥ���
           COPY    "CPTENSU.INC".
      *    �����ޥ����ղå�����
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    Ϸ�Ͱ��̥������Ѵ�
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *    �ģ¸���
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
           COPY    "ORCA102SCRAREA.INC".
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
           INITIALIZE                            FLG-AREA
           INITIALIZE                            IDX-AREA
           INITIALIZE                            WRK-AREA
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z03
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
               PERFORM 500-SET-SCREEN
           END-IF.
      *    
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-Z03                       TO  SPA-Z01SONOTA 
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO      TO      100-INIT-EXT
               END-IF
      *
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE   "Z90"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
      *
           EVALUATE    SPA-MOTOPG(1:3)
               WHEN    "Z98"
                   PERFORM 320-Z98SPASET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
      *    �����ѥ����Խ�����
           PERFORM                 310-SPA-SET-SEC
      *
           MOVE    1               TO  SPA-Z90GMN-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̣ӣУ������ӣУ��˥��å�
      *****************************************************************
       310-SPA-SET-SEC             SECTION.
      *
        310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ź԰٥����ɥ��åȽ���
      *****************************************************************
       320-Z98SPASET-SEC          SECTION.
      *
           IF      SPA-Z98-SRYCD   NOT =   SPACE
               MOVE    SPA-Z98-SRYCD       TO  Z90-SRYCDCHG
                                                        (SPA-Z90-IDX)
               MOVE    SPA-Z98-SRYCD       TO  SPA-Z90GMN-SRYCDCHG
                                                        (SPA-Z90-IDX)
               PERFORM 2001-SRYCD-CHK-SEC
           END-IF
           .
       320-Z98SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        ����
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-KAKUTEI-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
      *    ���ϸĽ�Υ��å�
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-Z90GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    ���̤�ӣУ����å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *    ���ܥ����å����̲��̽���
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    ��Ͽ����ɬ�����ϥ����å�
      *    IF      FLG-TOUROKU         =   1
      *        PERFORM 4109-TOUROKU-CHK-SEC
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            GO      TO      410-INPUT-CHK-EXT
      *        END-IF
      *    END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̣ӣУ��Խ�����
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               MOVE    Z90-SRYCDCHG (IDX)  TO
                                     SPA-Z90GMN-SRYCDCHG (IDX)
           END-PERFORM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ܥ����å�����
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    ���ϥ����ɥ����å�
           PERFORM 2001-SRYCD-CHK-SEC
           IF     (FLG-END             =   1 )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���Ϲ��ܤη���
      *****************************************************************
       2001-SRYCD-CHK-SEC               SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5   )   OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (FLG-END         =   1     )
               MOVE    IDX                 TO  SPA-Z90-IDX
      *        ����ɽ��
               IF      SPA-Z90GMN-SRYCDCHG(SPA-Z90-IDX)(1:2)   =   "//"
                   PERFORM 20011-Z98-SYORI-SEC
               ELSE
                   PERFORM 20012-SRYCD-TNSCHK-SEC
               END-IF
           END-PERFORM
      *
           IF     (FLG-END             =   1 )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      2001-SRYCD-CHK-EXT
           END-IF
      *
      *    ����Ժ��
           PERFORM 2002-SPACE-DEL-SEC
      *
           .
       2001-SRYCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ���¸�ߥ����å�����
      *****************************************************************
       20012-SRYCD-TNSCHK-SEC           SECTION.
      *
      *    �����ޥ���¸�ߥ����å�
           IF      SPA-Z90GMN-SRYCDCHG(SPA-Z90-IDX)     =   SPACE
               MOVE    SPACE       TO  SPA-Z90GMN-SRYNAME (SPA-Z90-IDX)
               GO      TO      20012-SRYCD-TNSCHK-EXT
           END-IF
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z90GMN-SRYCDCHG(SPA-Z90-IDX)
                                           TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-NAME            TO  SPA-Z90GMN-SRYNAME
                                                       (SPA-Z90-IDX)
      *        �±����Ž��ʬ��Ʊ���Ǥ��뤳��
               IF      TNS-HOSPSRYKBN      =   SPA-GMN-HOSPSRYKBN
                   CONTINUE
               ELSE
                   MOVE    "0004"              TO  SPA-ERRCD
               END-IF
      *        Ϸ�Ͷ�ʬ��Ʊ���������顼
               IF      TNS-ROUTEKKBN      =   SPA-GMN-ROUTEKKBN
                   MOVE    "0005"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-Z90GMN-SRYNAME
                                                       (SPA-Z90-IDX)
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPA-Z90-IDX         TO  SPA-Z90GMN-CUR
               GO      TO      20012-SRYCD-TNSCHK-EXT
           END-IF
      *
      *    ���֤�����å�
           IF      SPA-Z90GMN-SRYCDCHG(SPA-Z90-IDX)
                                           =   SPA-Z01-SRYCD
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
           PERFORM VARYING     IDY     FROM    1   BY 1
                   UNTIL      (IDY             >   5   )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (IDY             NOT =   SPA-Z90-IDX )  AND
                      (SPA-Z90GMN-SRYCDCHG(IDY)
                                           =   SPA-Z90GMN-SRYCDCHG
                                                         (SPA-Z90-IDX))
                   MOVE    "0002"              TO  SPA-ERRCD
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPA-Z90-IDX         TO  SPA-Z90GMN-CUR
               GO      TO      20012-SRYCD-TNSCHK-EXT
           END-IF
      *
           .
       20012-SRYCD-TNSCHK-EXT.
           EXIT.
      *****************************************************************
      *    ����Ժ������
      *****************************************************************
       2002-SPACE-DEL-SEC           SECTION.
      *
           MOVE    SPA-Z90GMN-TBL      TO  WRK-Z90GMN-TBL
           INITIALIZE                      SPA-Z90GMN-TBL
           MOVE    ZERO                TO  SPA-Z90-IDX
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   5
               IF      WRK-Z90GMN-TBL-G (IDX)  NOT =   SPACE
                   ADD     1               TO  SPA-Z90-IDX
                   MOVE    WRK-Z90GMN-TBL-G (IDX)
                                   TO  SPA-Z90GMN-TBL-G (SPA-Z90-IDX)
               END-IF
           END-PERFORM
      *
           .
       2002-SPACE-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ���ϥ����ɥ����å�����
      *****************************************************************
       20011-Z98-SYORI-SEC            SECTION.
      *
      *
           MOVE    "Z90"               TO  SPA-MOTOPG
           MOVE    "Z98"               TO  SPA-SAKIPG
           MOVE    "Z90"               TO  SPA-Z98-MOTOPG
      *
           MOVE    "INPUT"             TO  MCP-WIDGET 
      *
      *    ���̣ӣУ��Խ�
           INITIALIZE                  SPA-Z98-AREA
           MOVE    "2"                 TO  SPA-Z98-TYPE
      *
           INITIALIZE                  SPA-Z98GMN-AREA
           MOVE    SPACE               TO  Z98
      *
           INITIALIZE                      Z98
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "Z98    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       20011-Z98-SYORI-EXT.
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
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z90"               TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  Z90
      *
           INITIALIZE                      Z90
           MOVE    SPA-Z01-SRYCD       TO  Z90-SRYCD
           MOVE    SPA-Z01-NAMEH       TO  Z90-SRYNAME
      *
           PERFORM VARYING  IDX    FROM  1  BY  1  
                   UNTIL    IDX        >  5
               MOVE    SPA-Z90GMN-SRYCDCHG (IDX)
                                       TO  Z90-SRYCDCHG (IDX)
               MOVE    SPA-Z90GMN-SRYNAME  (IDX)
                                       TO  Z90-SRYCDNAME(IDX)
           END-PERFORM
      *
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
           IF      SPA-Z90GMN-CUR          =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-Z90GMN-CUR
               WHEN    01
                   MOVE    "SRYCDCHG1"     TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SRYCDCHG2"     TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SRYCDCHG3"     TO  MCP-WIDGET
               WHEN    04
                   MOVE    "SRYCDCHG4"     TO  MCP-WIDGET
               WHEN    05
                   MOVE    "SRYCDCHG5"     TO  MCP-WIDGET
               WHEN    06
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������륻�åȽ���
      *****************************************************************
       50011-CUR-SET-SEC                            SECTION.
      *
      *    �Ǹ�ιԤ�
           PERFORM VARYING  IDX    FROM  1  BY  1  
                   UNTIL    IDX        >  5
               IF      SPA-Z90GMN-SRYCDCHG (IDX)   =   SPACE
                   MOVE    IDX             TO  SPA-Z90GMN-CUR
                   MOVE    5               TO  IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-Z90GMN-CUR      =   ZERO
               MOVE    6                   TO  SPA-Z90GMN-CUR
           END-IF
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "�����ޥ�����¸�ߤ��ޤ���"
                                                 TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "���ť����ɤ���ʣ���Ƥ��ޤ�"
                                                 TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "������Υ����ɤǤ�"
                                                 TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "�±����Ž��ʬ���㤤�ޤ�"
                                                 TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "Ϸ��Ŭ�Ѷ�ʬ��Ʊ���Ǥ�"
                                                 TO  SPA-ERRMSG
      *R01.10
               WHEN    "0006"
                   MOVE    "�����ϤǤ��ޤ���"
                                                 TO  SPA-ERRMSG
           END-EVALUATE.
      *
           MOVE    SPACE               TO  ZERR
           MOVE    SPA-ERRCD           TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  ZERR-ERRMSG
      *
           MOVE    "Z90"               TO  SPA-MOTOPG
           MOVE    "ZERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "ZERR"              TO  MCP-WINDOW
           MOVE    "B01"               TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
      *
        510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Z03"                 TO  SPA-SAKIPG.
      *
           MOVE    "Z90"                 TO  SPA-MOTOPG.
      *
      ***  MOVE    "BACK"                TO  MCP-PUTTYPE.
           MOVE    "JOIN"                TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ǧ����
      *****************************************************************
       412-KAKUTEI-SEC                           SECTION.
      *
      *R1.10
      *    �����ԲĤȤ���ʸ����оݤʤ���
           MOVE    "0006"              TO  SPA-ERRCD
           GO      TO      412-KAKUTEI-EXT
      *
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    ���ϥ����å�����
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-KAKUTEI-EXT
           END-IF
      *
      *    �������Ƥ�УңǶ��̣ӣУ��˥��å�
           PERFORM  VARYING  IDX       FROM  1  BY  1
                    UNTIL    IDX       >  5
               MOVE    SPA-Z90GMN-SRYCDCHG (IDX)
                                 TO  SPA-Z90-SRYCDCHG (IDX)
               MOVE    SPA-Z90GMN-SRYNAME  (IDX)
                                 TO  SPA-Z90-SRYNAME (IDX)
           END-PERFORM
      *
      *    �����̤����
           PERFORM  210-BACK
      *
           .
       412-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    �����ޥ������ɹ�
      *****************************************************************
       900-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-Z01-YUKOSTYMD   TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-YUKOSTYMD   TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
           CALL   "ORCDBMAIN"                    USING
                                                     MCPAREA
                                                     MCPDATA-REC
                                                     SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *