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
       PROGRAM-ID.             ORCGJ99.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��׾Ȳ�
      *  ����ݡ��ͥ��̾  : �Х����ϡʣʣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-¿��Ǽ   01/05/22  �����ܥ�����ѹ�
      * 01.00.02     NACL-¿��Ǽ  05/11/30  MONFUNC �б�
      *****************************************************************
      *
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *
       DATA                    DIVISION.
       WORKING-STORAGE             SECTION.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���Ź԰��Ѷ��̥ѥ�᥿
           COPY    "J01COMMON-SPA".
      *
      *    ���̥����ΰ�
           COPY    "J04SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(02).
           03  IDX1                PIC 9(03).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-PIN.
               05  PIN9            PIC 9(02).
               05  PIN9-R          REDEFINES  PIN9.
                   07  PIN10       PIC 9(01).
                   07  PIN1        PIC 9(01).
           03  WRK-PED.
               05  PEDX            PIC X(02)  OCCURS  10.
           03  WRK-PKANA           PIC X(02).
      *
       01  PIN-TBL-AREA.
           03  KANA-TBL.
               05 FILLER PIC X(20) VALUE "��������������������".
               05 FILLER PIC X(20) VALUE "��������������������".
               05 FILLER PIC X(20) VALUE "��������������������".
               05 FILLER PIC X(20) VALUE "�����ĥƥȡ���������".
               05 FILLER PIC X(20) VALUE "�ʥ˥̥ͥΡ���������".
               05 FILLER PIC X(20) VALUE "�ϥҥեإۡ���������".
               05 FILLER PIC X(20) VALUE "�ޥߥ��⡡��������".
               05 FILLER PIC X(20) VALUE "�䡡�桡�衡��������".
               05 FILLER PIC X(20) VALUE "����������������".
               05 FILLER PIC X(20) VALUE "���󡫡�����������".
           03  KANA-TBL-R          REDEFINES  KANA-TBL.
               05  PKANA           PIC X(02)  OCCURS  100.
      *      
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
           COPY    "ORCA24SCRAREA.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-J04
           MOVE    SPA-WORK        TO  SPA-J01KYOUTU
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
           MOVE    SPA-J01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-J04         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *    INITIALIZE                  SPA-J99-DATA
      *
      *    ��������Խ�
      *    PERFORM 300-SCREEN-SEC
      *
      *    �����Խ�
           PERFORM 500-GMNHEN-SEC
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "J99"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-J99GMN-AREA
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  J99-PIN
           MOVE    SPA-J99-PED         TO  J99-PED
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
           MOVE    "PIN"               TO  MCP-WIDGET
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT
               WHEN    "CLICKED"
                   IF      MCP-WIDGET(2:2)     NUMERIC
                       MOVE    MCP-WIDGET(2:2)     TO  PIN9
                       PERFORM 410111-PIN-SET-SEC
                   ELSE
                       IF      MCP-WIDGET      =   "BSH"
                           PERFORM 410112-SHIFT-SEC
                       END-IF
                       EVALUATE    MCP-WIDGET
                           WHEN    "BSH"
                               PERFORM 410112-SHIFT-SEC
      *----(01.00.01) LINE ADD START ----------------------------------
      *                    ����
                           WHEN     "BAK"
                               PERFORM 410113-BACK-SEC
      *                    ��λ
                           WHEN     "BEND"
                               PERFORM 210-BACK
                       END-EVALUATE
      *----(01.00.01) LINE ADD END   ----------------------------------
                   END-IF
           END-EVALUATE
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        �ֹ�����
               WHEN    "ACTIVATE"      ALSO    "PIN"
                   PERFORM 41011-PIN-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϡ�����
      *****************************************************************
       41011-PIN-CHK-SEC            SECTION.
      *
           IF       J99-PIN            =   SPACE
      *        �������ϻ�����λ
               PERFORM 210-BACK
           ELSE
               EVALUATE    TRUE
      *        ���η����
                   WHEN   (J99-PIN(1:2)       =   "--") AND
                          (SPA-J99-IDX        >   0   )
                       MOVE    SPACE          TO  SPA-J99-PEDX
                                                          (SPA-J99-IDX)
                       COMPUTE SPA-J99-IDX    =   SPA-J99-IDX   -   1
      *        ���� 
                   WHEN   (J99-PIN(1:2)        =   "..") AND
                          (SPA-J99-IDX         >   0)
                       ADD    1               TO  SPA-J99-IDX
                       MOVE   SPACE           TO  SPA-J99-PEDX
                                                        (SPA-J99-IDX)
      *        ��ʸ���Ѵ�
                   WHEN   (J99-PIN(1:1)        =   "+")  AND
                          (SPA-J99-IDX         >   0  )
                       PERFORM 410112-SHIFT-SEC
      *        �����Ѵ�
                   WHEN    OTHER
                       MOVE    J99-PIN         TO  WRK-PIN
                       IF     (WRK-PIN(1:1)    =   SPACE) OR
                              (WRK-PIN(2:1)    =   SPACE) OR
                              (PIN10       NOT NUMERIC   ) OR
                              (PIN1        NOT NUMERIC   ) OR
                              (WRK-PIN         =   "82" OR "84") OR
                             ((PIN10           >=  1) AND
                              (PIN10           <=  8) AND
                              (PIN1            >   5))
                            CONTINUE
                       ELSE
                           IF      SPA-J99-IDX    >=  10
                               MOVE    "0002"          TO  SPA-ERRCD
                               GO      TO      41011-PIN-CHK-EXT
                           END-IF
      *                    �������å�
                           PERFORM 410111-PIN-SET-SEC
                       END-IF
      *
               END-EVALUATE
           END-IF
           .
       41011-PIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������åȡ�����
      *****************************************************************
       410111-PIN-SET-SEC              SECTION.
      *
           EVALUATE    PIN9
               WHEN    0
                   MOVE    100         TO  IDX1
               WHEN    90
                   MOVE    90          TO  IDX1
               WHEN    OTHER
                   IF      PIN10       =   0
                       MOVE    9           TO  PIN10
                       MOVE    PIN9        TO  IDX1
                   ELSE
                       COMPUTE IDX1        =   PIN9    -   10
                   END-IF
           END-EVALUATE
           MOVE    PKANA(IDX1)         TO  WRK-PKANA
           IF     (WRK-PKANA           =   "��"  ) AND
                  (SPA-J99-IDX         >   0     )
               EVALUATE    SPA-J99-PEDX (SPA-J99-IDX)
                   WHEN    "��"
                       MOVE    "��"            TO  WRK-PKANA
                   WHEN    "��"
                       MOVE    "��"            TO  WRK-PKANA
                   WHEN    "��"
                       MOVE    "��"            TO  WRK-PKANA
                   WHEN    "��"
                       MOVE    "��"            TO  WRK-PKANA
                   WHEN    "��"
                       MOVE    "��"            TO  WRK-PKANA
                   WHEN    OTHER
                       ADD    1                TO  SPA-J99-IDX
               END-EVALUATE
           ELSE
               IF    (WRK-PKANA        =   "��" ) AND
                     (SPA-J99-IDX      >   0    )
                   EVALUATE    SPA-J99-PEDX (SPA-J99-IDX)
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    "��"
                           MOVE    "��"            TO  WRK-PKANA
                       WHEN    OTHER
                           ADD    1                TO  SPA-J99-IDX
                   END-EVALUATE
               ELSE
                   ADD     1               TO  SPA-J99-IDX
               END-IF
           END-IF
      *
           MOVE    WRK-PKANA           TO  SPA-J99-PEDX
                                                (SPA-J99-IDX)
           .
       410111-PIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ʸ���Ѵ�������
      *****************************************************************
       410112-SHIFT-SEC                    SECTION.
      *
           IF      SPA-J99-IDX         =   0
               GO      TO      410112-SHIFT-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-PKANA
           EVALUATE    SPA-J99-PEDX (SPA-J99-IDX)
               WHEN    "��"
                   MOVE    "��"            TO  WRK-PKANA
               WHEN    "��"
                   MOVE    "��"            TO  WRK-PKANA
               WHEN    "��"
                   MOVE    "��"            TO  WRK-PKANA
               WHEN    "��"
                   MOVE    "��"            TO  WRK-PKANA
               WHEN    "��"
                   MOVE    "��"            TO  WRK-PKANA
               WHEN    "��"
                   MOVE    "��"            TO  WRK-PKANA
               WHEN    "��"
                   MOVE    "��"            TO  WRK-PKANA
               WHEN    "��"
                   MOVE    "��"            TO  WRK-PKANA
               WHEN    "��"
                   MOVE    "��"            TO  WRK-PKANA
               WHEN    "��"
                   MOVE    "��"            TO  WRK-PKANA
           END-EVALUATE
      *
           IF      WRK-PKANA       NOT =   SPACE
               MOVE    WRK-PKANA           TO  SPA-J99-PEDX
                                                (SPA-J99-IDX)
           END-IF
           .
       410112-SHIFT-EXT.
           EXIT.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *****************************************************************
      *    ���ࡡ����
      *****************************************************************
       410113-BACK-SEC                    SECTION.
      *
      *    ���η����
           IF      SPA-J99-IDX        >   0
               MOVE    SPACE          TO  SPA-J99-PEDX(SPA-J99-IDX)
               COMPUTE SPA-J99-IDX    =   SPA-J99-IDX   -   1
           END-IF
           .
       410113-BACK-EXT.
           EXIT.
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-J99-PED         TO  SPA-J99-DATA
      *
           MOVE    "J04"               TO  SPA-SAKIPG
           MOVE    "J99"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE   "BACK"               TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "J04"               TO  MCP-WINDOW
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
           MOVE    "J99"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
                   MOVE    "����ʸ���ޤǤǤ�"  TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  JERR
           MOVE    SPA-ERRMSG          TO  JERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "J99"               TO  SPA-MOTOPG
           MOVE    "JERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "JERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
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
      *