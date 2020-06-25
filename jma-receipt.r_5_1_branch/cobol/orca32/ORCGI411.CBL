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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGI411.
      ********************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������׾Ȳ� 
      *  ����ݡ��ͥ��̾  : �ץ�ӥ塼�ݸ��ȹ礻���򥵥֡ʣɣ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  12/11/09    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ��׾Ȳ��̥ѥ�᥿
           COPY    "I4COMMON-SPA".
      *
      *    ���̥����ΰ�
           COPY    "I4SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDK                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *
           03  WRK-HKNCOMBINUM     PIC  X(04).
      *
           03  FLG-GMN-INIT        PIC  9(01).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *    ���ѥ����å��ѥ�᥿
      *    COPY    "CPORCSKANACHK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
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
           COPY    "ORCA32SCRAREA.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-I41
           MOVE    SPA-WORK            TO  SPA-I4KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN    "PUTG"          ALSO   "CLICKED"
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
           MOVE    SPA-I4KYOUTU        TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-I41             TO  SPA-FREE
          .
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA.
           INITIALIZE                  CNT-AREA.
           INITIALIZE                  WRK-AREA.
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "I4ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ��������Խ�
               PERFORM 300-SCREEN-SEC
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE
           MOVE   "I411"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           PERFORM                     310-SPA-SET-SEC
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
           INITIALIZE                      SPA-I411-AREA
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-NAI-MAX-41
               IF      SPA-NAI-TZAISKBKBN-41 (IDX) =   "5"
      *            �ݸ�Ƚ��
                   PERFORM VARYING IDK     FROM    1   BY  1
                           UNTIL   IDK     >   SPA-NAI-SRYMATU
                       IF      SPA-NAI-DAY-41 (IDX IDK)    NOT =   ZERO
                           MOVE    ZERO        TO  WRK-HKNCOMBINUM
                           MOVE    SPA-NAI-DAY-41 (IDX IDK)
                                               TO  WRK-HKNCOMBINUM(2:3)
                           PERFORM VARYING     IDY     FROM    1   BY  1
                                   UNTIL       IDY     >   30
                               IF      SPA-I411-THKNCOMBI (IDY)
                                                       =   SPACE
                                   PERFORM 3101-HKNCOMBI-SET-SEC
                               END-IF
                               IF      SPA-I411-THKNCOMBI (IDY)
                                                   =   WRK-HKNCOMBINUM
                                   MOVE    30              TO  IDY
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           MOVE    1                   TO  SPA-I411-GMN-CUR
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ȹ礻�Խ�����
      *****************************************************************
       3101-HKNCOMBI-SET-SEC                  SECTION.
      *
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL       IDH     >   SPA-HKNCOMBI-MAX
               IF      SPA-I41-THKNCOMBI (IDH) =   WRK-HKNCOMBINUM
                   MOVE    SPA-I41-THKNCOMBI (IDH)
                                           TO  SPA-I411-THKNCOMBI (IDY)
                   MOVE    SPA-I41-THKNCOMBIMEI (IDH)
                                           TO  SPA-I411-THKNCOMBIMEI
                                                                 (IDY)
                   MOVE    SPA-I41-HKNNUM (IDH)
                                           TO  SPA-I411-THKNNUM  (IDY)
                   MOVE    SPA-I41-HKNKBN (IDH)
                                           TO  SPA-I411-THKNKBN  (IDY)
                   MOVE    SPA-I41-RSI-HKNKBN (IDH)
                                           TO  SPA-I411-TRSI-HKNKBN(IDY)
                   MOVE    SPA-I41-SISIKBN (IDH)
                                           TO  SPA-I411-TSISIKBN (IDY)
                   MOVE    IDY             TO  SPA-I411-HKNCOMBI-MAX
                   MOVE    SPA-HKNCOMBI-MAX    TO  IDH
               END-IF
           END-PERFORM
           .
       3101-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  I411
           INITIALIZE                      I411
      *
           MOVE    SPA-I411-SELNUM    TO  I411-SELNUM
      *
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX       >   SPA-I411-HKNCOMBI-MAX
               MOVE    SPA-I411-THKNCOMBI (IDX)
                                           TO  I411-HKNCOMBI (IDX)
               MOVE    SPA-I411-THKNCOMBIMEI(IDX)
                                           TO  I411-HKNCOMBIMEI (IDX)
           END-PERFORM
           MOVE    SPA-I411-HKNCOMBI-MAX   TO  I411-COUNT
      *
           PERFORM 5001-MAPCUR-SEC.
      *
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-I411-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM"        TO  MCP-WIDGET
           END-EVALUATE.
      *
       5001-MAPCUR-EXT.
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
                   MOVE    SPACE               TO  SPA-I411-SELNUM
                   PERFORM 210-BACK
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        �ֹ�����
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41011-SELNUM-CHK-SEC
      *        ������
               WHEN    ANY             ALSO    "HKNLIST"
                   PERFORM 41012-LISTSEL-SEC
           END-EVALUATE.
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ֹ����򡡽���
      *****************************************************************
       41011-SELNUM-CHK-SEC                SECTION.
      *
           MOVE    1                   TO  SPA-I411-GMN-CUR
      *
           MOVE    I411-SELNUM         TO  SPA-I411-SELNUM
      *    �����ֹ�����
           INITIALIZE                      ORCSNUMAREA.
           MOVE    SPA-I411-SELNUM     TO  SNUM-INX.
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA.
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  SPA-I411-SELNUM
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-I411-HKNCOMBI-MAX )
                           OR (FLG-OK      =   1      )
               IF      SPA-I411-THKNCOMBI(IDX) =   SPA-I411-SELNUM
                   MOVE    1               TO  FLG-OK
                   PERFORM 41012-HKNCOMBI-SELHEN-SEC
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
                PERFORM 210-BACK
           END-IF
           .
       41011-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ȹ礻�����Խ�����
      *****************************************************************
       41012-HKNCOMBI-SELHEN-SEC       SECTION.
      *
      *     �ݸ�Ƚ��
            EVALUATE    SPA-I411-THKNNUM (IDX)
               WHEN    "971"
      *            ϫ��
                   MOVE    "1"                 TO  SPA-XC01-HKNKBN
                   MOVE    SPA-I411-TRSI-HKNKBN (IDX)
                                               TO  SPA-XC01-RSI-HKNKBN
               WHEN    "973"
      *            ������
                   MOVE    "2"                 TO  SPA-XC01-HKNKBN
                   MOVE    SPA-I411-TRSI-HKNKBN (IDX)
                                               TO  SPA-XC01-RSI-HKNKBN
               WHEN    "975"
      *            ����
                   MOVE    "3"             TO  SPA-XC01-HKNKBN
                   MOVE    SPACE           TO  SPA-XC01-RSI-HKNKBN
               WHEN    OTHER
      *            ����¾
                   MOVE    "0"             TO  SPA-XC01-HKNKBN
                   MOVE    SPACE           TO  SPA-XC01-RSI-HKNKBN
           END-EVALUATE
           .
       41012-HKNCOMBI-SELHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       41012-LISTSEL-SEC       SECTION.
      *
      *    ������
           MOVE    SPACE               TO  SPA-I411-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-I411-HKNCOMBI-MAX
               IF      I411-SELECT (IDX)   =   "T"
                   MOVE    SPA-I411-THKNCOMBI(IDX)  TO   SPA-I411-SELNUM
                   MOVE    SPA-I411-HKNCOMBI-MAX    TO   IDX
               END-IF
           END-PERFORM
      *
           IF      SPA-I411-SELNUM     =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
      *        �ݸ��ȹ礻�����å�����
               MOVE    SPA-I411-SELNUM     TO  I411-SELNUM
               PERFORM 41011-SELNUM-CHK-SEC
           END-IF
           .
      *
       41012-LISTSEL-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "I41"               TO  SPA-SAKIPG
           MOVE    "I411"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "I41"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC.
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "I411"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "���ϥ��顼"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "�ݸ��ȹ礻���ϥ��顼"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  I4ERR.
           MOVE    SPA-ERRCD           TO  I4ERR-ERRCODE.
           MOVE    SPA-ERRMSG          TO  I4ERR-ERRMSG.
           MOVE    "B01"               TO  MCP-WIDGET.
      *
           MOVE    "I411"              TO  SPA-MOTOPG.
           MOVE    "I4ERR"             TO  SPA-SAKIPG.
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "I4ERR"             TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       510-ERRSET-EXT.
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
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *