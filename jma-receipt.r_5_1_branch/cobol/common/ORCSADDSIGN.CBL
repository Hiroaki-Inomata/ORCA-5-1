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
       PROGRAM-ID.             ORCSADDSIGN.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ����ʸ�����Խ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05.10.25    NaCl-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
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
      *
       01  FLG-AREA.
           03  FLG-STREDIT         PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX1                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-LEN             PIC 9(05).
           03  WRK-STR             PIC X(1000).
           03  WRK-STR-TMP         PIC X(1000).
      *
       01  CONST-AREA.
           03  CONST-LEN           PIC 9(05)   VALUE 1000.
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSADDSIGN.INC".
      *
      *
       PROCEDURE                    DIVISION    USING
                                    ORCSADDSIGNAREA.
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              IDX-AREA
                                   WRK-AREA
                                   FLG-AREA
      *
           MOVE    SADDSIGN-STR    TO      WRK-STR
      *
           IF    ( SADDSIGN-KBN    =       "1" )
               PERFORM 200-REPLACE-SIGN-SEC
           ELSE
               PERFORM 200-ADD-SIGN-SEC
           END-IF
      *
           MOVE    WRK-STR         TO      SADDSIGN-STR
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ���������֤���������
      *****************************************************************
       200-REPLACE-SIGN-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-STREDIT
      *
           PERFORM 210-GET-STRLEN-SEC
      *
           IF    ( WRK-LEN         >   1 )
               IF    ( WRK-STR (WRK-LEN - 1 : 2)   =   "��" )
                   MOVE    "%"     TO  WRK-STR (WRK-LEN - 1 : 2)
                   MOVE    1       TO  FLG-STREDIT
               END-IF
           END-IF
      *
           IF    ( WRK-STR (1 : 2)       =   "��" )
                   MOVE    "%"             TO  WRK-STR-TMP
                   MOVE    WRK-STR (3:)    TO  WRK-STR-TMP(2:)
                   MOVE    WRK-STR-TMP     TO  WRK-STR
                   MOVE    1               TO  FLG-STREDIT
           END-IF
      *
           IF    ( FLG-STREDIT     =   ZERO )
               IF    ( WRK-LEN     <   CONST-LEN )
                   MOVE    "%"     TO  WRK-STR (WRK-LEN + 1 : 1)
               END-IF
           END-IF
      *
           .
      *
       200-REPLACE-SIGN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������ɲý�������
      *****************************************************************
       200-ADD-SIGN-SEC            SECTION.
      *
           MOVE    SADDSIGN-STR    TO      WRK-STR
      *
           PERFORM 210-GET-STRLEN-SEC
      *
           MOVE    WRK-STR         TO      WRK-STR-TMP
      *
           EVALUATE    SADDSIGN-PTN
           WHEN    "2"
               STRING  "%"         DELIMITED   BY  SIZE
                       WRK-STR (1 : WRK-LEN)
                                   DELIMITED   BY  SIZE
               INTO    WRK-STR-TMP
               END-STRING
           WHEN    "3"
               STRING  "%"         DELIMITED   BY  SIZE
                       WRK-STR (1 : WRK-LEN)
                                   DELIMITED   BY  SIZE
                       "%"         DELIMITED   BY  SIZE
               INTO    WRK-STR-TMP
               END-STRING
           WHEN    OTHER
               STRING  WRK-STR (1 : WRK-LEN)
                                   DELIMITED   BY  SIZE
                       "%"         DELIMITED   BY  SIZE
               INTO    WRK-STR-TMP
               END-STRING
           END-EVALUATE
      *
           MOVE    WRK-STR-TMP     TO      WRK-STR
      *
           .
      *
       200-ADD-SIGN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ʸ����Ĺ��������
      *****************************************************************
       210-GET-STRLEN-SEC              SECTION.
      *
           MOVE    ZERO            TO      WRK-LEN
      *
           PERFORM VARYING IDX1    FROM    CONST-LEN   BY  -1
                   UNTIL ( IDX1    <   1 )
                    OR   ( WRK-LEN >   ZERO )
      *
               IF    ( WRK-STR (IDX1 : 1) NOT =   SPACE )
                   MOVE    IDX1    TO  WRK-LEN
               END-IF
      *
           END-PERFORM
      *
           .
       210-GET-STRLEN-EXT.
           EXIT.