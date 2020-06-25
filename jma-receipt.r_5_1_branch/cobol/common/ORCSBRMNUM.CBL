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
       IDENTIFICATION                      DIVISION.
       PROGRAM-ID.                         ORCSBRMNUM.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : �¼�̾�Խ�����
      *  �ֵѥ��顼������  : 00:���99:���顼
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/05/14    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       DATA                                DIVISION.
       FILE                                SECTION.
       WORKING-STORAGE                     SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-BRM-NUM-EDT             PIC X(06).
           03  WRK-STR                     PIC X(20).
           03  WRK-IDX1                    PIC 9(05).
           03  WRK-IDX2                    PIC 9(05).
           03  WRK-KANACHK-MAE-INPUT       PIC X(500).
           03  WRK-KANACHK-OUT-INPUT       PIC X(500).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    ���ʥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                             SECTION.
      *
           COPY    "CPORCSBRMNUM.INC".
      *
       PROCEDURE                           DIVISION    USING
           ORCSBRMNUMAREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                        SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      SBRMNUM-RC
      *
           IF    ( SBRMNUM-IN-BRMNUM   =   SPACE )
               MOVE    SPACE   TO  SBRMNUM-OT-BRMNUM
           ELSE
      *
               PERFORM 010-PARA-CHK-SEC
               IF    ( SBRMNUM-RC  =   ZERO )
                   IF    ( SBRMNUM-IN-PROCKBN  =   "2" )
                       PERFORM 200-EDIT-SEC
                   ELSE
                       PERFORM 100-EDIT-SEC
                   END-IF
               END-IF
           END-IF
      *
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ���ϰ��������å�����
      *****************************************************************
       010-PARA-CHK-SEC                SECTION.
      *
      *    ����Ⱦ�Ѻ��ߥ����å�
           MOVE    SBRMNUM-IN-BRMNUM   TO  WRK-KANACHK-MAE-INPUT
      *
           PERFORM 800-HANKAKU-CHK-SEC
           IF    ( FLG-ENTRYCHK    NOT =   ZERO )
               MOVE    1               TO  SBRMNUM-RC
           END-IF
      *
           .
       010-PARA-CHK-EXT.
           .
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       100-EDIT-SEC                        SECTION.
      *
           IF    ( SBRMNUM-IN-BRMNUM (1 : 1)  =   SPACE )
               MOVE    SPACE           TO  WRK-STR
               MOVE    1               TO  IDX1
               UNSTRING    SBRMNUM-IN-BRMNUM
                   DELIMITED   BY  ALL SPACE
                   INTO    WRK-STR
                   WITH    POINTER IDX1
               END-UNSTRING
               MOVE    SBRMNUM-IN-BRMNUM (IDX1 : )
                                       TO  WRK-BRM-NUM-EDT
           ELSE
               MOVE    SBRMNUM-IN-BRMNUM  TO  WRK-BRM-NUM-EDT
           END-IF
      *
           PERFORM VARYING IDX1    FROM    6   BY  -1
               UNTIL     ( IDX1    <   1 )
                 OR      ( WRK-BRM-NUM-EDT (IDX1 : 1)
                                   NOT =   SPACE )
               CONTINUE
           END-PERFORM
      *
           IF    ( IDX1    <   1 )
               MOVE    WRK-BRM-NUM-EDT     TO  SBRMNUM-OT-BRMNUM
               GO  TO  100-EDIT-EXT
           END-IF
      *
      *    �¼��ֹ椬�����ξ����������Ȥ�
           IF    ( WRK-BRM-NUM-EDT ( 1 : IDX1 )
                                       IS  NUMERIC )
               IF    ( WRK-BRM-NUM-EDT ( 1 : 1 )
                                       =   ZERO )
                   MOVE    1               TO  IDX2
                   MOVE    SPACE           TO  WRK-STR
                   UNSTRING    WRK-BRM-NUM-EDT
                       DELIMITED BY  ALL ZERO
                   INTO    WRK-STR
                   WITH    POINTER IDX2
                   END-UNSTRING
                   MOVE    WRK-BRM-NUM-EDT (IDX2 : )
                                           TO  SBRMNUM-OT-BRMNUM
               ELSE
                   MOVE    WRK-BRM-NUM-EDT TO  SBRMNUM-OT-BRMNUM
               END-IF
           ELSE
                   MOVE    WRK-BRM-NUM-EDT TO  SBRMNUM-OT-BRMNUM
           END-IF
      *
           .
       100-EDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Խ�����
      *****************************************************************
       200-EDIT-SEC                SECTION.
      *
           MOVE    SPACE       TO  WRK-STR
           MOVE    1           TO  WRK-IDX1
           STRING  SBRMNUM-IN-BRMNUM
               DELIMITED   BY  SPACE
               INTO    WRK-STR
               WITH    POINTER WRK-IDX1
           END-STRING
      *
           COMPUTE WRK-IDX1    =   WRK-IDX1    -   1
      *
           IF    ( WRK-IDX1    <   1 )
               MOVE    SBRMNUM-IN-BRMNUM  TO  SBRMNUM-OT-BRMNUM
               GO  TO  200-EDIT-EXT
           END-IF
      *
      *    �¼��ֹ椬�����ξ�祼���ͤ��Ԥ�
           IF    ( SBRMNUM-IN-BRMNUM ( 1 : WRK-IDX1 )
                                   IS  NUMERIC )
               COMPUTE WRK-IDX2    =   6   -   WRK-IDX1 + 1
               MOVE    ZERO    TO  SBRMNUM-OT-BRMNUM
               MOVE    SBRMNUM-IN-BRMNUM ( 1 : WRK-IDX1 )
                           TO  SBRMNUM-OT-BRMNUM (WRK-IDX2 : WRK-IDX1)
           ELSE
               MOVE    SBRMNUM-IN-BRMNUM
                           TO  SBRMNUM-OT-BRMNUM
           END-IF
      *
           .
       200-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    Ⱦ��ʸ�������å�
      *****************************************************************
       800-HANKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-MAE-INPUT
                                   =   SPACE )
               GO  TO  800-HANKAKU-CHK-EXT
           END-IF
      *
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-KANACHK-MAE-INPUT
                                   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      ( KANACHK-RC    =   ZERO )
               IF      ( KANACHK-SYUBETU   NOT =   1 )
                   DISPLAY "HANKAKU CHK ERR : " WRK-KANACHK-MAE-INPUT
                   MOVE    1       TO  FLG-ENTRYCHK
               END-IF
           ELSE
                   DISPLAY "HANKAKU CHK ERR : " WRK-KANACHK-MAE-INPUT
                   MOVE    1       TO  FLG-ENTRYCHK
           END-IF
      *
           .
       800-HANKAKU-CHK-EXT.
           EXIT.