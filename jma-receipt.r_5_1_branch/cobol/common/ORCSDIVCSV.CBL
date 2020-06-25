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
       PROGRAM-ID.             ORCSDIVCSV.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : CSV�쥳����ʬ��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  13/12/12    NACL-��ƣ     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-EDIT-AREA.
               05  FLG-COMMA           PIC 9(01).
               05  FLG-DQST            PIC 9(01).
               05  FLG-DQED            PIC 9(01).
               05  FLG-NODATA          PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-REC-LEN             PIC 9(04).
           03  WRK-POINT-AREA.
               05  WRK-PO-ST           PIC 9(04).
               05  WRK-PO-ED           PIC 9(04).
           03  WRK-ELEMENT-NO          PIC 9(04).
           03  WRK-MOVE-AREA.
               05  WRK-MO-ST           PIC 9(04).
               05  WRK-MO-BYTE         PIC 9(04).
               05  WRK-WK1             PIC 9(04).
      *
      *    �������ΰ�
       01  CONST-AREA.
           03  CONST-RECSIZE-MAX       PIC 9(04)  VALUE 3000.
           03  CONST-TBL-MAX           PIC 9(03)  VALUE 100.
           03  CONST-ELESIZE-MAX       PIC 9(03)  VALUE 300.
           03  CONST-DQ                PIC X(01)  VALUE X"22".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY  "CPORCSDIVCSV.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSDIVCSV-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE  FLG-AREA
           INITIALIZE  IDX-AREA
           INITIALIZE  WRK-AREA
      *
           PERFORM 100-PARAM-CHECK-SEC
      *
           PERFORM 200-DIVIDE-CSV-SEC
                   UNTIL   FLG-END =   1
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ���������å�
      *****************************************************************
       100-PARAM-CHECK-SEC         SECTION.
      *
           IF     (ORCSDIVCSV-REC      =   SPACE)  OR
                  (ORCSDIVCSV-ELE-C    =   ZERO )  OR
                  (ORCSDIVCSV-ELE-C    >   CONST-TBL-MAX )
               MOVE    1                   TO  ORCSDIVCSV-RC
               MOVE    1                   TO  FLG-END
           ELSE
               PERFORM 1001-REC-SIZE-SEC
           END-IF
           .
       100-PARAM-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ǡ�����ͭ��ʸ���������
      *****************************************************************
       1001-REC-SIZE-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-REC-LEN
      *
           PERFORM VARYING IDX
                           FROM    CONST-RECSIZE-MAX
                           BY      -1
                   UNTIL   IDX         <   1
               IF      ORCSDIVCSV-CHAR (IDX)
                                       NOT =   SPACE
                   MOVE    IDX                 TO  WRK-REC-LEN
                   MOVE    1                   TO  IDX
               END-IF
           END-PERFORM
           .
       1001-REC-SIZE-EXT.
           EXIT.
      *
      *****************************************************************
      *    CSV�쥳���ɤ���������Υǡ��������
      *****************************************************************
       200-DIVIDE-CSV-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-POINT-AREA
      *
           PERFORM VARYING IDX
                           FROM    1
                           BY      1
                   UNTIL   FLG-END     =   1
               COMPUTE WRK-PO-ST           =   WRK-PO-ED   +   1
               MOVE    ZERO                TO  FLG-EDIT-AREA
               MOVE    IDX                 TO  WRK-ELEMENT-NO
      *
               IF      ORCSDIVCSV-REC (WRK-PO-ST:)
                                           =   SPACE
                   MOVE    WRK-PO-ST           TO  WRK-PO-ED
               ELSE
      *            ���ܽ�λʸ����Υ���ް��֤����(WRK-PO-ED)
                   PERFORM 2001-CHAR-END-SEC
               END-IF
      *
      *        ���ܶ��̽���
               PERFORM 2002-ELEMENT-KYOTU-SEC
      *
               IF     (IDX                 =   ORCSDIVCSV-ELE-C)   OR
                      (WRK-PO-ED          >=   CONST-RECSIZE-MAX)  OR
                      (WRK-PO-ED          >=   WRK-REC-LEN)
                   MOVE    1                   TO  FLG-END
               END-IF
           END-PERFORM
           .
       200-DIVIDE-CSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    �оݹ��ܤ�ʸ����λ���֤����
      *****************************************************************
       2001-CHAR-END-SEC           SECTION.
      *
           PERFORM VARYING IDY
                           FROM    WRK-PO-ST
                           BY      1
                   UNTIL  (IDY        >    CONST-RECSIZE-MAX)
                       OR (IDY        >    WRK-REC-LEN)
                       OR (FLG-COMMA   =   1)
      *        �ǥ�ߥ��ΰ���
               IF      ORCSDIVCSV-CHAR (IDY)
                                           =   ORCSDIVCSV-DELI
                   MOVE    IDY                 TO  WRK-PO-ED
                   MOVE    1                   TO  FLG-COMMA
               END-IF
               IF     (IDY                 =   WRK-REC-LEN)  AND
                      (FLG-COMMA           =   0)
                   COMPUTE WRK-PO-ED           =   IDY +   1
               END-IF
      *        �ǥ�ߥ�(")¸�ߤ��뤫�ɤ���
               IF      ORCSDIVCSV-DQ       =   1
                   IF      ORCSDIVCSV-CHAR (IDY)
                                               =   CONST-DQ
                       IF      FLG-DQST            =   ZERO
                           MOVE    1                   TO  FLG-DQST
                       ELSE
                           MOVE    1                   TO  FLG-DQED
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
           .
       2001-CHAR-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ܶ��̽���
      *****************************************************************
       2002-ELEMENT-KYOTU-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-MOVE-AREA
      *
      *    �ǡ�������ά����Ƥ����硢FLG-NODATA��1�򥻥å�
           IF     (ORCSDIVCSV-DQ       =   1)  AND
                  (FLG-DQST            =   1)
      *        �ǥ�ߥ�(")��ޤ�Ǥ�����
               IF      FLG-DQED            =   1
                   COMPUTE WRK-WK1     =   WRK-PO-ST   +   2
               ELSE
                   COMPUTE WRK-WK1     =   WRK-PO-ST   +   1
               END-IF
               IF      WRK-WK1             =   WRK-PO-ED
                   MOVE    1                   TO  FLG-NODATA
               END-IF
           ELSE
      *        �ǥ�ߥ�(")��ޤ�Ǥ��ʤ����
               IF      WRK-PO-ST           =   WRK-PO-ED
                   MOVE    1                   TO  FLG-NODATA
               END-IF
           END-IF
      *
           IF      FLG-NODATA          =   ZERO
      *    ž�����ϰ��֡�ž������򻻽�
               IF     (ORCSDIVCSV-DQ       =   1)  AND
                      (FLG-DQST            =   1)
      *            �ǥ�ߥ�(")��ޤ�Ǥ�����
                   COMPUTE WRK-MO-ST           =   WRK-PO-ST   +   1
                   IF      FLG-DQED            =   1
                       COMPUTE WRK-MO-BYTE         =   WRK-PO-ED
                                                   -   WRK-PO-ST
                                                   -   2
                   ELSE
                       COMPUTE WRK-MO-BYTE         =   WRK-PO-ED
                                                   -   WRK-PO-ST
                                                   -   1
                   END-IF
               ELSE
      *            �ǥ�ߥ�(")��ޤ�Ǥ��ʤ����
                   MOVE    WRK-PO-ST           TO  WRK-MO-ST
                   COMPUTE WRK-MO-BYTE         =   WRK-PO-ED
                                               -   WRK-PO-ST
               END-IF
           END-IF
      *
           PERFORM 20021-DATA-COPY-SEC
           .
       2002-ELEMENT-KYOTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ǥǡ�����ơ��֥�س�Ǽ
      *****************************************************************
       20021-DATA-COPY-SEC         SECTION.
      *
           IF      WRK-MO-BYTE         >   CONST-ELESIZE-MAX
               MOVE    2                   TO  ORCSDIVCSV-RC
               MOVE    1                   TO  FLG-END
               GO  TO  20021-DATA-COPY-EXT
           END-IF
      *
           IF     (WRK-MO-BYTE         =   ZERO)   OR
                  (FLG-NODATA          =   1   )
               MOVE    SPACE               TO  ORCSDIVCSV-D (IDX)
           ELSE
               MOVE    ORCSDIVCSV-REC (WRK-MO-ST:WRK-MO-BYTE)
                                           TO  ORCSDIVCSV-D (IDX)
           END-IF
           .
       20021-DATA-COPY-EXT.
           EXIT.
      *