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
       PROGRAM-ID.             ORCSKANACHK2.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : Ⱦ�ѥ����å������ʳ�ĥ�����б���
      *  �ֵѥ��顼������  : 0:���1:Ⱦ�Ѻ��ߡ������Ѵ����顼
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  09/10/27    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.00    NACL-¿��Ǽ  10/10/01  ���ϡ������ΰ��X(5000)��
      *  05.01.00    NACL-¿��Ǽ  17/09/26  ���ԥ�����Ƚ���ɲ�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-ERR             PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-KANA2           PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANJI-ERR       PIC 9(01).
           03  FLG-KUHAKU          PIC 9(01).
           03  FLG-ZENKAKU1        PIC 9(01).
           03  FLG-ZENKAKU2        PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-MAX2            PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX-END             PIC 9(04).
      *
      *
        01  WRK-AREA.
           03  WRK-WYMD.
               05  WRK-WGO             PIC X(01).
               05  WRK-WYY             PIC 9(02).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
      *
           03  WRK-INDAY.
               05  WRK-YY              PIC X(10).
               05  WRK-MM              PIC X(02).
               05  WRK-DD              PIC X(02).
      *
           03  WRK-HMM                 PIC 9(02).
           03  WRK-HDD                 PIC 9(02).
      *
           03  WRK-MOJI                PIC X(01).
           03  WRK-KIGO                PIC X(01).
           03  WRK-ZENMOJI             PIC X(02).
           03  WRK-ZENMOJI2            PIC X(02).
      *
           03  WRK-MEISYO              PIC X(5000).
      *
           03  WRK-ERRCD               PIC X(02).
      *
           03  WRK-MAX-CNT             PIC 9(04).
      *
           03  WRK-IDX1                PIC 9(04).
           03  WRK-IDX2                PIC 9(04).
      *    ����ʸ����
           03  WRK-MAX-MOJI            PIC S9(9)  BINARY.
      *
      *R01.8
           03  WRK-PCNT                PIC 9(04).
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCXKANACONV2.INC".
      *
      *    ��ĥʸ���б�
           COPY    "CPORCSSTRING.INC".
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSKANACHK2.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSKANACHK2AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO                TO  KANACHK2-RC
           MOVE    ZERO                TO  KANACHK2-RC2
           MOVE    SPACE               TO  KANACHK2-OUT-INPUT
      *    ������Ƚ��
           MOVE    ZERO                TO  IDX-MAX
           PERFORM VARYING     IDX     FROM    5000 BY  -1
                   UNTIL       IDX     <   1
               IF     KANACHK2-MAE-INPUT (IDX:1)    NOT =   SPACE
                   MOVE    IDX             TO  IDX-MAX
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           IF      IDX-MAX             =   ZERO
               MOVE    ZERO                TO  KANACHK2-MAX
               MOVE    SPACE               TO  KANACHK2-OUT-INPUT
           ELSE
               MOVE    IDX-MAX             TO  KANACHK2-MAX
           END-IF
      *
           IF      IDX-MAX             >   ZERO
      *        ���Ѥ�����Ƚ��
               PERFORM 1000-KOUMOKU-CHK-SEC
      *
               EVALUATE    KANACHK2-SYORI
                   WHEN    "1"
      *            ����Ƚ�����
                   PERFORM 100-ZENKAKU-CHECK-SEC
                   WHEN    "2"
      *            �����ִ�����
                   PERFORM 200-ZENKAKU-HENKAN-SEC
               END-EVALUATE
      *H29.9
      *        ���ԥ����ɥ����å�
               PERFORM 1009-KAIGYO-CHK-SEC
      *
           END-IF
      *TEST
      *    DISPLAY "KANACHK2-MAE-INPUT:" KANACHK2-MAE-INPUT (1:50)
      *    DISPLAY "KANACHK2-OUT-INPUT:" KANACHK2-OUT-INPUT (1:50)
      *    DISPLAY "KANACHK2-RC :" KANACHK2-RC 
      *          ",KANACHK2-RC2:" KANACHK2-RC2
      *          ",KANACHK2-RC3:" KANACHK2-RC3
      *          ",KANACHK2-RC4:" KANACHK2-RC4
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ���ѥ����å�����
      *****************************************************************
       1000-KOUMOKU-CHK-SEC           SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK2-MAX-CNT     TO  KANACONV-LEN
           IF      KANACHK2-MAX-CNT     =   ZERO
               MOVE    IDX-MAX             TO  KANACONV-LEN
           END-IF
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    256                 TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK2-MAE-INPUT  TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
      *
           MOVE    KANACONV-OUTDATA    TO  KANACHK2-MAE-INPUT
           MOVE    KANACONV-OUTDATA    TO  KANACHK2-OUT-INPUT
           IF      KANACONV-MAXLEN     =   IDX-MAX
               MOVE    ZERO                TO  KANACHK2-RC2
           ELSE
               MOVE    7                   TO  KANACHK2-RC2
           END-IF
           MOVE    KANACONV-MAXLEN     TO  IDX-MAX
           MOVE    KANACONV-MAXLEN     TO  KANACHK2-MAX
           IF      KANACHK2-MAX-CNT    =   ZERO
               MOVE    5000                TO  KANACHK2-MAX-CNT
           END-IF
      *    ��ĥ����̵ͭ
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "check"             TO  ORCSSTR-COMMAND
           MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM2    NOT =   ZERO
               MOVE    1               TO  KANACHK2-RC4
           END-IF
      *TEST
      *    DISPLAY "1000-KOUMOKU-CHK"
      *    DISPLAY "KANACHK2-OUT-INPUT:" KANACHK2-OUT-INPUT (1:50)
      *    DISPLAY "ORCSSTR-NUM1:" ORCSSTR-NUM1
      **         ",ORCSSTR-NUM2:"  ORCSSTR-NUM2
      *
           .
       1000-KOUMOKU-CHK-EXT.
           EXIT.
      *
      *H29.9
      *****************************************************************
      *    ����Ƚ�����
      *****************************************************************
       1009-KAIGYO-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-PCNT
           INSPECT     KANACHK2-OUT-INPUT  TALLYING  WRK-PCNT
                                       FOR ALL  X"0A"
      *    ���ԥ����ɤ���
           IF      WRK-PCNT            >   ZERO
               MOVE    1                   TO  KANACHK2-RC5
           END-IF
      *
      *R01.8 �ѹ�
      *    ����Ƚ��
      *    INITIALIZE                  ORCSSTRINGAREA
      *    MOVE    "search"            TO  ORCSSTR-COMMAND
      *    MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
      *    MOVE    X"0A"               TO  ORCSSTR-STRING2
      *    CALL    "orcsstring"        USING
      *                                ORCSSTRINGAREA
      *    IF      ORCSSTR-NUM1    NOT =   ZERO
      *        MOVE    1                   TO  KANACHK2-RC5
      **** END-IF
           .
       1009-KAIGYO-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ����Ƚ�����
      *****************************************************************
       100-ZENKAKU-CHECK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-KANJI
           MOVE    ZERO                TO  FLG-KANA
           MOVE    ZERO                TO  FLG-KUHAKU
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK2-MAX-CNT     TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    271                 TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK2-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
      *        ���ѤΤ�
               MOVE    1                   TO  FLG-KANJI
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    KANACHK2-MAX-CNT    TO  KANACONV-LEN
               MOVE    5000                TO  KANACONV-STLEN
               MOVE    1                   TO  KANACONV-CONV-FLAG
               MOVE    271                 TO  KANACONV-CHAR-TYPE
               MOVE    KANACHK2-MAE-INPUT   TO  KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           ORCXKANACONVAREA
               IF      KANACONV-MAXLEN     =   IDX-MAX
                   CONTINUE
               ELSE
      *            Ⱦ�Ѷ�����ִ�������
                   MOVE    8                   TO  KANACHK2-RC2
               END-IF
               MOVE    KANACONV-OUTDATA    TO  KANACHK2-OUT-INPUT
               MOVE    2                   TO  KANACHK2-SYUBETU
               MOVE    KANACONV-MAXLEN     TO  KANACHK2-MAX
           ELSE
      *        Ⱦ�ѤΤߤ�Ƚ��
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    KANACHK2-MAX-CNT    TO  KANACONV-LEN
               MOVE    5000                TO  KANACONV-STLEN
               MOVE    0                   TO  KANACONV-CONV-FLAG
               MOVE    113                 TO  KANACONV-CHAR-TYPE
               MOVE    KANACHK2-MAE-INPUT  TO  KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           ORCXKANACONVAREA
               IF      KANACONV-RETURN     =   ZERO
                   MOVE    1                   TO  KANACHK2-SYUBETU
                   MOVE    KANACONV-MAXLEN     TO  KANACHK2-MAX
                   MOVE    KANACONV-MAXLEN     TO  IDX-MAX
      *            �֤˶��򤬤��뤫��Ƚ��
                   PERFORM 1001-KUHAKU-CHK-SEC
               ELSE
      *            ���ߥ��顼
                   MOVE    1                   TO  KANACHK2-RC
               END-IF
           END-IF
      *
      *    ���Ѷ����Ƚ��
           IF     (KANACHK2-SYUBETU     =   2    )  AND
                  (KANACHK2-RC          =   ZERO )
               PERFORM 1003-ZENSPACE-CHK-SEC
           END-IF
           .
       100-ZENKAKU-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �֤�Ⱦ�Ѷ��򤬤��뤫��Ƚ�����
      *****************************************************************
       1001-KUHAKU-CHK-SEC           SECTION.
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK2-MAX-CNT     TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    0                   TO  KANACONV-CONV-FLAG
           MOVE    112                 TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK2-MAE-INPUT   TO  KANACONV-INDATA
           CALL    "kanaconv"          USING
                                       ORCXKANACONVAREA
           IF      KANACONV-RETURN NOT =   ZERO
               MOVE    8                   TO  KANACHK2-RC2
           END-IF
      *
           .
       1001-KUHAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���Ѷ���Ƚ�����
      *****************************************************************
       1003-ZENSPACE-CHK-SEC           SECTION.
      *
      *    ����ʸ����
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "check"             TO  ORCSSTR-COMMAND
           MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           MOVE    ORCSSTR-NUM1        TO  WRK-MAX-MOJI
      *
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "search"            TO  ORCSSTR-COMMAND
           MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
           MOVE    "��"                TO  ORCSSTR-STRING2
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM1    NOT =   ZERO
      *        ���Ѷ��򤢤�
               MOVE    1                   TO  KANACHK2-RC3
      *        �ǽ�ʸ��Ƚ��
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
               MOVE    WRK-MAX-MOJI        TO  ORCSSTR-NUM1
               COMPUTE ORCSSTR-NUM2        =   WRK-MAX-MOJI
                                           +   1
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               IF      ORCSSTR-STRING2     =   "��"
      *            �Ǹ夬���Ѷ���
                   IF      KANACHK2-SYORIKBN    =   "1"
      *                �Ǹ�����Ѷ�����
                       PERFORM 10031-LASTDEL-HEN-SEC
                   ELSE
                       MOVE    3               TO  KANACHK2-RC3
                   END-IF
               END-IF
           END-IF
      *
      *    ��������
           IF     (KANACHK2-RC4        =   ZERO  )
           AND    (KANACHK2-RC3    NOT =   ZERO  )
               MOVE    ZERO                TO  FLG-ZENKAKU1
               MOVE    ZERO                TO  FLG-ZENKAKU2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   WRK-MAX-MOJI )
                              OR  (FLG-ZENKAKU1    =   1    )
                   INITIALIZE                  ORCSSTRINGAREA
                   MOVE    "substr"            TO  ORCSSTR-COMMAND
                   MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
                   MOVE    IDX                 TO  ORCSSTR-NUM1
                   MOVE    IDX                 TO  ORCSSTR-NUM2
                   CALL    "orcsstring"        USING
                                               ORCSSTRINGAREA
                   IF      ORCSSTR-STRING2     =   "��"
      *                ���Ѷ���
                       MOVE    1               TO  FLG-ZENKAKU2
                   ELSE
                       MOVE    1               TO  FLG-ZENKAKU1
                   END-IF
               END-PERFORM
               IF      FLG-ZENKAKU1        =   ZERO
      *            ���Ѷ���Τ�
                   MOVE    2               TO  KANACHK2-RC3
               END-IF
           END-IF
      *
           .
       1003-ZENSPACE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �Ǹ�����Ѷ���������
      *****************************************************************
       10031-LASTDEL-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDX-END
           PERFORM VARYING     IDX     FROM    WRK-MAX-MOJI   BY  -1
                   UNTIL      (IDX     <   1   )
                          OR  (IDX-END >   ZERO)
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
               MOVE    IDX                 TO  ORCSSTR-NUM1
               MOVE    IDX                 TO  ORCSSTR-NUM2
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               IF      ORCSSTR-STRING2     =   "��"
                   CONTINUE
               ELSE
                   MOVE    IDX                 TO  IDX-END
               END-IF
           END-PERFORM
      *
           MOVE    KANACHK2-OUT-INPUT  TO  WRK-MEISYO
           MOVE    SPACE               TO  KANACHK2-OUT-INPUT
           IF      IDX-END             >   ZERO
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    WRK-MEISYO          TO  ORCSSTR-STRING1
               MOVE    1                   TO  ORCSSTR-NUM1
               MOVE    IDX-END             TO  ORCSSTR-NUM2
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               MOVE    ORCSSTR-STRING2     TO  KANACHK2-OUT-INPUT
           END-IF
      *
           MOVE    ZERO                TO  KANACHK2-RC3
           IF      KANACHK2-OUT-INPUT  =   SPACE
               MOVE    ZERO                TO  KANACHK2-MAX
           ELSE
      *        ������
               MOVE    1                   TO  FLG-KANJI
               INITIALIZE                  ORCXKANACONVAREA
               MOVE    KANACHK2-MAX-CNT    TO  KANACONV-LEN
               MOVE    5000                TO  KANACONV-STLEN
               MOVE    1                   TO  KANACONV-CONV-FLAG
               MOVE    271                 TO  KANACONV-CHAR-TYPE
               MOVE    KANACHK2-OUT-INPUT  TO  KANACONV-INDATA
               CALL    "kanaconv"          USING
                                           ORCXKANACONVAREA
               MOVE    KANACONV-MAXLEN     TO  KANACHK2-MAX
      *
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "search"            TO  ORCSSTR-COMMAND
               MOVE    KANACHK2-OUT-INPUT  TO  ORCSSTR-STRING1
               MOVE    "��"                TO  ORCSSTR-STRING2
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               IF      ORCSSTR-NUM1    NOT =   ZERO
      *            ���Ѷ��򤢤�
                   MOVE    1                   TO  KANACHK2-RC3
               END-IF
           END-IF
           .
       10031-LASTDEL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���
      *****************************************************************
       200-ZENKAKU-HENKAN-SEC           SECTION.
      *
           MOVE    2                   TO  KANACHK2-SYUBETU
      *
           INITIALIZE                  ORCXKANACONVAREA
           MOVE    KANACHK2-MAX-CNT     TO  KANACONV-LEN
           MOVE    5000                TO  KANACONV-STLEN
           MOVE    1                   TO  KANACONV-CONV-FLAG
           MOVE    271                 TO  KANACONV-CHAR-TYPE
           MOVE    KANACHK2-MAE-INPUT  TO  KANACONV-INDATA
           CALL    "kanaconv"          USING
                                       ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   ZERO
               MOVE    2                   TO  KANACHK2-SYUBETU
               MOVE    KANACONV-OUTDATA    TO  KANACHK2-OUT-INPUT
               MOVE    KANACONV-MAXLEN     TO  KANACHK2-MAX
           ELSE
               MOVE    2                   TO  KANACHK2-RC
           END-IF
      *
      *    ���Ѷ����Ƚ��
           IF     (KANACHK2-SYUBETU     =   2    )  AND
                  (KANACHK2-RC          =   ZERO )
               PERFORM 1003-ZENSPACE-CHK-SEC
           END-IF
           .
       200-ZENKAKU-HENKAN-EXT.
           EXIT.
      *