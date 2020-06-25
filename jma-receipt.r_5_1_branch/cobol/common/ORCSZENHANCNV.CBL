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
       PROGRAM-ID.             ORCSZENHANCNV.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ���Ѣ�Ⱦ���Ѵ�����
      *  ������            : 
      *  08/09/16    NACL��ƣ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  19/04/01  �������б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
           03  FLG-YM              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-HENKAN-AREA.
               05  WRK-HENKAN-IN   PIC X(50).
               05  WRK-HENKAN-OUT  PIC X(25).
               05  WRK-HENKAN-LEN  PIC 9(02).
      *
               05  WRK-MOJI-IN     PIC X(50).
               05  WRK-MOJI-OUT    PIC X(25).
               05  WRK-MOJI-LEN    PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY   "CPORCXKANACONV.INC".
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   �����Ѵ�����
           COPY    "CPORCSGDAY.INC".
      *
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSZENHANCNV.INC".
      *****************************************************************
     *
       PROCEDURE                   DIVISION    USING
                                   ORCSZENHANCNVAREA
           .
     *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    ZERO                TO  LNK-ZENHANCNV-RC    
      *    ������ʬ�ˤ�긡������
           EVALUATE    LNK-ZENHANCNV-SYORI
               WHEN    "1"
                   PERFORM 200-YMD-HENSYU-SEC
               WHEN    "2"
                   PERFORM 200-NUM-HENSYU-SEC
               WHEN    "3"
                   PERFORM 200-MOJI-HENSYU-SEC
               WHEN    OTHER
                   MOVE    1               TO  LNK-ZENHANCNV-RC
           END-EVALUATE
      *
           IF      LNK-ZENHANCNV-RC    >   0
               INITIALIZE                  LNK-ZENHANCNV-OUT
           END-IF
      *??
           IF      LNK-ZENHANCNV-RC    >   0
               DISPLAY "SYORI="  LNK-ZENHANCNV-SYORI
               DISPLAY "DATA ="  LNK-ZENHANCNV-IN-DATA
               DISPLAY "LEN  ="  LNK-ZENHANCNV-IN-LEN
               DISPLAY "LEN1 ="  LNK-ZENHANCNV-IN-LEN1
               DISPLAY "RC   ="  LNK-ZENHANCNV-RC
           END-IF
      ***  DISPLAY "SYMD ="  LNK-ZENHANCNV-OT-SYMD
      ***  DISPLAY "WYMD ="  LNK-ZENHANCNV-OT-WYMD
      ***  DISPLAY "GYMD ="  LNK-ZENHANCNV-OT-GYMD
      ***  DISPLAY "DATA ="  LNK-ZENHANCNV-OT-DATA
      ***  DISPLAY "LEN  ="  LNK-ZENHANCNV-OT-LEN
      ***  DISPLAY "NUM  ="  LNK-ZENHANCNV-OT-NUM
      *??
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *     
      *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       200-YMD-HENSYU-SEC       SECTION.
      *
           IF      LNK-ZENHANCNV-IN-LEN
                                   NOT =   LNK-ZENHANCNV-IN-LEN1
              MOVE    9               TO  LNK-ZENHANCNV-RC
              GO  TO  200-YMD-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-YM
      *
           IF      LNK-ZENHANCNV-IN-DATA (1:4) 
                                       =   "ʿ��"  OR  "����"  OR
                                           "����"  OR  "����"
      *
      *2019.5.1����ο������б������¡�
                                       OR  "����"
      *
               PERFORM 2001-YMD-WAREKI-HENSYU-SEC
           ELSE
               IF      LNK-ZENHANCNV-IN-DATA (1:2) 
                                           =   "��"
                   PERFORM 2002-YMD-SEIREKI-HENSYU-SEC
               ELSE
                   MOVE    2               TO  LNK-ZENHANCNV-RC
                   GO  TO  200-YMD-HENSYU-EXT
               END-IF
           END-IF
           .
       200-YMD-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       2001-YMD-WAREKI-HENSYU-SEC     SECTION.
      *
           IF      LNK-ZENHANCNV-IN-DATA (17:4)
                                       =   SPACE
               MOVE    1                   TO  FLG-YM
           END-IF
      *
           EVALUATE    LNK-ZENHANCNV-IN-DATA (1:4) 
               WHEN    "ʿ��"
                   MOVE    "4"         TO  LNK-ZENHANCNV-OT-WYMD (1:1)
               WHEN    "����"
                   MOVE    "3"         TO  LNK-ZENHANCNV-OT-WYMD (1:1)
               WHEN    "����"
                   MOVE    "2"         TO  LNK-ZENHANCNV-OT-WYMD (1:1)
               WHEN    "����"
                   MOVE    "1"         TO  LNK-ZENHANCNV-OT-WYMD (1:1)
      *2019.5.1����ο������б������¡�
               WHEN    "����"
                   MOVE    "5"         TO  LNK-ZENHANCNV-OT-WYMD (1:1)
           END-EVALUATE
           MOVE    LNK-ZENHANCNV-IN-DATA (5:4)
                                       TO  WRK-MOJI-IN
           MOVE    4                   TO  WRK-MOJI-LEN
           PERFORM 7003-MOJI-HENKAN-SEC
           MOVE    WRK-HENKAN-OUT      TO  LNK-ZENHANCNV-OT-WYMD (2:2)
           MOVE    LNK-ZENHANCNV-IN-DATA (11:4)
                                       TO  WRK-MOJI-IN
           MOVE    4                   TO  WRK-MOJI-LEN                    
           PERFORM 7003-MOJI-HENKAN-SEC
           MOVE    WRK-HENKAN-OUT      TO  LNK-ZENHANCNV-OT-WYMD (4:2)
           IF      FLG-YM              =   ZERO
               MOVE    LNK-ZENHANCNV-IN-DATA (17:4)
                                           TO  WRK-MOJI-IN
               MOVE    4                   TO  WRK-MOJI-LEN                    
               PERFORM 7003-MOJI-HENKAN-SEC
               MOVE    WRK-HENKAN-OUT      TO
                                           LNK-ZENHANCNV-OT-WYMD (6:2)
           END-IF
      *
           INSPECT LNK-ZENHANCNV-OT-WYMD   REPLACING   ALL " "
                                                       BY  "0"
      *
      *    ��������
           INITIALIZE                      ORCSGDAYAREA
           IF      FLG-YM              =   ZERO
               MOVE    LNK-ZENHANCNV-OT-WYMD
                                           TO  SGDAY-INDAY
           ELSE
               MOVE    LNK-ZENHANCNV-OT-WYMD(1:5)
                                           TO  SGDAY-INDAY
               MOVE    "1"                 TO  SGDAY-INTYPE
           END-IF
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               IF      FLG-YM              =   ZERO
                   MOVE    SGDAY-SDAY          TO  LNK-ZENHANCNV-OT-SYMD
                   MOVE    SGDAY-OTDAY         TO  LNK-ZENHANCNV-OT-GYMD
               ELSE
                   MOVE    SGDAY-SDAY (1:6)    TO  LNK-ZENHANCNV-OT-SYMD
                   MOVE    SGDAY-OTDAY(1:6)    TO  LNK-ZENHANCNV-OT-GYMD
               END-IF
           ELSE
               MOVE    3                   TO  LNK-ZENHANCNV-RC
           END-IF                 
           .
       2001-YMD-WAREKI-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       2002-YMD-SEIREKI-HENSYU-SEC     SECTION.
      *
           IF      LNK-ZENHANCNV-IN-DATA (13:4)
                                       =   SPACE
               MOVE    1                   TO  FLG-YM
           END-IF
      *
           MOVE    LNK-ZENHANCNV-IN-DATA
                                       TO  WRK-MOJI-IN
           MOVE    8                   TO  WRK-MOJI-LEN
           PERFORM 7003-MOJI-HENKAN-SEC
      *
           INSPECT WRK-HENKAN-OUT (1:8)    REPLACING   ALL " "
                                                       BY  "0"
      *
      *    ��������
           INITIALIZE                      ORCSGDAYAREA
           IF      FLG-YM              =   ZERO
               MOVE    WRK-HENKAN-OUT (1:8)
                                           TO  SGDAY-INDAY
           ELSE
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    WRK-HENKAN-OUT (1:6)
                                           TO  SGDAY-INDAY
           END-IF
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               IF      FLG-YM              =   ZERO
                   MOVE    SGDAY-SDAY          TO  LNK-ZENHANCNV-OT-SYMD
                   MOVE    SGDAY-OTDAY         TO  LNK-ZENHANCNV-OT-GYMD
               ELSE
                   MOVE    SGDAY-SDAY (1:6)    TO  LNK-ZENHANCNV-OT-SYMD
                   MOVE    SGDAY-OTDAY(1:6)    TO  LNK-ZENHANCNV-OT-GYMD
               END-IF
           ELSE
               MOVE    3                   TO  LNK-ZENHANCNV-RC
           END-IF                 
           .
       2002-YMD-SEIREKI-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    ʸ���Ѵ�����
      *****************************************************************
       200-MOJI-HENSYU-SEC         SECTION.
      *
           MOVE    LNK-ZENHANCNV-IN-DATA (1:LNK-ZENHANCNV-IN-LEN)
                                   TO  WRK-MOJI-IN
           MOVE    LNK-ZENHANCNV-IN-LEN
                                   TO  WRK-MOJI-LEN
           PERFORM 7003-MOJI-HENKAN-SEC
           MOVE    WRK-HENKAN-OUT  TO  LNK-ZENHANCNV-OT-DATA
           MOVE    WRK-HENKAN-LEN  TO  LNK-ZENHANCNV-OT-LEN
           .
       200-MOJI-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       200-NUM-HENSYU-SEC         SECTION.
      *
           MOVE    LNK-ZENHANCNV-IN-DATA (1:LNK-ZENHANCNV-IN-LEN)
                                   TO  WRK-MOJI-IN
           MOVE    LNK-ZENHANCNV-IN-LEN
                                   TO  WRK-MOJI-LEN
           PERFORM 7003-MOJI-HENKAN-SEC
           MOVE    WRK-HENKAN-OUT  TO  LNK-ZENHANCNV-OT-DATA
           MOVE    WRK-HENKAN-LEN  TO  LNK-ZENHANCNV-OT-LEN
      *
      *    
           INITIALIZE                  ORCSNUMAREA
           MOVE    LNK-ZENHANCNV-OT-DATA
                                   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF      SNUM-RC             =   ZERO
               MOVE    SNUM-OUTNUM     TO  LNK-ZENHANCNV-OT-NUM
           ELSE
               MOVE    4               TO  LNK-ZENHANCNV-RC
           END-IF
           .
       200-NUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ʸ����Ⱦ��ʸ���Ѵ�����
      *****************************************************************
       7003-MOJI-HENKAN-SEC            SECTION.
      *
           INITIALIZE                      ORCXKANACONVAREA 
           MOVE    1000                TO  KANACONV-STLEN
           MOVE    WRK-MOJI-LEN        TO  KANACONV-LEN
           MOVE    1                   TO  KANACONV-CONV-FLAG
           MOVE    112                 TO  KANACONV-CHAR-TYPE
           MOVE    WRK-MOJI-IN         TO  KANACONV-INDATA
           CALL    "kanaconv"          USING   ORCXKANACONVAREA
           IF      KANACONV-RETURN     =   0
               MOVE   KANACONV-OUTDATA (1:KANACONV-MAXLEN)
                                           TO  WRK-HENKAN-OUT
               MOVE   KANACONV-MAXLEN      TO  WRK-HENKAN-LEN
           END-IF
      *
           .
       7003-MOJI-HENKAN-EXT.
           EXIT.
