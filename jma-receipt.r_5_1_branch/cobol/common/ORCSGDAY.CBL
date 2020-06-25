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
       PROGRAM-ID.             ORCSGDAY.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ���ղ������ϥ����å�����
      *  �ֵѥ��顼������  : 00:���10:���ե��顼��99���ѥ�᡼������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  XX.XX.XX    �ΣΣ�        ��������
      *****************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  02/11/21  ����ǯ�ǣ��������ǰʾ奨�顼
      * 01.00.02     NACL-¿��Ǽ  02/12/12  ���ϥ����פʤ��ɲ�
      * 04.06.00     NACL-¿��Ǽ  10/08/30  ǯ���ά�б�
      * 05.00.00     NACL-¿��Ǽ  19/04/01  �������б�
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
           03  FLG-TEN             PIC 9(01).
           03  FLG-SEKI            PIC 9(01).
           03  FLG-TYPE            PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
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
           03  WRK-SYSYMD.
               05  WRK-SYSYM.
                   07  WRK-SYSYY           PIC 9(04).
                   07  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD               PIC 9(02).
           03  WRK-SYSYMD-R                REDEFINES   WRK-SYSYMD.
               05  WRK-SYSYYR              PIC 9(04).
               05  WRK-SYSMMDD             PIC 9(04).
           03  WRK-SYSZYM.
               05  WRK-SYSZYY              PIC 9(04).
               05  WRK-SYSZMM              PIC 9(02).
           03  WRK-SYSWNEN.
               05  WRK-SYSWGO              PIC X(01).
               05  WRK-SYSWYY              PIC 9(02).
           03  WRK-HENYMD.
               05  WRK-HENYY               PIC 9(04).
               05  WRK-HENMMDD             PIC 9(04).
               05  WRK-HENMMDD-R           REDEFINES   WRK-HENMMDD.
                   07  WRK-HENMM               PIC 9(02).
                   07  WRK-HENDD               PIC 9(02).
      *
           03  WRK-HYY                 PIC X(03).
      *
           03  WRK-HMM                 PIC 9(02).
           03  WRK-HDD                 PIC 9(02).
      *
           03  WRK-INSUJI              PIC X(04).
           03  WRK-OTSUJI              PIC 9(04).
      *
           03  WRK-KETA                PIC 9(04).
      *
           03  WRK-ERRCD               PIC X(02).
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *   �����ֹ��Ѵ�����
      **** COPY    "CPORCSPTID.INC".
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ģ¸���
       01  MCPDATA-REC                 PIC X(5000).
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSGDAY.INC".
      *
       PROCEDURE                    DIVISION    USING
           ORCSGDAYAREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
      *
           MOVE    ZERO                TO  SGDAY-RC
           INITIALIZE                      SGDAY-OT-AREA
      *
      *    ���������Ѵ�
           IF      SGDAY-INDAY         =   SPACE  OR  ZERO
               MOVE    99                  TO  SGDAY-RC
           ELSE
      *        Ⱦ�ѥ����å�
               PERFORM 100-INDAY-INITCHK-SEC
           END-IF
           IF      SGDAY-RC            =   ZERO
      *        ���������Ѵ�
               PERFORM 100-INDAY-CHK-SEC
               IF      WRK-ERRCD       NOT =   SPACE
                   MOVE    99                  TO  SGDAY-RC
                   MOVE    SGDAY-INDAY         TO  SGDAY-OTDAY
               END-IF
           END-IF
      *
      *    �����Ѵ�
           IF      SGDAY-RC            =   ZERO
               PERFORM 200-DAY-HEN-SEC
           END-IF
      *
           .
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    Ⱦ�ѥ����å�����
      *****************************************************************
       100-INDAY-INITCHK-SEC               SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SGDAY-INDAY         TO  KANACHK-MAE-INPUT
           MOVE    10                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SGDAY-INDAY
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )
               CONTINUE
           ELSE
      *        Ⱦ�Ѱʳ��ϥ��顼
               MOVE    99                  TO  SGDAY-RC
               MOVE    SGDAY-INDAY         TO  SGDAY-OTDAY
           END-IF
           .
       100-INDAY-INITCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ո�������
      *****************************************************************
       100-INDAY-CHK-SEC               SECTION.
      *
      *    �����ƥ������Խ�
           IF     (SGDAY-SYSYMD        =   SPACE
                                       OR  ZERO )
               CONTINUE
           ELSE
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    SGDAY-SYSYMD        TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               IF      STS-DAY-RC1         =   ZERO
      *            ����ǯ
                   MOVE    LNK-DAY1-WYMD(1:3)  TO  WRK-SYSWNEN
      *            ǯ����
                   MOVE    SGDAY-SYSYY         TO  WRK-SYSYY
                   MOVE    SGDAY-SYSMM         TO  WRK-SYSMM
                   MOVE    SGDAY-SYSDD         TO  WRK-SYSDD
      *            ����
                   IF      WRK-SYSMM           =   01
                       MOVE    12                  TO  WRK-SYSZMM
                       COMPUTE WRK-SYSZYY          =   WRK-SYSYY   -   1
                   ELSE
                       COMPUTE WRK-SYSZMM          =   WRK-SYSMM   -   1
                       MOVE    WRK-SYSYY           TO  WRK-SYSZYY
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-INDAY
      *
           MOVE    ZERO                TO  IDX1
           MOVE    ZERO                TO  FLG-TEN
           MOVE    ZERO                TO  WRK-KETA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   10  )  OR
                              (WRK-ERRCD   NOT =   SPACE )
             IF      SGDAY-INDAY(IDX:1)  NOT =   SPACE
      *
               IF      SGDAY-INDAY(IDX:1)      =   "."
                   ADD     1               TO  FLG-TEN
                   MOVE    ZERO            TO  IDX1
               ELSE
                   EVALUATE    FLG-TEN
      *                 ǯ
                       WHEN    ZERO
                           ADD     1                   TO  IDX1
                           MOVE    SGDAY-INDAY(IDX:1)  TO
                                                       WRK-YY(IDX1:1)
                           MOVE    IDX1                TO  WRK-KETA
      *                 ��
                       WHEN    1
                           ADD     1                   TO  IDX1
                           IF      IDX1                >   2
                               MOVE    "99"            TO  WRK-ERRCD
                           ELSE
                               MOVE    SGDAY-INDAY(IDX:1)  TO
                                                       WRK-MM(IDX1:1)
                           END-IF
      *                 ��
                       WHEN    2
      *                    ǯ��λ������ϥ��顼
                           IF      SGDAY-INTYPE        =   "1"
                               MOVE    "99"            TO  WRK-ERRCD
                           END-IF
                           ADD     1                   TO  IDX1
                           IF      IDX1                >   2
                               MOVE    "99"            TO  WRK-ERRCD
                           ELSE
                               MOVE    SGDAY-INDAY(IDX:1)  TO
                                                       WRK-DD(IDX1:1)
                           END-IF
      *                 ����¾
                       WHEN    OTHER
                           MOVE    "99"                TO  WRK-ERRCD
                   END-EVALUATE
               END-IF
             END-IF
           END-PERFORM
      *
           EVALUATE    SGDAY-INTYPE
               WHEN    SPACE
      *            ǯ����
                   IF      FLG-TEN             =   ZERO
                       IF      WRK-KETA        NOT =   7   AND 8
                           MOVE    "99"                TO  WRK-ERRCD
                       END-IF
                   ELSE
                       IF      WRK-DD          =   SPACE
                           MOVE    "99"                TO  WRK-ERRCD
                       END-IF
                   END-IF
               WHEN    "1"
      *        ǯ��
                   IF      FLG-TEN             =   ZERO
                       IF      WRK-KETA        NOT =   5  AND 6
                           MOVE    "99"                TO  WRK-ERRCD
                       END-IF
                   END-IF
               WHEN    "2"
      *        �����å��ʤ�
                   IF      FLG-TEN             =   ZERO
                       EVALUATE    WRK-KETA
                           WHEN    2
      *                        ����ǯ
                               MOVE    3                   TO  FLG-TYPE
                               MOVE    "01"                TO  WRK-MM
                               MOVE    "01"                TO  WRK-DD
                           WHEN    3
                           WHEN    4
      *                        ǯ
                               MOVE    3                   TO  FLG-TYPE
                           WHEN    5
                           WHEN    6
      *                        ǯ��
                               MOVE    2                   TO  FLG-TYPE
                           WHEN    7
                           WHEN    8
      *                        ǯ����
                               MOVE    1                   TO  FLG-TYPE
                           WHEN    OTHER
                               MOVE    "99"                TO  WRK-ERRCD
                       END-EVALUATE
                   ELSE
                       MOVE    1                   TO  FLG-TYPE
                       IF      WRK-YY          =   SPACE
                           MOVE    "99"                TO  WRK-ERRCD
                       END-IF
                       IF      WRK-DD          =   SPACE
                           MOVE    "01"                TO  WRK-DD
                           MOVE    2                   TO  FLG-TYPE
                       END-IF
                       IF      WRK-MM          =   SPACE
                           MOVE    "01"                TO  WRK-MM
                           MOVE    3                   TO  FLG-TYPE
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    ǯ�������Ϥǥ����ƥ����դ����꤬�����
      *    IF     (WRK-ERRCD           =   SPACE )  OR
           IF     (WRK-SYSYM           =   SPACE )
             OR   (SGDAY-INTYPE    NOT =   SPACE )
               CONTINUE
           ELSE
               EVALUATE    FLG-TEN
               WHEN        ZERO
      *            ���Τ����ϡ������ƥ�ǯ��Ȥ���
                   EVALUATE    WRK-KETA
                   WHEN    1
                   WHEN    2
                       MOVE    WRK-SYSYMD          TO  WRK-HENYMD
                       MOVE    SGDAY-INDAY(1:WRK-KETA) TO  WRK-INSUJI
                       PERFORM 1001-SUUJI-CHK-SEC
                       MOVE    WRK-OTSUJI          TO  WRK-HENDD
                       MOVE    SPACE               TO  WRK-ERRCD
                       MOVE    1                   TO  FLG-TYPE
                       MOVE    8                   TO  WRK-KETA
                       IF      SGDAY-SYSKBN        =   "2"
      *                    �����ƥ����դ��̤�����λ��ϡ�����Ȥ���
                           IF      WRK-HENYMD          >   WRK-SYSYMD
                               MOVE    WRK-SYSZYM      TO  WRK-HENYMD
                                                                 (1:6)
                           END-IF
                       END-IF
                       MOVE    WRK-HENYMD          TO  SGDAY-INDAY
                   WHEN    3
                   WHEN    4
      *                �����Τ����ϡ������ƥ�ǯ��Ȥ���
                       MOVE    SGDAY-INDAY(1:WRK-KETA) TO  WRK-INSUJI
                       PERFORM 1001-SUUJI-CHK-SEC
                       MOVE    WRK-OTSUJI          TO  WRK-HENMMDD
                       EVALUATE    SGDAY-SYSKBN
                       WHEN    "1"
      *                    ������λ�������Ȥ���
                           IF      WRK-HENMM           =   12
                               MOVE    WRK-SYSZYY          TO  WRK-HENYY
                           ELSE
                               MOVE    WRK-SYSYY           TO  WRK-HENYY
                           END-IF
                       WHEN    "2"
      *                    �����ƥ����դ��̤��������ǯ�Ȥ���
                           IF      WRK-HENMMDD         >   WRK-SYSMMDD
                               COMPUTE WRK-HENYY       =   WRK-SYSYY
                                                       -   1
                           ELSE
                               MOVE    WRK-SYSYY           TO  WRK-HENYY
                           END-IF
                       WHEN    OTHER
      *                    �����ƥ�ǯ
                           MOVE    WRK-SYSYY           TO  WRK-HENYY
                       END-EVALUATE
                       MOVE    SPACE               TO  WRK-ERRCD
                       MOVE    1                   TO  FLG-TYPE
                       MOVE    8                   TO  WRK-KETA
                       MOVE    WRK-HENYMD          TO  SGDAY-INDAY
                   WHEN    6
      *                �����ǯ�����Τ����ϡ���������ꤹ��
                       MOVE    SPACE               TO  WRK-WYMD
                       STRING  WRK-SYSWGO          DELIMITED  BY  SIZE
                               SGDAY-INDAY         DELIMITED  BY  SPACE
                                                   INTO    WRK-WYMD
                       END-STRING
                       MOVE    WRK-WYMD            TO  SGDAY-INDAY
                       MOVE    SPACE               TO  WRK-ERRCD
                       MOVE    1                   TO  FLG-TYPE
                       MOVE    7                   TO  WRK-KETA
                   END-EVALUATE
               WHEN    1
      *            ǯ�η夬��������Ϸ�����Ƚ�Ǥ���
                   IF      WRK-KETA            =   1
                                               OR  2
                       MOVE    WRK-MM              TO  WRK-DD
                       MOVE    SPACE               TO  WRK-MM
                       MOVE    WRK-YY(1:WRK-KETA)  TO  WRK-INSUJI
                       PERFORM 1001-SUUJI-CHK-SEC
                       MOVE    WRK-OTSUJI          TO  WRK-HMM
                       MOVE    WRK-HMM             TO  WRK-MM
                       EVALUATE    SGDAY-SYSKBN
                       WHEN    "1"
      *                    ������λ�����ǯ�Ȥ���
                           IF      WRK-MM          =   "12"
                               MOVE    WRK-SYSZYY          TO  WRK-YY
                           ELSE
                               MOVE    WRK-SYSYY           TO  WRK-YY
                           END-IF
                       WHEN    "2"
      *                    �����ƥ����դ��̤�����ϲ�����Ȥ���
                           IF     (WRK-MM          >   WRK-SYSMM )
                               COMPUTE WRK-SYY         =   WRK-SYSYY
                                                       -   1
                               MOVE    WRK-SYY             TO  WRK-YY
                           ELSE
                               MOVE    WRK-SYSYY           TO  WRK-YY
                           END-IF
                       WHEN    OTHER
      *                    �����ƥ�ǯ
                               MOVE    WRK-SYSYY           TO  WRK-YY
                       END-EVALUATE
                       MOVE    2               TO  FLG-TEN
                       MOVE    SPACE           TO  WRK-ERRCD
                       MOVE    4               TO  WRK-KETA
                   END-IF
               WHEN    2
      *            ǯ�η夬���������ǯ�Τߤ�Ƚ�Ǥ���
                   IF     (WRK-KETA            =   1
                                               OR  2 )
                     AND  (WRK-YY(1:WRK-KETA)   NUMERIC   )
                       MOVE    SPACE               TO  WRK-HYY
                       STRING  WRK-SYSWGO          DELIMITED  BY  SIZE
                               WRK-YY              DELIMITED  BY  SPACE
                                                   INTO    WRK-HYY
                       END-STRING
                       MOVE    WRK-HYY             TO  WRK-YY
                       MOVE    SPACE               TO  WRK-ERRCD
                       MOVE    3                   TO  WRK-KETA
                   END-IF
               END-EVALUATE
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      100-INDAY-CHK-EXT
           END-IF
      *
      *    �ɡ��ɤʤ��Υ����å�
           IF      FLG-TEN             =   ZERO
               EVALUATE    WRK-KETA
                   WHEN    3
                   WHEN    5
                   WHEN    7
                       MOVE    SGDAY-INDAY       TO  WRK-WYMD
                       IF      WRK-KETA          =   3
                           MOVE    SGDAY-INDAY(1:3)  TO  WRK-WYMD(1:3)
                           MOVE    "0101"            TO  WRK-WYMD(4:4)
                       END-IF
                       IF      WRK-KETA          =   5
                           MOVE    SGDAY-INDAY(1:5)  TO  WRK-WYMD(1:5)
                           MOVE    01                TO  WRK-WDD
                       END-IF
                       MOVE    1                 TO  FLG-SEKI
      *                ����
                       EVALUATE    SGDAY-INDAY(1:1)
                           WHEN    "M"
                           WHEN    "m"
                           WHEN    "1"
                               MOVE    "1"                 TO  WRK-WGO
                           WHEN    "T"
                           WHEN    "t"
                           WHEN    "2"
                               MOVE    "2"                 TO  WRK-WGO
                           WHEN    "S"
                           WHEN    "s"
                           WHEN    "3"
                               MOVE    "3"                 TO  WRK-WGO
                           WHEN    "H"
                           WHEN    "h"
                           WHEN    "4"
                               MOVE    "4"                 TO  WRK-WGO
      *
      *2019.5.1����ο������б������¡�
                           WHEN    "R"
                           WHEN    "r"
                           WHEN    "5"
                               MOVE    "5"                 TO  WRK-WGO
      *
                       END-EVALUATE
                       IF      WRK-WYMD        NOT NUMERIC
                           MOVE    "99"        TO  WRK-ERRCD
                       END-IF
      *            ����
                   WHEN    4
                   WHEN    8
                   WHEN    6
                       MOVE    SGDAY-INDAY       TO  WRK-SYMD
                       IF      WRK-KETA          =   4
                           MOVE    SGDAY-INDAY(1:4)  TO  WRK-SYMD(1:4)
                           MOVE    "0101"            TO  WRK-SYMD(5:4)
                       END-IF
                       IF      WRK-KETA          =   6
                           MOVE    SGDAY-INDAY(1:6)  TO  WRK-SYMD(1:6)
                           MOVE    01                TO  WRK-SDD
                       END-IF
      *                �����ǯ���������ʲ��Ǥ��뤳��
                       IF      WRK-SYMD(1:2)       >=  "30"
                           MOVE    "99"            TO  WRK-ERRCD
                       END-IF
                       MOVE    2                   TO  FLG-SEKI
                       IF      WRK-SYMD        NOT NUMERIC
                           MOVE    "99"            TO  WRK-ERRCD
                       END-IF
      *
                   WHEN    2
                       MOVE    1                 TO  FLG-SEKI
      *
                   WHEN    OTHER
                       MOVE    "99"            TO  WRK-ERRCD
               END-EVALUATE
               IF      WRK-KETA        >   2
                   GO      TO      100-INDAY-CHK-EXT
               END-IF
           ELSE
      *        ǯ�η��
               IF      WRK-KETA        >   4
                   MOVE    "99"                TO  WRK-ERRCD
                   GO      TO      100-INDAY-CHK-EXT
               END-IF
           END-IF
      *
      *    ���ʤ��λ��������Ȥ���
           IF      FLG-TEN             =   1
               MOVE    "01"            TO  WRK-DD
           END-IF
      *
      *    ǯ�����å�
      *    ǯ�η��
           IF      WRK-KETA        =   4
      *        ���񣴷�
               MOVE    WRK-YY              TO  WRK-INSUJI
               PERFORM 1001-SUUJI-CHK-SEC
               IF      WRK-ERRCD           =   SPACE
                   MOVE    WRK-OTSUJI          TO  WRK-SYY
                   MOVE    2                   TO  FLG-SEKI
               END-IF
           ELSE
               EVALUATE    WRK-YY(1:1)    
      *            ��������å�
                   WHEN    "M"
                   WHEN    "m"
                   WHEN    "1"
                       MOVE    "1"                 TO  WRK-WGO
                       MOVE    WRK-YY(2:2)         TO  WRK-INSUJI
                       PERFORM 1001-SUUJI-CHK-SEC
                       MOVE    WRK-OTSUJI          TO  WRK-WYY
                       MOVE    1                   TO  FLG-SEKI
                   WHEN    "T"
                   WHEN    "t"
                   WHEN    "2"
                       MOVE    "2"                 TO  WRK-WGO
                       MOVE    WRK-YY(2:2)         TO  WRK-INSUJI
                       PERFORM 1001-SUUJI-CHK-SEC
                       MOVE    WRK-OTSUJI          TO  WRK-WYY
                       MOVE    1                   TO  FLG-SEKI
                   WHEN    "S"
                   WHEN    "s"
                   WHEN    "3"
                       MOVE    "3"                 TO  WRK-WGO
                       MOVE    WRK-YY(2:2)         TO  WRK-INSUJI
                       PERFORM 1001-SUUJI-CHK-SEC
                       MOVE    WRK-OTSUJI          TO  WRK-WYY
                       MOVE    1                   TO  FLG-SEKI
                   WHEN    "H"
                   WHEN    "h"
                   WHEN    "4"
                       MOVE    "4"                 TO  WRK-WGO
                       MOVE    WRK-YY(2:2)         TO  WRK-INSUJI
                       PERFORM 1001-SUUJI-CHK-SEC
                       MOVE    WRK-OTSUJI          TO  WRK-WYY
                       MOVE    1                   TO  FLG-SEKI
      *
      *2019.5.1����ο������б������¡�
                   WHEN    "R"
                   WHEN    "r"
                   WHEN    "5"
                       MOVE    "5"                 TO  WRK-WGO
                       MOVE    WRK-YY(2:2)         TO  WRK-INSUJI
                       PERFORM 1001-SUUJI-CHK-SEC
                       MOVE    WRK-OTSUJI          TO  WRK-WYY
                       MOVE    1                   TO  FLG-SEKI
      *
                   WHEN    OTHER
                       MOVE    WRK-YY              TO  WRK-INSUJI
                       PERFORM 1001-SUUJI-CHK-SEC
                       IF      WRK-ERRCD           =   SPACE
                           MOVE    WRK-OTSUJI          TO  WRK-SYY
                           MOVE    2                   TO  FLG-SEKI
                       END-IF
               END-EVALUATE
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      100-INDAY-CHK-EXT
           END-IF
     *
           PERFORM 1002-MMDD-HEN-SEC
           IF      FLG-SEKI           =   1
               MOVE    WRK-HMM            TO  WRK-WMM
               MOVE    WRK-HDD            TO  WRK-WDD
           ELSE
               MOVE    WRK-HMM            TO  WRK-SMM
               MOVE    WRK-HDD            TO  WRK-SDD
           END-IF
           .
       100-INDAY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       1001-SUUJI-CHK-SEC                SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-INSUJI          TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "99"                TO  WRK-ERRCD
               MOVE    ZERO                TO  WRK-OTSUJI
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-OTSUJI
           END-IF
           .
       1001-SUUJI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       1002-MMDD-HEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-HMM
           MOVE    ZERO                TO  WRK-HDD
      *
      *    ��
           MOVE    WRK-MM              TO  WRK-INSUJI
           PERFORM 1001-SUUJI-CHK-SEC
           MOVE    WRK-OTSUJI          TO  WRK-HMM
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      1002-MMDD-HEN-EXT
           END-IF
      *
      *    ��
           IF      WRK-DD              =   SPACE
               MOVE    "01"                TO  WRK-INSUJI
           END-IF
      *
           MOVE    WRK-DD              TO  WRK-INSUJI
           PERFORM 1001-SUUJI-CHK-SEC
           MOVE    WRK-OTSUJI          TO  WRK-HDD
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      1002-MMDD-HEN-EXT
           END-IF
           .
       1002-MMDD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       200-DAY-HEN-SEC                SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY1-AREA
      *
           IF      FLG-SEKI            =   1
      *        ���������Ѵ�
               MOVE    "12"                TO  LNK-DAY1-IRAI
               MOVE    WRK-WYMD            TO  LNK-DAY1-WYMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
               ELSE
                   MOVE    10                  TO  SGDAY-RC
                   GO      TO      200-DAY-HEN-EXT
               END-IF
           ELSE
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY1-AREA
               IF      STS-DAY-RC1         =   ZERO
                   MOVE    LNK-DAY1-WYMD       TO  WRK-WYMD
               ELSE
                   MOVE    10                  TO  SGDAY-RC
                   GO      TO      200-DAY-HEN-EXT
               END-IF
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  SGDAY-OTDAY
      *
           MOVE    WRK-SYMD            TO  SGDAY-SDAY
           MOVE    WRK-WYMD            TO  SGDAY-WDAY
      *
           IF      SGDAY-INTYPE        =   "2"
               MOVE    SPACE               TO  SGDAY-SDAY
               MOVE    SPACE               TO  SGDAY-WDAY
               MOVE    SPACE               TO  SGDAY-OTDAY
               EVALUATE   FLG-TYPE
                   WHEN    1
                       MOVE    LNK-DAY2-EDTYMD1    TO  SGDAY-OTDAY
                       MOVE    WRK-SYMD            TO  SGDAY-SDAY
                       MOVE    WRK-WYMD            TO  SGDAY-WDAY
                   WHEN    2
                       MOVE    LNK-DAY2-EDTYMD1(1:6)
                                                   TO  SGDAY-OTDAY(1:6)
                       MOVE    WRK-SYMD(1:6)       TO  SGDAY-SDAY (1:6)
                       MOVE    WRK-WYMD(1:5)       TO  SGDAY-WDAY (1:5)
                   WHEN    3
                       MOVE    LNK-DAY2-EDTYMD1(1:3)
                                                   TO  SGDAY-OTDAY(1:3)
                       MOVE    WRK-SYMD(1:4)       TO  SGDAY-SDAY (1:4)
                       MOVE    WRK-WYMD(1:3)       TO  SGDAY-WDAY (1:3)
               END-EVALUATE
           END-IF
           .
       200-DAY-HEN-EXT.
           EXIT.
      *