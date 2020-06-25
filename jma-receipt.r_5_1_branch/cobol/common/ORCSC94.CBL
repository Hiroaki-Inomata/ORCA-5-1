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
       PROGRAM-ID.                 ORCSC94.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ����ɽ���Խ�������SPA-COM -> SPA-GMN)
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  01/08/03    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL��¿��Ǽ 02/10/18  �����ɲ�
      *  01.00.02    NACL-¿��Ǽ  03/06/17  ����Խ�����
      *  04.05.00    NACL-¿��Ǽ  09/10/XX  ¿����Ϳ������������¾�б�
      *  04.06.00    NACL-¿��Ǽ  11/02/14  ��ˡ�������б�
      *****************************************************************
      *
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
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SP              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDS1                PIC 9(04).
           03  IDS2                PIC 9(04).
      *
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
      *
           03  IDZ                 PIC 9(02).
           03  IDX-ICD1            PIC 9(01).
           03  IDX-ICD             PIC 9(02).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-TENSU           PIC S9(08).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-TENSU-X-R       REDEFINES   WRK-TENSU-X.
               05  WRK-TENSU-Z2    PIC --------.
           03  WRK-TENSUS-X.
               05  WRK-TENSUS-Z    PIC ZZZZ9.99.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZ.
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZZZZZZ.
           03  WRK-KEICOM-X.
               05  WRK-KEICOM-Z    PIC ZZZZZZZ9.
           03  WRK-KEICOMS-X.
               05  WRK-KEICOMS-Z   PIC ZZZZ9V99.
           03  WRK-GOKEI-X.
               05  WRK-GOKEI-Z     PIC ZZZZZZZZ.
           03  WRK-KEI-X.
               05  WRK-KEI-Z       PIC ZZZZZZZ.
      *
      *!!  03  WRK-SURYO-H         PIC X(09).
           03  WRK-SURYO-H         PIC X(11).
           03  WRK-KAISU-H         PIC X(03).
           03  WRK-BUNGSU-H        PIC X(09).
           03  WRK-KAKERU          PIC X(01).
           03  WRK-ATAI            PIC X(44).
      *
           03  WRK-HEN-INPUTCD         PIC X(54).
           03  WRK-HEN-INPUTCDW        PIC X(54).
           03  WRK-JIKAN               PIC X(01).
      *    ���ϥ������Ѵ���
           03  WRK-IDX-FT              PIC 9(04).
           03  WRK-CD-MCNT             PIC 9(04).
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-MAISU           PIC 9(05).
      *
           03  WRK-IDX             PIC 9(04).
           03  WRK-MEISYO-X        PIC X(22).
      *
      *    ���ϥ����ɹ��ܷ��
       01  CONS-INPUT              PIC 9(02)   VALUE   54.
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *    ���Ź԰ٻ����Ѷ��̥ѥ�᥿
           COPY    "CPORCSC94.INC".
      *
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                   DIVISION    USING
           ORCSC94AREA
           SPA-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *
      *    �оݹԥ����å�
           IF      LNK-SC94-IDX    >=   1  AND  <=  SPA-MAX-LINE
               CONTINUE
           ELSE
               GO      TO     000-PROC-EXT
           END-IF
      *
           MOVE    LNK-SC94-IDX        TO  IDX
      *    ���Ź԰٥����ɤ�ꡢ���������
      *
           IF      SPA-GMN-INPUTCD(IDX)(1:1)       =   "." OR "S"
                                                   OR  "*"
                                                   OR  "#"
                                                   OR  "$"
               MOVE    SPA-GMN-INPUTCD(IDX)(1:1)   TO  WRK-MOJI
           ELSE
               MOVE    SPA-COM-INPUTCD(IDX)(1:1)   TO  WRK-MOJI
               IF      SPA-COM-INPUTCD(IDX)(1:3)   =   "059"
                   MOVE    "7"                     TO  WRK-MOJI
               END-IF
               IF      SPA-COM-INPUTCD(IDX)(1:3)   =   "008"
                   MOVE    "8"                     TO  WRK-MOJI
               END-IF
           END-IF
      *
           IF      WRK-MOJI            =   "."
               GO     TO      000-PROC-EXT
           END-IF
      *
           IF      WRK-MOJI            =   "#"
                                       OR  "$"
               GO     TO      000-PROC-EXT
           END-IF
      *    �������
           IF      WRK-MOJI            =   "*"
               PERFORM 300-NYUIN-GMN-SEC
               GO     TO      000-PROC-EXT
           END-IF
      *
      *    ����λ������Ȥ���
           IF      SPA-COM-SRYKBN (IDX)    =   "95"  OR  "96"
               IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "6" OR "7" ) OR
                      (WRK-MOJI                    =   "8" OR "7" )
               OR     (SPA-COM-INPUTCD(IDX)(1:1)   =   "1" )
                   CONTINUE
               ELSE
                   MOVE    "0"                 TO  WRK-MOJI
               END-IF
           END-IF
      *
           EVALUATE    LNK-SC94-KBN
      *        �������Ϲ����Խ��Ѥءʲ������ϰʳ��κ��Խ���
               WHEN    "1"
                   PERFORM 100-COM-GMNINPUT-SEC
      *            �����ͤ�����Խ��Ѥ�
               WHEN    "2"
                   PERFORM 200-GMNINPUT-GMN-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ���ť����ɤ�����Խ��ѽ���
      *****************************************************************
       100-COM-GMNINPUT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-KAISU-X
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    SPACE               TO  WRK-KAKERU
           MOVE    SPACE               TO  WRK-ATAI
           MOVE    SPACE               TO  WRK-BUNGSU-X
      *
           EVALUATE    WRK-MOJI
      *            �굻��
               WHEN    "1"
                   IF     (SPA-COM-SURYO  (IDX)     >  1  )  OR
      *            ���̥����ե饰
                         ((SPA-SURYOZERO-FLG       =   1  )  AND
                           (SPA-COM-SURYO (IDX)    =   0  )  AND
                           (SPA-COM-KZMCOMPSIKIBETU (IDX)
                                               NOT =   0 ))
      *            ���̣�����
                   OR    ((SPA-COM-SURFLG (IDX)    =   "1"  )  AND
                          (SPA-COM-SURYO  (IDX)    =   1    ))
                       MOVE    SPA-COM-SURYO  (IDX)    TO  WRK-SURYO
                       PERFORM 2001-SURYO-HEN-SEC
                   END-IF
      *            ������     ���̡����
               WHEN    "6"
                   MOVE    SPA-COM-SURYO  (IDX)    TO  WRK-SURYO
      *            ������
                   IF      SPA-COM-KANSURYO (IDX)    >   ZERO
                       MOVE    SPA-COM-KANSURYO (IDX)    TO  WRK-SURYO
                   END-IF
                   PERFORM 2001-SURYO-HEN-SEC
      *            ���������Ƕ�ۤϤʤ�
                   IF     (SPA-COM-TEN         (IDX)   =   ZERO ) AND
                          (SPA-COM-TENSIKIBETU (IDX)   =   1    )
                       MOVE    SPACE               TO  WRK-SURYO-X
                   END-IF
      *            ������   ���̡����
               WHEN    "7"
                   MOVE    SPA-COM-SURYO  (IDX)    TO  WRK-SURYO
                   PERFORM 2001-SURYO-HEN-SEC
      *            ���������ѡ�ʬ���
                   IF      SPA-COM-DATAKBN(IDX)        =   "3"
      *****************IF      SPA-COM-BUNGSU (IDX)    >   ZERO
                       IF      SPA-COM-BUNGSU (IDX)    >   1
                           MOVE    SPA-COM-BUNGSU (IDX)
                                                   TO  WRK-BUNGSU-Z
                       END-IF
                   END-IF
      *
      *            ������
               WHEN    "0"
                   IF      SPA-COM-KEIFLG(IDX)     =   "1"
                       IF      SPA-COM-KISOTEN (IDX)   =   ZERO
                           MOVE    SPA-COM-KEI(IDX)    TO  WRK-KEI-Z
                           MOVE    WRK-KEI-X           TO  WRK-SURYO-X
      *                    �����ն������
                           IF      SPA-COM-SRYSYUKBN(IDX)  =   "809"
      *H31.3                   CLAIM,API�б���800��������
                                                           OR  "800"
                               MOVE    SPA-COM-JIHIMONEY(IDX)
                                                       TO  WRK-KEI-Z
                               MOVE    WRK-KEI-X       TO  WRK-SURYO-X
                           END-IF
                       END-IF
                   END-IF
                   IF     (SPA-COM-INPUTCD(IDX)(1:3)   =   "095" OR
                                                           "096") AND
                          (SPA-COM-SURYO  (IDX)    NOT =   1    )
                       MOVE    SPA-COM-SURYO  (IDX)    TO  WRK-SURYO
                       PERFORM 2001-SURYO-HEN-SEC
                   END-IF
      *            ���̳����Խ�
                   IF     (SPA-COM-INPUTCD(IDX)(1:7)   =  "0992000") AND
                          (SPA-COM-SURYO  (IDX)    NOT =  ZERO     )
                       MOVE    SPA-COM-SURYO  (IDX)    TO  WRK-SURYO
                       PERFORM 2001-SURYO-HEN-SEC
                   END-IF
      *            ��ˡ������
                   IF    (SPA-COM-INPUTCD(IDX)(1:3)    =   "001"  )  AND
                         (SPA-COM-YCOMKBN(IDX)         =   1      )  AND
                         (SPA-COM-INPUTCHI-G(IDX)  NOT =   SPACE  )
                      MOVE    SPACE                TO  WRK-ATAI
                      STRING 
                          SPA-COM-ATAI1   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI2   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI3   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI4   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI5   (IDX)   DELIMITED   BY  SPACE
                          INTO                 WRK-ATAI
                       END-STRING
                   END-IF
      *H30.4
                   IF    (SPA-COM-INPUTCD(IDX)(1:7)   =   "0992081")
                    AND  (SPA-COM-INPUTCHI-G(IDX)  NOT =   SPACE  )
                      MOVE    SPACE                TO  WRK-ATAI
                      STRING 
                          SPA-COM-ATAI1   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI2   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI3   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI4   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI5   (IDX)   DELIMITED   BY  SPACE
                          INTO                 WRK-ATAI
                       END-STRING
                   END-IF
      *            ������
               WHEN    "8"
                   IF      SPA-COM-INPUTCHI-G (IDX)    NOT =   SPACE
                      MOVE    SPACE                TO  WRK-ATAI
                      STRING 
                          SPA-COM-ATAI1   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI2   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI3   (IDX)   DELIMITED   BY  SPACE
                          " "                     DELIMITED   BY  SIZE
                          SPA-COM-ATAI4   (IDX)   DELIMITED   BY  SPACE
                          INTO                 WRK-ATAI
                       END-STRING
                   END-IF
      *        ��«���å�
               WHEN    "S"
                   IF      SPA-COM-SURYO  (IDX)    =   ZERO
                       MOVE    1               TO  SPA-COM-SURYO (IDX)
                   END-IF
                   IF      SPA-COM-SURYO  (IDX)  NOT =  1
                       MOVE    SPA-COM-SURYO  (IDX)    TO  WRK-SURYO
                       PERFORM 2001-SURYO-HEN-SEC
                   END-IF
           END-EVALUATE
      *
      *****IF    ((SPA-COM-KAIFLG (IDX)    =   "1"  )  AND
      *           (SPA-COM-KAISU  (IDX)    >   1    ))  OR
      *          ( SPA-COM-INPUTCD(IDX)(1:1)   =   "S" AND
      *           (SPA-COM-KAISU  (IDX)    >   1    ))  OR
      *          ((SPA-COM-KAIFLG (IDX)    =   "1"  )  AND
      *****       (SPA-COM-SRYKBN (IDX)(1:1)   =   "2" ))
           IF      SPA-COM-KAIFLG (IDX)    =   "1"
               MOVE    "*"                     TO  WRK-KAKERU
               MOVE    SPA-COM-KAISU  (IDX)    TO  WRK-KAISU-Z
           ELSE
               MOVE    SPACE                   TO  SPA-COM-KAIFLG (IDX)
           END-IF
      *
           MOVE    ZERO                TO  IDK2
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-SURYO-H 
           MOVE    SPACE               TO  WRK-BUNGSU-H 
      *    ���
           IF      LNK-SC94-PG         =   "J04"
               MOVE    SPACE               TO  WRK-KAISU-H
           ELSE
               PERFORM VARYING     IDK1    FROM    1   BY  1
                       UNTIL       IDK1    >   3
                   IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                       ADD     1           TO  IDK2
                       MOVE    WRK-KAISU-X (IDK1:1)
                                           TO  WRK-KAISU-H (IDK2:1)
                   END-IF
               END-PERFORM
           END-IF
      *    ʬ��
           MOVE    1                   TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   8
               IF      WRK-BUNGSU-X(IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-BUNGSU-X (IDK1:1)
                                       TO  WRK-BUNGSU-H (IDK2:1)
               END-IF
           END-PERFORM
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   11
               IF      WRK-SURYO-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-SURYO-X (IDK1:1)
                                       TO  WRK-SURYO-H (IDK2:1)
               END-IF
           END-PERFORM
      *
      *    ��ưȯ���ʤ�
           IF     (SPA-COM-AUTOFLG2(IDX)   =   "1"  )  AND
                  (IDK2                    >   ZERO )
               ADD     1                   TO  IDK2
               MOVE    "@"                 TO  WRK-SURYO-H (IDK2:1)
           END-IF
      *ver.4.5
      *    ����������
           IF     (SPA-COM-KANSURYO (IDX)  >   ZERO )  AND
                  (IDK2                    >   ZERO )
               ADD     1                   TO  IDK2
               MOVE    "A"                 TO  WRK-SURYO-H (IDK2:1)
           END-IF
      *    ������
           IF     (SPA-COM-INPUTNAIF (IDX) =   "1"  )
      *    AND    (IDK2                    >   ZERO )
               ADD     1                   TO  IDK2
               MOVE    "I"                 TO  WRK-SURYO-H (IDK2:1)
           END-IF
      *    ������
           IF     (SPA-COM-INPUTCONT (IDX) =   "1"  )
               ADD     1                   TO  IDK2
               MOVE    "C"                 TO  WRK-SURYO-H (IDK2:1)
           END-IF
      *ver.4.5
      *
      *    �����ͺ��Խ�
      *     ʸ����Ĵ���γ��ϰ��֡�����
           IF     (SPA-GMN-INPUTCD(IDX)(1:1)  IS  NUMERIC    ) AND
                  (SPA-GMN-INPUTCD(IDX)(2:1)      =   SPACE  ) AND
                  (SPA-GMN-INPUTCD(IDX)(3:1)  NOT =   SPACE  )
      *        ���ֲû�����
               MOVE    SPA-GMN-INPUTCD(IDX)(1:1)      TO  WRK-JIKAN
           ELSE
               MOVE    SPACE                          TO  WRK-JIKAN
           END-IF
      *
           IF      SPA-COM-ICD-INPUTCD (IDX)   =   SPACE
               MOVE    SPA-COM-INPUTCD (IDX)   TO  WRK-HEN-INPUTCD
           ELSE
               MOVE    SPA-COM-ICD-INPUTCD (IDX)
                                               TO  WRK-HEN-INPUTCD
           END-IF
           MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
      *
           IF      WRK-ATAI            =   SPACE
               STRING  WRK-HEN-INPUTCD         DELIMITED   BY  SPACE
                       " "                     DELIMITED   BY  SIZE
                       WRK-SURYO-H             DELIMITED   BY  SPACE
                       WRK-BUNGSU-H            DELIMITED   BY  "  "
                       WRK-KAKERU              DELIMITED   BY  SPACE
                       WRK-KAISU-H             DELIMITED   BY  SPACE
                       INTO                   SPA-GMN-INPUTCD(IDX)
               END-STRING
           ELSE
               STRING  WRK-HEN-INPUTCD         DELIMITED   BY  SPACE
                       " "                     DELIMITED   BY  SIZE
                       WRK-ATAI                DELIMITED   BY  "  "
                       WRK-KAKERU              DELIMITED   BY  SPACE
                       WRK-KAISU-H             DELIMITED   BY  SPACE
                       INTO                    SPA-GMN-INPUTCD(IDX)
               END-STRING
      *!!!!
      *    ��³������
               IF      WRK-SURYO-H         NOT =   SPACE
                 MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
                 STRING  WRK-HEN-INPUTCD         DELIMITED   BY  SPACE
                         " "                     DELIMITED   BY  SIZE
                         WRK-SURYO-H             DELIMITED   BY  SPACE
                         " "                     DELIMITED   BY  SIZE
                         WRK-ATAI                DELIMITED   BY  "  "
                         WRK-KAKERU              DELIMITED   BY  SPACE
                         WRK-KAISU-H             DELIMITED   BY  SPACE
                         INTO                    SPA-GMN-INPUTCD(IDX)
                 END-STRING
               END-IF
           END-IF
      *
           IF      WRK-JIKAN       NOT =   SPACE
               MOVE    SPACE               TO  WRK-HEN-INPUTCD
               STRING  WRK-JIKAN               DELIMITED   BY  SPACE
                       " "                     DELIMITED   BY  SIZE
                       SPA-GMN-INPUTCD(IDX)    DELIMITED   BY  SIZE
                       INTO                    WRK-HEN-INPUTCD
               END-STRING
               MOVE    WRK-HEN-INPUTCD         TO  SPA-GMN-INPUTCD(IDX)
           END-IF
      *
      *    ���Ϸ������
           MOVE    ZERO                TO  IDX-ICD
           IF      WRK-JIKAN           =   ZERO
               MOVE    1                   TO  WRK-IDX-FT
           ELSE
               MOVE    3                   TO  WRK-IDX-FT
           END-IF
           PERFORM VARYING     IDX2    FROM    WRK-IDX-FT BY  1
                   UNTIL      (IDX2        >   CONS-INPUT   )  OR
                              (SPA-GMN-INPUTCD(IDX)(IDX2:1)
                                           =   SPACE OR  "*" )
               ADD     1               TO  IDX-ICD
           END-PERFORM
      *    ����ɽ������Υ��å�
           MOVE    IDX-ICD             TO  SPA-GMN-INCNT (IDX)
      *    ���ϥ�����ɽ���Ѥ�
           MOVE    "1"                 TO  SPA-COM-GMNFLG (IDX)
      *
           .
       100-COM-GMNINPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ͤ�����Խ��ѽ���
      *****************************************************************
       200-GMNINPUT-GMN-SEC            SECTION.
      *
      *    ����
           IF     (WRK-MOJI            =   "6" OR  "7"  OR  "S" ) OR
                 ((WRK-MOJI            =   "1" )   AND
                  (SPA-COM-TANICD (IDX)    NOT =   ZERO
                                               AND SPACE ))
              OR ((SPA-COM-INPUTCD(IDX)(1:3)   =   "095" OR
                                                   "096") AND
                  ((SPA-COM-SURYO  (IDX)   NOT =   1   )  OR
                   (SPA-COM-YKZKBN (IDX)   NOT =   ZERO)))
      *        ���ޡ���ࡢ�����Ź԰١���«���åȤλ������̡ᣱ�϶���
               MOVE    SPA-COM-SURYO (IDX)
                                           TO  WRK-SURYO
               PERFORM 2001-SURYO-HEN-SEC
               MOVE    WRK-SURYO-X         TO  SPA-GMN-SURYO (IDX)
      *        �������������Ƕ�ۤϤʤ�
               IF     (WRK-MOJI                    =   "6" ) AND
                      (SPA-COM-TEN         (IDX)   =   ZERO ) AND
                      (SPA-COM-TENSIKIBETU (IDX)   =   1    )
                   MOVE    SPACE               TO  SPA-GMN-SURYO (IDX)
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-GMN-SURYO (IDX)
           END-IF
      *
      *    �޽�λ�ԤΤߤ��Խ�����«���åȤλ������Ϲԡ�
           IF     (SPA-COM-ZAIEND (IDX)    =   "1" )  OR
                  (SPA-COM-KAIFLG (IDX)    =   "1" )  OR
                  (WRK-MOJI                =   "S" )  OR
                  (SPA-COM-SRYSYUKBN(IDX)  =   "950"  OR "960" )
      *H26.10
              OR  (SPA-COM-SRYSYUKBN(IDX)  =   "961"          )
      *
      *        ����
               IF      SPA-COM-PLUSKBN (IDX)   =   1
                   COMPUTE WRK-TENSU           =   SPA-COM-TENSU(IDX)
                                               *   -1
                   MOVE    WRK-TENSU               TO  WRK-TENSU-Z2
               ELSE
                   MOVE    SPA-COM-TENSU(IDX)      TO  WRK-TENSU-Z
               END-IF
               MOVE    WRK-TENSU-X             TO  SPA-GMN-TENSU(IDX)
      *        ���
               MOVE    SPA-COM-KAISU (IDX)     TO  WRK-KAISU-Z
               MOVE    WRK-KAISU-Z             TO  SPA-GMN-KAISU(IDX)
      *        ��
               IF      SPA-COM-PLUSKBN (IDX)   =   1
                   COMPUTE WRK-TENSU           =   SPA-COM-KEI (IDX)
                                               *   -1
                   MOVE    WRK-TENSU               TO  SPA-GMN-KEI(IDX)
               ELSE
                   MOVE    SPA-COM-KEI   (IDX)     TO  SPA-GMN-KEI(IDX)
               END-IF
           ELSE
               MOVE    SPACE                   TO  SPA-GMN-TENSU(IDX)
               MOVE    SPACE                   TO  SPA-GMN-KAISU(IDX)
               MOVE    ZERO                    TO  SPA-GMN-KEI  (IDX)
           END-IF
      *
      *    �����ͤ�ɽ�������ϥ����ɤ��ִ�����
           IF      SPA-COM-GMNFLG (IDX)    =   SPACE
               PERFORM 2002-INPUTCD-HEN-SEC
      *        ���ϥ�����ɽ���Ѥ�
               MOVE    "1"                 TO  SPA-COM-GMNFLG (IDX)
           END-IF
           .
       200-GMNINPUT-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ɽ�����Խ�����
      *****************************************************************
       2001-SURYO-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *    ���̣����ϥե饰
           IF      SPA-SURYOZERO-FLG   =   1
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
           END-IF
      *    �ե��������Ļ��û�����������Ψ�϶���
           IF      SPA-COM-INPUTCD(IDX)    =   "799990070"
                                           OR  "770020070"
               MOVE    SPACE               TO  WRK-SURYO-X
               GO      TO      2001-SURYO-HEN-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDM     FROM    5  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
               IF      WRK-SURYO-X(1:5)    =   SPACE
                   MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
           .
       2001-SURYO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ���ϥ����ɤμ�ɽ�������ɤ��Խ�����
      *****************************************************************
       2002-INPUTCD-HEN-SEC         SECTION.
      *
      *     ʸ����Ĵ���γ��ϰ��֡�����
           IF     (SPA-GMN-INPUTCD(IDX)(1:1)  IS  NUMERIC    ) AND
                  (SPA-GMN-INPUTCD(IDX)(2:1)      =   SPACE  ) AND
                  (SPA-GMN-INPUTCD(IDX)(3:1)  NOT =   SPACE  )
      *        ���ֲû�����
               MOVE    3                   TO  WRK-IDX-FT
           ELSE
               MOVE    1                   TO  WRK-IDX-FT
           END-IF
           MOVE    ZERO                TO  WRK-CD-MCNT
           PERFORM VARYING     IDX2    FROM    WRK-IDX-FT  BY  1
                   UNTIL      (IDX2        >   CONS-INPUT )   OR
                              (SPA-GMN-INPUTCD(IDX)(IDX2:1)
                                           =   SPACE   OR  "*" )
               MOVE    IDX2                TO  WRK-CD-MCNT
           END-PERFORM
      *
      *    ��ɽ�������ɤη�������å�
           MOVE    ZERO                TO  IDX-ICD
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   20      )  OR
                              (SPA-COM-ICD-INPUTCD(IDX)(IDX2:1)
                                           =   SPACE )
               ADD     1               TO  IDX-ICD
           END-PERFORM
      *
           MOVE    SPA-GMN-INPUTCD(IDX)
                                       TO  WRK-HEN-INPUTCD
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
                                               (WRK-IDX-FT:)
               IF      IDX-ICD             >   ZERO
                   MOVE    SPA-COM-ICD-INPUTCD(IDX)
                                           TO  SPA-GMN-INPUTCD(IDX)
                                                  (WRK-IDX-FT:IDX-ICD)
               ELSE
                   MOVE    9                   TO  IDX-ICD
                   MOVE    SPA-COM-INPUTCD(IDX)
                                               TO  SPA-GMN-INPUTCD(IDX)
                                                    (WRK-IDX-FT:9)
               END-IF
               COMPUTE IDY             =   WRK-CD-MCNT   +  1
               COMPUTE IDZ             =   WRK-IDX-FT    +  IDX-ICD
      *
               MOVE    WRK-HEN-INPUTCD(IDY:)
                                       TO  SPA-GMN-INPUTCD(IDX)
                                                          (IDZ:)
           END-IF
      *    ����ɽ������Υ��å�
           MOVE    ZERO                TO  WRK-CD-MCNT
           PERFORM VARYING     IDX2    FROM    WRK-IDX-FT  BY  1
                   UNTIL      (IDX2        >   CONS-INPUT )   OR
                              (SPA-GMN-INPUTCD(IDX)(IDX2:1)
                                           =   SPACE   OR  "*" )
      *         MOVE    IDX2                TO  WRK-CD-MCNT
               ADD     1                   TO  WRK-CD-MCNT
           END-PERFORM
      *****COMPUTE SPA-GMN-INCNT (IDX) =   WRK-IDX-FT    +  WRK-CD-MCNT
      *                                 -   1
      *    ����ɽ������Υ��å�
           MOVE    WRK-CD-MCNT         TO  SPA-GMN-INCNT (IDX)
      *
           .
       2002-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������Խ��ѽ���
      *****************************************************************
       300-NYUIN-GMN-SEC            SECTION.
      *
           EVALUATE    LNK-SC94-KBN
      *        �������Ϲ����Խ��Ѥءʲ������ϰʳ��κ��Խ���
               WHEN    "1"
                   PERFORM 3001-COM-GMNINPUT-SEC
      *
      *            �����ͤ�����Խ��Ѥ�
               WHEN    "2"
                   PERFORM 3001-GMNINPUT-GMN-SEC
           END-EVALUATE
           .
       300-NYUIN-GMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������Խ��ѽ���
      *****************************************************************
       3001-COM-GMNINPUT-SEC            SECTION.
      *
           MOVE    "1"                     TO  SPA-COM-KAIFLG (IDX)
           MOVE    SPA-COM-KAISU  (IDX)    TO  WRK-KAISU-Z
      *
           MOVE    ZERO                TO  IDK2
           MOVE    SPACE               TO  WRK-KAISU-H 
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
      *
           IF      LNK-SC94-SYORIFLG  =   1
      *        �����λ�
               STRING  "*"                 DELIMITED   BY  SPACE
                       WRK-KAISU-H         DELIMITED   BY  SPACE
                       INTO                   SPA-GMN-INPUTCD(IDX)
               END-STRING
           ELSE
      *        �����λ�
               STRING  "*"                 DELIMITED   BY  SPACE
                       WRK-KAISU-H         DELIMITED   BY  SPACE
                       "/"                 DELIMITED   BY  SIZE
                       INTO                   SPA-GMN-INPUTCD(IDX)
               END-STRING
           END-IF
      *    ̾�Τ����Υ��å�
           PERFORM 3003-NYUIN-MEISYO-SEC
           .
       3001-COM-GMNINPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ̾�Τ������Խ��ѽ���
      *****************************************************************
       3003-NYUIN-MEISYO-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO-X
           MOVE    ZERO                TO  WRK-IDX
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2        >   CONS-INPUT
               EVALUATE    SPA-GMN-INPUTCD(IDX)(IDX2:1)
                   WHEN    "/"
                       MOVE    IDX2            TO  WRK-IDX
               END-EVALUATE
           END-PERFORM
           IF     WRK-IDX              =   ZERO
      *        ������ʤ�
               MOVE    SPA-SRYYMD(7:2)     TO  WRK-MEISYO-X
           ELSE
               ADD     1                   TO  WRK-IDX
               IF      SPA-GMN-INPUTCD(IDX)(WRK-IDX:1)
                                           =   "."  OR  ","  OR "-"
      *            ����������ʤ�
                   MOVE    SPA-SRYYMD(7:2)     TO  WRK-MEISYO-X
                   MOVE    SPA-GMN-INPUTCD(IDX)(WRK-IDX:)
                                               TO  WRK-MEISYO-X(3:)
               ELSE
                   MOVE    SPA-GMN-INPUTCD(IDX)(WRK-IDX:)
                                               TO  WRK-MEISYO-X
               END-IF
           END-IF
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MEISYO-X        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF      KANACHK-RC      NOT =   ZERO
               MOVE    WRK-MEISYO-X        TO  SPA-GMN-MEISYO (IDX)
           ELSE
               COMPUTE IDX3            =   KANACHK-MAX     +   1
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO (IDX)
               MOVE    "��ʬ"              TO  SPA-GMN-MEISYO (IDX)
                                                              (IDX3:)
           END-IF
           .
       3003-NYUIN-MEISYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������Խ��ѽ���
      *****************************************************************
       3001-GMNINPUT-GMN-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-SURYO (IDX)
      *
      *    ����������Խ�
      *        ����
           IF      SPA-COM-PLUSKBN (IDX)   =   1
               COMPUTE WRK-TENSU           =   SPA-COM-TENSU(IDX)
                                               *   -1
               MOVE    WRK-TENSU               TO  WRK-TENSU-Z2
           ELSE
               MOVE    SPA-COM-TENSU(IDX)      TO  WRK-TENSU-Z
           END-IF
      **** MOVE    SPA-COM-TENSU(IDX)      TO  WRK-TENSU-Z
           MOVE    WRK-TENSU-X             TO  SPA-GMN-TENSU(IDX)
      *    ���
           MOVE    SPA-COM-KAISU (IDX)     TO  WRK-KAISU-Z
           MOVE    WRK-KAISU-Z             TO  SPA-GMN-KAISU(IDX)
      *    ��
           IF      SPA-COM-PLUSKBN (IDX)   =   1
               COMPUTE WRK-TENSU           =   SPA-COM-KEI (IDX)
                                           *   -1
               MOVE    WRK-TENSU               TO  SPA-GMN-KEI(IDX)
           ELSE
               MOVE    SPA-COM-KEI (IDX)       TO  SPA-GMN-KEI(IDX)
           END-IF
      *    ̾�Τ����Υ��å�
           PERFORM 3003-NYUIN-MEISYO-SEC
           .
       3001-GMNINPUT-GMN-EXT.
           EXIT.
      *