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
       PROGRAM-ID.                 ORCSC90.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : �ޤ��Ȥλ��ꥵ�֤�ꡢ�Խ����׻�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-¿��Ǽ   01.05.25  ���������׻�����ַ�����ѹ�
      *  01.00.02    NACL-¿��Ǽ  04.11.18  �ե�������׻��ѹ�
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
           03  IDM                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDS1                PIC 9(04).
           03  IDS2                PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC ZZZZZZZZ.
           03  WRK-TENSUS-X.
               05  WRK-TENSUS-Z    PIC ZZZZ9.99.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZZZZZZ.
           03  WRK-KEICOM-X.
               05  WRK-KEICOM-Z    PIC ZZZZZZZ9.
           03  WRK-KEICOMS-X.
               05  WRK-KEICOMS-Z   PIC ZZZZ9V99.
           03  WRK-GOKEI-X.
               05  WRK-GOKEI-Z     PIC ZZZZZZZZ.
           03  WRK-GMN-INPUTCD     PIC X(22).
      *
           03  WRK-SYOKEIS         PIC 9(08)V9(8).
           03  WRK-SYOKEIS2        PIC 9(08)V9(8).
           03  WRK-SYOKEIS3        PIC 9(08)V9(8).
           03  WRK-SYOKEI          PIC 9(08).
           03  WRK-TENSUKEIS       PIC 9(08)V9(6).
           03  WRK-FIRTEN          PIC 9(08).
      *
      *    ���ͱշ׻���
           03  WRK-SURYO-CHU9      PIC 9(05).
           03  WRK-SURYO-CHU       PIC 9(05)V9(05).
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-MAISU           PIC 9(05).
      *
      *���Ź԰ٽ��������
       01  MAX-LINE                PIC 9(03)   VALUE   400.
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *    ���Ź԰ٻ����Ѷ��̥ѥ�᥿
           COPY    "CPORCSC90.INC".
      *
      *    �����ΰ�
       01  LNK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //LNK-//.
      *
       PROCEDURE                   DIVISION    USING
           ORCSC90AREA
           LNK-SCR-REC
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
           IF      LNK-SC90-IDX        <   1  OR  >  MAX-LINE
               GO      TO      000-PROC-EXT
           END-IF
      *
           MOVE    LNK-SC90-IDX        TO  IDX
      *
           EVALUATE    LNK-SC90-KBN
      *        �����Խ�
               WHEN    "1"
                   PERFORM 100-MEISAI-SET-SEC
      *        �����׻�
               WHEN    "2"
                   PERFORM 200-TENSUKEI-SEC
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       100-MEISAI-SET-SEC              SECTION.
      *
           IF      LNK-COM-INPUTCD(IDX) (1:3)  =   "059"
               MOVE    "7"                 TO  WRK-MOJI
           ELSE
               MOVE    LNK-COM-INPUTCD(IDX)(1:1)   TO  WRK-MOJI
           END-IF
      *
      **** EVALUATE    LNK-COM-INPUTCD(1:1)
           EVALUATE    WRK-MOJI
      *        �굻��
               WHEN    "1"
                   PERFORM 1001-SYUGI-SET-SEC
      *        ����
               WHEN    "6"
                   EVALUATE    LNK-COM-YKZKBN(IDX)
                       WHEN    "6"
      *                    ������
                           PERFORM 1003-GAIYAKU-SET-SEC
                       WHEN    "4"
      *                    ������
                           PERFORM 1006-CHUUSYA-SET-SEC
                       WHEN    OTHER
      *                    ������
                           PERFORM 1002-YAKUZAI-SET-SEC
                   END-EVALUATE
      *        ���
               WHEN    "7"
                   IF      LNK-COM-DATAKBN (IDX)    =   "3"
      *                �ե������Խ�
                       PERFORM 1005-FIRUMU-SET-SEC
                   ELSE
      *                �ե����ʳ����Խ�
                       PERFORM 1004-KIZAI-SET-SEC
                   END-IF 
           END-EVALUATE
           .
       100-MEISAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �굻���Խ�����
      *****************************************************************
       1001-SYUGI-SET-SEC           SECTION.
      *
      *****����
      *    IF      LNK-COM-SURYO       >   1
      *        MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *        PERFORM 10011-SURYO-HEN-SEC
      *        MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *    ELSE
      *        MOVE    SPACE               TO  LNK-GMN-SURYO
      *****END-IF
      *    ����
      *        ��û��������Ͻ��פ��ʤ�
           IF      LNK-COM-TENSIKIBETU (IDX)   =   5
                                               OR  6
               CONTINUE
           ELSE
      *        ��§�û��оݳ���ʬȽ��
               IF      LNK-COM-TUSOKUGAIKBN (IDX)  =   "1"
                   ADD     LNK-COM-KISOTEN (IDX)  TO  LNK-SC90-TENGAKEI
               ELSE
                   ADD     LNK-COM-KISOTEN(IDX)   TO  LNK-SC90-TENSUKEI
               END-IF
           END-IF
      *    ���
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU  (IDX)
           .
       1001-SYUGI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������Խ�����
      *****************************************************************
       1002-YAKUZAI-SET-SEC           SECTION.
      *
      *    ����
      *    MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *    PERFORM 10011-SURYO-HEN-SEC
      *    MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *    ����
           COMPUTE WRK-SYOKEIS2        =  (LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)  )
           ADD     WRK-SYOKEIS2        TO  LNK-SC90-ZAIKEI
      *    ���
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU (IDX)
           .
       1002-YAKUZAI-SET1-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       1006-CHUUSYA-SET-SEC           SECTION.
      *
      *    ����
      *    MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *    PERFORM 10011-SURYO-HEN-SEC
      *    MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *
           MOVE    ZERO                TO  LNK-SC90-HAIKI
      *
      *    ���ͤǡ�����ץ�λ���ñ�̰ʲ��׻�
      *    IF    ((LNK-COM-SRYKBN (IDX)    =   "31" OR "32" OR
      *                                                "33"
      *D          �����ɲ�
      *D                                   OR  "40" 
      *                                                   ) AND
      *          ((LNK-COM-TANICD (IDX)    =   "014" OR "022" OR "046")
      *        OR  ((IDX                     <   MAX-LINE  )  AND
      *             (LNK-COM-INPUTCD(IDX + 1)  =   "099309901"))) )
      *        ���֤λ������������Ѵ�
      *       OR  ((LNK-COM-SRYKBN (IDX)    =   "40" )  AND
      *            (IDX                     <   MAX-LINE  )  AND
      ************ (LNK-COM-INPUTCD(IDX + 1)  =   "099309901"))
      *    IF     (LNK-COM-TANICD (IDX)    =   "014"
      *                                    OR  "022"
      ***                                  OR  "046"  )  AND
      *    �����Ѵ��Τ��뤹�٤Ƥ����������оݤȤ���
           IF     (IDX                     <   MAX-LINE )  AND
                  (LNK-COM-INPUTCD(IDX + 1)  =   "099309901")
               IF      LNK-COM-SURYO(IDX)(6:5) >   ZERO
                   COMPUTE WRK-SURYO-CHU9      =   LNK-COM-SURYO (IDX)
                                               +   0.99999
                   MOVE    WRK-SURYO-CHU9      TO  WRK-SURYO-CHU
               ELSE
                   MOVE    LNK-COM-SURYO (IDX)     TO  WRK-SURYO-CHU
               END-IF
      *        ����
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                           *   WRK-SURYO-CHU
           ELSE
      *        ����
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN(IDX)
                                           *   LNK-COM-SURYO  (IDX)
           END-IF
      *
           ADD     WRK-SYOKEIS2        TO  LNK-SC90-ZAIKEI
      *    ���
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU (IDX)
           .
       1006-CHUUSYA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       1003-GAIYAKU-SET-SEC            SECTION.
      *
      *    ����ϣ���Ȥ���ʿ��̡��������̤Ȥ����(ORCSC20��Ƚ���
      *    COMPUTE LNK-COM-SURYO (IDX) =   LNK-COM-SURYO (IDX)
      *                                *   LNK-SC90-KAISU 
      *    ����
      *    MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *    PERFORM 10011-SURYO-HEN-SEC
      *    MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *    ����
           COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)
           ADD     WRK-SYOKEIS2        TO  LNK-SC90-GAIKEI
      *    ���
      *****MOVE    1                   TO  LNK-COM-KAISU  (IDX)
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU  (IDX)
           .
       1003-GAIYAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ����Խ�����
      *****************************************************************
       1004-KIZAI-SET-SEC            SECTION.
      *
      *    ����
      *    MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *    PERFORM 10011-SURYO-HEN-SEC
      *    MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *    ����
      *        ��������̣�Ƚ��
           IF      LNK-COM-TENSIKIBETU (IDX)   =   5
                                               OR  6
      *
      *        ���
               MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU (IDX)
      *
               GO     TO  1004-KIZAI-SET-EXT
           END-IF
      *
           IF      LNK-COM-TOKUKIZAISBT1 (IDX) =   ZERO
      *        (�������ʡ�����)
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                           *   LNK-COM-SURYO   (IDX)
      *
               ADD     WRK-SYOKEIS2        TO  LNK-SC90-KIZKEI
           ELSE
      *        ����
               IF      LNK-COM-SANSOKBN (IDX)  =   ZERO
      *            (�������ʡ�����)���ͼθ���
                   COMPUTE WRK-SYOKEIS2        =  (LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX))
                                               +   0.5
                   MOVE    WRK-SYOKEIS2        TO  WRK-SYOKEIS
                   ADD     WRK-SYOKEIS         TO  LNK-SC90-CHIKEI
               END-IF
           END-IF
      *
      *    ���
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU (IDX)
      *
           .
       1004-KIZAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ʥե������Խ�����
      *****************************************************************
       1005-FIRUMU-SET-SEC           SECTION.
      *
      *    ����
      *    MOVE    LNK-COM-SURYO       TO  WRK-SURYO
      *    PERFORM 10011-SURYO-HEN-SEC
      *    MOVE    WRK-SURYO-X         TO  LNK-GMN-SURYO
      *    ����û�
      *****ADD     LNK-COM-SURYO       TO  LNK-SC90-MAISU
      *
      *    ʬ���
           IF      LNK-COM-BUNGSU (IDX)    =   ZERO
               MOVE    1                   TO  LNK-COM-BUNGSU (IDX)
           END-IF
           ADD     LNK-COM-BUNGSU (IDX)    TO  LNK-SC90-BUNGSU
      *---(01.00.03) ---  
      *    ����û�
           COMPUTE WRK-MAISU           =   LNK-COM-SURYO  (IDX)
                                       *   LNK-COM-BUNGSU (IDX)
           ADD     WRK-MAISU           TO  LNK-SC90-MAISU
      *---(01.00.03)
      *    ����
      *    (�������ʡ�����)
           COMPUTE WRK-SYOKEIS         =   LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)
           MOVE    WRK-SYOKEIS         TO  WRK-SYOKEI
      *
      *    �������ǰʳ��η׻�
           IF      LNK-COM-SRYKBN (IDX)    =   "70"
      *        �ե���������û�
               MOVE    WRK-SYOKEI          TO  LNK-SC90-FIRTEN
           ELSE
               PERFORM 10051-FIRUMU-ETC-SEC
           END-IF
      *
      *    ���
           MOVE    LNK-SC90-KAISU      TO  LNK-COM-KAISU (IDX)
      *
           .
       1005-FIRUMU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ǰʳ��η׻�����
      *****************************************************************
       10051-FIRUMU-ETC-SEC           SECTION.
      *    �������Ƚ��
           IF      LNK-COM-JGNTEN (IDX) =   ZERO
      *        �ե���������û�
               ADD     WRK-SYOKEI          TO  LNK-SC90-FIRTEN
           ELSE
               COMPUTE WRK-TENSUKEIS       =   WRK-SYOKEI
                                           /   10
                                           +   0.5
               MOVE    WRK-TENSUKEIS       TO  WRK-FIRTEN
               IF      WRK-FIRTEN          >   LNK-COM-JGNTEN (IDX)
                   MOVE    LNK-COM-JGNTEN(IDX) TO  WRK-FIRTEN
      *            ��������˾�������򽸷�
                   ADD     WRK-FIRTEN          TO  LNK-SC90-KIZTEN
               ELSE
                   ADD     WRK-SYOKEI          TO  LNK-SC90-FIRTEN
               END-IF
           END-IF
      *
           .
       10051-FIRUMU-ETC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���׻�����
      *****************************************************************
       200-TENSUKEI-SEC           SECTION.
      *    ���������׻�����
      *    IF      LNK-SC90-ZAIKEI     =   ZERO
      *        MOVE      ZERO              TO  LNK-SC90-ZAITEN
      *    ELSE
      *        PERFORM 2001-ZAITENU-SET-SEC
      *    END-IF
      *
      *    �����������׻�����
      *    IF      LNK-SC90-GAIKEI     =   ZERO
      *        MOVE      ZERO              TO  LNK-SC90-GAITEN
      *    ELSE
      *        PERFORM 2002-GAITENU-SET-SEC
      *    END-IF
      *
      *    ���������׻��ʺ���ǤޤȤ�Ʒ׻���
           PERFORM 2001-KEIZAITENU-SET-SEC
      *
      *    ��������׻�����
      *    IF      LNK-SC90-KIZKEI     =   ZERO
      *        MOVE      ZERO              TO  LNK-SC90-KIZTEN
      *    ELSE
               PERFORM 2003-KIZTEN-SET-SEC
      *****END-IF
      *
      *    �������ǡ������׻�����
           IF      LNK-SC90-CHIKEI     =   ZERO
               MOVE      ZERO              TO  LNK-SC90-CHITEN
           ELSE
               PERFORM 2004-CHITEN-SET-SEC
           END-IF
      *
           COMPUTE LNK-SC90-KIZTEN     =   LNK-SC90-KIZTEN
                                       +   LNK-SC90-CHITEN
           .
       200-TENSUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������׻�����
      *****************************************************************
       2001-KEIZAITENU-SET-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SYOKEIS2
           COMPUTE WRK-SYOKEIS2        =   LNK-SC90-ZAIKEI
                                       +   LNK-SC90-GAIKEI
      *
      * =============================================================
      *---// ����ʬ��������15���ʲ� �� ���� �����ްʳ��λ���������  //
      *-- // ����ʬ��������15��Ķ�� �� �ʣ���ʬ��������15��/ 10 
      *==============================================================
      *
           IF      WRK-SYOKEIS2        <=  15
               IF     (LNK-COM-SRYKBN (IDX)    =   "21"  OR  "22"  OR
                                                   "23"  OR
                                                   "31"  OR  "32"  OR
                                                   "33"   )  AND
                      (WRK-SYOKEIS2        >   ZERO       )
                   MOVE    1               TO  WRK-SYOKEIS3
               ELSE
                   MOVE    ZERO            TO  WRK-SYOKEIS3
               END-IF
           ELSE
               COMPUTE WRK-SYOKEIS3        =  ((WRK-SYOKEIS2   -  15)
                                           /    10 )
                                           +   1
           END-IF
      *
           COMPUTE LNK-SC90-ZAITEN     =   WRK-SYOKEIS3   + 0.999999
      *
           .
       2001-KEIZAITENU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���������׻�����
      *****************************************************************
       2001-ZAITENU-SET-SEC           SECTION.
      *
      *****MOVE    ZERO                TO  WRK-SYOKEIS2
           MOVE    ZERO                TO  WRK-SYOKEIS3
      *
      * =============================================================
      *---// ����ʬ��������15���ʲ� �� ���� �����ްʳ��λ���������  //
      *-- // ����ʬ��������15��Ķ�� �� �ʣ���ʬ��������15��/ 10 
      *==============================================================
      *
           IF      LNK-SC90-ZAIKEI         <=  15
               IF    ( LNK-COM-SRYKBN (IDX)    =   "21"  OR  "22"  OR
                                                   "23"  OR
                                                   "31"  OR  "32"  OR
                                                   "33"    )  AND
                      ( LNK-SC90-ZAIKEI        >    ZERO   )
      *
      ***********  MOVE    1               TO  WRK-SYOKEIS2
                   MOVE    1               TO  WRK-SYOKEIS3
               ELSE
      ***********  MOVE    ZERO            TO  WRK-SYOKEIS2
                   MOVE    ZERO            TO  WRK-SYOKEIS3
               END-IF
           ELSE
      ******** COMPUTE WRK-SYOKEIS2    =  (LNK-SC90-ZAIKEI   -  15)
               COMPUTE WRK-SYOKEIS3    =  ((LNK-SC90-ZAIKEI  -  15)
                                       / 10 )
                                       +   1
           END-IF
      *
      *****COMPUTE LNK-SC90-ZAITEN     =   WRK-SYOKEIS2   + 0.99
           COMPUTE LNK-SC90-ZAITEN     =   WRK-SYOKEIS3   + 0.999999
      *
           .
       2001-ZAITENU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������׻������ʳ�������
      *****************************************************************
       2002-GAITENU-SET-SEC           SECTION.
      *
      * =============================================================
      *---// ����ʬ��������15���ʲ� �� ���������ްʳ��λ�������     //
      *-- // ����ʬ��������15��Ķ�� �� �ʣ���ʬ��������15��/ 10 + 1 //
      *=============================================================
           IF      LNK-SC90-GAIKEI         <=  15
               IF    ( LNK-COM-SRYKBN (IDX)    =   "21"  OR
                                               "22"  OR
                                               "23"  OR
                                               "31"  OR
                                               "32"  OR
                                               "33"       )  AND
                      (LNK-SC90-GAIKEI     >   ZERO       )
      *************MOVE    1               TO  WRK-SYOKEIS2
                   MOVE    1               TO  WRK-SYOKEIS3
               ELSE
      ************ MOVE    ZERO            TO  WRK-SYOKEIS2
                   MOVE    ZERO            TO  WRK-SYOKEIS3
               END-IF
           ELSE
      *********COMPUTE WRK-SYOKEIS2    =  (LNK-SC90-GAIKEI    -  15)
               COMPUTE WRK-SYOKEIS3    =  ((LNK-SC90-GAIKEI   -  15)
                                                   / 10)
                                                   +   1
           END-IF
      *
      *****COMPUTE LNK-SC90-GAITEN     =   WRK-SYOKEIS2   + 0.99
           COMPUTE LNK-SC90-GAITEN     =   WRK-SYOKEIS3   + 0.999999
      *
           .
       2002-GAITENU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������׻������ʥե���ࡢ���ǡ����ǰʳ���
      *****************************************************************
       2003-KIZTEN-SET-SEC             SECTION.
      *
      *?   �������ǰʳ��Υե����׻�
      *    IF      LNK-COM-SRYKBN (01) =   "70"
      *        MOVE    LNK-SC90-KIZKEI     TO  WRK-SYOKEIS3
      *    ELSE
      *        COMPUTE WRK-SYOKEIS3    =   LNK-SC90-KIZKEI
      *                                +   LNK-SC90-FIRTEN
      *?   END-IF
      *    �������ǰʳ��Υե����׻�
           IF      LNK-COM-SRYKBN (01) NOT =   "70"
               MOVE    LNK-SC90-FIRTEN     TO  WRK-SYOKEIS3
               IF      WRK-SYOKEIS3        >   ZERO
      *        ��۷סࣱ�����ͼθ���
                   COMPUTE WRK-SYOKEIS2        =  (WRK-SYOKEIS3
                                               /   10  )
                                               +   0.5
                   ADD     WRK-SYOKEIS2        TO  LNK-SC90-KIZTEN
               END-IF
           END-IF
      *    ���
           MOVE    LNK-SC90-KIZKEI     TO  WRK-SYOKEIS3
           IF      WRK-SYOKEIS3        >   ZERO
      *    ��۷סࣱ�����ͼθ���
               COMPUTE WRK-SYOKEIS2        =  (WRK-SYOKEIS3
                                           /   10  )
                                           +   0.5
               ADD     WRK-SYOKEIS2        TO  LNK-SC90-KIZTEN
           END-IF
      *
           .
       2003-KIZTEN-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ��������׻����������ǡ�
      *****************************************************************
       2004-CHITEN-SET-SEC             SECTION.
      *
      *    ��۷סࣱ�����ͼθ���
           COMPUTE WRK-SYOKEIS2        =  (LNK-SC90-CHIKEI
                                       /   10  )
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  LNK-SC90-CHITEN
      *
           .
       2004-CHITEN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ɽ�����Խ�����
      *****************************************************************
      *10011-SURYO-HEN-SEC                  SECTION.
      *
      *    MOVE    SPACE               TO  WRK-SURYO-X
      *    MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
      *    MOVE    ZERO                TO  FLG-SP
      *    PERFORM VARYING     IDM     FROM    3  BY  -1
      *            UNTIL       IDM     <   1
      *        IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
      *            MOVE    1               TO  FLG-SP
      *        END-IF
      *        IF      FLG-SP          =   1
      *            MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
      *        END-IF
      *    END-PERFORM
      *
      *    IF      WRK-SURYO-Z3        NOT =   SPACE
      *        MOVE    "."                 TO  WRK-SURYO-Z2
      *            MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
      *        END-IF
      *    END-IF
      *    .
      *10011-SURYO-HEN-EXT.
      *    EXIT.
      *