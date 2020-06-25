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
       PROGRAM-ID.                 ORCSC80200203.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ���Ź԰٥����å�������ʤ���¾��
      *                      �ȣ������������ޤ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
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
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-NAIYAK          PIC 9(01).
           03  FLG-TONPUK          PIC 9(01).
           03  FLG-GAIYAK          PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-ZAI             PIC 9(01).
           03  FLG-NAIYAK2         PIC 9(01).
           03  FLG-TONPUK2         PIC 9(01).
           03  FLG-GAIYAK2         PIC 9(01).
      *
           03  FLG-YAKJYO         PIC 9(01).
           03  FLG-ADD            PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-SYU             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-MOJI            PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *
           03  WRK-SYOKEIS         PIC 9(08)V99.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ZAIKEI          PIC 9(08)V99.
           03  WRK-GAIKEI          PIC 9(08)V99.
           03  WRK-GAITEN          PIC 9(08).
           03  WRK-KIZKEI          PIC 9(08).
           03  WRK-KIZTEN          PIC 9(08).
           03  WRK-FIRTEN          PIC 9(08).
           03  WRK-MAISU           PIC 9(08).
           03  WRK-KEIMAI          PIC 9(08).
           03  WRK-TENSUKEI        PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
      *
           03  WRK-IDX             PIC 9(04).
      *
      *    ��¸��
       01  HOZ-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HOZ-//.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ޥ�����
      *01  TENSU-REC.
      *    COPY    "CPTENSU.INC".
      *
      *    ��������
      *01  SANTEI-REC.
      *    COPY    "CPSANTEI.INC".
      *
      *    �����å��ޥ�����
      *01  CHK-REC.
      *    COPY    "CPCHK.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *
      *    ���Ź԰ٻ����Խ��ѥ�᥿
           COPY    "CPORCSC90.INC".
      *
      *    ���Ź԰ٻ���������å��ѥ�᥿
           COPY    "CPORCSC91.INC".
      *
      *    ���Ź԰������Խ������ܥ����å�
           COPY    "CPORCSC92.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    ���Ź԰ٻ����Ѷ��̥ѥ�᥿
           COPY    "CPORCSC00.INC".
      *
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC00AREA
           WRK-SCR-REC
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *
           EVALUATE    LNK-ORCSC-KBN
      *        ���̡������å����Խ�����ư����
               WHEN    "1"
                   PERFORM 100-SEL-SYORI-SEC
      *        ���ΤΥ����å�
               WHEN    "2"
                   PERFORM 200-ALL-SYORI-SEC
      *        ��ư����
               WHEN    "3"
                   PERFORM 300-AUTO-SYORI-SEC
               WHEN    OTHER
                   MOVE    "1001"          TO  SPA-ERRCD
           END-EVALUATE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���̤Υ����å����Խ�����ư�������
      *****************************************************************
       100-SEL-SYORI-SEC                SECTION.
      *
      *    �����å�����ư����
           PERFORM 1001-NAI-CHK-SEC
           IF     (WRK-GMN-INPUTCD(IDX-MAX)(1:1)
                                       =   "."   )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               GO      TO      100-SEL-SYORI-EXT
           END-IF
      *
      *    �����׻�
           PERFORM 1002-TENSU-KEI-SEC
      *
           .
       100-SEL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������ƥ����å�����ư�����Խ�����
      *****************************************************************
       1001-NAI-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-SYU
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "S"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                       CONTINUE
      *
                   WHEN    OTHER
      *                �Խ������å���
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
           MOVE    WRK-ERRCD          TO  SPA-ERRCD
      *
           .
       1001-NAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������ƥ����å�����ư�����Խ�����
      *****************************************************************
       10011-CHK-SEC                  SECTION.
      *
      *    �ƥ����å�����
           EVALUATE    WRK-MOJI
               WHEN    "1"
      *            ���Ļ��μ�ư�û����ɲä���
                   PERFORM 10011-NENREI-SET-SEC
      *        ���
               WHEN    "7"
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                �ե����λ����顼
                       MOVE    "0047"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ǯ��û��ɲý����ʶ��̡�
      *****************************************************************
       10011-NENREI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-Z IDX2)
                                               =   SPACE  OR
                                                   ZERO )
      *        �û��ɲ�
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-Z IDX2)  TO  WRK-SRYCD
      *        �û��������å�����
               PERFORM 100111-KASAN-CHK-SEC
      *        �û����ɲý���
               MOVE    IDX-Z               TO  IDX-S
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD             =   1
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       10011-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ư�û���¸�ߥ����å��ý���
      *****************************************************************
       100111-KASAN-CHK-SEC        SECTION.
      *
      *    ���ּ�ư�û���¸�ߥ����å�
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   )   OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
           END-PERFORM
           .
       100111-KASAN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �û����ɲý���
      *****************************************************************
       1002-KASAN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ADD
      *
           IF      FLG-ARI             =   ZERO
               ADD     1               TO  IDX-S
           ELSE
               MOVE    IDX-T           TO  IDX-S
           END-IF
      *
      *    �����ޥ����Խ������ܥ����å�
           MOVE    WRK-SCR-REC         TO  HOZ-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI   TO  LNK-SC92-LSTSRYKAMEI
           MOVE    LNK-ORCSC-TEISEI-AREA   TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       WRK-SCR-REC
                                       SPA-AREA
      *
      *    ���顼�λ����ɲäʤ�
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    HOZ-SCR-REC         TO  WRK-SCR-REC
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S
                                           -   1
               END-IF
               GO      TO      1002-KASAN-SET-EXT
           END-IF
      *
           MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYKBN(IDX-S)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
           MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-S)
      *
      *    ��ư�ɲ�ʬ�����ϻ����ɲá�
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           ADD     1                   TO  WRK-GMN-MAX
      *
           MOVE    1                   TO  FLG-ADD
           ADD     1                   TO  IDX-Z
           .
       1002-KASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����׻��Խ�����
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-GMN-KEI   (IDX-Z)
      *
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR  "S"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "0"
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                       CONTINUE
      *
                   WHEN    OTHER
      *                �Խ������å���
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
           END-PERFORM
      *
      *    �ǽ���
      *    ���������׻�
           MOVE    "2"                 TO  LNK-SC90-KBN
           MOVE    IDX-MAX             TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    ���û�
      *****PERFORM 1006-KASAN-SET-SEC
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   LNK-SC90-TENGAKEI
      *    �������Խ�
           MOVE    LNK-SC90-TENSUKEI   TO  WRK-COM-SYUTEN1 (IDX-MAX)
           COMPUTE WRK-COM-YKZTEN (IDX-MAX)
                                       =   LNK-SC90-ZAITEN
                                       +   LNK-SC90-GAITEN
      *    �������
           MOVE    LNK-SC90-KIZTEN     TO  WRK-COM-KIZAITEN (IDX-MAX)
      *
      *    �����ס��׻�
           COMPUTE WRK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-ZAITEN
                                           +   LNK-SC90-GAITEN
                                           +   LNK-SC90-KIZTEN
      *    ��
           COMPUTE WRK-COM-KEI (IDX-MAX)   =   WRK-COM-TENSU  (IDX-MAX)
                                           *   LNK-ORCSC-KAISU
      *    ���å��ѹ������
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
      *
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����׻�����
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
           MOVE    LNK-ORCSC-KAISU     TO  LNK-SC90-KAISU
           MOVE    "1"                 TO  LNK-SC90-KBN
           MOVE    IDX-Z               TO  LNK-SC90-IDX
           CALL    "ORCSC90"           USING
                                       ORCSC90AREA
                                       WRK-SCR-REC
      *
      *    �굻��Ƚ��
           IF      (WRK-MOJI                   =   "1"  )  AND
                   (WRK-COM-DATAKBN (IDX-Z)    =   "1"  )
               MOVE    IDX-Z               TO  IDX-SYU
           END-IF
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ź԰٥����å�����
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ư�������
      *****************************************************************
       300-AUTO-SYORI-SEC              SECTION.
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *