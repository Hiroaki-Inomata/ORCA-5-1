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
       PROGRAM-ID.                 ORCSC90TEN2.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : �ޤ��������׻� (ORCSC90TEN )
      *                      �ȣ���ǯ������������SRYYMD �ɲá�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  10/03/03    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.03.00    NACL-¿��Ǽ  07/03/06  ���������׻�����
      *  04.05.00    NACL-¿��Ǽ  09/06/XX  ���̾��������б�
      *  04.05.00    NACL-¿��Ǽ  10/01/XX  ��ɾ�б�
      *  04.05.00    NACL-¿��Ǽ  10/03/XX  ʿ������ǯ��������б�
      *  04.07.00    NACL-¿��Ǽ  12/03/XX  ʿ������ǯ��������б�
      *  04.07.00    NACL-¿��Ǽ  12/08/28  �����������б�
      *  04.08.00    NACL-¿��Ǽ  18/06/XX  ��������С��б�
      *  05.00.00    NACL-¿��Ǽ  19/03/XX  �����ʤɷ�������С��б�
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
           03  FLG-GENTEN          PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDS1                PIC 9(04).
           03  IDS2                PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-END             PIC 9(04).
           03  IDX-SYU             PIC 9(04).
      *
           03  IDX-H               PIC 9(04).
           03  IDX-K               PIC 9(04).
      *
           03  IDX7                PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-SYOKEIS         PIC 9(10)V9(8).
           03  WRK-SYOKEIS2        PIC 9(10)V9(8).
           03  WRK-SYOKEIS3        PIC 9(10)V9(8).
           03  WRK-SYOKEI          PIC 9(10).
           03  WRK-TENSUKEIS       PIC 9(10)V9(6).
           03  WRK-TENSUKEI-9      PIC 9(10).
      *    ����
           03  WRK-ZAIKEI          PIC 9(10)V9(8).
      *    ���
           03  WRK-KIZKEI          PIC 9(10)V9(5).
           03  WRK-KIZKEI-3        PIC 9(10)V9(5).
      *    ���Х��
           03  WRK-KIZKEI-4        PIC 9(10)V9(5).
      *    �ե����
           03  WRK-FIRKEI          PIC 9(10)V9(5).
           03  WRK-NYU-FIRKEI      PIC 9(10)V9(5).
           03  WRK-NYU-FIRTEN      PIC 9(10)V9(5).
           03  WRK-FIRTEN-G        PIC 9(10).
           03  WRK-FIRTEN          PIC 9(10).
      *    ����
           03  WRK-CHISOKEI        PIC 9(10)V9(5).
           03  WRK-CHITEN          PIC 9(10).
      *    ����
           03  WRK-SANSOKIN        PIC 9(10)V9(5).
           03  WRK-SANSOKIN-SUM    PIC 9(10)V9(5).
           03  WRK-SANSOGOK        PIC 9(10).
           03  WRK-SANTEN          PIC 9(10).
      *
      *    ��������
           03  WRK-SC90-YKZTEN         PIC 9(10).
      *    �������
           03  WRK-SC90-KIZTEN         PIC 9(10).
      *    �ե��������
           03  WRK-SC90-FIRTEN         PIC 9(10).
      *
      *    ���ͱշ׻���
           03  WRK-SURYO-CHU9      PIC 9(05).
           03  WRK-SURYO-CHU       PIC 9(05)V9(05).
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-MAISU           PIC 9(05).
      *ver.4.7
           03  WRK-GENTEN          PIC 9(08).
      *H31.3
           03  WRK-GOK-TENSU       PIC 9(14).
           03  WRK-GOK-KIZAI       PIC 9(14).
      *
      *���Ź԰ٽ��������
       01  MAX-LINE                PIC 9(03)   VALUE   400.
      *
      *    �����ΰ�
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *    ���Ź԰ٻ����Ѷ��̥ѥ�᥿
           COPY    "CPORCSC90TEN2.INC".
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
           IF      LNK-SC90-IDX-STR    =   ZERO
               MOVE    1                   TO  IDX-STR
           ELSE
               MOVE    LNK-SC90-IDX-STR    TO  IDX-STR
           END-IF
           IF      LNK-SC90-IDX-END    =   ZERO
               MOVE    MAX-LINE            TO  IDX-END
           ELSE
               MOVE    LNK-SC90-IDX-END    TO  IDX-END
           END-IF
      *
      *    �굻��ν���
           PERFORM 100-MEISAI-SET-SEC
      *
           EVALUATE    LNK-SC90-KBN
      *    �޹��
               WHEN    "2"
      *            ���ǡ����ǽ���
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
           MOVE    ZERO                TO  IDX-MAX
      *    �굻���ʳ�����������
           PERFORM VARYING     IDX     FROM    IDX-STR  BY  1
                   UNTIL      (IDX     >   IDX-END     )  OR
                             ((LNK-COM-INPUTCD(IDX)    =   SPACE ) AND
                              (LNK-GMN-INPUTCD(IDX)    =   SPACE ))
               IF      LNK-COM-INPUTCD(IDX) (1:3)  =   "059"
                   MOVE    "7"                 TO  WRK-MOJI
               ELSE
                   MOVE    LNK-COM-INPUTCD(IDX)(1:1)   TO  WRK-MOJI
               END-IF
      *
               MOVE    IDX                 TO  IDX-MAX
      *        ��ɾ���оݳ�
               IF      LNK-COM-YAKUHYO (IDX)   =   "1"
                   CONTINUE
               ELSE
      *
      *****    MOVE    IDX                 TO  IDX-MAX
               EVALUATE    WRK-MOJI
      *        ����
               WHEN    "6"
                   EVALUATE    LNK-COM-YKZKBN(IDX)
                       WHEN    "4"
      *                    ������
      *H27.9               �������λ����Ѵ��б�
                       WHEN    "6"
                           PERFORM 1002-CHUUSYA-SET-SEC
      *ver.4.7
                       WHEN    "3"
      *                    ����¾
                           PERFORM 10021-SONOTA-SET-SEC
      *
                       WHEN    OTHER
      *                    ������
                           PERFORM 1001-YAKUZAI-SET-SEC
                   END-EVALUATE
      *        ���
               WHEN    "7"
                   IF      LNK-COM-TENSIKIBETU (IDX)   =   1
                                                       OR  2
                       IF      LNK-COM-DATAKBN (IDX)   =   "3"
      *                    �ե������Խ�
                           PERFORM 1003-FIRUMU-SET-SEC
                       ELSE
      *                    �ե����ʳ����Խ�
                           PERFORM 1004-KIZAI-SET-SEC
                       END-IF
                   END-IF
               END-EVALUATE
      *
               END-IF
           END-PERFORM
      *
      *    ���������׻��ʺ���ǤޤȤ�Ʒ׻���
           PERFORM 1005-KEIZAITENU-SET-SEC
      *    ��������׻�����
           PERFORM 1006-KIZTEN-KEI-SEC
      *    �ե���������׻�����
           PERFORM 1007-FIRUMU-KEI-SEC
      *    �굻�ι�
           IF      LNK-SC90-IDX        =   ZERO
               MOVE    IDX-MAX             TO  IDX-SYU
           ELSE
               MOVE    LNK-SC90-IDX        TO  IDX-SYU
      *        �굻��
               MOVE    1                   TO  LNK-COM-SYUFLG(IDX-SYU)
           END-IF
      *ver.4.7
      *    ����������
           IF      WRK-GENTEN          >   ZERO
               MOVE    WRK-GENTEN          TO  WRK-SC90-YKZTEN
           END-IF
      *
      *H31.2
      *    ��פ����奪���С��ϥ��顼
           IF    ((WRK-SC90-YKZTEN *   LNK-SC90-YKZTEN)
                                       >   999999999   )
             OR  ((WRK-SC90-KIZTEN *   LNK-SC90-KIZTEN )
                                       >   999999999   )
             OR  ((WRK-SC90-FIRTEN  *   LNK-SC90-FIRTEN )
                                       >   999999999   )
               MOVE    "0249"          TO  LNK-SC90-ERRCD
           END-IF
      *
      *    �굻����
           MOVE    LNK-SC90-TENSU      TO  LNK-COM-SYUTEN-M (IDX-SYU)
           ADD     LNK-SC90-TENSU      TO  LNK-SC90-TENSUKEI
      *    ����
           MOVE    WRK-SC90-YKZTEN     TO  LNK-COM-YKZTEN-M(IDX-SYU)
           ADD     WRK-SC90-YKZTEN     TO  LNK-SC90-YKZTEN
      *    ���
           MOVE    WRK-SC90-KIZTEN     TO  LNK-COM-KIZTEN-M (IDX-SYU)
           ADD     WRK-SC90-KIZTEN     TO  LNK-SC90-KIZTEN
      *    �ե��������
           MOVE    WRK-SC90-FIRTEN     TO  LNK-COM-FIRTEN-M (IDX-SYU)
           ADD     WRK-SC90-FIRTEN     TO  LNK-SC90-FIRTEN
           .
       100-MEISAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    �����������Խ�����
      *****************************************************************
       1001-YAKUZAI-SET-SEC           SECTION.
      *
      *    ����
           COMPUTE WRK-SYOKEIS2        =  (LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)  )
           ADD     WRK-SYOKEIS2        TO  WRK-ZAIKEI
      *
      *H31.2
      *    ��ۤ������奪���С��ϥ��顼
           IF     (LNK-COM-KISOTEN (IDX)   *   LNK-COM-SURYO(IDX))
                                           >   9999999999
               MOVE    "0249"          TO  LNK-SC90-ERRCD
           END-IF
           .
       1001-YAKUZAI-SET-EXT.
           EXIT.
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       1002-CHUUSYA-SET-SEC           SECTION.
      *
      *    �����Ѵ�Ƚ��
      *!   IF     (LNK-COM-TANICD (IDX)    =   "014"
      *                                    OR  "022"
      *!                                   OR  "046"  )  AND
      *    �����Ѵ��Τ��뤹�٤Ƥ��������ޤ��оݤȤ���
           IF     (IDX                     <   MAX-LINE    )  AND
                  (LNK-COM-INPUTCD(IDX + 1)  =   "099309901")
               MOVE    LNK-COM-SURYO(IDX)    TO  WRK-SURYO
               IF      WRK-SURYO-S2          >   ZERO
                   COMPUTE WRK-SURYO-CHU9      =   LNK-COM-SURYO (IDX)
                                               +   0.99999
                   MOVE    WRK-SURYO-CHU9          TO  WRK-SURYO-CHU
               ELSE
                   MOVE    LNK-COM-SURYO (IDX)     TO  WRK-SURYO-CHU
               END-IF
      *H31.2
      *        ��ۤ������奪���С��ϥ��顼
               IF     (LNK-COM-KISOTEN (IDX)   *   WRK-SURYO-CHU)
                                           >   9999999999
                   MOVE    "0249"          TO  LNK-SC90-ERRCD
               END-IF
      *        ����
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                           *   WRK-SURYO-CHU
           ELSE
      *H28.4
      *        ������������ѹ�
               IF     (LNK-COM-SIPPUKBN (IDX)  =   "1" )
                  AND (LNK-COM-SIPGEN (IDX)    =   "1"
                                               OR  "2" )
                   MOVE    LNK-COM-SIP-SURYO (IDX) TO  WRK-SURYO
               ELSE
                   MOVE    LNK-COM-SURYO  (IDX)    TO  WRK-SURYO
               END-IF
      *�ʤ�
               MOVE    LNK-COM-SURYO  (IDX)    TO  WRK-SURYO
      *
      *H31.2
      *        ��ۤ������奪���С��ϥ��顼
               IF     (LNK-COM-KISOTEN (IDX)   *   WRK-SURYO)
                                           >   9999999999
                   MOVE    "0249"          TO  LNK-SC90-ERRCD
               END-IF
      *
      *        ����
               COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN(IDX)
                                           *   WRK-SURYO
           END-IF
           ADD     WRK-SYOKEIS2        TO  WRK-ZAIKEI
           .
       1002-CHUUSYA-SET-EXT.
           EXIT.
      *ver.4.7
      *****************************************************************
      *    ����¾�ʸ������Խ�����
      *****************************************************************
       10021-SONOTA-SET-SEC           SECTION.
      *
           IF     (LNK-COM-TENSIKIBETU (IDX) =   7 )
             AND  (LNK-COM-TEN (IDX)     =   ZERO  )
      *!!!!!
      *      �������������������ʬ�Τ�
             AND  (LNK-COM-INPUTCD (IDX)   =   "630010001")
      *!!!!!
               MOVE    LNK-COM-SURYO(IDX)  TO  WRK-GENTEN
               MOVE    LNK-COM-SURYO(IDX)  TO  LNK-COM-KISOTEN (IDX)
               MOVE    1                   TO  LNK-COM-PLUSKBN(IDX)
           END-IF
           .
       10021-SONOTA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ʥե����˲������ǰʳ��Խ�����
      *****************************************************************
       1003-FIRUMU-SET-SEC           SECTION.
      *
      *H31.2
      *    ��ۤ������奪���С��ϥ��顼
           IF     (LNK-COM-KISOTEN (IDX)   *   LNK-COM-SURYO(IDX))
                                           >   9999999999
               MOVE    "0249"          TO  LNK-SC90-ERRCD
           END-IF
      *
      *    ����
      *    (�������ʡ�����)
           COMPUTE WRK-SYOKEIS         =   LNK-COM-KISOTEN (IDX)
                                       *   LNK-COM-SURYO   (IDX)
           MOVE    WRK-SYOKEIS         TO  WRK-SYOKEI
      *
      *    ��������
           IF      LNK-COM-SRYKBN (IDX)    =   "70"
      *        ������ǯ��û���ʬȽ��
               IF      LNK-COM-TOKUKIZAIAGEKSNKBN (IDX)
                                           =   1
      *            ���Ļ��û�ʬ
                   PERFORM 10031-FIRUMU-KASAN-SEC
      ************ ADD     WRK-SYOKEI          TO  WRK-NYU-FIRKEI
               ELSE
      *            �ե���������û�
                   ADD     WRK-SYOKEI          TO  WRK-FIRKEI
               END-IF
           ELSE
      *        �������Ƚ��ʲ������ǰʳ���
               IF      LNK-COM-JGNTEN (IDX) =   ZERO
      *            �ե���������û�
                   ADD     WRK-SYOKEI          TO  WRK-FIRKEI
               ELSE
                   COMPUTE WRK-TENSUKEIS       =   WRK-SYOKEI
                                               /   10
                                               +   0.5
                   MOVE    WRK-TENSUKEIS       TO  WRK-FIRTEN
                   IF      WRK-FIRTEN          >   LNK-COM-JGNTEN (IDX)
                       MOVE    LNK-COM-JGNTEN(IDX) TO  WRK-FIRTEN
      *                ��������˾�������򽸷�
                       ADD     WRK-FIRTEN          TO  WRK-FIRTEN-G
                   ELSE
                       ADD     WRK-SYOKEI          TO  WRK-FIRKEI
                   END-IF
               END-IF
           END-IF
           .
       1003-FIRUMU-SET-EXT.
           EXIT.
      *****************************************************************
      *    ����Խ�����
      *****************************************************************
       1004-KIZAI-SET-SEC            SECTION.
      *
      *H31.2
      *    ��ۤ������奪���С��ϥ��顼
           IF     (LNK-COM-KISOTEN (IDX)   *   LNK-COM-SURYO(IDX))
                                           >   9999999999
               MOVE    "0249"          TO  LNK-SC90-ERRCD
           END-IF
      *
           EVALUATE    LNK-COM-TOKUKIZAISBT1 (IDX)
               WHEN    ZERO
      *            �̾�
      *            (�������ʡ�����)
                   COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                               *   LNK-COM-SURYO   (IDX)
                   ADD     WRK-SYOKEIS2        TO  WRK-KIZKEI
               WHEN    2
                   IF      LNK-COM-SANSOKBN (IDX)  =   ZERO
      *            ����
      *                (�������ʡ�����)���ͼθ���
                       COMPUTE WRK-SYOKEIS2    =  (LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX))
                                               +   0.5
                       MOVE    WRK-SYOKEIS2        TO  WRK-SYOKEIS
                       ADD     WRK-SYOKEIS         TO  WRK-CHISOKEI
                   END-IF
      *            ����
                   IF      LNK-COM-SANSOKBN (IDX)  =   1
      *                (�������ʡ�����)
                       COMPUTE WRK-SYOKEIS2    =   LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX)
                       ADD     WRK-SYOKEIS2        TO  WRK-SANSOKIN-SUM
                   END-IF
               WHEN    3
      *            ������Ψ���ꥸ����
      *            (�������ʡ�����)
                   COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                               *   LNK-COM-SURYO   (IDX)
                   ADD     WRK-SYOKEIS2        TO  WRK-KIZKEI-3
               WHEN    4
      *            ���Х��
      *            (�������ʡ�����)
                   COMPUTE WRK-SYOKEIS2        =   LNK-COM-KISOTEN (IDX)
                                               *   LNK-COM-SURYO   (IDX)
                   ADD     WRK-SYOKEIS2        TO  WRK-KIZKEI-4
           END-EVALUATE
      *
           .
       1004-KIZAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������׻�����
      *****************************************************************
       1005-KEIZAITENU-SET-SEC           SECTION.
      *
           MOVE    WRK-ZAIKEI          TO  WRK-SYOKEIS2
      *
      * =============================================================
      *---// ����ʬ��������15���ʲ� �� ���� �����ްʳ��λ���������  //
      *-- // ����ʬ��������15��Ķ�� �� �ʣ���ʬ��������15��/ 10 
      *==============================================================
      *
           IF      WRK-SYOKEIS2        <=  15
               IF     (LNK-COM-SRYKBN (01)     =   "21"  OR  "22"  OR
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
           COMPUTE WRK-SC90-YKZTEN     =   WRK-SYOKEIS3   + 0.999999
      *
           .
       1005-KEIZAITENU-EXT.
           EXIT.
      *****************************************************************
      *    ��������׻������ʥե���ࡢ���ǡ����ǰʳ���
      *****************************************************************
       1006-KIZTEN-KEI-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-SC90-KIZTEN
      *    ����¾���
           IF      WRK-KIZKEI          >   ZERO
      *    ��۷סࣱ�����ͼθ���
               COMPUTE WRK-SYOKEIS2        =  (WRK-KIZKEI
                                           /   10  )
                                           +   0.5
               ADD     WRK-SYOKEIS2        TO  WRK-SC90-KIZTEN
           END-IF
      *    ������Ψ���ꥸ����
           IF      WRK-KIZKEI-3        >   ZERO
      *H22.4 ����
      *        ��۷סࣵ�����ͼθ�������H22.4 �����
               COMPUTE WRK-SYOKEIS2        =  (WRK-KIZKEI-3
                                           /   50  )
                                           +   0.5
               ADD     WRK-SYOKEIS2        TO  WRK-SC90-KIZTEN
           END-IF
      *H24.4 ����
      *    ���Х��
           IF      WRK-KIZKEI-4        >   ZERO
      *        ��۷ס�1000���ͼθ���
               COMPUTE WRK-SYOKEIS2        =  (WRK-KIZKEI-4
                                           /   1000 )
                                           +   0.5
               ADD     WRK-SYOKEIS2        TO  WRK-SC90-KIZTEN
           END-IF
      *
           .
       1006-KIZTEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����������Ļ��û��ե���������׻�����
      *****************************************************************
       10031-FIRUMU-KASAN-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-TENSUKEIS
           COMPUTE IDX7                =   IDX   +   1
           IF      LNK-COM-INPUTCD(IDX7)
                                       =   "799990070"
      *        ľ�������Ļ��û��������
               COMPUTE WRK-TENSUKEIS       =   WRK-SYOKEI
                                           *   LNK-COM-KISOTEN (IDX7)
                                           /   100
               COMPUTE WRK-NYU-FIRTEN      =   WRK-SYOKEI
                                           +   WRK-TENSUKEIS
           ELSE
               MOVE    WRK-SYOKEI          TO  WRK-NYU-FIRTEN
           END-IF
      *
           ADD     WRK-NYU-FIRTEN          TO  WRK-NYU-FIRKEI
      *
           .
       10031-FIRUMU-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ե���������׻�����
      *****************************************************************
       1007-FIRUMU-KEI-SEC           SECTION.
      *
           IF      WRK-NYU-FIRKEI      >   ZERO
               COMPUTE WRK-FIRKEI          =   WRK-FIRKEI
                                           +   WRK-NYU-FIRKEI
           END-IF
      *
           IF      WRK-FIRKEI        >   ZERO
      *        ��۷סࣱ�����ͼθ���
               COMPUTE WRK-SYOKEIS2        =  (WRK-FIRKEI
                                           /   10  )
                                           +   0.5
               MOVE    WRK-SYOKEIS2        TO  WRK-SC90-FIRTEN
           ELSE
      *        �������ʬ
               MOVE    WRK-FIRTEN-G        TO  WRK-SC90-FIRTEN
           END-IF
      *
           .
       1007-FIRUMU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����׻�����
      *****************************************************************
       200-TENSUKEI-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-CHISOKEI
           MOVE    ZERO                TO  WRK-SANSOKIN-SUM
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  FLG-GENTEN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   MAX-LINE  )  OR
                             ((LNK-COM-INPUTCD(IDX)    =   SPACE ) AND
                              (LNK-GMN-INPUTCD(IDX)    =   SPACE ))
               ADD     1                   TO  IDX-MAX
               IF     (LNK-COM-INPUTCD (IDX)(1:1)  =   "7"  ) AND
                      (LNK-COM-TENSIKIBETU (IDX)   =   1
                                                   OR  2    )
                   IF      LNK-COM-TOKUKIZAISBT1(IDX)   =  2
      *                ���ǡ����Ǥν���
                       PERFORM 2004-KIZAI-SET-SEC
                   END-IF
               END-IF
      *        ����
               IF     (LNK-COM-PLUSKBN (IDX)   =   1   )
                   MOVE    1               TO  FLG-GENTEN
               END-IF
               IF      FLG-GENTEN          =   1
                   MOVE    1               TO  LNK-COM-PLUSKBN (IDX)
               END-IF
           END-PERFORM
      *    ���������׻�����
           IF      WRK-CHISOKEI        =   ZERO
               MOVE      ZERO              TO  LNK-SC90-CHITEN
           ELSE
               PERFORM 2007-CHITEN-KEI-SEC
           END-IF
      *    ���������׻�����
           IF      WRK-SANSOKIN-SUM    =   ZERO
               MOVE      ZERO              TO  LNK-SC90-SANTEN
           ELSE
               PERFORM 2008-SANSO-KEI-SEC
           END-IF
      *
           MOVE    LNK-SC90-CHITEN     TO  LNK-COM-CHISOTEN(IDX-MAX)
           MOVE    LNK-SC90-SANTEN     TO  LNK-COM-SANSOTEN(IDX-MAX)
      *
      *    ��������Խ�
           MOVE    LNK-SC90-TENSUKEI   TO  LNK-COM-SYUTEN1 (IDX-MAX)
      *    ����
           MOVE    LNK-SC90-YKZTEN     TO  LNK-COM-YKZTEN  (IDX-MAX)
      *    ���
      *H31.3
      *    COMPUTE LNK-COM-KIZAITEN(IDX-MAX)
      *                                =   LNK-SC90-KIZTEN
      *                                +   LNK-SC90-SANTEN
      **                               +   LNK-SC90-CHITEN
           COMPUTE WRK-GOK-KIZAI
                                       =   LNK-SC90-KIZTEN
                                       +   LNK-SC90-SANTEN
                                       +   LNK-SC90-CHITEN
           MOVE    WRK-GOK-KIZAI       TO  LNK-COM-KIZAITEN(IDX-MAX)
      *    �ե����
           IF      LNK-COM-SRYKBN (01)    =   "70"
      *        �ե��������굻������
               MOVE    LNK-SC90-FIRTEN     TO  LNK-COM-SYUTEN2(IDX-MAX)
      *        �ե��������굻���زû�
               COMPUTE LNK-COM-SYUTEN1 (IDX-MAX)
                                           =   LNK-COM-SYUTEN1 (IDX-MAX)
                                           +   LNK-SC90-FIRTEN
           ELSE
               ADD     LNK-SC90-FIRTEN     TO  LNK-COM-KIZAITEN(IDX-MAX)
           END-IF
      *
      *    �����ס��׻�
      *    COMPUTE LNK-COM-TENSU (IDX-MAX) =   LNK-SC90-TENSUKEI
      *                                    +   LNK-SC90-YKZTEN
      *                                    +   LNK-SC90-KIZTEN
      *                                    +   LNK-SC90-FIRTEN
      *                                    +   LNK-SC90-SANTEN
      *                                    +   LNK-SC90-CHITEN
      *H31.3
      *    �����ס��׻�
           COMPUTE WRK-GOK-TENSU           =   LNK-SC90-TENSUKEI
                                           +   LNK-SC90-YKZTEN
                                           +   LNK-SC90-KIZTEN
                                           +   LNK-SC90-FIRTEN
                                           +   LNK-SC90-SANTEN
                                           +   LNK-SC90-CHITEN
      *    ����ޤ��Խ�����
           MOVE    WRK-GOK-TENSU           TO  LNK-COM-TENSU (IDX-MAX)
      *    ����ޤ�
           IF      WRK-GOK-TENSU       >   9999999
               MOVE    "0249"              TO  LNK-SC90-ERRCD
           END-IF
      *
      *    ����ʬ
           IF      FLG-GENTEN          =   1
               MOVE    1               TO  LNK-COM-PLUSKBN (IDX-MAX)
           END-IF
      *
           .
       200-TENSUKEI-EXT.
           EXIT.
      *****************************************************************
      *    ����Խ�����
      *****************************************************************
       2004-KIZAI-SET-SEC            SECTION.
      *
           EVALUATE    LNK-COM-TOKUKIZAISBT1 (IDX)
               WHEN    2
                   IF      LNK-COM-SANSOKBN (IDX)  =   ZERO
      *            ����
      *                (�������ʡ�����)���ͼθ���
                       COMPUTE WRK-SYOKEIS2    =  (LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX))
                                               +   0.5
                       MOVE    WRK-SYOKEIS2        TO  WRK-SYOKEIS
                       ADD     WRK-SYOKEIS         TO  WRK-CHISOKEI
                   END-IF
      *            ����
                   IF      LNK-COM-SANSOKBN (IDX)  =   1
      *                (�������ʡ�����)
                       COMPUTE WRK-SYOKEIS2    =   LNK-COM-KISOTEN(IDX)
                                               *   LNK-COM-SURYO  (IDX)
                       ADD     WRK-SYOKEIS2        TO  WRK-SANSOKIN-SUM
                   END-IF
           END-EVALUATE
      *
           .
       2004-KIZAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������׻����������ǡ�
      *****************************************************************
       2007-CHITEN-KEI-SEC             SECTION.
      *
      *    ��۷סࣱ�����ͼθ���
           COMPUTE WRK-SYOKEIS2        =  (WRK-CHISOKEI
                                       /   10  )
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  LNK-SC90-CHITEN
      *
           .
       2007-CHITEN-KEI-SEC-EXT.
           EXIT.
      *****************************************************************
      *     ���Ǥ��������׻�����
      *****************************************************************
       2008-SANSO-KEI-SEC                SECTION.
      *
           MOVE      ZERO              TO  LNK-SC90-SANTEN
      *
      *    �����������Ψ�û��Ȥ���
           MOVE    ZERO                TO  WRK-SANSOKIN-SUM
           MOVE    ZERO                TO  IDX-H
           MOVE    ZERO                TO  IDX-K
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   IDX-MAX )
               EVALUATE    LNK-COM-INPUTCD (IDX)
      *            ��������Ψ��ư�û�
                   WHEN    "770020070"
                       MOVE    IDX             TO  IDX-H
      *            �ⵤ�����Ƿ׻�
                   WHEN    "770030070"
                       MOVE    IDX             TO  IDX-K
               END-EVALUATE
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-SYOKEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-MAX )
               MOVE    ZERO                TO  WRK-SANSOKIN
               MOVE    ZERO                TO  WRK-SANSOGOK
      *        ����
               IF     (LNK-COM-INPUTCD (IDX)(1:1)  =   "7"  ) AND
                      (LNK-COM-TENSIKIBETU (IDX)   =   1
                                                   OR  2    ) AND
                      (LNK-COM-TOKUKIZAISBT1(IDX)  =   2    ) AND
                      (LNK-COM-SANSOKBN (IDX)      =   1    )
      *            (�������ʡ�����)
                   COMPUTE WRK-SYOKEIS2    =   LNK-COM-KISOTEN(IDX)
                                           *   LNK-COM-SURYO  (IDX)
      *            ���Ƕ�ۡܡʻ��Ƕ�ۡ�����Ψ���������ˡ��ͼθ���
                   COMPUTE WRK-SYOKEIS3    =   WRK-SYOKEIS2
                                           *   LNK-COM-KISOTEN (IDX-H)
                                           /   100
                   COMPUTE WRK-SANSOKIN    =   WRK-SYOKEIS2
                                           +   WRK-SYOKEIS3
      *            �ⵤ�����Ƿ׻�
                   IF      IDX-K           >   ZERO
      *                ���Ƕ�ۡܡʻ��Ƕ�ۡ�����Ψ����������
      *                                           �ߵ��������ͼθ���
                       COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
                                               *   LNK-COM-SURYO (IDX-K)
                       MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
                   END-IF
      *            ��ۡ��ͼθ���
                   COMPUTE WRK-SANSOGOK        =   WRK-SANSOKIN
                                               +   0.5
                   COMPUTE WRK-SYOKEI      =   WRK-SYOKEI
                                           +   WRK-SANSOGOK
               END-IF
           END-PERFORM
      *
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   IDX-MAX
      *        EVALUATE    LNK-COM-INPUTCD (IDX)
      *            �ⵤ�����Ƿ׻�
      *            WHEN    "770030070"
      *                ���Ƕ�ۡܡʻ��Ƕ�ۡ�����Ψ����������
      *                                           �ߵ��������ͼθ���
      *                COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
      *                                        *   LNK-COM-SURYO (IDX)
      *                MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
      *        END-EVALUATE
      *    END-PERFORM
      *    ���Ǥ��������׻�
      *    ��۷ס��ͼθ���
      *    COMPUTE WRK-SYOKEI          =   WRK-SANSOKIN
      *                                +   0.5
      *    ��۷סࣱ�����ͼθ���
           COMPUTE WRK-SYOKEIS2        =   WRK-SYOKEI
                                       /   10
                                       +   0.5
           MOVE    WRK-SYOKEIS2        TO  WRK-SANTEN
      *    ���Ǥ��������������˲û�
           MOVE    WRK-SANTEN          TO  LNK-SC90-SANTEN
      *
           .
       2008-SANSO-KEI-EXT.
           EXIT.
      *
      *