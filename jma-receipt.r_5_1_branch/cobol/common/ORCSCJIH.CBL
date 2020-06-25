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
       PROGRAM-ID.                 ORCSCJIH.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ���Ź԰٥����å�������ʼ����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/04/12    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  02/11/20  �����մ���ɲ�
      * 01.00.02     NACL-¿��Ǽ  03/04/30  ���������ɲ�
      * 04.07.00     NACL-¿��Ǽ  14/10/03  �����Ƿ׻�����.961�б�
      * 04.08.00     NACL-¿��Ǽ  17/06/XX  ���ʥ������б�¾
      * 04.08.00     NACL-¿��Ǽ  17/09/XX  ���񥳡���ü�������б�
      * 05.00.00     NACL-¿��Ǽ  19/09/XX  �����ǣ������б�
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
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
      *
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-MOJI            PIC X(01). 
      *
           03  WRK-ERRCD           PIC X(04). 
      *H29.9   �������ʲ������
      *    03  WRK-KINGAK-9        PIC 9(08)V99.
           03  WRK-KINGAK-9        PIC 9(08)V9(5).
           03  WRK-KINGAK          PIC 9(08).
      *
           03  WRK-ZEIKIN-9        PIC 9(08)V9(5).
           03  WRK-ZEIKIN          PIC 9(08).
           03  WRK-TANKA           PIC 9(08)V9(5).
           03  WRK-KISOTEN         PIC 9(08)V9(5).
      *
           03  WRK-ZEIRITU         PIC 9(01)V99.
      *
           03  WRK-TENSIKIBETU     PIC 9(01).
      *
      *������Ψ
       01  CONS-ZEI1               PIC 9(01)V99   VALUE   0.05.
       01  CONS-ZEI2               PIC 9(01)V99   VALUE   0.08.
      *R01.10����
       01  CONS-ZEI3               PIC 9(01)V99   VALUE   0.10.
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ޥ�����
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *
      *    ���Ź԰ٻ����Խ��ѥ�᥿
      *****COPY    "CPORCSC90.INC".
      *
      *    ���Ź԰ٻ���������å��ѥ�᥿
      **** COPY    "CPORCSC91.INC".
      *
      *    ���Ź԰������Խ������ܥ����å�
      **** COPY    "CPORCSC92.INC".
      *
      *    ���̺��Խ��ѥ�᥿
           COPY    "CPORCSC94.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *H29.9
      *    ���Ź԰��Ѷ��̥ѥ�᥿
           COPY    "K01COMMON-SPA".
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *H29.9
      *    ���񥳡���ü����������
           IF      LNK-ORCSC-PG        =   "J04"
               PERFORM 400-JIHI-HASU-SEC
           ELSE
               MOVE    SPA-WORK        TO  SPA-K01KYOUTU
           END-IF
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
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "." OR "S"
                                                               OR "*"
                   MOVE    WRK-GMN-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
               ELSE
                   MOVE    WRK-COM-INPUTCD(IDX-Z)(1:1) TO  WRK-MOJI
                   IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059"
                       MOVE    "7"                     TO  WRK-MOJI
                   END-IF
               END-IF
      *
               EVALUATE    WRK-MOJI
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "*"
                       CONTINUE
                   WHEN    "S"
      *                ���åȤ������ԲĤȤ���
                       MOVE    "0055"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
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
           EVALUATE    WRK-MOJI
               WHEN    "0"
      *            ������ϤΤ��뼫��ϲ���ϣ��Ȥ���
      *!           IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "095" OR
      *!                                                   "096" ) AND
      *!                  (WRK-COM-KISOTEN(IDX-Z)      =   ZERO  )
      *!               IF      LNK-ORCSC-KAISU         >   1
      *!                   MOVE    "0008"          TO  WRK-ERRCD
      *!                   MOVE    "1"             TO  WRK-COM-CUR
      *!                                                  (IDX-Z)
      *!               END-IF
      *!           END-IF
      *            �ȣ����������������ǹ����ɲ�
                   IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "095" OR
                                                           "096" )
                       IF     (WRK-COM-TENSIKIBETU(IDX-Z)  =  4  )  AND
                              (WRK-COM-SRYKBN  (IDX-Z)     =  "95")
                           MOVE    "0086"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-Z)
                       END-IF
                   END-IF
      *H31.3   ���ޤ�Ƚ��
               WHEN    "6"
      *            �׻��Ǥ����������̤Ǥ��뤳��
                   EVALUATE    WRK-COM-TENSIKIBETU(IDX-Z)
                       WHEN    1
                       WHEN    2
                       WHEN    4
                       WHEN    5
                           CONTINUE
                       WHEN    OTHER
                           MOVE    "0055"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-Z)
                   END-EVALUATE
      *
               WHEN    OTHER
      *            �׻��Ǥ����������̤Ǥ��뤳��
                   EVALUATE    WRK-COM-TENSIKIBETU(IDX-Z)
                       WHEN    1
                       WHEN    2
                       WHEN    3
                       WHEN    4
                           CONTINUE
                       WHEN    OTHER
                           MOVE    "0055"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                          (IDX-Z)
                   END-EVALUATE
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
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
      *
      *    ������Ψ�Խ�
           MOVE    CONS-ZEI1           TO  WRK-ZEIRITU
           IF      SPA-SRYYMD          >=  "20140401"
               MOVE    CONS-ZEI2           TO  WRK-ZEIRITU
           END-IF
      *R01.10 ����
           IF      SPA-SRYYMD          >=  "20191001"
               MOVE    CONS-ZEI3           TO  WRK-ZEIRITU
           END-IF
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )   OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               MOVE    SPACE               TO  WRK-GMN-SURYO (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-TENSU (IDX-Z)
               MOVE    SPACE               TO  WRK-GMN-KAISU (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-TENSU (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-INGAITEN(IDX-Z)
      *
               MOVE    ZERO                TO  WRK-COM-SYUTEN1 (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-SYUTEN2 (IDX-Z)
               MOVE    ZERO                TO  WRK-COM-KIZAITEN (IDX-Z)
      *
               MOVE    ZERO                TO  WRK-KINGAK
      *
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."  OR  "S"
                                                                OR "*"
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
      *                ����׻���
                       PERFORM 10021-JIHI-KEI-SEC
                   WHEN    "8"
                   WHEN    "."
                   WHEN    "S"
                   WHEN    "*"
                       CONTINUE
      *
                   WHEN    OTHER
      *                �Խ������å���
                       PERFORM 10021-KEISAN-SEC
               END-EVALUATE
      *
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "095" OR
                                                       "096" ) AND
                      (WRK-COM-KISOTEN(IDX-Z)      =   ZERO  )
                   COMPUTE WRK-KINGAK          =   WRK-COM-JIHIMONEY
                                                               (IDX-Z)
                                               *   LNK-ORCSC-KAISU
                   MOVE    WRK-KINGAK          TO  WRK-COM-KEI (IDX-Z)
                   MOVE    WRK-KINGAK          TO  WRK-GMN-KEI (IDX-Z)
                   MOVE    "1"                 TO  WRK-COM-KEIFLG(IDX-Z)
               ELSE
      *
                   MOVE    WRK-KINGAK          TO  WRK-COM-JIHIMONEY
                                                               (IDX-Z)
                   COMPUTE WRK-KINGAK          =   WRK-KINGAK
                                               *   LNK-ORCSC-KAISU
                   MOVE    WRK-KINGAK          TO  WRK-COM-KEI (IDX-Z)
                   MOVE    WRK-KINGAK          TO  WRK-GMN-KEI (IDX-Z)
      *
                   MOVE    "2"                 TO  WRK-COM-KEIFLG(IDX-Z)
               END-IF
      *H30.5
      *        ��ۤϣ���ޤ�
               IF      WRK-COM-KEI (IDX-Z) >   9999999
                   MOVE    "1088"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               END-IF
      *
               MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU(IDX-Z)
               MOVE    LNK-ORCSC-KAISU     TO  WRK-GMN-KAISU(IDX-Z)
           END-PERFORM
      *
           .
       1002-TENSU-KEI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ����׻�����
      *****************************************************************
       10021-JIHI-KEI-SEC                  SECTION.
      *
      *H29.6
      *    ���ʥ������оݳ�
           IF      WRK-COM-INPUTCD(IDX-Z)(1:3) =   "058"
               GO  TO  10021-JIHI-KEI-EXT
           END-IF
      *
      *H26.10
      *    ��ۡ����Ǿ����Ƿ׻�
           IF     (WRK-COM-AUTOFLG2 (IDX-Z)    =   "1"   )
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "095" OR
                                                       "096" ) AND
                      (WRK-COM-SRYSYUKBN(IDX-Z)    =   "960" ) AND
                      (WRK-COM-KISOTEN(IDX-Z)      =   ZERO  ) AND
                      (WRK-COM-KEIFLG   (IDX-Z)    =   "1"   ) AND
                      (WRK-COM-JIHIMONEY(IDX-Z)    >   ZERO  )
                   PERFORM 100211-JIHI-KEISAN-SEC
               ELSE
      *            ���κ��
                   MOVE    SPACE           TO  WRK-COM-AUTOFLG2 (IDX-Z)
      *            ɽ��
                   INITIALIZE                  ORCSC94AREA
                   MOVE    "1"                 TO  LNK-SC94-KBN
                   MOVE    "K02"               TO  LNK-SC94-PG
                   MOVE    IDX-Z               TO  LNK-SC94-IDX
                   CALL    "ORCSC94"           USING
                                               ORCSC94AREA
                                               WRK-SCR-REC
                                               SPA-AREA
               END-IF
           END-IF
      *
      *
           IF      WRK-COM-KISOTEN(IDX-Z)      =   ZERO
               CONTINUE
           ELSE
      *        �����Ƿ׻�
               IF      (SPA-SRYYMD     >=  "20070401" )  AND
                       (WRK-COM-SRYKBN(IDX-Z)  =   "96")  AND
                       (WRK-COM-TENSIKIBETU(IDX-Z)  =  0 )
      *
      *H26.10      ñ���߿��̤��о�
                   IF      WRK-COM-SRYSYUKBN(IDX-Z) =   "961"
                       COMPUTE WRK-KISOTEN     =   WRK-COM-KISOTEN
                                                            (IDX-Z)
                                               *   WRK-COM-SURYO(IDX-Z)
                       PERFORM 10021-ZEIKEISEN-SEC
                       MOVE    WRK-TANKA       TO  WRK-KINGAK-9
                   ELSE
      *
                       MOVE    WRK-COM-KISOTEN (IDX-Z)  TO  WRK-KISOTEN
                       PERFORM 10021-ZEIKEISEN-SEC
                       COMPUTE WRK-KINGAK-9    =   WRK-TANKA
                                               *   WRK-COM-SURYO(IDX-Z)
                   END-IF
               ELSE
                   MOVE    WRK-COM-KISOTEN (IDX-Z)  TO  WRK-TANKA
                   COMPUTE WRK-KINGAK-9    =   WRK-TANKA
                                           *   WRK-COM-SURYO(IDX-Z)
               END-IF
      *
      *H29.9
      *        ü������
               EVALUATE    SPA-JIHI-HASU-KBN
               WHEN    "1"
      *        �������ʲ��ͼθ���
                   COMPUTE WRK-KINGAK      =   WRK-KINGAK-9
                                           +   0.5
               WHEN    "3"
      *        �������ʲ��ڤ�夲
                   COMPUTE WRK-KINGAK      =   WRK-KINGAK-9
                                           +   0.9999
               WHEN    OTHER
      *        �������ʲ��ڤ�Τ�
                   MOVE    WRK-KINGAK-9    TO  WRK-KINGAK
               END-EVALUATE
           END-IF
      *
           .
       10021-JIHI-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ƿ׻�����
      *****************************************************************
       10021-ZEIKEISEN-SEC                  SECTION.
      *
      *
           COMPUTE WRK-ZEIKIN-9        =   WRK-KISOTEN
                                       *   WRK-ZEIRITU
      *
      *****IF      SPA-TAX-HASU-KBN    =   "1"
      *        �������ʲ��ͼθ���
      *        COMPUTE WRK-ZEIKIN      =   WRK-ZEIKIN-9
      *                                +   0.5
      *    ELSE
      *        �������ʲ��ڤ�Τ�
      *        MOVE    WRK-ZEIKIN-9        TO  WRK-ZEIKIN
      *****END-IF
           EVALUATE    SPA-TAX-HASU-KBN
               WHEN    "1"
      *        �������ʲ��ͼθ���
                   COMPUTE WRK-ZEIKIN      =   WRK-ZEIKIN-9
                                           +   0.5
               WHEN    "3"
      *        �������ʲ��ڤ�夲
                   COMPUTE WRK-ZEIKIN      =   WRK-ZEIKIN-9
                                           +   0.99999
               WHEN    OTHER
      *            �������ʲ��ڤ�Τ�
                   MOVE    WRK-ZEIKIN-9        TO  WRK-ZEIKIN
           END-EVALUATE
      *
           COMPUTE WRK-TANKA           =   WRK-KISOTEN
                                       +   WRK-ZEIKIN
           .
       10021-ZEIKEISEN-EXT.
           EXIT.
      *
      *H26.10
      *****************************************************************
      *    ���϶�ۤξ����Ƿ׻�����
      *****************************************************************
       100211-JIHI-KEISAN-SEC                  SECTION.
      *
      *
           COMPUTE WRK-ZEIKIN-9        =   WRK-COM-JIHIMONEY(IDX-Z)
                                       *   WRK-ZEIRITU
      *
           EVALUATE    SPA-TAX-HASU-KBN
               WHEN    "1"
      *        �������ʲ��ͼθ���
                   COMPUTE WRK-ZEIKIN      =   WRK-ZEIKIN-9
                                           +   0.5
               WHEN    "3"
      *        �������ʲ��ڤ�夲
                   COMPUTE WRK-ZEIKIN      =   WRK-ZEIKIN-9
                                           +   0.99999
               WHEN    OTHER
      *            �������ʲ��ڤ�Τ�
                   MOVE    WRK-ZEIKIN-9        TO  WRK-ZEIKIN
           END-EVALUATE
           COMPUTE WRK-TANKA           =   WRK-COM-JIHIMONEY(IDX-Z)
                                       +   WRK-ZEIKIN
      *
           IF      WRK-TANKA           >   9999999
      *H30.5
      *        ��ۤϣ���ޤ�
               MOVE    "1088"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR (IDX-Z)
               GO  TO    100211-JIHI-KEISAN-EXT
           END-IF
      *
           MOVE    WRK-TANKA           TO  WRK-COM-JIHIMONEY(IDX-Z)
           MOVE    WRK-TANKA           TO  WRK-COM-KEI      (IDX-Z)
           MOVE    WRK-TANKA           TO  WRK-GMN-KEI      (IDX-Z)
           MOVE    SPACE               TO  WRK-COM-AUTOFLG2 (IDX-Z)
      *    ɽ��
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    "K02"               TO  LNK-SC94-PG
           MOVE    IDX-Z               TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       WRK-SCR-REC
                                       SPA-AREA
           .
       100211-JIHI-KEISAN-EXT.
           EXIT.
      *!!
      *
      *****************************************************************
      *    �����׻�����
      *****************************************************************
       10021-KEISAN-SEC                  SECTION.
      *
           IF     (WRK-COM-INPUTCD(IDX-Z)(1:5) =   "09591" OR
                                                   "09592" OR
                                                   "09593" OR
                                                   "09594" ) AND
                  (WRK-COM-KISOTEN(IDX-Z)  NOT =   WRK-COM-TEN(IDX-Z))
               MOVE    WRK-COM-TEN(IDX-Z)  TO  WRK-COM-KISOTEN(IDX-Z)
           END-IF
      *
      *H29.6
      *    ��ۡ�����Ƚ��
           MOVE    ZERO                TO  WRK-TENSIKIBETU
           IF      WRK-COM-INPUTCD(IDX-Z)(1:1) =   "1"
               EVALUATE    WRK-COM-TENSIKIBETU(IDX-Z)
      *        ���
               WHEN    1
                   MOVE    1           TO  WRK-TENSIKIBETU
      *        ����
               WHEN    3
      *        ��ƻ�ܸ���������
               WHEN    4
                   MOVE    3           TO  WRK-TENSIKIBETU
               END-EVALUATE
           END-IF
           IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "7"
                                               OR  "6" )
             OR   (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "059")
               EVALUATE    WRK-COM-TENSIKIBETU(IDX-Z)
      *        ���
               WHEN    1
      *        ��ƻ�ܸ���������
               WHEN    2
                   MOVE    1           TO  WRK-TENSIKIBETU
      *        ��ۡ�����)�ʶ�ۤϷ׻��Ѥߡ�
               WHEN    4
      *H31.3
               WHEN    5
                   MOVE    1           TO  WRK-TENSIKIBETU
               END-EVALUATE
           END-IF
      *
      ***  EVALUATE    WRK-COM-TENSIKIBETU(IDX-Z)
           EVALUATE    WRK-TENSIKIBETU
      *        ���
               WHEN    1
      *            �����Ƿ׻�
                   IF      (WRK-COM-SRYSYUKBN(IDX-Z)  =   "961")
      *H26.10          ñ���߿��̡�����
                       COMPUTE WRK-KISOTEN     =   WRK-COM-KISOTEN
                                                            (IDX-Z)
                                               *   WRK-COM-SURYO(IDX-Z)
                       PERFORM 10021-ZEIKEISEN-SEC
                       COMPUTE WRK-KINGAK-9    =   WRK-TANKA
                                               +   0.5
                       MOVE    WRK-KINGAK-9    TO  WRK-KINGAK
                   ELSE
      *
                   IF      (SPA-SRYYMD     >=  "20070401" )  AND
                           (WRK-COM-SRYKBN(IDX-Z)  =   "96")
                       MOVE    WRK-COM-KISOTEN (IDX-Z)
                                                   TO  WRK-KISOTEN
                       PERFORM 10021-ZEIKEISEN-SEC
                   ELSE
                       MOVE    WRK-COM-KISOTEN (IDX-Z)  TO  WRK-TANKA
                   END-IF
                   IF      WRK-COM-KZMCOMPSIKIBETU (IDX-Z)  =   "1"
                       MOVE    WRK-TANKA           TO  WRK-KINGAK
                   ELSE
                       COMPUTE WRK-KINGAK-9    =   WRK-TANKA
                                               *   WRK-COM-SURYO(IDX-Z)
                                               +   0.5
                       MOVE    WRK-KINGAK-9    TO  WRK-KINGAK
                   END-IF
      *
                   END-IF
      *        ����
               WHEN    3
                   COMPUTE WRK-KISOTEN     =   WRK-COM-KISOTEN (IDX-Z)
                                           *   10
                   IF      (SPA-SRYYMD     >=  "20070401" )  AND
                           (WRK-COM-SRYKBN(IDX-Z)  =   "96")
                       PERFORM 10021-ZEIKEISEN-SEC
                   ELSE
                       MOVE    WRK-KISOTEN     TO  WRK-TANKA
                   END-IF
                   MOVE    WRK-TANKA       TO  WRK-KINGAK
           END-EVALUATE
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���񥳡���ü���������
      *****************************************************************
       400-JIHI-HASU-SEC                SECTION.
      *
           INITIALIZE                   SPA-K01KYOUTU
      *    �����ƥ���������ʰ��ŵ��ؾ����
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  SYSKANRI-REC
      *
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
      *        ���񥳡���ü������
               MOVE    SYS-1001-JIHI-HASU-KBN  TO  SPA-JIHI-HASU-KBN
           END-IF
      *
      *    ü���ھ夲�����
           IF      SPA-JIHI-HASU-KBN   =   SPACE
               MOVE    "2"                 TO  SPA-JIHI-HASU-KBN
           END-IF
           IF      SPA-SRYYMD          <   "20170901"
               MOVE    "2"                 TO  SPA-JIHI-HASU-KBN
           END-IF
          .
       400-JIHI-HASU-EXT.
           EXIT.
      *
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
      *****************************************************************
      *    �����ޥ������ɹ��ʥ�����
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
                   MOVE    SPACE               TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               MOVE    SPACE               TO  SYSKANRI-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *