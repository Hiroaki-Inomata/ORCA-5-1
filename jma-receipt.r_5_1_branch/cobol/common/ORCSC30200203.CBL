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
       PROGRAM-ID.                 ORCSC30200203.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ���Ź԰٥����å�����������͡�
      *                      �ȣ������������ޤ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC��¿��Ǽ  01/04/19  �����Ź԰���ǡ���ũ�ϣ���Ȥ���
      * 01.00.02     MCC��¿��Ǽ  01/05/16  ��ũ���ͤ�Ƚ�����ż��̤Ȥ���
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
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-CHKMST          PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-NAIYAK          PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBETU         PIC 9(01).
           03  FLG-SYUGI           PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *
           03  FLG-TENTEKI         PIC 9(01).
      *
           03  FLG-SYUGIADD        PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SYU             PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-KBN             PIC 9(04).
      *
           03  IDX-MAX             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-HAI             PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD-2         PIC X(09).
      *
           03  WRK-MOJI            PIC X(01). 
           03  WRK-CHUSYA          PIC 9(09).
      *
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05).
      *
           03  WRK-ZAITEN          PIC 9(08).
           03  WRK-GAITEN          PIC 9(08).
           03  WRK-KAISU           PIC 9(04).
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
      *
       01  WRK-SONOTA.
           03  WRK-ERRCD           PIC X(04). 
      *
       01  WRK-SORT-AREA.
           03  WRK-SRT-TBL2A.
               05  WRK-SRT-TBL2            OCCURS   30.
                   07  WRK-SRT-KNKMEI      PIC X(40).
                   07  WRK-SRT-SRYYM       PIC X(06).
                   07  WRK-SRT-SRYYMW      PIC X(07).
                   07  WRK-SRT-SRYCD       PIC X(09).
           03  WRK-KEY-SRYYM               PIC X(06).
           03  WRK-KEY-SRYCD               PIC X(09).
      *
      *    ��¸�ѣӣУ�
       01  HZN-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //HZN-//.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ޥ�����
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    ��������
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    �����å��ޥ�����
       01  CHK-REC.
           COPY    "CPCHK.INC".
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
      *    ���Ź԰������Խ������ܥ����å�
           COPY    "CPORCSC92.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
           INITIALIZE                  IDX-AREA
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
                   MOVE    "1099"          TO  SPA-ERRCD
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
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-ERRCD          TO  SPA-ERRCD
           END-IF
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
           MOVE    ZERO                TO  FLG-SYUGI
           MOVE    ZERO                TO  FLG-MADOKU
           MOVE    ZERO                TO  FLG-SEIBETU
           INITIALIZE                  WRK-AREA
           MOVE    SPACE               TO  WRK-ERRCD
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
      *
                   IF      WRK-COM-INPUTCD(IDX-Z)  =   "099303301"
                       MOVE    "1"                     TO  WRK-MOJI
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
      *    ��ũ�λ����굻���μ�ư����
           IF      WRK-COM-SRYSYUKBN(01)   =   "330"
               PERFORM 1002-TENTEKI-CHK-SEC
           END-IF
      *
      *    �鲼���ͤλ����굻���μ�ư����
           IF      WRK-COM-SRYSYUKBN(01)   =   "310"
               PERFORM 1002-CHUSYA-CHK-SEC
           END-IF
      *
      *    ���̤Τߤλ����굻���μ�ưȯ��
           IF     (FLG-SYUGI           =   ZERO  )  AND
                  (WRK-ERRCD           =   SPACE )
               PERFORM 1003-SYUGIJIDOU-SEC
           END-IF
      *
           IF      IDX-SYU             >   ZERO
      *        ��ư�û����ɲä���
               PERFORM 1004-JIDO-KASAN-SEC
           END-IF
      *
           IF      WRK-ERRCD(1:1)  NOT =   "K"
               MOVE    WRK-ERRCD          TO  SPA-ERRCD
           END-IF
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
      *        �굻��
               WHEN    "1"
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
                       IF      FLG-SYUGI           =   1
                           MOVE    "0019"          TO  WRK-ERRCD
                           MOVE    "1"             TO  WRK-COM-CUR
                                                               (IDX-Z)
                       END-IF
                       MOVE    1                   TO  FLG-SYUGI
      *                ���Ϥ�ͭ�ä�������ư�û������ϤȤ���
                       MOVE    SPACE                TO  WRK-COM-AUTOKBN
                                                                (IDX-Z)
      *                �굻���ΰ��֥��å�
                       MOVE    IDX-Z               TO  IDX-SYU
                   END-IF
      *
      *        ����
               WHEN    "6"
      *            ���Ƕ�ʬ
                   IF      WRK-COM-MADOKUKBN (IDX-Z)   =   1
                       MOVE   1                TO  FLG-MADOKU
                   END-IF
      *            ��ʪ��Ū���޶�ʬ
                   IF      WRK-COM-SEIBUTUKBN(IDX-Z)   =   1
                       MOVE   1                TO  FLG-SEIBETU
                   END-IF
      *            ���ͱ��̲û�
                   COMPUTE WRK-CHUSYA      =  (WRK-COM-CSYYOURYO(IDX-Z)
                                           *   WRK-COM-SURYO  (IDX-Z))
                                           +   WRK-CHUSYA
      *            �����Ѵ�
                   PERFORM 1006-HAIKI-SYORI-SEC
      *
      *        ���
               WHEN    "7"
      *                �ե����λ����顼
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
                       MOVE    "0047"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
      *
      *            ���ǡ����Ǥϥ��顼
                   IF      WRK-COM-TOKUKIZAISBT1 (IDX-Z) =   2
                       MOVE    "0039"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR  (IDX-Z)
                   END-IF
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����Ѵ������Ȥ�̵ͭ
      *****************************************************************
       1006-HAIKI-SYORI-SEC           SECTION.
      *
      *    ����ץ�λ��������ʲ�Ƚ��
           IF      WRK-COM-TANICD (IDX-Z)  =   "014" OR "022" OR "046"
               MOVE    WRK-COM-SURYO(IDX-Z)    TO  WRK-SURYO
               IF     (WRK-SURYO-S2              >   ZERO    )  AND
                      (WRK-COM-AUTOFLG2 (IDX-Z)  =   SPACE   )
                   MOVE    IDX-Z                 TO  IDX-HAI
                   PERFORM 10011-HAIKI-ADD-SEC
               END-IF
           END-IF
           .
       1006-HAIKI-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �굻����ưȯ��������
      *****************************************************************
       1003-SYUGIJIDOU-SEC           SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
           EVALUATE    WRK-COM-SRYSYUKBN (01)
      *        �鲼��������
               WHEN    "310"
                   IF      LNK-ORCSC-ROUJIN      =   ZERO
                       MOVE    "130000510"     TO  WRK-SRYCD
                   ELSE
                       MOVE    "130700510"     TO  WRK-SRYCD
                   END-IF
      *        ��̮����
               WHEN    "320"
                   IF      LNK-ORCSC-ROUJIN      =   ZERO
                       MOVE    "130003510"     TO  WRK-SRYCD
                   ELSE
                       MOVE    "130700610"     TO  WRK-SRYCD
                   END-IF
      *        ��ũ����
               WHEN    "330"
                   IF      LNK-ORCSC-ROUJIN      =   ZERO
                       IF      LNK-ORCSC-NENREI-YY     <   6
                           MOVE    "130003710"     TO  WRK-SRYCD
                       ELSE
                           MOVE    "130003810"     TO  WRK-SRYCD
                       END-IF
                   ELSE
                       MOVE    "130700710"     TO  WRK-SRYCD
                   END-IF
      *            Ʊ�������ܰʹ�
      *????        IF      LNK-ACCT2-SRYCD     NOT =   SPACE
      *????            MOVE    "099303301"         TO  WRK-SRYCD
      *????        END-IF
           END-EVALUATE
      *
           IF      WRK-SRYCD       NOT =   SPACE
               MOVE    1                   TO  IDX-S
               MOVE    ZERO                TO  FLG-ARI
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
               IF      FLG-ADD             =   1
                   MOVE    "1"             TO  WRK-COM-AUTOKBN (IDX-S)
                   MOVE    IDX-S           TO  IDX-SYU
                   ADD     1               TO  IDX-MAX
               END-IF
           END-IF
      *
           .
       1003-SYUGIJIDOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ũ�λ��������ˤ��굻�����ѹ�
      *****************************************************************
       1002-TENTEKI-CHK-SEC                  SECTION.
      *
      *    Ʊ�������ܰʹߤ��ѹ��ʤ�
           IF     (IDX-SYU             >   ZERO    )  AND
                  (WRK-COM-INPUTCD(IDX-SYU)
                                       =   "099303301")
               COMPUTE WRK-CHUSYA      =   LNK-ACCT2-CSYYOURYO
                                       +   WRK-CHUSYA
           END-IF
      *
      *    ���̤ˤ�ꡢ�굻���ѹ�
           IF      LNK-ORCSC-ROUJIN    =   ZERO
               MOVE    "130009310"         TO  WRK-SRYCD
           ELSE
               MOVE    "130700950"         TO  WRK-SRYCD
           END-IF
           IF      LNK-ORCSC-NENREI-YY     <   6
      *        ����̤���ǡ�100ml �ʾ�
               IF      WRK-CHUSYA      >=  100
                   MOVE    "130003710"     TO  WRK-SRYCD
               END-IF
           ELSE
      *        ���аʾ�ǡ�500ml �ʾ�
               IF      WRK-CHUSYA      >=  500
                   IF      LNK-ORCSC-ROUJIN    =   ZERO
                       MOVE    "130003810"     TO  WRK-SRYCD
                   ELSE
                       MOVE    "130700710"     TO  WRK-SRYCD
                   END-IF
               END-IF
           END-IF
      *
      *
      *    Ʊ�������ܰʹߤ��ѹ��ʤ�
           IF     (IDX-SYU             >   ZERO    )  AND
                  (WRK-COM-INPUTCD(IDX-SYU)
                                       =   "099303301")
               IF      WRK-SRYCD       NOT =   LNK-ACCT2-SRYCD
                   IF      WRK-COM-KZMFLG (IDX-SYU)    =   SPACE
                       MOVE    "K006"          TO  WRK-ERRCD
                       MOVE    "1"             TO  WRK-COM-KZMFLG
                                                              (IDX-SYU)
                   END-IF
               END-IF
               GO    TO      1002-TENTEKI-CHK-EXT
           END-IF
      *
           IF     (IDX-SYU             >   ZERO    )  AND
                  (WRK-COM-INPUTCD(IDX-SYU)
                                   NOT =   WRK-SRYCD )
               MOVE    IDX-SYU             TO  IDX-T
               MOVE    1                   TO  FLG-ARI
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
               IF      FLG-ADD             =   1
                   MOVE    "1"             TO  WRK-COM-AUTOKBN (IDX-S)
      *
      *            ���ż��̺��Խ�
                   IF      WRK-COM-SRYSYUKBN(IDX-SYU) 
                                           NOT =   WRK-COM-SRYSYUKBN
                                                             (01)
                       MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-COM-SRYSYUKBN
                                                             (01)
                       MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-GMN-INPUTCD
                                                            (01)
                                                            (2:3)
      *
                       PERFORM VARYING     IDX     FROM    1   BY  1
                                UNTIL     (IDX         >   SPA-MAX-LINE)
                                       OR (WRK-GMN-INPUTCD (IDX)
                                                      =   SPACE )
                           MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-COM-SRYSYUKBN
                                                            (IDX)
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       1002-TENTEKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �鲼�ڤλ��������ˤ��굻�����ѹ�
      *****************************************************************
       1002-CHUSYA-CHK-SEC                  SECTION.
      *
      *    ���̤���5ML�ʾ�λ�����̮���ͤ��ѹ�
           MOVE    SPACE               TO  WRK-SRYCD
           IF      WRK-CHUSYA          >=  5
               IF      LNK-ORCSC-ROUJIN      =   ZERO
                   MOVE    "130003510"     TO  WRK-SRYCD
               ELSE
                   MOVE    "130700610"     TO  WRK-SRYCD
               END-IF
           END-IF
      *
           IF     (IDX-SYU             >   ZERO    )  AND
                  (WRK-COM-INPUTCD(IDX-SYU)
                                   NOT =   WRK-SRYCD )
               MOVE    IDX-SYU             TO  IDX-T
               MOVE    1                   TO  FLG-ARI
               MOVE    1                   TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
               IF      FLG-ADD             =   1
                   MOVE    "1"             TO  WRK-COM-AUTOKBN (IDX-S)
      *
      *            ���ż��̺��Խ�
                   IF      WRK-COM-SRYSYUKBN(IDX-SYU) 
                                           NOT =   WRK-COM-SRYSYUKBN
                                                             (01)
                       IF      WRK-GMN-INPUTCD(01)(1:1)   =   "."
                           MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-COM-SRYSYUKBN
                                                             (01)
                           MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-GMN-INPUTCD
                                                            (01)(2:3)
      *                    ���ż���̾�Ρʸ���Ȥ����
                           MOVE    "��̮����"
                                               TO  WRK-GMN-MEISYO (01)
                       END-IF
      *
                       PERFORM VARYING     IDX     FROM    1   BY  1
                                UNTIL     (IDX         >   SPA-MAX-LINE)
                                      OR  (WRK-GMN-INPUTCD (IDX)
                                                      =   SPACE )
                           MOVE    WRK-COM-SRYSYUKBN(IDX-SYU)
                                               TO  WRK-COM-SRYSYUKBN
                                                            (IDX)
                           MOVE    WRK-COM-TNSSRYKBN(IDX-SYU)
                                               TO  WRK-COM-SRYKBN
                                                            (IDX)
                       END-PERFORM
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       1003-TENTEKI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ư�û���������
      *****************************************************************
       1004-JIDO-KASAN-SEC           SECTION.
      *
      *    �굻����ľ�����ɲä���
           MOVE    IDX-SYU             TO  IDX-S
      *
      *    ǯ��û����åȽ���
           PERFORM 1007-NYUYOJI-SET-SEC
      *
      *    ��ʪ��Ū����
           IF      FLG-SEIBETU         =   1
               MOVE    "130000110"         TO  WRK-SRYCD
      *        �û������å�����
               PERFORM 1100-KASAN-CHK-SEC
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
           END-IF
      *
      *    ���ǲû�
           IF      FLG-MADOKU          =   1
               MOVE    "130000310"         TO  WRK-SRYCD
      *        �û������å�����
               PERFORM 1100-KASAN-CHK-SEC
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
           END-IF
           .
       1004-JIDO-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ǯ��û��ɲý����ʶ��̡�
      *****************************************************************
       1007-NYUYOJI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-SYU IDX2)
                                           =   SPACE  OR  ZERO )
      *        �û��ɲ�
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-SYU IDX2)
                                           TO  WRK-SRYCD
               MOVE    IDX-SYU             TO  IDX-S
      *        �û������å�����
               PERFORM 1100-KASAN-CHK-SEC
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
               IF      FLG-ADD             =   1
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       1007-NYUYOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ư�û���¸�ߥ����å��ý���
      *****************************************************************
       1100-KASAN-CHK-SEC        SECTION.
      *
      *    ���ּ�ư�û���¸�ߥ����å�
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE   ) OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF       WRK-COM-INPUTCD (IDX1) =   WRK-SRYCD
                    MOVE    IDX1           TO  IDX-T
                    MOVE    1              TO  FLG-ARI
               END-IF
           END-PERFORM
           .
       1100-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ư�û������ɲý���
      *****************************************************************
       1100-JIDOTUIKA-SEC           SECTION.
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
           MOVE    WRK-SCR-REC         TO  HZN-SCR-REC
           INITIALIZE                  ORCSC92AREA
           IF      FLG-ARI             =   ZERO
               MOVE    "1"                 TO  LNK-SC92-KBN
           ELSE
               MOVE    SPACE               TO  WRK-COM-CHKFLG (IDX-S)
               MOVE    SPACE               TO  WRK-COM-GMNFLG (IDX-S)
           END-IF
           MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC92-SYORIFLG
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    WRK-SRYCD           TO  LNK-SC92-SRYCD
           MOVE    LNK-ORCSC-NENREI    TO  LNK-SC92-NENREI
           MOVE    LNK-ORCSC-ROUJIN    TO  LNK-SC92-ROUJIN
           MOVE    LNK-ORCSC-LSTSRYKA  TO  LNK-SC92-LSTSRYKA
           MOVE    LNK-ORCSC-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
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
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    HZN-SCR-REC     TO  WRK-SCR-REC
               IF      FLG-ARI             =   ZERO
                   COMPUTE IDX-S           =   IDX-S   -   1
               END-IF
               GO      TO      1100-JIDOTUIKA-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-ADD
      *    �굻�����ѹ��λ������Ŷ�ʬ�Υ��åȤϤ��ʤ�
           IF      FLG-SYUGIADD         =   1
              GO      TO      1100-JIDOTUIKA-EXT
           END-IF
           IF      IDX-S               =   1  OR  ZERO
              GO      TO      1100-JIDOTUIKA-EXT
           END-IF
           MOVE    WRK-COM-SRYKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYKBN(IDX-S)
           MOVE    SPACE               TO  WRK-GMN-SRYKBN(IDX-S)
           MOVE    WRK-COM-SRYSYUKBN (IDX-S - 1)
                                       TO  WRK-COM-SRYSYUKBN(IDX-S)
      *
           MOVE    "2"                 TO  WRK-COM-AUTOKBN (IDX-S)
      *
           .
        1100-JIDOTUIKA-EXT.
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
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
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
           PERFORM 1006-KASAN-SET-SEC
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
      *
      *    ���å��ѹ������
           MOVE    WRK-COM-KEI  (IDX-MAX)  TO  LNK-ORCSC-ZAIKEI
           MOVE    WRK-COM-TENSU(IDX-MAX)  TO  LNK-ORCSC-ZAITEN
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
           MOVE    ZERO                TO  LNK-SC90-HAIKI
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
      *    �ƥ����å�����
      **** EVALUATE    WRK-MOJI
      *        ����
      *        WHEN    "6"
      *            �Ѵ���ͭ�ä������˴������Ȥ��ɲä���
      *            IF      LNK-SC90-HAIKI          =   1
      *                MOVE    IDX-Z               TO  IDX-HAI
      *                PERFORM 10011-HAIKI-ADD-SEC
      *            END-IF
      **** END-EVALUATE
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �˴������ȼ�ưȯ��������
      *****************************************************************
       10011-HAIKI-ADD-SEC           SECTION.
      *
           MOVE    "099309901"         TO  WRK-SRYCD
           IF      WRK-COM-INPUTCD (IDX-HAI + 1) =   WRK-SRYCD
               GO      TO      10011-HAIKI-ADD-EXT
           END-IF
      *
           MOVE    IDX-HAI             TO  IDX-S
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  FLG-SYUGIADD
           PERFORM 1100-JIDOTUIKA-SEC
      *
           .
       10011-HAIKI-ADD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���û��׻�����
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
      *    ��ư�û��ʳ��βû��ʻ��ֳ���ǯ��ʳ���
           MOVE    ZERO                TO  WRK-TENSUKEI-G
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
      *        �굻������������������Ȥ���
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO)
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                             (IDX-SYU)
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-G
      *
      *    ��ư�û��βû��ʻ��ֳ���ǯ���
           MOVE    ZERO                TO  WRK-TENSUKEI-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   1   )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   LNK-SC90-TENSUKEI
                                               *   WRK-COM-KISOTEN(IDX)
                                               /   100
                                               +   0.5
                   MOVE    WRK-TENSUKEI        TO  WRK-TENSUKEI-9
                   ADD     WRK-TENSUKEI-9      TO  WRK-TENSUKEI-G
               END-IF
           END-PERFORM
      *
           COMPUTE LNK-SC90-TENSUKEI   =   LNK-SC90-TENSUKEI
                                       +   WRK-TENSUKEI-G
      *
           .
       1006-KASAN-SET-EXT.
           EXIT.
     *
      *****************************************************************
      *    ��ũ���Ͳû���������
      *****************************************************************
      *��α
       510242-TEN-KASAN-SEC           SECTION.
      *
      *    ���̤ˤ�ꡢ�굻���ɲ�
           MOVE    SPACE               TO  WRK-SRYCD
           IF      LNK-ORCSC-NENREI-YY   <   6
      *        ����̤���ǡ�100ml �ʾ�
               IF      WRK-CHUSYA      >=  100
                   MOVE    "130003710"     TO  WRK-SRYCD
               END-IF
           ELSE
      *        ���аʾ�ǡ�500ml �ʾ�
               IF      WRK-CHUSYA      >=  500
                   MOVE    "130003810"     TO  WRK-SRYCD
               END-IF
           END-IF
      *
           IF      WRK-SRYCD       NOT =   SPACE
               MOVE    ZERO                TO  FLG-SYUGIADD
               PERFORM 1100-JIDOTUIKA-SEC
           END-IF
           .
       510242-TEN-KASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Ź԰٥����å�����
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
      *----(01.00.01) LINE ADD START ----------------------------------
      *
           MOVE    ZERO                TO  FLG-TENTEKI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD(IDX) =   SPACE)  OR
                              (SPA-ERRCD        NOT =   SPACE)
      *        ��ũ�ϣ���ΤߤȤ���
               IF     (WRK-GMN-INPUTCD (IDX)(1:1)   =   "."  )  AND
                      (WRK-COM-SRYSYUKBN (IDX)      =   "330")
                   IF      FLG-TENTEKI         =   1
                       MOVE    "0057"              TO  SPA-ERRCD
                       MOVE    "1"                 TO  WRK-COM-CUR(IDX)
                   END-IF
                   MOVE    1               TO  FLG-TENTEKI
               END-IF
           END-PERFORM
      *----(01.00.01) LINE ADD START ----------------------------------
      *
           .
       200-ALL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ư�������
      *****************************************************************
       300-AUTO-SYORI-SEC                SECTION.
      *
      *
           .
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *