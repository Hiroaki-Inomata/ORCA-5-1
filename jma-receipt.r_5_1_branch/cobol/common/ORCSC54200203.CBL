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
       PROGRAM-ID.                 ORCSC54200203.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ���Ź԰٥����å������������
      *                      �ȣ������������ޤ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
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
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARIOK           PIC 9(01).
           03  FLG-MADOKU          PIC 9(01).
           03  FLG-SEIBETU         PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-MIJYU           PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-ZS              PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-SANSO           PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-MAX             PIC 9(04).
      *
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-MOJI            PIC X(01). 
           03  WRK-SRYCD           PIC X(09). 
           03  WRK-TIMEFLG         PIC 9(01). 
      *
           03  WRK-TENSUKEI        PIC 9(08)V99.
           03  WRK-TENSUKEI-9      PIC 9(08).
           03  WRK-TENSUKEI-G      PIC 9(08).
      *
           03  WRK-SANSO           PIC 9(08).
           03  WRK-SANSOKIN        PIC 9(08)V99.
           03  WRK-SANKEI          PIC 9(08).
           03  WRK-SANTEN          PIC 9(08).
      *
           03  WRK-SYOKEIS         PIC 9(08)V9999.
           03  WRK-SYOKEIS2        PIC 9(08)V99.
           03  WRK-SYOKEI          PIC 9(08).
      *
           03  WRK-ERRCD           PIC X(04).
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
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ���Ź԰ٻ����Խ��ѥ�᥿
           COPY    "CPORCSC90.INC".
      *
      *    ���Ź԰ٻ���������å��ѥ�᥿
           COPY    "CPORCSC91.INC".
      *
      *    ���Ź԰ٻ���������å��ѥ�᥿
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
           MOVE    ZERO                TO  IDX-ZS
           MOVE    ZERO                TO  IDX-SANSO
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (SPA-ERRCD       NOT =   SPACE )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *
               ADD     1                   TO  IDX-MAX
      *
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
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
                       CONTINUE
      *
                   WHEN    OTHER
      *                �Խ������å���
                       PERFORM 10011-CHK-SEC
               END-EVALUATE
           END-PERFORM
      *
           IF      IDX-ZS              >   ZERO
      *        ���Ļ������ֳ��μ�ư�û����ɲä���
               MOVE    SPA-ERRCD          TO  WRK-ERRCD
               MOVE    SPACE              TO  SPA-ERRCD
               PERFORM 1002-JIDO-KASAN-SEC
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
               WHEN    "1"
      *            �굻�� �û������å�
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "2"
                       PERFORM 1005-KASAN-CHK-SEC
                   END-IF
      *            ̤�ϻ��û������å�
                   IF      WRK-COM-INPUTCD (IDX-Z) =   "150286690"
                       PERFORM 10013-MIJYUKUJI-SEC
                   END-IF
      *            �굻��
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "1"
                       MOVE    IDX-Z           TO  IDX-ZS
                   END-IF
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
      *        ���
               WHEN    "7"
                   IF      WRK-COM-DATAKBN (IDX-Z) =   "3"
      *                �ե����λ����顼
                       MOVE    "0047"          TO  SPA-ERRCD
                       MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
                   END-IF
      *
      *            ���Ƕ�ʬȽ��
                   IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
      *                ���Ǥλ�����������Ψ��ư�û�
                       IF      WRK-COM-INPUTCD (IDX-Z) =   "770000000"
                           MOVE    IDX-Z       TO  IDX-SANSO
                           PERFORM 10011-SANSO-ADD-SEC
                       ELSE
                           IF      IDX-SANSO        =   ZERO
                               MOVE    "0038"       TO  SPA-ERRCD
                               MOVE    "1"          TO  WRK-COM-CUR
                                                              (IDX-Z)
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       10011-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������Ψ��ư�û�����
      *****************************************************************
       10011-SANSO-ADD-SEC                  SECTION.
      *
      *    ��������Ψ��ư�û�
           MOVE    IDX-Z               TO  IDX-S
           MOVE    "770020070"         TO  WRK-SRYCD
           IF      WRK-COM-INPUTCD (IDX-Z + 1) =   WRK-SRYCD
               MOVE    1               TO  FLG-ARI
               MOVE    IDX-S           TO  IDX-T
               ADD     1               TO  IDX-T
           ELSE
               MOVE    ZERO            TO  FLG-ARI
           END-IF
           PERFORM 1002-KASAN-SET-SEC
      *
           .
       10011-SANSO-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ̤�ϻ��û������å�����
      *****************************************************************
       10013-MIJYUKUJI-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    ǯ��Ƚ��ʣ����������
           IF     (LNK-ORCSC-NENREI-YY =   ZERO )  AND
                  (LNK-ORCSC-NENREI-MM <=  3    )
      *
      *        ǯ����ֻ���
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY5-AREA
               MOVE    "51"                TO  LNK-DAY5-IRAI
               MOVE    SPA-BIRTHDAY        TO  LNK-DAY5-START
               MOVE    SPA-SRYYMD          TO  LNK-DAY5-END
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY5-AREA
               IF      LNK-DAY5-NISSU2     <   90
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-IF
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0041"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
               MOVE    ZERO            TO  WRK-COM-SANFLG(IDX-Z)
           ELSE
               MOVE    1               TO  WRK-COM-SANFLG(IDX-Z)
               MOVE    1               TO  FLG-MIJYU
           END-IF
      *
           .
       10013-MIJYUKUJI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ��ư�û��ɲý���
      *****************************************************************
       1002-JIDO-KASAN-SEC        SECTION.
      *
           MOVE    IDX-ZS              TO  IDX-S
      *
      *    ���Ļ��û�
           IF     (LNK-ORCSC-NENREI-YY <   6  )  AND
                  (FLG-MIJYU           =   ZERO)
               PERFORM 10011-YOUJIKASAN-SET-SEC
           ELSE
               PERFORM 10013-YOUJIKASAN-CHK-SEC
           END-IF
      *
      *    ���ֲû�
           IF      (LNK-ORCSC-TIMEFLG          NOT =   ZERO   OR
                    WRK-COM-TIMEFLG (IDX-ZS)   NOT =   ZERO )  AND
                   (WRK-COM-TIMEKSNKBN(IDX-ZS) NOT =   ZERO )
               PERFORM 10012-JIKANKASAN-SET-SEC
           END-IF
      *
      *    ǯ��û�����
      *    PERFORM 10013-NENREI-SET-SEC
           .
       1002-JIDO-KASAN-EXT.
           EXIT.
      *****************************************************************
      *    ���Ļ��û����ɲý���
      *****************************************************************
       10011-YOUJIKASAN-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *    �������û�
           MOVE    "150231590"         TO  WRK-SRYCD
      *    �û����ɲý���
           PERFORM 100111-KASAN-CHK-SEC
      *    �û����ɲý���
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    1               TO  WRK-COM-SANFLG(IDX-S)
               GO      TO      10011-YOUJIKASAN-SET-EXT
           END-IF
      *
      *    ���Ļ��βû�
           MOVE    "150231690"         TO  WRK-SRYCD
      *    �û����ɲý���
           PERFORM 100111-KASAN-CHK-SEC
      *    �û����ɲý���
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    1               TO  WRK-COM-SANFLG(IDX-S)
               GO      TO      10011-YOUJIKASAN-SET-EXT
           END-IF
      *
      *    �Ļ��û�
           MOVE    "150265390"         TO  WRK-SRYCD
      *    �û����ɲý���
           PERFORM 100111-KASAN-CHK-SEC
      *    �û����ɲý���
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    1               TO  WRK-COM-SANFLG(IDX-S)
               GO      TO      10011-YOUJIKASAN-SET-EXT
           END-IF
      *
           .
       10011-YOUJIKASAN-SET-EXT.
           EXIT.
      *****************************************************************
      *    ���Ļ��û��������å�����
      *****************************************************************
       10013-YOUJIKASAN-CHK-SEC        SECTION.
      *
      *    ̤�ϻ��û����ϻ������Ļ��û����ʤ�����
           IF      FLG-MIJYU           =   1
      *
      *        �������û�
               MOVE    "150231590"         TO  WRK-SRYCD
               PERFORM 100111-KASAN-CHK-SEC
               IF      FLG-ARI             =   1
                   MOVE    "0040"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-T)
                   GO      TO      10013-YOUJIKASAN-CHK-EXT
               END-IF
      *
      *        ���Ļ��βû�
               MOVE    "150231690"         TO  WRK-SRYCD
               PERFORM 100111-KASAN-CHK-SEC
               IF      FLG-ARI             =   1
                   MOVE    "0040"          TO  SPA-ERRCD
                   MOVE    "1"             TO  WRK-COM-CUR(IDX-T)
                   GO      TO      10013-YOUJIKASAN-CHK-EXT
               END-IF
           END-IF
      *
           .
       10013-YOUJIKASAN-CHK-EXT.
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
      *****************************************************************
      *    ���ֲû��û����ɲý���
      *****************************************************************
       10012-JIKANKASAN-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *
      *
           MOVE    LNK-ORCSC-TIMEFLG   TO  WRK-TIMEFLG
           IF      WRK-TIMEFLG         =   ZERO
               MOVE    WRK-COM-TIMEFLG(IDX-ZS)     TO  WRK-TIMEFLG
           END-IF
      *
      *    ���ֲû�
           EVALUATE    WRK-TIMEFLG
      *      /* ���ֳ� */
               WHEN    1
                   MOVE    "150231790"     TO  WRK-SRYCD
      *     /* ���� */
               WHEN    2
                   MOVE    "150231890"     TO  WRK-SRYCD
      *        /* ���� */
               WHEN    3
                   MOVE    "150231990"     TO  WRK-SRYCD
           END-EVALUATE
      *
           IF      WRK-SRYCD            =    SPACE
               GO      TO      10012-JIKANKASAN-SET-EXT
           END-IF
      *
      *    ���ּ�ư�û���¸�ߥ����å�
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  IDX-T
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1        >   SPA-MAX-LINE     ) OR
                              (FLG-ARI                 =   1    ) OR
                              (WRK-GMN-INPUTCD (IDX1)  =   SPACE )
               IF     (WRK-COM-DATAKBN    (IDX1)       =   "2"  )  AND
                      (WRK-COM-TIMEKSNKBN (IDX1)   NOT =   ZERO )
                    MOVE    1              TO  FLG-ARI
                    MOVE    IDX1           TO  IDX-T
               END-IF
           END-PERFORM
           IF      IDX-T               =   ZERO
               CONTINUE
           ELSE
               MOVE    IDX-T               TO  IDX-S
           END-IF
      *    �û����ɲý���
           PERFORM 1002-KASAN-SET-SEC
           IF      FLG-ADD             =   1
               MOVE    1               TO  WRK-COM-SANFLG(IDX-S)
           END-IF
      *
           .
       10012-JIKANKASAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ǯ��û��ɲý����ʶ��̡�
      *****************************************************************
       10013-NENREI-SET-SEC            SECTION.
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   4       )   OR
                              (WRK-COM-AGEKSNSRYCD(IDX-ZS IDX2)
                                               =   SPACE  OR
                                                   ZERO )
      *        �û��ɲ�
               MOVE    WRK-COM-AGEKSNSRYCD (IDX-ZS IDX2)  TO  WRK-SRYCD
      *        �û��������å�����
               PERFORM 100111-KASAN-CHK-SEC
      *        �û����ɲý���
               PERFORM 1002-KASAN-SET-SEC
               IF      FLG-ADD            =   1
                   MOVE    4                   TO  IDX2
               END-IF
           END-PERFORM
      *
           .
       10013-NENREI-SET-EXT.
           EXIT.
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
           MOVE    IDX-S               TO  LNK-SC92-GMN-IDX
           MOVE    LNK-ORCSC-PG        TO  LNK-SC92-PG
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
      *    �û��������å�����
      *****************************************************************
       1005-KASAN-CHK-SEC                  SECTION.
      *
      *    �û��ǡ����λ������˼굻�������뤳��
           MOVE    ZERO                TO  FLG-ARI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >=  IDX-Z
               IF     (WRK-COM-INPUTCD(IDX1)(1:1)  =   "1" )  AND
                      (WRK-COM-DATAKBN(IDX1)   NOT =   "2" )
                   IF      WRK-COM-CHUKSNCD (IDX1)     =
                                               WRK-COM-CHUKSNCD(IDX-Z)
                       MOVE    1               TO  FLG-ARIOK
                   END-IF
                   MOVE    1               TO  FLG-ARI
                   MOVE    IDX-Z           TO  IDX1
               END-IF
           END-PERFORM
      *
           IF      FLG-ARI             =   ZERO
               MOVE    "0013"          TO  SPA-ERRCD
               MOVE    "1"             TO  WRK-COM-CUR(IDX-Z)
           END-IF
      *
           .
       1005-KASAN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����׻��Խ�����
      *****************************************************************
       1002-TENSU-KEI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  IDX-ZS
           MOVE    ZERO                TO  IDX-SANSO
           MOVE    ZERO                TO  WRK-SANSO
           MOVE    ZERO                TO  WRK-SANKEI
           MOVE    ZERO                TO  WRK-SANTEN
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  ORCSC90AREA
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE   )   OR
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
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1)     =   "."
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
      *    �����Ʒ׻��Խ�
           PERFORM 1007-KIZAI-TEN-SEC
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
               MOVE    IDX-Z               TO  IDX-ZS
           END-IF
      *
      *    ���Ƿ׻�
           IF      WRK-COM-SANSOKBN (IDX-Z)    =   1
               PERFORM 100211-SANSO-KEI-SEC
           END-IF
      *
           .
       10021-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ƿ׻�����
      *****************************************************************
       100211-SANSO-KEI-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SANSO
           EVALUATE    WRK-COM-INPUTCD (IDX-Z)
      *        ���Ƕ��
               WHEN    "770000000"
                   COMPUTE WRK-SANSOKIN    =   WRK-COM-KISOTEN (IDX-Z)
                                           *   WRK-COM-SURYO (IDX-Z)
      *        ��������Ψ��ư�û�
               WHEN    "770020070"
      *            ���Ƕ�ۡܡʻ��Ƕ�ۡ�����Ψ���������ˡ��ͼθ���
                   COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
                                           *   WRK-COM-KISOTEN (IDX-Z)
                                           /   100
                   COMPUTE WRK-SANSOKIN        =   WRK-SANSOKIN
                                               +   WRK-SYOKEIS
      *
      *        �ⵤ�����Ƿ׻�
               WHEN    "770030070"
      *            ���Ƕ�ۡܡʻ��Ƕ�ۡ�����Ψ����������
      *                                       �ܵ��������ͼθ���
                   COMPUTE WRK-SYOKEIS     =   WRK-SANSOKIN
                                           *   WRK-COM-SURYO (IDX-Z)
                   MOVE    WRK-SYOKEIS     TO  WRK-SANSOKIN
           END-EVALUATE
      *
           .
       100211-SANSO-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���û��׻�����
      *****************************************************************
       1006-KASAN-SET-SEC              SECTION.
           MOVE    ZERO                TO  WRK-TENSUKEI-G
      *
      *    ��ư�û��ʳ��βû��ʻ��ֳ���ǯ��ʳ���
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-MAX-LINE )   OR
                              (WRK-GMN-INPUTCD (IDX)   =   SPACE )
      *        �굻������������������Ȥ���
               IF     (WRK-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (WRK-COM-TENSIKIBETU(IDX)    =   5   )  AND
                      (WRK-COM-SANFLG     (IDX)    =   ZERO )
                   MOVE    ZERO                TO  WRK-TENSUKEI
                   COMPUTE WRK-TENSUKEI        =   WRK-COM-KISOTEN
                                                               (IDX-ZS)
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
                   UNTIL      (IDX         >   SPA-MAX-LINE  )   OR
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
      *    �����Ʒ׻��Խ�����
      *****************************************************************
       1007-KIZAI-TEN-SEC                SECTION.
      *
      *    ���Ǥ��������׻�
           IF      WRK-SANSOKIN    NOT =   ZERO
      *        ��۷ס��ͼθ���
               COMPUTE WRK-SYOKEI          =   WRK-SANSOKIN
                                           +   0.5
      *        ��۷סࣱ�����ͼθ���
               COMPUTE WRK-SYOKEIS2        =   WRK-SYOKEI
                                           /   10
                                           +   0.5
               MOVE    WRK-SYOKEIS2        TO  WRK-SANTEN
      *        ���Ǥ��������������˲û�
               ADD     WRK-SANTEN          TO  LNK-SC90-KIZTEN
      *
           END-IF
           .
       1007-KIZAI-TEN-EXT.
           EXIT.
      *
      *
      *
      *****************************************************************
      *    �����Ź԰٥����å�����
      *****************************************************************
       200-ALL-SYORI-SEC                SECTION.
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
       300-AUTO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������������å�����
      *****************************************************************
       3001-SANTEI-CHK-SEC         SECTION.
      *
      *    INITIALIZE                      ORCSC91AREA
      *    MOVE    "2"                 TO  LNK-SC91-KBN
      *    MOVE    LNK-ORCSC-SYORIFLG  TO  LNK-SC91-SYORIFLG
      *    MOVE    WRK-COM-INPUTCD(IDX)
      *                                TO  LNK-SC91-SRYCD
      *    CALL    "ORCSC91"       USING
      *                                MCPAREA
      *                                ORCSC91AREA
      *                                WRK-SCR-REC
      *                                SPA-AREA
      *
           IF      LNK-SC91-ERRCD      =   SPACE
      *        �ɲä���
               MOVE    ZERO            TO  FLG-ERR
           ELSE
      *        �ɲäʤ�
               MOVE    1               TO  FLG-ERR
           END-IF
      *
           .
       3001-SANTEI-CHK-EXT.
           EXIT.
      *
      *