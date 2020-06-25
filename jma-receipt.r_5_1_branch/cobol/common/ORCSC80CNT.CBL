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
       PROGRAM-ID.                 ORCSC80CNT.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ��ϥӥ껻������������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/10/08    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  04/03/23  �ȣ���ǯ��������ɲ�
      * 01.00.02     NACL-¿��Ǽ  05/11/22  MONFUNC �б� ¾
      * 03.05.00     NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
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
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
      *
           03  FLG-SANTEI-ARI      PIC 9(01).
           03  FLG-RIGAKU          PIC 9(01).
      *
           03  FLG-SOUKI-ALL       PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX2                PIC 9(02).
      *
           03  IDD                 PIC 9(02).
           03  IDD2                PIC 9(02).
      *
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-RIGAKU-KBN      PIC X(01).
           03  WRK-RIGAKU-RYO      PIC X(01).
           03  WRK-RIGAKU-RYO-A    PIC X(01).
           03  WRK-RIGAK-CNT       PIC 9(03).
      *    �������
           03  WRK-TEI-CNT                 PIC 9(03).
           03  WRK-MCNT                    PIC 9(03).
      *
      *    �ݸ��̻�������ơ��֥�
           COPY   "CMSANTEITBL.INC".
      *
      *    ��ϥӥ�������ϲû��ơ��֥�
           COPY   "CMRIHAKSNTBL.INC".
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
      *    ���Ź԰٥ޥ�����
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ���Ų�ץޥ�����
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
      *    COPY    "CPORCSDAY.INC".
      *    COPY    "CPORCSLNK.INC".
      *
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    ��ϥӥ껻�ꥫ����ȥѥ�᥿
           COPY    "CPORCSC80CNT.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���̥����ΰ�
      *??  COPY    "K02SCR-SPA".
      *
      *    ���Ź԰ٻ����Ѷ��̥ѥ�᥿
           COPY    "CPORCSC00.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
      *??  SPA-K02
           ORCSC00AREA
           ORCSC80CNTAREA
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
      *    �����ޤǻ���ѤߤΤ��٤ƤΥ�ϥӥꥫ�����
           PERFORM 100-RIGAKU-ALLCNT-SEC
      *
      *    �����ϥӥ�û����Υ������
           PERFORM 200-SOUKI-ALLCNT-SEC
      *
           .
       000-PROC-EXT
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    �����ޤǻ���ѤߤΤ��٤ƤΥ�ϥӥꥫ����Ƚ���
      *****************************************************************
       100-RIGAKU-ALLCNT-SEC           SECTION.
      *
           INITIALIZE                  ORCSC80CNT-RIGAKU-AREA
      *
      *    ��������ѤߤΤ��٤Ƥ�������ˡ�򥫥���Ȥ���
      *    �������򸡺�
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    SPA-SRYYMD (1:6)    TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
           PERFORM UNTIL       FLG-SANTEI      =   1
      *
               MOVE    SANTEI-SRYCD        TO  WRK-SRYCD
               PERFORM 20021-RIGAKU-TBL-SEC
      *        ��ϥӥ�Ǥ��뤳��
               MOVE    ZERO                TO  FLG-SANTEI-ARI
               IF      WRK-RIGAKU-RYO      =   "K"  OR  "S"
      *            ϫ�Ҥλ����ݸ���
                   IF      SPA-HKNKBN      NOT =   ZERO
                      IF      SPA-HKNCOMBINUM  =   SANTEI-HKNCOMBINUM
                           MOVE    1               TO  FLG-SANTEI-ARI
                       END-IF
                   ELSE
                      IF       SANTEI-HKNCOMBINUM  =   SPACE OR  ZERO
                           MOVE    1               TO  FLG-SANTEI-ARI
                       END-IF
                   END-IF
               END-IF
      *
               IF      FLG-SANTEI-ARI      =   1
      *            �������
                   PERFORM 20011-SANTEI-CNT-SEC
               END-IF
      *
               MOVE    "tbl_santei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100-RIGAKU-ALLCNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ϥӥ�ơ��֥�Ƚ�����
      *****************************************************************
       20021-RIGAKU-TBL-SEC         SECTION.
      *
           SET     TBL-SAN             TO  1
           SEARCH      TBL-SANTEI-OCC  VARYING   TBL-SAN
                   AT  END
                       MOVE    ZERO        TO  FLG-RIGAKU
                       MOVE    SPACE       TO  WRK-RIGAKU-KBN
                       MOVE    SPACE       TO  WRK-RIGAKU-RYO
               WHEN    WRK-SRYCD           =   TBL-SANTEI-SRYCD
                                                           (TBL-SAN)
                   MOVE    1                   TO  FLG-RIGAKU
                   MOVE    TBL-RIGAKU-KBN   (TBL-SAN)
                                               TO  WRK-RIGAKU-KBN
                   MOVE    TBL-RIGAKU-RYO   (TBL-SAN)
                                               TO  WRK-RIGAKU-RYO
           END-SEARCH
      *
           .
       20021-RIGAKU-TBL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �������򸡺�����
      *****************************************************************
       20011-SANTEI-CNT-SEC              SECTION.
      *
      *    �����λ�����������̵ͭ��Ƚ��
           MOVE    ZERO                TO  WRK-TEI-CNT
           IF      ORCSC80CNT-SYORIFLG     =   1
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ     >   SPA-MAX-LINE)  OR
                                  (LNK-ORCSC-TEI-SRYCD(IDZ)
                                           =   SPACE )
                   IF      WRK-SRYCD           =   LNK-ORCSC-TEI-SRYCD
                                                                 (IDZ)
                       ADD     LNK-ORCSC-TEI-KAISU(IDZ)
                                               TO  WRK-TEI-CNT
                   END-IF
               END-PERFORM
           END-IF
      *    �����ޤǤΥ������
           MOVE    SPA-SRYYMD(7:2)     TO  IDD
      *
      *    ����ʬ�Υ��ꥢ
           IF      ORCSC80CNT-SYORIFLG     =   1
               IF      SANTEI-MSANTEICNT   <   WRK-TEI-CNT
                   MOVE    ZERO            TO  SANTEI-MSANTEICNT
               ELSE
                   COMPUTE SANTEI-MSANTEICNT   =   SANTEI-MSANTEICNT
                                               -   WRK-TEI-CNT
               END-IF
               IF      SANTEI-DAY (IDD)    <   WRK-TEI-CNT
                   MOVE    ZERO                TO  SANTEI-DAY (IDD)
               ELSE
                   COMPUTE SANTEI-DAY (IDD)    =   SANTEI-DAY (IDD)
                                               -   WRK-TEI-CNT
               END-IF
           END-IF
      *
      *    �����λ�������
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    31                  TO  IDD
           END-IF
      *
      *    �����ޤǤβ��
           MOVE    ZERO                TO  WRK-MCNT
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   IDD
               ADD     SANTEI-DAY (IDX2)   TO  WRK-MCNT
           END-PERFORM
      *
      *    ��ϥӥ�μ�����˽��פ���
      *    ����������ʬ
           EVALUATE    WRK-RIGAKU-RYO
                                       ALSO  WRK-RIGAKU-KBN
      *        ���̤�����
               WHEN    "K"             ALSO  "R"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-K
                                                                   (01)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-KN
                                                                   (01)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-KN
                                                              (01 IDX2)
                   END-PERFORM
      *        ���Ĥ�����
               WHEN    "S"             ALSO  "R"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-S
                                                                   (01)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-SN
                                                                   (01)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-SN
                                                              (01 IDX2)
                   END-PERFORM
      *        ���̤κ��
               WHEN    "K"             ALSO  "S"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-K
                                                                   (02)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-KN
                                                                   (02)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-KN
                                                              (02 IDX2)
                   END-PERFORM
      *        ���Ĥκ��
               WHEN    "S"             ALSO  "S"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-S
                                                                   (02)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-SN
                                                                   (02)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-SN
                                                              (02 IDX2)
                   END-PERFORM
      *        ���̤θ���
               WHEN    "K"             ALSO  "G"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-K
                                                                   (03)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-KN
                                                                   (03)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-KN
                                                              (03 IDX2)
                   END-PERFORM
      *        ���Ĥθ���
               WHEN    "S"             ALSO  "G"
                   ADD     WRK-MCNT            TO  ORCSC80CNT-TCNT-S
                                                                   (03)
                   ADD     SANTEI-MSANTEICNT   TO  ORCSC80CNT-TCNT-SN
                                                                   (03)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   31
                       ADD     SANTEI-DAY (IDX2)   TO
                                                   ORCSC80CNT-DCNT-SN
                                                              (03 IDX2)
                   END-PERFORM
           END-EVALUATE
      *
           .
       20011-SANTEI-CNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ϥӥ�û����Υ�����Ƚ���
      *****************************************************************
       200-SOUKI-ALLCNT-SEC         SECTION.
      *
      *    ����ǯ��ο��Ų�ץޥ����ɤ߹���
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    "80"                TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT         =   1       )
      *
               IF     (ACCT-ZAITEN         =   ZERO )  OR
                      (ACCT-ZAIKAISU       =   ZERO )
      *          ���ޤȤ����Ϥϥ�ϥӥ��оݳ�
                 OR   (ACCT-HKNCOMBI       =   "9999")
                   CONTINUE
               ELSE
      *            ���Ź԰٥ޥ������Խ�
                   PERFORM 2001-SRYACT-HEN-SEC
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200-SOUKI-ALLCNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ź԰٥ޥ������Խ�����
      *****************************************************************
       2001-SRYACT-HEN-SEC            SECTION.
      *
      *    ���Ų�ץޥ�����������Ź԰٥ޥ������򸡺�����
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 950-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           MOVE    ZERO                TO  FLG-SOUKI-ALL
           MOVE    SPACE               TO  WRK-RIGAKU-RYO-A
      *
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
      *            ��ϥӥ�Ǥ��뤳��
                   MOVE    SRY-SRYCD(IDX2)     TO  WRK-SRYCD
                   PERFORM 20021-RIGAKU-TBL-SEC
                   IF      WRK-RIGAKU-RYO  NOT =   SPACE
      **************   IF      WRK-RIGAKU-RYO-A    NOT =   SPACE
      **                  PERFORM 2004-SOUKI-HENSYU-SEC
      **************   END-IF
                       MOVE    WRK-RIGAKU-RYO      TO  WRK-RIGAKU-RYO-A
                   END-IF
      *            �û�Ƚ��
                   MOVE    SRY-SRYCD(IDX2)     TO  WRK-SRYCD
                   PERFORM 20023-SOUKI-TBL-SEC
                   IF      FLG-SOUKI           =   1
                       MOVE    1                   TO  FLG-SOUKI-ALL
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 950-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ����ʤ�
           IF     (WRK-RIGAKU-RYO-A    =   SPACE  )  OR
                  (FLG-SOUKI-ALL       =   ZERO   )
               GO      TO      2001-SRYACT-HEN-EXT
           END-IF
      *
      *    �����ϥӥ�û����û�����
           PERFORM 2004-SOUKI-HENSYU-SEC
      *
           .
       2001-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �����ϥӥ�û����û�����
      *****************************************************************
       2004-SOUKI-HENSYU-SEC         SECTION.
      *
      *    �����ޤǤΥ�����ȤȤ���
           MOVE    SPA-SRYYMD(7:2)     TO  IDD
      *
      *    �����ξ��
           IF      ORCSC80CNT-SYORIFLG     =   1
               IF      ACCT-DAY (1 IDD)    >=  WRK-TEI-CNT
                   COMPUTE ACCT-DAY (1 IDD)    =   ACCT-DAY (1 IDD)
                                               -   WRK-TEI-CNT
               ELSE
                   MOVE    ZERO                TO  ACCT-DAY (1 IDD)
               END-IF
           END-IF
      *
      *    �����λ�������
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    31                  TO  IDD
           END-IF
      *
      *    �������
           PERFORM VARYING     IDD2    FROM    1   BY  1
                   UNTIL       IDD2    >   IDD
               ADD     ACCT-DAY (1 IDD2)  TO  ORCSC80CNT-GCNT  (IDD2)
           END-PERFORM
      *
      *
           .
       2004-SOUKI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    �����ϥӥ�û����ơ��֥븡������
      *****************************************************************
       20023-SOUKI-TBL-SEC         SECTION.
      *
      *�ȣ�����������
           IF      SPA-SRYYMD          <   "20040401"
      *        �����ϥӥ�û�
               SET     TBL-RIDX            TO  1
               SEARCH      TBL-RIHABIRI-OCC    VARYING   TBL-RIDX
                       AT  END
                           MOVE    ZERO            TO  FLG-SOUKI
                       WHEN    WRK-SRYCD           =   TBL-RIHA-SRYCD
                                                           (TBL-RIDX)
                           MOVE    1               TO  FLG-SOUKI
               END-SEARCH
           ELSE
      *�ȣ�����������
      *        �����ϥӥ�û�
               SET     TBL-RIDX2           TO  1
               SEARCH      TBL-RIHABIRI2-OCC    VARYING   TBL-RIDX2
                       AT  END
                           MOVE    ZERO            TO  FLG-SOUKI
                       WHEN    WRK-SRYCD           =   TBL-RIHA2-SRYCD
                                                           (TBL-RIDX2)
                           MOVE    1               TO  FLG-SOUKI
               END-SEARCH
           END-IF
      *
           IF      FLG-SOUKI           =   1
      *        �����λ�����������̵ͭ��Ƚ��
               MOVE    ZERO                TO  WRK-TEI-CNT
               IF      ORCSC80CNT-SYORIFLG     =   1
                   PERFORM VARYING     IDZ     FROM    1   BY  1
                           UNTIL      (IDZ     >   SPA-MAX-LINE )  OR
                                      (LNK-ORCSC-TEI-SRYCD(IDZ)
                                               =   SPACE )
                       IF      WRK-SRYCD       =   LNK-ORCSC-TEI-SRYCD
                                                                 (IDZ)
                           ADD     LNK-ORCSC-TEI-KAISU(IDZ)
                                                   TO  WRK-TEI-CNT
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           .
       20023-SOUKI-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ޥ����ɤ߹���
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ų�ץޥ����ɤ߹���
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ź԰٥ޥ����ɤ߹���
      *****************************************************************
       950-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       950-SRYACT-READ-EXT.
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
           .
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
           .
       990-DBCLOSE-EXT.
           EXIT.
      *