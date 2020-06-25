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
       PROGRAM-ID.             ORAPI012SUB2V3.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : �ݸ��ȹ礻�����ֵ��Խ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  17/07/XX    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-PARA                PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
      *
      *    ��������ΰ�
      *01  CNT-AREA.
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDH                 PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDD1                PIC 9(04).
           03  IDD2                PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    �����Խ�
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *
           03  WRK-SYSYMD              PIC X(08).
      *
           03  WRK-ENDYMD.
               05  WRK-ENDYY           PIC 9(04).
               05  WRK-ENDMM           PIC 9(02).
               05  WRK-ENDDD           PIC 9(02).
      *
           03  WRK-HKNCOMBINUM         PIC X(04).
      *
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-HOJOKBN             PIC X(01).
      *    ��ôΨ�����Խ�
           03  WRK-FTNRATE             PIC 9(02)V99.
           03  WRK-Z9                  PIC 9.ZZ.
           03  WRK-FTNRATEMEI          PIC X(04).
           03  WRK-ZZ9                 PIC ZZZZZ9.
      *
           03  WRK-INDATA              PIC X(20).
           03  WRK-OTDATA              PIC X(20).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
      *    �ݸ��ȹ礻����¸
       01  TBL-HKNCOMBI-AREA.
           02  TBL-HKNCOMBI-REC        OCCURS    200.
           COPY    "CPHKNCOMBI.INC"
                              REPLACING  //COMB-// BY //TBL-COMB-//.
      *
       01  TBL-HKNCOMB-MAX             PIC 9(04).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *
      *    �ݸ��Ȥ߹�碌
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *    �ݸ��ֹ�
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    �ѥ�᥿�ơ��֥�
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *
      *    �����ʬ̾���Խ��ѥ�᥿
           COPY    "CPORAPI012SUB1.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �ѥ�᥿
           COPY    "CPORAPI012SUB2V3.INC".
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ���Բ��̥����ΰ�
           COPY    "P02SCR-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORAPI012SUB2V3AREA
           SPA-AREA
           SPA-P02SCRAREA
           .
      *
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  AP12SUB2V3-COMBINATION-INF-G
           INITIALIZE                  AP12SUB2V3-ERRCD
           MOVE    ZERO                TO  AP12SUB2V3-MAX
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
      *
      *    ���������
           PERFORM 100-INIT-SEC
      *    �硡����
           IF      AP12SUB2V3-ERRCD    =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC               SECTION.
      *
           IF      AP12SUB2V3-SYSYMD   =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-SYSYMD
           ELSE
               MOVE    AP12SUB2V3-SYSYMD   TO  WRK-SYSYMD
           END-IF
      *
           IF      AP12SUB2V3-SYORIKBN =   "2"
      *        ������
               MOVE    2               TO  FLG-SYORI
           ELSE
      *        ��Ͽ����
               MOVE    1               TO  FLG-SYORI
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC               SECTION.
      *
      *    ������
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      PARA-EDANUM         >   200
      *        ��ǯ���ޤǤ��о�
               MOVE    WRK-SYSYMD          TO  WRK-ENDYMD
               COMPUTE WRK-ENDYY           =   WRK-ENDYY
                                           -   3
               MOVE    01                  TO  WRK-ENDDD
      *        �ٹ�
               MOVE    "73"                TO  AP12SUB2V3-ERRCD
           ELSE
               MOVE    SPACE               TO  WRK-ENDYMD
           END-IF
           PERFORM 2001-HKNCOMBI-SET-SEC
      *
           IF      IDY                 >   200
      *        ��ǯ���ޤǤ��о�
               MOVE    WRK-SYSYMD          TO  WRK-ENDYMD
               COMPUTE WRK-ENDYY           =   WRK-ENDYY
                                           -   2
               MOVE    01                  TO  WRK-ENDDD
      *        �ٹ�
               MOVE    "74"                TO  AP12SUB2V3-ERRCD
      *
               PERFORM 2001-HKNCOMBI-SET-SEC
           END-IF
      *
           IF      IDY                 >   200
      *        ��ǯ���ޤǤ��о�
               MOVE    WRK-SYSYMD          TO  WRK-ENDYMD
               COMPUTE WRK-ENDYY           =   WRK-ENDYY
                                           -   1
               MOVE    01                  TO  WRK-ENDDD
      *        �ٹ�
               MOVE    "75"                TO  AP12SUB2V3-ERRCD
      *
               PERFORM 2001-HKNCOMBI-SET-SEC
           END-IF
      *
           IF      TBL-HKNCOMB-MAX     >   ZERO
               PERFORM 2002-HKNCOMBI-HENSYU-SEC
           END-IF
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ȹ礻�����ΰ��ݸ������Խ�����
      *****************************************************************
       2001-HKNCOMBI-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDY
           INITIALIZE                      TBL-HKNCOMBI-AREA
           MOVE    ZERO                TO  TBL-HKNCOMB-MAX
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM UNTIL     FLG-PARA      =   1
      *
               MOVE    PARA-DATA-REC       TO  HKNCOMBI-REC
      *
               IF     (WRK-ENDYMD      NOT =   SPACE )
                AND   (COMB-TEKEDYMD       <   WRK-ENDYMD)
      *            �оݳ�
                   CONTINUE
               ELSE
      *            �о�
                   ADD     1               TO  IDY
                   IF      IDY             >   200
                       MOVE    1               TO  FLG-PARA
                   ELSE
                       MOVE    HKNCOMBI-REC    TO  TBL-HKNCOMBI-REC
                                                             (IDY)
                       MOVE    IDY             TO  TBL-HKNCOMB-MAX
                   END-IF
               END-IF
               IF      FLG-PARA            =   ZERO
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2001-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ȹ礻�����ΰ��ݸ������Խ�����
      *****************************************************************
       2002-HKNCOMBI-HENSYU-SEC         SECTION.
      *
           INITIALIZE                      AP12SUB2V3-COMBINATION-INF-G
           MOVE    ZERO                TO  AP12SUB2V3-MAX
           MOVE    ZERO                TO  WRK-HKNCOMBINUM
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   TBL-HKNCOMB-MAX
               MOVE    ZERO                TO  WRK-HKNCOMBINUM
               MOVE    ZERO                TO  IDH2
      *        �ݸ��ȹ礻�ֹ�ι߽�
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL       IDH     >   TBL-HKNCOMB-MAX
                   IF      (TBL-COMB-HKNCOMBINUM(IDH)  NOT =   ZERO)
                      AND  (TBL-COMB-HKNCOMBINUM(IDH)
                                               >   WRK-HKNCOMBINUM )
                       MOVE    IDH             TO  IDH2
                       MOVE    TBL-COMB-HKNCOMBINUM(IDH)
                                               TO  WRK-HKNCOMBINUM
                   END-IF
               END-PERFORM
               IF      IDH2            >   ZERO
                   MOVE    TBL-HKNCOMBI-REC (IDH2) TO  HKNCOMBI-REC
                   PERFORM 20011-COMBINATION-HEN-SEC
                   MOVE    ZERO            TO  TBL-COMB-HKNCOMBINUM
                                                             (IDH2)
                   MOVE    IDY                 TO  AP12SUB2V3-MAX
                END-IF
           END-PERFORM
      *
           .
       2002-HKNCOMBI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ȹ礻�����ΰ��ݸ������Խ�����
      *****************************************************************
       20011-COMBINATION-HEN-SEC     SECTION.
      *
      *    �ݸ��ȹ礻�ֹ�
           MOVE    COMB-HKNCOMBINUM
                                   TO  AP12SUB2V3-COMBINATION-NUMBER
                                                               (IDY)
      *
           IF      COMB-HKNID      =   ZERO
      *        �ݸ��ʤ�
               CONTINUE
           ELSE
      *        �ݸ��ɣ�
               MOVE    COMB-HKNID      TO  AP12SUB2V3-COMB-MAIN-ID
                                                               (IDY)
      *        �ݸ��μ���
               MOVE    COMB-HKNNUM     TO  AP12SUB2V3-COMB-MAIN-CLASS
                                                               (IDY)
      *        �ݸ���̾��
               MOVE    COMB-SYU-TANSEIDONAME
                                   TO  AP12SUB2V3-COMB-MAIN-NAME (IDY)
      *        ��³��ʬ
               MOVE    COMB-CONTKBN    TO  AP12SUB2V3-COMB-CONTINUATION
                                                             (IDY)
      *        �����ʬ
               MOVE    COMB-HOJOKBN    TO  AP12SUB2V3-COMB-ASSISTANCE
                                                             (IDY)
      *        �����ʬ̾��
               MOVE    COMB-HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-HOJOKBN    TO  WRK-HOJOKBN
               PERFORM 200111-HOJOKBN-NAME-HEN-SEC
               MOVE    API012SUB1-HOJOKBN-NAME
                                       TO  AP12SUB2V3-COMB-ASSI-NAME
                                                                  (IDY)
      *        �ܿͲ�²��ʬ
               MOVE    COMB-HONKZKKBN  TO  AP12SUB2V3-COMB-FAMILY (IDY)
           END-IF
      *
      *    ������ô���
           COMPUTE WRK-FTNRATE     =   COMB-NYU-KFTNRATE
                                   /   100
           MOVE    WRK-FTNRATE     TO  WRK-Z9
           MOVE    WRK-Z9          TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI  TO  AP12SUB2V3-COMBINATION-RATE-ADM
                                                               (IDY)
      *    ������ô���
           COMPUTE WRK-FTNRATE     =   COMB-GAI-KFTNRATE
                                   /   100
           MOVE    WRK-FTNRATE     TO  WRK-Z9
           MOVE    WRK-Z9          TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI  TO  AP12SUB2V3-COMBINATION-RATE-OUT
                                                               (IDY)
      *    ͭ��������
           MOVE    COMB-TEKSTYMD   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE    TO  AP12SUB2V3-COMB-START-DATE(IDY)
      *
           MOVE    COMB-TEKEDYMD   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE    TO  AP12SUB2V3-COMB-END-DATE (IDY)
      *
      *    ������λ�������������Խ�
           IF      FLG-SYORI           =   2
      *        �����ʬ
               IF      COMB-DELKBN     =   "1"
                   MOVE    "Delete"        TO  AP12SUB2V3-COMB-MODE
                                                                (IDY)
               ELSE
      *            ������Ƚ��
                   IF      COMB-CHGKBN     =   "A"
                       MOVE    "New"       TO  AP12SUB2V3-COMB-MODE
                                                               (IDY)
                   END-IF
               END-IF
           END-IF
      *
      *    �ݸ�
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-PTHKN-MAX )
                          OR  (COMB-HKNID  =   ZERO )
               IF      COMB-HKNID      =   SPA-PTHKN-HKNID (IDX)
      *            �����λ����ɣĥ��ꥢ
                   IF     (SPA-PTHKN-MODE  (IDX)   =   "1" )
                    AND   (FLG-SYORI               =   2   )
                       MOVE    ZERO        TO  AP12SUB2V3-COMB-MAIN-ID
                                                               (IDY)
                   END-IF
      *
      *            �ݸ����ֹ�
                   MOVE    SPA-PTHKN-HKNJANUM (IDX)
                                       TO  AP12SUB2V3-COMB-MAIN-NUMBER
                                                               (IDY)
      *            ����
                   MOVE    SPA-PTHKN-KIGO (IDX)
                                       TO  AP12SUB2V3-COMB-MARK  (IDY)
      *            �ֹ�
                   MOVE    SPA-PTHKN-NUM  (IDX)
                                       TO  AP12SUB2V3-COMB-NUMBER (IDY)
      *R02.4       ����
                   MOVE    SPA-PTHKN-EDABAN  (IDX)
                                       TO  AP12SUB2V3-COMB-EDABAN (IDY)
                   MOVE    SPA-PTHKN-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    �������
           PERFORM VARYING     IDY2    FROM    1   BY  1
                   UNTIL      (IDY2    >   4      )
                          OR  (COMB-KOHID (IDY2)   =   ZERO )
      *        ����ɣ�
               MOVE    COMB-KOHID (IDY2)
                                       TO  AP12SUB2V3-INSURANCE-ID
                                                             (IDY IDY2)
      *        ����μ���
               MOVE    COMB-KOHHKNNUM (IDY2)
                                   TO  AP12SUB2V3-INSURANCE-CLASS
                                                             (IDY IDY2)
      *        ����μ���̾��
               MOVE    COMB-KOH-TANSEIDONAME (IDY2)
                                   TO  AP12SUB2V3-INSURANCE-NAME
                                                             (IDY IDY2)
      *        �������
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   SPA-PTKOH-MAX )
                   IF      COMB-KOHID (IDY2)   =   SPA-PTKOH-KOHID(IDX2)
      *                �����λ����ɣĥ��ꥢ
                       IF     (SPA-PTKOH-MODE (IDX2)   =   "1" )
                        AND   (FLG-SYORI               =   2   )
                           MOVE    ZERO
                                       TO  AP12SUB2V3-INSURANCE-ID
                                                             (IDY IDY2)
                       END-IF
      *
      *                ��ô���ֹ�
                       MOVE    SPA-PTKOH-FTNJANUM (IDX2)
                                   TO  AP12SUB2V3-PROVIDER-NUMBER
                                                             (IDY IDY2)
      *                ������ֹ�
                       MOVE    SPA-PTKOH-JKYSNUM (IDX2)
                                   TO  AP12SUB2V3-RECIPIENT-NUMBER
                                                             (IDY IDY2)
                       MOVE    SPA-PTKOH-MAX       TO  IDX2
                   END-IF
               END-PERFORM
      *        ��ôΨ����ô���Խ�
               PERFORM 200112-KOHFTN-HENSYU-SEC
           END-PERFORM
      *
           .
       20011-COMBINATION-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �����ʬ̾���Խ�
      *****************************************************************
       200111-HOJOKBN-NAME-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORAPI012SUB1AREA
           MOVE    WRK-HKNNUM          TO  API012SUB1-HKNNUM
           MOVE    WRK-HOJOKBN         TO  API012SUB1-HOJOKBN
           MOVE    WRK-SYSYMD          TO  API012SUB1-SYSYMD
           CALL    "ORAPI012SUB1"          USING
                                           ORAPI012SUB1AREA
                                           SPA-AREA
      *
           .
       200111-HOJOKBN-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��ôΨ����ô���Խ�����
      *****************************************************************
       200112-KOHFTN-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
      *    �ݸ��ֹ�ʸ����
           MOVE    COMB-KOHHKNNUM (IDY2)   TO  HKN-HKNNUM
      *    ��ʧ��ʬ
           MOVE    COMB-KOHPAYKBN (IDY2)   TO  HKN-PAYKBN
           MOVE    COMB-TEKSTYMD       TO  HKN-TEKSTYMD
           MOVE    COMB-TEKSTYMD       TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-HKNNUM-READ-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    �ܿ͡ݳ���ݲ����ô���
           COMPUTE WRK-FTNRATE         =   HKN-HON-GAIKFTNRATE
                                       /   100
           MOVE    WRK-FTNRATE         TO  WRK-Z9
           MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI      TO  AP12SUB2V3-RATE-OUTPATIENT
                                                             (IDY IDY2)
      *    �����³�
           IF      HKN-HON-GAIKKOTGAK  =   ZERO
      *        �ܿ͡ݳ�������ݾ�³�
               MOVE    HKN-HON-GAIDJGNGAK  TO  WRK-ZZ9
           ELSE
      *        �ܿ͡ݳ���ݲ�ݾ�³�
               MOVE    HKN-HON-GAIKKOTGAK  TO  WRK-ZZ9
           END-IF
           MOVE    WRK-ZZ9             TO  WRK-INDATA
           PERFORM 800-MOJIHEN-SEC
           MOVE    WRK-OTDATA          TO  AP12SUB2V3-MONEY-OUTPATIENT
                                                             (IDY IDY2)
      *
      *    �ܿ͡������ݲ����ô���
           COMPUTE WRK-FTNRATE         =   HKN-HON-NYUKFTNRATE
                                       /   100
           MOVE    WRK-FTNRATE         TO  WRK-Z9
           MOVE    WRK-Z9              TO  WRK-FTNRATEMEI
           MOVE    WRK-FTNRATEMEI      TO  AP12SUB2V3-RATE-ADMISSION
                                                             (IDY IDY2)
      *    �����³�
           IF      HKN-HON-NYUKKOTGAK  =   ZERO
      *        �ܿ͡����������ݾ�³�
               MOVE    HKN-HON-NYUDJGNGAK  TO  WRK-ZZ9
           ELSE
      *        �ܿ͡������ݲ�ݾ�³�
               MOVE    HKN-HON-NYUKKOTGAK  TO  WRK-ZZ9
           END-IF
           MOVE    WRK-ZZ9             TO  WRK-INDATA
           PERFORM 800-MOJIHEN-SEC
           MOVE    WRK-OTDATA          TO  AP12SUB2V3-MONEY-ADMISSION
                                                             (IDY IDY2)
           .
       200112-KOHFTN-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       800-MOJIHEN-SEC                SECTION.
      *
           MOVE    SPACE              TO  WRK-OTDATA
           MOVE    ZERO               TO  IDD2
           PERFORM VARYING     IDD1   FROM    1   BY  1
                   UNTIL       IDD1   >   20
               IF      WRK-INDATA(IDD1:1)     NOT =   SPACE
                   MOVE    WRK-INDATA(IDD1:)  TO  WRK-OTDATA
                   MOVE    20                 TO  IDD1
               END-IF
           END-PERFORM
           .
       800-MOJIHEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    WRK-SYY             TO  WRK-HEN-YY
           MOVE    WRK-SMM             TO  WRK-HEN-MM
           MOVE    WRK-SDD             TO  WRK-HEN-DD
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   "99999999"
               MOVE    "12"                TO  WRK-HEN-MM
               MOVE    "31"                TO  WRK-HEN-DD
           END-IF
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ݸ��ֹ�ޥ�������������
      *****************************************************************
       980-HKNNUM-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO                TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       980-HKNNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ѥ�᥿�����ɤ߹���
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥�CALL����
      *****************************************************************
       900-ORCDBMAIN-SEC                SECTION.
      *
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *
      *