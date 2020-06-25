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
       IDENTIFICATION                   DIVISION.
       PROGRAM-ID.                      ORCGP02W3.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ݸ��ȹ礻�ɣ�����
      *  ����ݡ��ͥ��̾  : �ݸ��ȹ礻�ɣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC-����      ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  02.00.01    NACL-¿��Ǽ  02/07/31  �ѥ�᥿�ե������ģ¤�
      *  02.00.02    NACL-¿��Ǽ  05/11/14  MONFUNC �б�¾
      *  03.05.00    NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                      DIVISION.
       CONFIGURATION                    SECTION.
       INPUT-OUTPUT                     SECTION.
       FILE-CONTROL.
      *
      *
       DATA                             DIVISION.
      *FILE                             SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-PARA                          PIC 9(01).
           03  FLG-NC01                          PIC 9(01).
           03  FLG-HKNCOMBI                      PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-PARAEDANUM                    PIC 9(04).
           03  CNT-AUTOCOMBINUM                  PIC 9(04).
      *
      *    ����ǥå����ΰ�
       01  INDEX-AREA.
           03  IDX1                              PIC 9(04).
           03  IDX2                              PIC 9(04).
      *
      *    �����쥳�����ֹ��Ǽ
       01  WRK-RECEDATBL.
           03  WRK-RECEDA                    PIC 9(04) OCCURS 4 TIMES.
      *    ���ȹ礻�ѥ�᥿�ǡ����ѥ쥤������
       01  WRK-NC01PAREA-AREA.
           03  WRK-NC01PARA-OCC                OCCURS   999.
               05  NC01PARA-REC.
                   07  PARA-NC01-REC           PIC X(500).
                   07  PARA-NC01-FLG           PIC 9(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ݸ��Ȥ߹�碌�ʣģ��ѡ�
       01  DBHKNCOMBI-REC.
           COPY  "CPHKNCOMBI.INC" REPLACING  //COMB-// BY //DBCOMB-//.
      *
      *    �ݸ��Ȥ߹�碌�ʥե������ѡ�
       01  HKNCOMBI-REC.
           COPY  "CPHKNCOMBI.INC".
      *
      *    �����ֹ��Ѵ�
       01  PTNUM-REC.
           COPY  "CPPTNUM.INC".
      *
      *    �ѥ�᥿�ơ��֥�
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                                  SECTION.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ������Ͽ�����ΰ�
           COPY    "P02COMMON-SPA".
      *
       PROCEDURE                                 DIVISION    USING
           SPA-AREA
           SPA-P02KYOUTU
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                              SECTION.
      *
      *    �Ȥ߹�碌�ѥ�᥿��������
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                  CNT-AREA
           INITIALIZE                  FLG-AREA
      *
      *    �ѥ�᥿�ե�����˺��������������ȹ礻��ơ��֥��
           PERFORM 100-NC01AREA-TBL-SEC
      *
      *  ����¸���ȹ礻�ֹ�򤢤��餷���ȹ礻�˷�³����
           PERFORM 100-NUMBCONT-SEC
      *��  �������Ȥ߹�碌���ֹ����֤���
           IF  SPA-ERRCD             =  SPACE
               PERFORM  200-NUMBNEW-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT  PROGRAM.
      *
      *****************************************************************
      *    �ѥ�᥿�ե�����˺��������������ȹ礻��ơ��֥���¸
      *****************************************************************
       100-NC01AREA-TBL-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-NC01PAREA-AREA
           INITIALIZE                  WRK-NC01PAREA-AREA
           MOVE    ZERO                TO  IDX2
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "NC01PARA"          TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1                   TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =  1  )  OR
                      (IDX2                >=  999)
               ADD     1                   TO  IDX2
               MOVE    PARA-DATA-REC       TO  NC01PARA-REC(IDX2)
               MOVE    ZERO                TO  PARA-NC01-FLG (IDX2)
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100-NC01AREA-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ֹ��³�����롼����
      *****************************************************************
       100-NUMBCONT-SEC                SECTION.
      *
           MOVE    ZERO                TO  CNT-PARAEDANUM
      *    �ȹ礻����ǡ������ʥѥ�᡼���ե��������
           MOVE    ZERO                TO  SPA-P02-COMBDCNT
           MOVE    SPACE               TO  SPA-ERRCD
      *
           INITIALIZE                      DBHKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  DBCOMB-HOSPNUM
           MOVE    SPA-PTID            TO  DBCOMB-PTID
           MOVE    SPACE               TO  DBCOMB-DELKBN
           MOVE    DBHKNCOMBI-REC      TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 812-COMBIFET-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
           PERFORM
               UNTIL  (FLG-HKNCOMBI        =   1 )  OR
                      (SPA-ERRCD       NOT =   SPACE)
               IF      DBCOMB-HKNCOMBINUM    <  9000
                   PERFORM 1001-NUMBCONT-SEC
                   IF     (WRK-RECEDA(1)   NOT =  ZERO)  OR
                          (WRK-RECEDA(2)   NOT =  ZERO)  OR
                          (WRK-RECEDA(3)   NOT =  ZERO)  OR
                          (WRK-RECEDA(4)   NOT =  ZERO)  
                       IF      CNT-PARAEDANUM      =   999
                           MOVE    "0010"              TO  SPA-ERRCD
                       ELSE
                           PERFORM  1002-COMBIWRT-SEC
                       END-IF
                   ELSE
                       IF      CNT-PARAEDANUM      =   999
                           MOVE    "0010"              TO  SPA-ERRCD
                       ELSE
                           PERFORM  1003-COMBIDEL-SEC
                       END-IF
                   END-IF
               END-IF
               IF      SPA-ERRCD           =  SPACE
                   MOVE    "tbl_hkncombi"      TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 812-COMBIFET-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM  990-DBCLOSE-SEC
           .
       100-NUMBCONT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ֹ��³�����롼����
      *****************************************************************
       1001-NUMBCONT-SEC                     SECTION.
      *
      *H26.4
      *    ϫ�ҡ������դ��ܿͲ�²��ʬ�϶���Ȥ���
           IF    (DBCOMB-HKNNUM            =   "971"
                                           OR  "973")
             AND (DBCOMB-HONKZKKBN     NOT =   SPACE)
                 MOVE    SPACE             TO  DBCOMB-HONKZKKBN
           END-IF
      *
           INITIALIZE                  WRK-RECEDATBL
      *
           PERFORM     VARYING     IDX2    FROM    1   BY  1
               UNTIL  (IDX2                >   999 )  OR
                      (PARA-NC01-REC(IDX2) =   SPACE )
             MOVE  PARA-NC01-REC (IDX2)      TO HKNCOMBI-REC
      *!!
      *      ϫ�ҡ������դ��ܿͲ�²��ʬ�϶���Ȥ���
             IF    (COMB-HKNNUM            =   "971"
                                           OR  "973")
               AND (COMB-HONKZKKBN     NOT =   SPACE)
                 MOVE    SPACE             TO  COMB-HONKZKKBN
             END-IF
      *
             IF  COMB-HOSPNUM                =  DBCOMB-HOSPNUM     AND
                 COMB-PTID                   =  DBCOMB-PTID        AND
                 COMB-HKNID                  =  DBCOMB-HKNID       AND
                 COMB-HKNNUM                 =  DBCOMB-HKNNUM      AND
                 COMB-HONKZKKBN              =  DBCOMB-HONKZKKBN   AND
                 COMB-HOJOKBN                =  DBCOMB-HOJOKBN     AND
                 COMB-CONTKBN                =  DBCOMB-CONTKBN     AND
                 COMB-KOH1ID                 =  DBCOMB-KOH1ID      AND
                 COMB-KOH1HKNNUM             =  DBCOMB-KOH1HKNNUM  AND
                 COMB-KOH2ID                 =  DBCOMB-KOH2ID      AND
                 COMB-KOH2HKNNUM             =  DBCOMB-KOH2HKNNUM  AND
                 COMB-KOH3ID                 =  DBCOMB-KOH3ID      AND
                 COMB-KOH3HKNNUM             =  DBCOMB-KOH3HKNNUM  AND
                 COMB-KOH4ID                 =  DBCOMB-KOH4ID      AND
                 COMB-KOH4HKNNUM             =  DBCOMB-KOH4HKNNUM  AND
                 PARA-NC01-FLG (IDX2)        =  ZERO
      *����������Ŭ�ѳ���ǯ������Ŭ�ѽ�λǯ����������Ʊ���ǡ���
                 IF  (COMB-TEKSTYMD          =  DBCOMB-TEKSTYMD )  AND
                     (COMB-TEKEDYMD          =  DBCOMB-TEKEDYMD )  AND
                     (WRK-RECEDA(1)          =  ZERO            )
                     MOVE    IDX2            TO  WRK-RECEDA(1)
                 END-IF
      *����������Ŭ�ѳ���ǯ�����Τ�Ʊ���ǡ���
                 IF (COMB-TEKSTYMD           =  DBCOMB-TEKSTYMD )  AND
                    (COMB-TEKEDYMD       NOT =  DBCOMB-TEKEDYMD )  AND
                    (WRK-RECEDA(2)           =  ZERO            )
                     MOVE    IDX2            TO WRK-RECEDA(2)
                 END-IF
      *����������Ŭ�ѽ�λǯ�����Τ�Ʊ���ǡ���
                 IF (COMB-TEKSTYMD       NOT =  DBCOMB-TEKSTYMD )  AND
                    (COMB-TEKEDYMD           =  DBCOMB-TEKEDYMD )  AND
                    (WRK-RECEDA(3)           =  ZERO            )
                     MOVE    IDX2            TO WRK-RECEDA(3)
                 END-IF
      *����������Ŭ��ǯ������������ʣ���Ƥ���ǡ���
                 IF (COMB-TEKSTYMD       NOT =  DBCOMB-TEKSTYMD )  AND
                    (COMB-TEKEDYMD       NOT =  DBCOMB-TEKEDYMD )  AND
                    (WRK-RECEDA(4)           =  ZERO            )  AND
                   ((COMB-TEKSTYMD           >  DBCOMB-TEKSTYMD    AND
                     COMB-TEKSTYMD           <  DBCOMB-TEKEDYMD)   OR
                    (COMB-TEKEDYMD           >  DBCOMB-TEKSTYMD    AND
                     COMB-TEKEDYMD           <  DBCOMB-TEKEDYMD)   OR
                    (COMB-TEKSTYMD           <  DBCOMB-TEKSTYMD    AND
                     COMB-TEKEDYMD           >  DBCOMB-TEKEDYMD))
                     MOVE    IDX2            TO WRK-RECEDA(4)
                 END-IF
             END-IF
      *
             IF      WRK-RECEDA(1)       NOT =  ZERO
                 MOVE   999              TO  IDX2
             END-IF
           END-PERFORM
      *
           .
       1001-NUMBCONT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ֹ��³�����ϥ롼����
      *****************************************************************
       1002-COMBIWRT-SEC                         SECTION.
      *
      *  �����Ȥ߹�碌�ǡ����˽����ѥե饰��Ω�Ƥ�
           EVALUATE    TRUE
               WHEN    WRK-RECEDA(1)   NOT =   ZERO
                   MOVE    1               TO  IDX1
               WHEN    WRK-RECEDA(2)   NOT =   ZERO
                   MOVE    2               TO  IDX1
               WHEN    WRK-RECEDA(3)   NOT =   ZERO
                   MOVE    3               TO  IDX1
               WHEN    WRK-RECEDA(4)   NOT =   ZERO
                   MOVE    4               TO  IDX1
           END-EVALUATE
           MOVE    WRK-RECEDA(IDX1)    TO  IDX2
           IF      IDX2                =   ZERO
               CONTINUE
           ELSE
               PERFORM 10021-COMBIWRT2-SEC
           END-IF
           .
       1002-COMBIWRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ֹ��³�����ϥ롼����
      *****************************************************************
       10021-COMBIWRT2-SEC                         SECTION.
      *
           MOVE    PARA-NC01-REC (IDX2)
                                       TO  HKNCOMBI-REC
           MOVE    1                   TO  PARA-NC01-FLG (IDX2)
      *
      *    �ѥ�᡼���ǡ��������Ƥ���Ϥ���
      *    �ݸ��ȹ礻
           COMPUTE CNT-PARAEDANUM      =   CNT-PARAEDANUM  +  1
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
           MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PARA            =   ZERO
               MOVE    DBCOMB-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
      *
               MOVE    HKNCOMBI-REC        TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           ELSE
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
               MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
               MOVE    DBCOMB-HKNCOMBINUM  TO  COMB-HKNCOMBINUM
               MOVE    HKNCOMBI-REC        TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
           .
       10021-COMBIWRT2-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ֹ��������ϥ롼����
      *****************************************************************
       1003-COMBIDEL-SEC                         SECTION.
      *
      *  �ѥ�᡼���ǡ��������Ƥ���Ϥ���
           COMPUTE CNT-PARAEDANUM      =   CNT-PARAEDANUM  +  1
      *    �ݸ��ȹ礻
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
           MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PARA            =   ZERO
               MOVE    "1"                 TO  DBCOMB-DELKBN
               MOVE    DBHKNCOMBI-REC      TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        �ȹ礻����ǡ������ʥѥ�᡼���ե��������
               ADD    1                    TO  SPA-P02-COMBDCNT
           ELSE
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
               MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
      *
               MOVE    "1"                 TO  DBCOMB-DELKBN
               MOVE    DBHKNCOMBI-REC      TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        �ȹ礻����ǡ������ʥѥ�᡼���ե��������
               ADD    1                    TO  SPA-P02-COMBDCNT
           END-IF
           .
       1003-COMBIDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ȹ礻���ϥ롼����
      *****************************************************************
       200-NUMBNEW-SEC                           SECTION.
      *
      *    �ӣУ�����¸���Ƥ���
           MOVE    SPA-MAE-AUTOCOMBINUM    TO  CNT-AUTOCOMBINUM
           IF      SPA-MAE-AUTOCOMBINUM    =   8999
               MOVE    "0011"              TO  SPA-ERRCD
           END-IF
      *
      ***  INITIALIZE                      PARA-REC
      *    MOVE    "P02"               TO  PARA-GYOUMUID
      *    MOVE    SPA-TERMID          TO  PARA-TERMID
      *    MOVE    "NC01PARA"          TO  PARA-FILEMEI
      *
      *    MOVE    PARA-REC            TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "PARA-KEY2"         TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "PARA-KEY2"         TO  ORC-DBPATH
      *        PERFORM 990-PARA-READ-SEC
      *    ELSE
      *        INITIALIZE                  PARA-REC
      *        MOVE    1                   TO  FLG-PARA
      *****END-IF
      *****MOVE    SPACE               TO  SPA-ERRCD
      *
           PERFORM     VARYING     IDX2    FROM    1   BY  1
               UNTIL  (IDX2                >   999 )  OR
                      (PARA-NC01-REC(IDX2) =   SPACE )  OR
                      (SPA-ERRCD       NOT =   SPACE )
               IF      PARA-NC01-FLG (IDX2)  =   ZERO
                   MOVE    PARA-NC01-REC (IDX2)  TO  HKNCOMBI-REC
                   IF      CNT-AUTOCOMBINUM    =   8999
                       MOVE    "0011"              TO  SPA-ERRCD
                   ELSE
                       COMPUTE CNT-AUTOCOMBINUM    =   CNT-AUTOCOMBINUM
                                                   +   1
                   END-IF
                   IF      CNT-PARAEDANUM      =   9999
                       MOVE  "0010"                TO SPA-ERRCD
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
      *  �ѥ�᡼���ǡ��������Ƥ���Ϥ���
                       COMPUTE CNT-PARAEDANUM      =   CNT-PARAEDANUM
                                                   +   1
                       MOVE    CNT-AUTOCOMBINUM    TO  COMB-HKNCOMBINUM
      *  ����������ѹ���ʬ�������¾�ǻ��Ѥ��ʤ����ȡ�
                       MOVE    "A"                 TO  COMB-CHGKBN
      *
                       PERFORM 2001-PARAHKNCOMBI-INS-SEC
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
      *        �ӣУ����ݸ��ȹ礻�ֹ��ѹ�
               MOVE    CNT-AUTOCOMBINUM    TO  SPA-PTNUM-AUTOCOMBINUM
           END-IF
      *
           .
       200-NUMBNEW-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ѥ�᡼���ɲå롼����
      *****************************************************************
       2001-PARAHKNCOMBI-INS-SEC       SECTION.
      *    �ݸ��ȹ礻
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
           MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PARA            =   ZERO
               MOVE    HKNCOMBI-REC        TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           ELSE
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
               MOVE    CNT-PARAEDANUM      TO  PARA-EDANUM
               MOVE    HKNCOMBI-REC        TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           .
       2001-PARAHKNCOMBI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡�����
      *****************************************************************
      *
      *****************************************************************
      *    �ݸ��ȹ礻�ƣţԣãȽ���
      *****************************************************************
       812-COMBIFET-SEC                          SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  DBHKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  DBHKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
      *
           .
       812-COMBIFET-EXT.
           EXIT.
      *****************************************************************
      *    �ѥ�᥿�����ɤ߹���
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
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
       990-DBCLOSE-SEC         SECTION.
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
      *