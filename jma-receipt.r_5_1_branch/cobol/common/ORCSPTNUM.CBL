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
       PROGRAM-ID.             ORCSPTNUM.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : �����ֹ����Ͻ���
      *  �ֵѥ��顼������  : 00:���99:���ߥ��顼
      *                      10:�оݥǡ����ʤ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02.06.04    ¿��Ǽ        ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *  02.07.00    NACL-����     05/10/24  MONFUNC �б�
      *  03.03.00    NACL-¿��Ǽ   06/10/06  ̾���������ɲ�
      *  03.05.00    NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      *  04.04.00    NACL-¿��Ǽ  09/01/29  �ܱ�ʬ���б�
      *  04.05.00    NACL-¿��Ǽ  09/10/27  ��ĥ�����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
           03  IDX-S1              PIC 9(04).
           03  IDX-S2              PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PTNUM           PIC X(20).
           03  WRK-PTID            PIC 9(10).
      *
           03  WRK-NAME            PIC X(50).
           03  WRK-NAME1           PIC X(50).
           03  WRK-NAME2           PIC X(50).
      *
           03  WRK-E-NAME          PIC X(50).
           03  WRK-H-NAME          PIC X(50).
           03  WRK-H-NAME-G.
               05  WRK-H-NAME1         PIC X(02).
      *
           03  WRK-KANACHK-MAX         PIC 9(04).
           03  WRK-KANACHK-SYUBETU     PIC 9(01).
           03  WRK-KANACHK-OUT-INPUT   PIC X(1000).
      *
      *    ��¸�����ΰ�
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���ԥޥ�����
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    �����ֹ��Ѵ��ޥ���
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *    �����ֹ湽����������
           COPY  "CPSK1009.INC".
      *
           COPY    "MCPAREA".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK2.INC".
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
      *    ��ĥʸ���б�
           COPY    "CPORCSSTRING.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *    COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSPTNUM.INC".
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSPTNUMAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  SPTNUM-RC
           MOVE    ZERO                TO  SPTNUM-SYUBETU
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
      *    �����ΰ�
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
      *    �����ֹ湽���Խ�
           PERFORM 1000-SYS1009-HEN-SEC
      *
           IF      SPA-ENCODING        =   2
      *        ��ĥ�����б�
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"                 TO  KANACHK2-SYORI
               MOVE    SPTNUM-PTNUM        TO  KANACHK2-MAE-INPUT
               MOVE    20                  TO  KANACHK2-MAX-CNT
               CALL    "ORCSKANACHK2"      USING
                                           ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT  TO  SPTNUM-PTNUM
               MOVE    KANACHK2-OUT-INPUT  TO  WRK-KANACHK-OUT-INPUT
               MOVE    KANACHK2-MAX        TO  WRK-KANACHK-MAX
               MOVE    KANACHK2-SYUBETU    TO  WRK-KANACHK-SYUBETU
               IF      KANACHK2-RC      NOT =   ZERO
      *        ���ߥ��顼
                   MOVE    99              TO  SPTNUM-RC
                   MOVE    ZERO            TO  WRK-KANACHK-MAX
                   MOVE    9               TO  WRK-KANACHK-SYUBETU
               END-IF
           ELSE
      *        ��ĥ�����Բ�
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPTNUM-PTNUM        TO  KANACHK-MAE-INPUT
               MOVE    20                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPTNUM-PTNUM
               MOVE    KANACHK-OUT-INPUT   TO  WRK-KANACHK-OUT-INPUT
               MOVE    KANACHK-MAX         TO  WRK-KANACHK-MAX
               MOVE    KANACHK-SYUBETU     TO  WRK-KANACHK-SYUBETU
               IF      KANACHK-RC       NOT =   ZERO
      *        ���ߥ��顼
                   MOVE    99              TO  SPTNUM-RC
                   MOVE    ZERO            TO  WRK-KANACHK-MAX
                   MOVE    9               TO  WRK-KANACHK-SYUBETU
               END-IF
           END-IF
      *    �������������ʾ�λ��������Ȥ���
           IF      WRK-KANACHK-MAX     >   20
               MOVE    20              TO  WRK-KANACHK-MAX
           END-IF
      *
           EVALUATE    WRK-KANACHK-SYUBETU
               WHEN    1
      *            Ⱦ�ѡʴ����ֹ��
                   PERFORM 100-PTID-SET-SEC
               WHEN    2
      *            ���ѡʴ��Ի�̾��
                   MOVE    WRK-KANACHK-OUT-INPUT   TO  SPTNUM-PTNUM
                   PERFORM 200-NAME-SET-SEC
      *
               WHEN    OTHER
                   MOVE    99              TO  SPTNUM-RC
           END-EVALUATE
           .
      *
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �����ֹ湽�����������Խ�����
      *****************************************************************
       1000-SYS1009-HEN-SEC                SECTION.
      *
      *    ���ŵ����ֹ�
           IF      SPTNUM-GRPHOSPNUM    NOT =   ZERO
               MOVE    SPTNUM-GRPHOSPNUM        TO  WRK-SPA-HOSPNUM
      *        �����ֹ湽�����������Խ�
               PERFORM 10001-SYS1009-SPA-SEC
           END-IF
           .
       1000-SYS1009-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ湽�����������Խ�����
      *****************************************************************
       10001-SYS1009-SPA-SEC                SECTION.
      *
           INITIALIZE                       WRK-SPA-1009-TBL
      *
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
           MOVE    SPTNUM-GRPHOSPNUM   TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  SYSKANRI-REC
           PERFORM 900-SYSKANRI-KEY10-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1009-REC
               MOVE    SYS-1009-PTNUMKSIKBN
                                       TO  WRK-SPA-1009-PTNUMKSIKBN
               MOVE    SYS-1009-JIYKSIKBN
                                       TO  WRK-SPA-1009-JIYKSIKBN
               MOVE    SYS-1009-JIYKSIKETA
                                       TO  WRK-SPA-1009-JIYKSIKETA
               MOVE    SYS-1009-HJNKSIKBN
                                       TO  WRK-SPA-1009-HJNKSIKBN
               MOVE    SYS-1009-HJNKSINENKBN
                                       TO  WRK-SPA-1009-HJNKSINENKBN
               MOVE    SYS-1009-HJNKSIRENNUMKBN
                                       TO  WRK-SPA-1009-HJNKSIRENNUMKBN
               MOVE    SYS-1009-HJNKSIRENNUMKETA
                                       TO  WRK-SPA-1009-HJNKSIRENNUMKETA
      *��ĥ������ʬ
               MOVE    SYS-1009-KKCKSIKBN
                                       TO  WRK-SPA-1009-KKCKSIKBN
               MOVE    SYS-1009-KKCKSIMAEKETA
                                       TO  WRK-SPA-1009-KKCKSIMAEKETA
               MOVE    SYS-1009-KKCKSIRENNUMKETA
                                       TO  WRK-SPA-1009-KKCKSIRENNUMKETA
               MOVE    SYS-1009-KKCKSIATOKETA
                                       TO  WRK-SPA-1009-KKCKSIATOKETA
           END-IF
           .
       1201-SYS1009-SPA-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���ԣɣĸ�������
      *****************************************************************
       100-PTID-SET-SEC                SECTION.
      *
           MOVE    1                   TO  SPTNUM-SYUBETU
      *
      *    ��������(ɸ�๽������������
           IF     (SPA-1009-PTNUMKSIKBN    =   "2" )  OR
                 ((SPA-1009-PTNUMKSIKBN    =   "1" ) AND
                  (SPA-1009-JIYKSIKBN      =   "2" ))
               MOVE    SPACE               TO  WRK-PTNUM
               MOVE    SPTNUM-PTNUM        TO  WRK-PTNUM
               MOVE    SPACE               TO  SPTNUM-PTNUM
               MOVE    ZERO                TO  IDX2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   20
                   IF      WRK-PTNUM(IDX:1)    =   SPACE
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDX2
                       MOVE    WRK-PTNUM(IDX:1)
                                           TO  SPTNUM-PTNUM(IDX2:1)
                   END-IF
               END-PERFORM
           END-IF
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    WRK-SPA-HOSPNUM     TO  SPTID-HOSPNUM
           MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               WRK-SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    10                  TO  SPTNUM-RC
               MOVE    SPTID-PTNUM         TO  SPTNUM-PTNUM
           ELSE
               MOVE    SPTID-PTID          TO  SPTNUM-PTID
               MOVE    SPTID-PTNUM         TO  SPTNUM-PTNUM
           END-IF
      *
           .
       100-PTID-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ի�̾��������
      *****************************************************************
       200-NAME-SET-SEC                SECTION.
      *
           MOVE    2                   TO  SPTNUM-SYUBETU
      *
           MOVE    SPTNUM-PTNUM(1:WRK-KANACHK-MAX)
                                       TO  WRK-NAME
      *    COMPUTE IDX                 =   KANACHK2-MAX  +  1
      *    MOVE    "%"                 TO  WRK-NAME(IDX:1)
      *    ��ʬ����
           PERFORM 201-NAME-HEN-SEC
      *
      *    ��������
           INITIALIZE                  PTINF-REC
           MOVE    WRK-SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SPACE               TO  PTINF-NAME
           MOVE    WRK-NAME
                                       TO  PTINF-NAME
           MOVE    WRK-NAME
                                       TO  PTINF-KANANAME
      *
      *    ���ԥޥ������������̾�Ǹ���
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key5"          TO  MCP-PATHNAME
               PERFORM 910-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-PTID
           PERFORM UNTIL      (IDX             >   1         ) OR
                              (FLG-PTINF       =   1         )
               ADD     1                   TO  IDX
               MOVE    PTINF-PTID          TO  WRK-PTID
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ���Է���
           IF      IDX                 =   1
               INITIALIZE                      ORCSPTIDAREA
               MOVE    WRK-SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    WRK-PTID            TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   WRK-SPA-AREA
               IF      SPTID-RC        NOT =   ZERO
                   MOVE    10                  TO  SPTNUM-RC
               ELSE
                   MOVE    SPTID-PTID          TO  SPTNUM-PTID
                   MOVE    SPTID-PTNUM         TO  SPTNUM-PTNUM
               END-IF
           END-IF
      *    ���Ԥʤ�
           IF      IDX                 =   0
               MOVE    10                  TO  SPTNUM-RC
           END-IF
      *    ����ʣ��
           IF      IDX                 >   1
               MOVE    20                  TO  SPTNUM-RC
           END-IF
      *
           .
       200-NAME-SET-EXT.
           EXIT.
      *    
      *****************************************************************
      *    ��̾��ʬ�����Խ�����
      *****************************************************************
       201-NAME-HEN-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-NAME1
           MOVE    SPACE               TO  WRK-NAME2
      *    ���μ��Ρ����
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "sub"               TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "����"              TO  ORCSSTR-STRING2
           MOVE    "��"                TO  ORCSSTR-STRING3
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           MOVE    ORCSSTR-STRING1     TO  WRK-NAME
      *    ���ϻ�̾�Ǹ����Ȥ���
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "search"            TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "��"                TO  ORCSSTR-STRING2
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM1        =   ZERO
               MOVE    WRK-NAME            TO  WRK-NAME1
           ELSE
               MOVE    ORCSSTR-NUM1        TO  IDX-S1
               MOVE    ORCSSTR-NUM2        TO  IDX-S2
               IF      ORCSSTR-NUM1        =   1
                   MOVE    SPACE               TO  WRK-NAME1
               ELSE
                   INITIALIZE                  ORCSSTRINGAREA
                   MOVE    "substr"            TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME            TO  ORCSSTR-STRING1
                   MOVE    1                   TO  ORCSSTR-NUM1
                   COMPUTE ORCSSTR-NUM2        =   IDX-S1
                                               -   1
                   CALL    "orcsstring"        USING
                                               ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2     TO  WRK-NAME1
               END-IF
      *
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    WRK-NAME            TO  ORCSSTR-STRING1
               COMPUTE ORCSSTR-NUM1        =   IDX-S1
                                           +   1
               MOVE    0                   TO  ORCSSTR-NUM2
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               MOVE    ORCSSTR-STRING2     TO  WRK-NAME2
           END-IF
      *
           IF     (WRK-NAME1       NOT =   SPACE ) AND
                  (WRK-NAME2       NOT =   SPACE )
               MOVE    SPACE           TO  WRK-NAME
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                       "��"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    WRK-NAME
               END-STRING
           END-IF
           IF     (WRK-NAME1           =   SPACE ) AND
                  (WRK-NAME2       NOT =   SPACE )
               MOVE    SPACE           TO  WRK-NAME
               STRING  "%"             DELIMITED  BY  SIZE
                       "��"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    WRK-NAME
               END-STRING
           END-IF
           IF     (WRK-NAME1       NOT =   SPACE ) AND
                  (WRK-NAME2           =   SPACE )
               MOVE    SPACE           TO  WRK-NAME
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    WRK-NAME
               END-STRING
           END-IF
      *
           .
       201-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ԥޥ������ɹ�����
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTINF
               MOVE    MCPDATA-REC         TO  PTINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ���ܥ����å�����
      *****************************************************************
       900-SYSKANRI-KEY10-SEC     SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   INITIALIZE                  SYSKANRI-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSKANRI-KEY10-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *