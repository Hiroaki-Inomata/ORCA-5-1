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
       PROGRAM-ID.             ORCSPTID.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ���ԣɣļ�������
      *  �ֵѥ��顼������  : 00:���10:�ѥ�᡼���������оݥǡ����ʤ�
      *                    : 02:�����ƥ�����ʴ����ֹ���֡��ɹ����顼
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  XX.XX.XX    �ΣΣ�        ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *  01.00.01    MCC-����      01/05/22  �����ֹ楼���ͤ����
      *  01.00.02    MCC-¿��Ǽ    01/11/27  ��ͳ������ʬ���ɲý���
      *  02.07.00    NACL-����     05/10/24  MONFUNC �б�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *  04.04.00    NACL-¿��Ǽ  09/01/29  �ܱ�ʬ���б�
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
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *----(01.00.01) LINE ADD START --------------------------------
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-END             PIC 9(02).
           03  WRK-RENKETA         PIC 9(02).
           03  WRK-PTNUM           PIC X(20).
           03  WRK-ST              PIC 9(02).
      *----(01.00.01) LINE ADD END   --------------------------------
           03  WRK-SADDSIGN-STR    PIC X(1000).
      *
      *    ��¸�����ΰ�
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *    �����ֹ湽����������
           COPY  "CPSK1009.INC".
      *
      *    �����ֹ��Ѵ��ޥ���
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "CPORCSADDSIGN.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSPTID.INC".
      *
          COPY     "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSPTIDAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  SPTID-RC
      *    �����ΰ�
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           IF      SPTID-HOSPNUM       =   ZERO
               MOVE    1                   TO  SPTID-RC
           END-IF
      *
           IF     (SPTID-PTNUM      NOT =   SPACE )  AND
                  (SPTID-PTID       NOT =   ZERO  )
               MOVE    1                   TO  SPTID-RC
           ELSE
               IF      SPTID-PTNUM      NOT =   SPACE
                   PERFORM 100-PTID-SET-SEC
               ELSE
                   PERFORM 110-PTNUM-SET-SEC
               END-IF
           END-IF
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    ���ԣɣĸ�������
      *****************************************************************
       100-PTID-SET-SEC                SECTION.
      *
      *    �����ֹ湽���Խ�
           PERFORM 120-SYS1009HEN-SEC
      *
      *----(01.00.01) LINE ADD START --------------------------------
           PERFORM 120-KETA-CNT-SEC
           IF  SPTID-RC            NOT =   ZERO
               GO  TO                  100-PTID-SET-EXT
           END-IF
      *----(01.00.01) LINE ADD END   --------------------------------
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPTID-HOSPNUM       TO  PTNUM-HOSPNUM
      *
           MOVE    SPTID-PTNUM         TO  WRK-SADDSIGN-STR
           PERFORM 800-ORCSADDSIGN-SEC
           MOVE    WRK-SADDSIGN-STR    TO  PTNUM-PTNUM
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTNUM
           ELSE
               MOVE    2                   TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
           PERFORM
               UNTIL      (FLG-PTNUM   NOT =   ZERO )  OR
                          (FLG-OK          =   1    )
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTNUM-REC
                   MOVE    ZERO                TO  FLG-PTNUM
      *
                   IF      PTNUM-PTNUM         =   SPTID-PTNUM
                       MOVE    1               TO  FLG-OK
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-OK              =   1
               MOVE    PTNUM-PTID      TO  SPTID-PTID
           ELSE
               MOVE    FLG-PTNUM       TO  SPTID-RC
           END-IF
      *
           .
       100-PTID-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ湽�����������Խ�����
      *****************************************************************
       120-SYS1009HEN-SEC                SECTION.
      *
      *    ���ŵ����ֹ椬�ӣУ��Ȱ㤦
           IF     (SPTID-HOSPNUM    NOT =   ZERO  )
           AND    (SPTID-SYS1009-KBN    =   1     )
      *        �����ֹ湽�����������Խ�
               MOVE    SPTID-HOSPNUM       TO  WRK-SPA-HOSPNUM
               PERFORM 1201-SYS1009-SPA-SEC
           END-IF
           .
       120-SYS1009HEN-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ湽�����������Խ�����
      *****************************************************************
       1201-SYS1009-SPA-SEC                SECTION.
      *
           INITIALIZE                       WRK-SPA-1009-TBL
      *
           MOVE    SPACE               TO  SYS-1009-REC
           INITIALIZE                  SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1009-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1009-EDYUKYMD
           MOVE    SPTID-HOSPNUM       TO  SYS-1009-HOSPNUM
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
           ELSE
               MOVE    1               TO  SPTID-RC
           END-IF
           .
       1201-SYS1009-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ渡������
      *****************************************************************
       110-PTNUM-SET-SEC                SECTION.
      *
           MOVE    SPTID-HOSPNUM       TO  PTNUM-HOSPNUM
           MOVE    SPTID-PTID          TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTNUM-REC
                   MOVE    ZERO                TO  FLG-PTNUM
               ELSE
                   MOVE    1                   TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTNUM           =   ZERO
               MOVE    PTNUM-PTNUM     TO  SPTID-PTNUM
           ELSE
               MOVE    2               TO  SPTID-RC
           END-IF
      *
           .
       110-PTNUM-SET-EXT.
           EXIT.
      *
      *----(01.00.01) LINE ADD START --------------------------------
      *****************************************************************
      *    ���ԣɣĸ�������
      *****************************************************************
       120-KETA-CNT-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           MOVE    SPTID-PTNUM         TO  WRK-PTNUM
      *
      *    ɸ�๽�����Τ��Ѵ�
      *****IF  WRK-SPA-1009-PTNUMKSIKBN    =   "1"  OR  "3"
           IF  WRK-SPA-1009-PTNUMKSIKBN    =   "3"
               GO  TO                  120-KETA-CNT-EXT
           END-IF
      *    ��ͳ�����ǿ������λ��Ѵ�
           IF  (WRK-SPA-1009-PTNUMKSIKBN   =   "1"  )  AND
               (WRK-SPA-1009-JIYKSIKBN NOT =   "2"  )
               GO  TO                  120-KETA-CNT-EXT
           END-IF
      *
      *    �ѥ�᡼����IN�ˤδ����ֹ����򥫥����
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL    (IDX                    >   20   )
                       OR       (SPTID-PTNUM(IDX:1)     =   SPACE)
               MOVE    IDX             TO  WRK-END
           END-PERFORM
      *
      *    ��ͳ�����η���Խ�
           IF  WRK-SPA-1009-PTNUMKSIKBN    =   "1"
               MOVE    WRK-SPA-1009-JIYKSIKETA     TO  WRK-RENKETA
           ELSE
      *    ɸ�๽���η��
      *    �����ƥ�����ޥ��������ꤵ�줿����򥫥����
               MOVE    WRK-SPA-1009-HJNKSIRENNUMKETA   TO  WRK-RENKETA
               IF  WRK-SPA-1009-HJNKSIKBN      =   "1"  OR  "2"
                   EVALUATE    WRK-SPA-1009-HJNKSINENKBN
                   WHEN    "1"
                       ADD     3           TO  WRK-RENKETA
                   WHEN    "2"
                       ADD     4           TO  WRK-RENKETA
                   END-EVALUATE
               END-IF
               IF  WRK-SPA-1009-HJNKSIKBN      =   "1"  OR  "3"
                   ADD     1               TO  WRK-RENKETA
               END-IF
           END-IF
      *
      *    �ѥ�᡼����IN�ˤδ����ֹ��������ꤵ�줿�����Ķ�������
           IF  WRK-END                 >   WRK-RENKETA
               MOVE    1               TO  SPTID-RC
               GO      TO              120-KETA-CNT-EXT
           END-IF
      *    ���ϰ��ֻ���
           COMPUTE     WRK-ST          =   WRK-RENKETA
                                       -   WRK-END   +   1
           IF  ( WRK-ST                =   ZERO )  OR
               ( WRK-ST                >   20   )
               MOVE    1               TO  SPTID-RC
               GO      TO              120-KETA-CNT-EXT
           END-IF
      *    �������뤿�ᡢ�����ֹ�򥼥����Խ�
           MOVE    SPACE               TO  SPTID-PTNUM
           MOVE    ZERO                TO  SPTID-PTNUM (1:WRK-RENKETA)
           MOVE    WRK-PTNUM (1:WRK-END)
                                       TO  SPTID-PTNUM (WRK-ST:)
      *
           .
       120-KETA-CNT-EXT.
           EXIT.
      *
      *----(01.00.01) LINE ADD END   --------------------------------
      *
      *****************************************************************
      *    ����ʸ�����Խ�����
      *****************************************************************
       800-ORCSADDSIGN-SEC             SECTION.
      *
           INITIALIZE                  ORCSADDSIGNAREA
           MOVE    WRK-SADDSIGN-STR    TO  SADDSIGN-STR
           CALL    "ORCSADDSIGN"       USING ORCSADDSIGNAREA
           MOVE    SADDSIGN-STR        TO  WRK-SADDSIGN-STR
      *
           .
       800-ORCSADDSIGN-EXT.
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
                                               WRK-SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *