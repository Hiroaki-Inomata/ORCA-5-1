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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGSSUB02.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��Ǽ
      *  ����ݡ��ͥ��̾  : ��Ǽ�����Խ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  04/11/25    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *  03.04.01    NACL-����     06/12/19  �ץ�ӥ塼�ɲ�
      *  03.05.00    NACL-����     07/05/20  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYURRK          PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDXM                PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-DENPNUM         PIC 9(07).
      *
       01  CONST-AREA.
           03  CONST-EDANUM-MAX    PIC 9(03)   VALUE 99.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ��Ǽ����
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    ��Ǽ����
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "CPSSUB02.INC".
       01  LNKSMEI-AREA.
         02  LNKSMEI-OCC           OCCURS    99.
           COPY "CPSYUMEI.INC"     REPLACING //SMEI-//
                                   BY        //LNKSMEI-//.
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           SSUB02-AREA
           LNKSMEI-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE              FLG-AREA
                                   IDX-AREA
                                   WRK-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              SSUB02-OT
                                   LNKSMEI-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 900-SYUMEI-KEY6-SEL-SEC
      *
           MOVE    ZERO                TO  IDXM
           PERFORM UNTIL ( FLG-SYUMEI  NOT =   ZERO )
                    OR   ( IDXM        >=  CONST-EDANUM-MAX )
      *
               COMPUTE IDXM    =   IDXM    +   1
               MOVE    IDXM            TO  SSUB02-MAX
      *
               MOVE    1               TO  SSUB02-VISIBLE (IDXM)
      *
               IF    ( SMEI-JOUTAIKBN      =   "2" OR "4" OR "6" )
                AND  ( SMEI-NYUHEN-MONEY   =   ZERO )
                       MOVE    ZERO    TO  SSUB02-VISIBLE (IDXM)
               END-IF
      *
               IF    ( SMEI-JOUTAIKBN      =   "4" OR  "6" )
                OR  (( SMEI-JOUTAIKBN      =   "8" )
                 AND ( SMEI-NYUHEN-MONEY   <   ZERO ))
                   MOVE    "1"         TO  SSUB02-MINUS (IDXM)
               END-IF
      *
               EVALUATE    SMEI-JOUTAIKBN
               WHEN    SPACE
                   MOVE   "����ۤʤ�"
                                       TO  SSUB02-JOUTAI (IDXM)
               WHEN    "1"
                   IF   ( SMEI-SKYMONEY    >   SMEI-NYUHEN-MONEY )
                       MOVE   "���ᡦ̤��"
                                       TO  SSUB02-JOUTAI (IDXM)
                   ELSE
                       MOVE   "���ᡦ����"
                                       TO  SSUB02-JOUTAI (IDXM)
                   END-IF
      *
               WHEN    "2"
                   MOVE    "����"      TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "3"
                   MOVE    "�Ʒ׻�"    TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "4"
                   MOVE    "�ֶ�"      TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "5"
                   MOVE    "������"  TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "6"
                   MOVE    "������"  TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "7"
                   MOVE    "��³"      TO  SSUB02-JOUTAI (IDXM)
      *
               WHEN    "8"
                   MOVE    "��������"  TO  SSUB02-JOUTAI (IDXM)
               WHEN    "9"
                   MOVE    "����"      TO  SSUB02-JOUTAI (IDXM)
               END-EVALUATE
      *
               EVALUATE    SMEI-JOUTAIKBN
               WHEN    " "
               WHEN    "1"
               WHEN    "9"
                   MOVE    1           TO  SSUB02-PROC-FLG (IDXM)
               WHEN    "3"
               WHEN    "7"
               WHEN    "8"
               WHEN    "5"
                   MOVE    2           TO  SSUB02-PROC-FLG (IDXM)
               WHEN    "2"
                   MOVE    3           TO  SSUB02-PROC-FLG (IDXM)
               WHEN    "4"
               WHEN    "6"
                   MOVE    4           TO  SSUB02-PROC-FLG (IDXM)
               END-EVALUATE
      *
               IF    ( SMEI-SYURRKNUM  NOT =   ZERO )
                   PERFORM 900-SYURRK-KEY-SEL-SEC
                   IF    ( FLG-SYURRK      =   ZERO )
                       MOVE    SYURRK-CHOSEI1
                                       TO  SSUB02-CHOSEI1 (IDXM)
                       MOVE    SYURRK-CHOSEI2
                                       TO  SSUB02-CHOSEI2 (IDXM)
                   ELSE
                       MOVE    ZERO    TO  SMEI-SYURRKNUM
                                           SMEI-SYUEDANUM
                   END-IF
               END-IF
      *
               MOVE    SYUMEI-REC      TO  LNKSMEI-OCC (IDXM)
      *
               PERFORM 900-SYUMEI-KEY6-FET-SEC
      *
           END-PERFORM
      *
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ���ٸ�������
      *****************************************************************
       900-SYUMEI-KEY6-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SPA-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SPA-PTID            TO  SMEI-PTID
           MOVE    SPA-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SSUB02-DENPNUM      TO  SMEI-DENPNUM
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY6-SEL-EXT.
           EXIT.
      *
      **************************************************************
      *    ��Ǽ����FETCH����
      **************************************************************
       900-SYUMEI-KEY6-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUMEI
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-KEY6-FET-EXT.
           EXIT.
      *****************************************************************
      *    ��Ǽ���򸡺�����
      *****************************************************************
       900-SYURRK-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYURRK
      *
           INITIALIZE                      SYURRK-REC
           MOVE    SMEI-HOSPNUM        TO  SYURRK-HOSPNUM
           MOVE    SMEI-NYUGAIKBN      TO  SYURRK-NYUGAIKBN
           MOVE    SMEI-PTID           TO  SYURRK-PTID
           MOVE    SMEI-DENPNUM        TO  SYURRK-DENPNUM
           MOVE    SMEI-SYURRKNUM      TO  SYURRK-SYURRKNUM
           MOVE    SMEI-SYUEDANUM      TO  SYURRK-SYUEDANUM
           MOVE    SYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYURRK-REC
           ELSE
               MOVE    1               TO  FLG-SYURRK
               INITIALIZE                  SYURRK-REC
           END-IF
      *
           MOVE    "tbl_syurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYURRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£ģţ̣ţԣŽ���
      *****************************************************************
       910-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ£գУģ��ԣŽ���
      *****************************************************************
       910-DBUPDATE-SEC                SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥��󥵡��Ƚ���
      *****************************************************************
       910-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
