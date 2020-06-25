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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSODR11S02.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ���������Խ����֡��ݸ���������å���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  14/05/30    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-HKNJAINF                PIC 9(01).
           03  FLG-HKNNUM                  PIC 9(01).
           03  FLG-ENTRYCHK                PIC 9(01).
           03  FLG-END                     PIC 9(01).
      *
       01  CNT-AREA.
           03  CNT-SPACE                   PIC 9(05).
      *
       01  IDX-AREA.
           03  IDX-ITM                 PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-KANACHK-MAX             PIC 9(05).
           03  WRK-HKNNUM                  PIC X(03).
           03  WRK-KANACHK-STR             PIC X(100).
           03  WRK-LENGTH                  PIC S9(05).
           03  WRK-STR                     PIC X(100).
           03  WRK-TMP                     PIC X(100).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ݸ��Ծ���
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    �ݸ��ֹ����
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *    �����å��ǥ��å�
           COPY    "CPORCCHKDGT.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                         SECTION.
      *
           COPY    "CPORCSODR11S02.INC".
           COPY    "MCPAREA".
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   SODR11S02-AREA
                                               MCPAREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       CNT-AREA
                                       WRK-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SODR11S02-KANREN    =   SPACE )
               PERFORM 2001-KOBETSU-CHK-SEC
           ELSE
               PERFORM 2001-KANREN-CHK-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ���̹��ܥ����å�����
      *****************************************************************
       2001-KOBETSU-CHK-SEC            SECTION.
      *
           EVALUATE    TRUE
           WHEN    ( SODR11S02-HKNJANUM    NOT =   SPACE )
               PERFORM 20011-HKNJANUM-CHK-SEC
           WHEN    ( SODR11S02-HONKZKKBN   NOT =   SPACE )
               PERFORM 20011-HONKZKKBN-CHK-SEC
           WHEN    ( SODR11S02-KIGO        NOT =   SPACE )
               PERFORM 20011-KIGO-CHK-SEC
           WHEN    ( SODR11S02-NUM         NOT =   SPACE )
               PERFORM 20011-NUM-CHK-SEC
           END-EVALUATE
      *
           .
       2001-KOBETSU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ����ֹ�����å�����
      *****************************************************************
       20011-HKNJANUM-CHK-SEC          SECTION.
      *
           MOVE    SPACE                   TO  SODR11S02-HKNJANAME
      *
           IF    ( SODR11S02-HKNJANUM    =   SPACE )
               CONTINUE
           ELSE
      *
               MOVE    ZERO                TO  WRK-KANACHK-MAX
      *
      *        ��������å�
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SODR11S02-HKNJANUM
                                           TO  KANACHK-MAE-INPUT
               MOVE    8                   TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SODR11S02-HKNJANUM
      *        Ⱦ�ѤǴ֤˶��򤬤ʤ�����
               IF    ( KANACHK-RC      NOT =   ZERO  )
                OR   ( KANACHK-RC2     NOT =   ZERO  )
                OR   ( KANACHK-SYUBETU     =   2     )
                   MOVE    "0186"          TO  SODR11S02-ERRCD
               END-IF
               IF    ( SODR11S02-ERRCD           =   SPACE )
                   IF    ( KANACHK-MAX     =   4  OR  6  OR  8 )
                       MOVE    KANACHK-MAX TO  WRK-KANACHK-MAX
                   ELSE
                       MOVE    "0186"      TO  SODR11S02-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SODR11S02-ERRCD           =   SPACE )
      *
                   PERFORM 900-HKNJAINF-KEY-SEL-SEC
      *
      *            �ݸ����ֹ椢��
                   IF    ( FLG-HKNJAINF        =   ZERO )
                       MOVE    HKNJA-HKNJANAME TO  SODR11S02-HKNJANAME
                       MOVE    HKNJA-HKNNUM    TO  WRK-HKNNUM
      *                ���椬����ʳ��λ������Խ�
                       IF    ( HKNJA-KIGO  NOT =   SPACE )
                        AND  ( SODR11S02-KIGO  =   SPACE )
                           MOVE    HKNJA-KIGO  TO  SODR11S02-KIGO
                       END-IF
      *
                   ELSE
      *
      *                �ݸ����ֹ�⥸��饹10�����å�
                       IF    ( WRK-KANACHK-MAX >   4 )
      *
                           INITIALIZE                  LNK-CHKDGT-AREA
                           MOVE    2               TO  LNK-I-SYORIKBN
                           MOVE    SODR11S02-HKNJANUM
                                                   TO  LNK-I-CODE
                           CALL    "ORCSCHKDGT"        USING
                                                       LNK-CHKDGT-AREA
                           IF    ( LNK-O-ERRCD     NOT =   ZERO )
                               MOVE    "0186"      TO  SODR11S02-ERRCD
                           END-IF
                       END-IF
      *
                       IF    ( SODR11S02-ERRCD              =    SPACE )
      *
      *                    ˡ����Խ�
                           EVALUATE    WRK-KANACHK-MAX
                           WHEN    4
      *                        ���ܴɾ�
                               MOVE    "001"       TO  WRK-HKNNUM
                           WHEN    6
      *                        ����
                               MOVE    "060"       TO  WRK-HKNNUM
                           WHEN    8
      *                        ����¾
                               PERFORM 200111-HKNNUM-HBTNUM-SEC
                           END-EVALUATE
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    WRK-HKNNUM          TO  SODR11S02-HKNNUM
      *
               IF    ( SODR11S02-ERRCD     =   SPACE )
                   IF    ( WRK-HKNNUM      =   SPACE )
                       MOVE    "1"         TO  SODR11S02-KANYUKBN
                   ELSE
                       IF    ( WRK-HKNNUM  =   "039" OR "040" )
                           MOVE    "0187"  TO  SODR11S02-ERRCD
                       ELSE
                           IF    ( WRK-HKNNUM  =   "060" OR "067"
                                                    OR "068" OR "069" )
                               MOVE    "2" TO  SODR11S02-KANYUKBN
                           ELSE
                               MOVE    "1" TO  SODR11S02-KANYUKBN
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ˡ���ֹ�����å�����
      *****************************************************************
       200111-HKNNUM-HBTNUM-SEC         SECTION.
      *
           PERFORM 900-HKNNUM-KEY4-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNNUM      NOT =  ZERO )
      *
               IF    ( HKN-HKNKOHSBTKBN    =   "1" )
                   MOVE    HKN-HKNNUM      TO  WRK-HKNNUM
               END-IF
      *
               PERFORM 900-HKNNUM-KEY4-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-HKNNUM-HBTNUM-EXT.
           EXIT.
      *****************************************************************
      *    �ܿͲ�²��ʬ�����å�����
      *****************************************************************
       20011-HONKZKKBN-CHK-SEC         SECTION.
      *
           IF    ( SODR11S02-HONKZKKBN =   SPACE )
               CONTINUE
           ELSE
               IF    ( SODR11S02-HONKZKKBN =   "1" OR  "5" )
                   CONTINUE
               ELSE
                   MOVE    "0188"      TO  SODR11S02-ERRCD
               END-IF
           END-IF
      *
           .
       20011-HONKZKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���ݸ��Ծڵ�������å�����
      *****************************************************************
       20011-KIGO-CHK-SEC              SECTION.
      *
           IF    ( SODR11S02-KIGO      =   SPACE )
               CONTINUE
           ELSE
               MOVE    SODR11S02-KIGO  TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-STR TO  SODR11S02-KIGO
               IF    ( FLG-ENTRYCHK    NOT =   ZERO )
                   MOVE    "0191"      TO  SODR11S02-ERRCD
               END-IF
      *
               IF    ( SODR11S02-ERRCD =   SPACE )
                   MOVE    ZERO            TO  CNT-SPACE
                   INSPECT SODR11S02-KIGO  TALLYING
                           CNT-SPACE       FOR ALL "��"
                   IF    ( CNT-SPACE       >   ZERO )
                       MOVE    "0189"  TO  SODR11S02-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SODR11S02-ERRCD           =   SPACE )
                   MOVE    SODR11S02-KIGO
                                       TO  WRK-STR
                   PERFORM 800-LENGTH-CHK-SEC
                   IF    ( WRK-LENGTH  >   38 )
                       MOVE    "0189"  TO  SODR11S02-ERRCD
                   END-IF
               END-IF
      *
           END-IF
      *
           .
       20011-KIGO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���ݸ��Ծ��ֹ�����å�����
      *****************************************************************
       20011-NUM-CHK-SEC               SECTION.
      *
           IF    ( SODR11S02-NUM   =   SPACE )
               CONTINUE
           ELSE
               MOVE    SODR11S02-NUM       TO  WRK-KANACHK-STR
               PERFORM 800-ZENKAKU-CHK-SEC
               MOVE    WRK-KANACHK-STR     TO  SODR11S02-NUM
               IF    ( FLG-ENTRYCHK    NOT =   ZERO )
                   MOVE    "0192"          TO  SODR11S02-ERRCD
               END-IF
      *
               IF    ( SODR11S02-ERRCD           =   SPACE )
                   MOVE    ZERO            TO  CNT-SPACE
                   INSPECT SODR11S02-NUM   TALLYING
                           CNT-SPACE       FOR ALL "��"
                   IF    ( CNT-SPACE       >   ZERO )
                       MOVE    "0190"      TO  SODR11S02-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SODR11S02-ERRCD           =   SPACE )
                   MOVE    SODR11S02-NUM   TO  WRK-STR
                   PERFORM 800-LENGTH-CHK-SEC
                   IF    ( WRK-LENGTH  >   38 )
                       MOVE    "0190"      TO  SODR11S02-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       20011-NUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��Ϣ�����å�����
      *****************************************************************
       2001-KANREN-CHK-SEC             SECTION.
      *
           IF    ( SODR11S02-HKNJANUM  =   SPACE )
               MOVE    "8018"          TO  SODR11S02-ERRCD
           END-IF
      *
           IF    ( SODR11S02-ERRCD               =   SPACE )
               IF    ( SODR11S02-HONKZKKBN =   SPACE )
                   MOVE    "8019"      TO  SODR11S02-ERRCD
               END-IF
           END-IF
      *
           IF    ( SODR11S02-ERRCD           =   SPACE )
               IF    ( SODR11S02-NUM   =   SPACE )
                   MOVE    "8020"      TO  SODR11S02-ERRCD
               END-IF
           END-IF
      *
           IF    ( SODR11S02-ERRCD           =   SPACE )
               MOVE    SPACE           TO  WRK-STR
               STRING  SODR11S02-KIGO  DELIMITED BY SPACE
                       SODR11S02-NUM   DELIMITED BY SPACE
               INTO    WRK-STR
               END-STRING
               PERFORM 800-LENGTH-CHK-SEC
               IF    ( WRK-LENGTH  >   38 )
                   MOVE    "8021"      TO  SODR11S02-ERRCD
               END-IF
           END-IF
      *
           .
       2001-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ���ѥ����å�
      *****************************************************************
       800-ZENKAKU-CHK-SEC              SECTION.
      *
           MOVE    ZERO            TO  FLG-ENTRYCHK
      *
           IF  ( WRK-KANACHK-STR   =   SPACE )
               CONTINUE
           ELSE
               MOVE    SPACE           TO  ORCSKANACHKAREA
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"             TO  KANACHK-SYORI
               MOVE    WRK-KANACHK-STR TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT
                                       TO  WRK-KANACHK-STR
               IF      ( KANACHK-RC    =   ZERO )
      *            ���ѤǤʤ����ϥ��顼�Ȥ��롣
                   IF      ( KANACHK-SYUBETU   NOT =   2 )
                       MOVE    1       TO  FLG-ENTRYCHK
                   END-IF
               ELSE
                       MOVE    1       TO  FLG-ENTRYCHK
               END-IF
           END-IF
      *
           .
       800-ZENKAKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ʸ����Ĺ�����å�
      *****************************************************************
       800-LENGTH-CHK-SEC              SECTION.
      *
           MOVE    1               TO  WRK-LENGTH
           MOVE    SPACE           TO  WRK-TMP
      *
           STRING  WRK-STR DELIMITED   BY  SPACE
           INTO    WRK-TMP
           WITH POINTER  WRK-LENGTH
           END-STRING
           COMPUTE WRK-LENGTH = WRK-LENGTH -   1
      *
           .
       800-LENGTH-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��Ծ��󸡺�����
      *****************************************************************
       900-HKNJAINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNJAINF
      *
      *    �ݸ����ֹ桡���������å�
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    SODR11S02-HKNJANUM  TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNJAINF-REC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNJAINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ֹ渡������
      *****************************************************************
       900-HKNNUM-KEY4-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUM
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    SODR11S02-HKNJANUM(1:2)
                                   TO  HKN-HBTNUM
           MOVE    SODR11S02-SRYYMD TO HKN-TEKSTYMD
           MOVE    SODR11S02-SRYYMD TO HKN-TEKEDYMD
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNNUM-REC
           ELSE
               INITIALIZE              HKNNUM-REC
                MOVE    1          TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-KEY4-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ֹ�FETCH����
      *****************************************************************
       900-HKNNUM-KEY4-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUM
      *
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key4"          TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNNUM-REC
           ELSE
               INITIALIZE              HKNNUM-REC
                MOVE    1          TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-KEY4-FET-EXT.
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
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.