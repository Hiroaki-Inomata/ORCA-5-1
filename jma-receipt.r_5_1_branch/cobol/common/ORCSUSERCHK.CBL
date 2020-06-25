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
       PROGRAM-ID.             ORCSUSERCHK.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �桼���ץ�����൯ưȽ�ꥵ��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/10/07    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      *  04.08.00    NACL-¿��Ǽ  16/06/22  ��ɼ�ֹ��ɲ��б�
      *  05.00.00    NACL-����    17/08/10  Ginbee�б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "ENUM-VALUE".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
           03  FLG-GYOUMU-ARI      PIC 9(01).
           03  FLG-GMN-ARI         PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-KANRICD         PIC X(04).
      *
           03  WRK-GYOUMU-FLG      PIC X(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY  "CPSYSKANRI.INC".
      *    �桼���ץ�����൯ư
           COPY  "CPSK9700.INC".
      *    �桼���ץ�����൯ư
           COPY  "CPSK9701.INC".
           COPY  "CPSK3103.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �桼���ץ�����൯ư���֥ѥ�᥿
           COPY    "CPORCSUSERPG.INC".
      *
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      **** COPY    "ORCA-DBPATH".
      *
      *    ����ե�����̾����
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ݶ�����å�
           COPY    "CPORCSBILLINGCHECK.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �桼���ץ�����൯ư�����å����֥ѥ�᥿
           COPY    "CPORCSUSERCHK.INC".
grpsys*
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSUSERCHKAREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SBC-AREA
      *
           INITIALIZE                  SGETTEMP-AREA
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           IF    ( SGETTEMP-MW     =   SPA-GINBEE )
               CALL    "ORCSBILLINGCHECK"  USING
                                           SBC-AREA
                                           SPA-AREA
           END-IF
      *
           EVALUATE    ORCSUSERCHK-SYORI
               WHEN    "1"
      *            �����å��Τ�
                   PERFORM 100-SYSKANRI-CHK-SEC
                   MOVE    WRK-GYOUMU-FLG  TO  ORCSUSERCHK-GYOUMU-FLG
                   MOVE    FLG-GYOUMU-ARI  TO  ORCSUSERCHK-GYOUMU-ARI
               WHEN    "2"
      *            ����ɽ�������å����¹�
                   PERFORM 100-SYSKANRI-CHK-SEC
                   IF      FLG-GYOUMU-ARI      =   1
      *                �桼���ץ�����൯ư
                       PERFORM 200-SYORNM-SYORI-SEC
                   END-IF
           END-EVALUATE
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �桼���ץ�����൯ư����������
      *****************************************************************
       100-SYSKANRI-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYS-9700-REC
           INITIALIZE                      SYS-9700-REC
           MOVE    "9700"              TO  SYS-9700-KANRICD
           MOVE    "1"                 TO  SYS-9700-KBNCD
           MOVE    "00000000"          TO  SYS-9700-STYUKYMD
           MOVE    "99999999"          TO  SYS-9700-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9700-HOSPNUM
           MOVE    SYS-9700-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           MOVE    SPACE               TO  WRK-GYOUMU-FLG
           MOVE    ZERO                TO  FLG-GYOUMU-ARI
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9700-REC
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   25
                   IF      SYS-9700-GYOUMUID (IDX)
                                           =   ORCSUSERCHK-GMNPG
                       MOVE    SYS-9700-GYOUMU-KANRICD (IDX)
                                               TO  WRK-KANRICD
      *                ����ɽ��
                       MOVE    SYS-9700-GYOUMU-FLG (IDX)
                                               TO  WRK-GYOUMU-FLG
      *                ���٤���������¹�
                       IF      SYS-9700-GYOUMU-CNT (IDX)
                                               >   ZERO
                            MOVE    1              TO  FLG-GYOUMU-ARI
                       END-IF
                       MOVE    25              TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
           IF    ( SGETTEMP-MW         =   SPA-GINBEE )
            AND  ( SBC-BILLINGUSER     =   SPACE )
            AND  ( WRK-KANRICD     NOT =   SPACE )
               MOVE    ZERO            TO  FLG-GYOUMU-ARI
               PERFORM 1001-SYSKANRI-CHK-SEC
           END-IF
      *
           .
       100-SYSKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �桼���ץ�����൯ư����������
      *****************************************************************
       1001-SYSKANRI-CHK-SEC            SECTION.
      *
           MOVE    SPACE               TO  SYS-9701-REC
           INITIALIZE                      SYS-9701-REC
           MOVE    WRK-KANRICD         TO  SYS-9701-KANRICD
           MOVE    "00000000"          TO  SYS-9701-STYUKYMD
           MOVE    "99999999"          TO  SYS-9701-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9701-HOSPNUM
           MOVE    SYS-9701-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM UNTIL    ( FLG-SYSKANRI     NOT =   ZERO )
                    OR      ( FLG-GYOUMU-ARI   NOT =   ZERO )
      *
               MOVE    MCPDATA-REC         TO  SYS-9701-REC
      *
               IF    ( SYS-9701-EXECUTE-SW =   "1" )
      *
                   PERFORM 900-SYS-3103-READ-SEC
                   IF    ( SYS-3103-REPOS-NAME =   SPACE )
                       MOVE    1               TO  FLG-GYOUMU-ARI
                   END-IF
      *
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1001-SYSKANRI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ��̳��ư����
      *****************************************************************
       200-SYORNM-SYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYS-9701-REC
           INITIALIZE                      SYS-9701-REC
           MOVE    WRK-KANRICD         TO  SYS-9701-KANRICD
           MOVE    "00000000"          TO  SYS-9701-STYUKYMD
           MOVE    "99999999"          TO  SYS-9701-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9701-HOSPNUM
           MOVE    SYS-9701-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           PERFORM UNTIL     (FLG-SYSKANRI     =   1 )
               MOVE    MCPDATA-REC         TO  SYS-9701-REC
      *
               PERFORM 900-SYS-3103-READ-SEC
               IF    ( SPA-MW                  =   SPA-GINBEE )
                AND  ( SBC-BILLINGUSER         =   SPACE )
                AND  ( SYS-3103-REPOS-NAME NOT =   SPACE )
                   CONTINUE
               ELSE
                   IF     (SYS-9701-EXECUTE-SW     =   "1" ) AND
                          (SYS-9701-EXECUTE-YTPE   =   "2" )
                       PERFORM 2001-SHELL-SYORI-SEC
                   END-IF
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200-SYORNM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �桼���ץ�����൯ư����
      *****************************************************************
       2001-SHELL-SYORI-SEC             SECTION.
      *
           INITIALIZE                  ORCSUSERPGAREA
      *    �����У�
           MOVE    ORCSUSERCHK-GMNPG   TO  ORCSUSERPG-GMNPG
      *    ������ʬ
           MOVE    ORCSUSERCHK-SYORI-FLG   TO  ORCSUSERPG-SYORI-FLG
      *    �����ƥ���������
           MOVE    ORCSUSERCHK-SYSYMD  TO  ORCSUSERPG-SYSYMD
      *    ������
           MOVE    ORCSUSERCHK-SRYYMD  TO  ORCSUSERPG-SRYYMD
      *    ���ԣɣ�
           MOVE    ORCSUSERCHK-PTID    TO  ORCSUSERPG-PTID
      *    �����ֹ�
           MOVE    ORCSUSERCHK-PTNUM   TO  ORCSUSERPG-PTNUM
      *    ���Ų�
           MOVE    ORCSUSERCHK-SRYKA   TO  ORCSUSERPG-SRYKA
      *    �ɥ�����
           MOVE    ORCSUSERCHK-DRCD    TO  ORCSUSERPG-DRCD
      *    �ݸ��ȹ礻
           MOVE    ORCSUSERCHK-HKNCOMBI    TO  ORCSUSERPG-HKNCOMBI
      *    ��ɼ�ֹ�
           MOVE    ORCSUSERCHK-DENPNUM     TO  ORCSUSERPG-DENPNUM
      *    ������Ģɼ̾
           MOVE    SYS-9701-SYORINM    TO  ORCSUSERPG-SYORINM
      *    �¹�
           MOVE    SYS-9701-SHELL      TO  ORCSUSERPG-SHELL
      *    �ץ��̾
           MOVE    SYS-9701-PRTNM      TO  ORCSUSERPG-PRTNM
      *    ��ݥ��ȥ�̾
           IF    ( SPA-MW                  =   SPA-GINBEE )
            AND  ( SBC-BILLINGUSER         =   "1" )
               MOVE    SYS-3103-REPOS-NAME
                                       TO  ORCSUSERPG-REPOS-NAME
           END-IF
      *
           CALL    "ORCSUSERPG"        USING
                                       ORCSUSERPGAREA
                                       SPA-AREA
      *
           .
       2001-SHELL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       900-SYS-3103-READ-SEC                SECTION.
      *
           MOVE    SPACE               TO  SYS-3103-REC
           INITIALIZE                      SYS-3103-REC
           MOVE    "3103"              TO  SYS-3103-KANRICD
           MOVE    SYS-9701-KANRICD    TO  SYS-3103-KBNCD
           MOVE    "0"                 TO  SYS-3103-KBNCD(5:1)
           MOVE    SYS-9701-KBNCD(1:2) TO  SYS-3103-KBNCD(6:2)
           MOVE    SPA-SYSYMD          TO  SYS-3103-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-3103-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3103-HOSPNUM
           MOVE    SYS-3103-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    SPACE               TO  SYS-3103-REC
           INITIALIZE                      SYS-3103-REC
           IF  MCP-RC              =   ZERO
               PERFORM 990-SYSKANRI-READ-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    MCPDATA-REC TO  SYS-3103-REC
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYS-3103-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       990-SYSKANRI-READ-EXT.
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