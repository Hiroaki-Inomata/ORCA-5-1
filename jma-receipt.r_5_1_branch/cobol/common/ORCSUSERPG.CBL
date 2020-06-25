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
       PROGRAM-ID.             ORCSUSERPG.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �桼���ץ�����൯ư����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/09/01    NACL��¿��Ǽ  ��������
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
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
      *    ����ΰ�
      *01  WRK-AREA.
      *
      *    �桼���ץ�������ѥ������scripts/�ʲ��ʤ�ɤ��Ǥ�褤��
       01  WRK-SHELL-USERPG.
           03  FILLER             PIC X(08) VALUE
                                             "scripts/".
           03  WRK-SHELL-SHELL    PIC X(100).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY  "CPSYSKANRI.INC".
      *    �桼���ץ�����൯ư
           COPY  "CPSK9700.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
           COPY    "CPMKPASS.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *
      **** COPY    "ORCA-DBPATH".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �桼���ץ�����൯ư���֥ѥ�᥿
           COPY    "CPORCSUSERPG.INC".
grpsys*
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCSUSERPGAREA
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
      *
      *    �桼���ץ�����൯ư
           PERFORM 100-SHELL-SYORI-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �桼���ץ�����൯ư����
      *****************************************************************
       100-SHELL-SYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    ORCSUSERPG-SHELL    TO  WRK-SHELL-SHELL
      *    �ե�����ǥ��쥯�ȥ���ֻ���
           INITIALIZE                      ORCSMKPASSAREA
           MOVE    WRK-SHELL-USERPG    TO  MKPASS-IN-01
           CALL    "ORCSMKPASSSITE"    USING   ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    ORCSUSERPG-REPOS-NAME
                                       TO  SHELLTBL-REPOS-NAME
      *
           DISPLAY "@@REPOS-NAME="  SHELLTBL-REPOS-NAME
      *
      *    �ѥ�᥿���å�
      *
           MOVE    ORCSUSERPG-GMNPG    TO  SHELLTBL-ARG1
           MOVE    ORCSUSERPG-SYORI-FLG
                                       TO  SHELLTBL-ARG2
           MOVE    ORCSUSERPG-SYSYMD   TO  SHELLTBL-ARG3
           MOVE    ORCSUSERPG-SRYYMD   TO  SHELLTBL-ARG4
           MOVE    ORCSUSERPG-PTID     TO  SHELLTBL-ARG5
           MOVE    ORCSUSERPG-PTNUM    TO  SHELLTBL-ARG6
           MOVE    ORCSUSERPG-SRYKA    TO  SHELLTBL-ARG7
           MOVE    ORCSUSERPG-DRCD     TO  SHELLTBL-ARG8
           MOVE    ORCSUSERPG-HKNCOMBI TO  SHELLTBL-ARG9
           MOVE    ORCSUSERPG-SYORINM  TO  SHELLTBL-ARG10
           MOVE    ORCSUSERPG-PRTNM    TO  SHELLTBL-ARG11
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG12
      *    ��ɼ�ֹ�
           MOVE    ORCSUSERPG-DENPNUM  TO  SHELLTBL-ARG13
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "shell"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
            .
       100-SHELL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       990-SYSKANRI-READ-SEC                SECTION.
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