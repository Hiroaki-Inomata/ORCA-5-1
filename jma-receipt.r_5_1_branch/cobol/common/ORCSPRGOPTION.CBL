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
       PROGRAM-ID.             ORCSPRGOPTION.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �ץ�����४�ץ����ѥ�᥿����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  08/02/22    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
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
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PRGOPTION       PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-CHK2            PIC 9(01).
      *
           03  FLG-CHKEND           PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(05).
           03  IDX2                PIC 9(05).
           03  IDX3                PIC 9(05).
           03  IDX4                PIC 9(05).
           03  IDY                 PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-MAX             PIC 9(05).
           03  WRK-PARA            PIC X(100).
           03  WRK-OPTION          PIC X(44000).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ץ�����४�ץ����
       01  PRGOPTION-REC.
           COPY  "CPPRGOPTION.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���̥ѥ�᥿
           COPY    "CPORCSPRGOPTION.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSPRGOPTIONAREA
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
      *
           INITIALIZE                  ORCSPRGOPTION-OUT-G
      *
      *    ����Խ�����
           PERFORM 100-SYORI-SEC
      *    �����Խ�
           IF      ORCSPRGOPTION-RC    =   ZERO
               PERFORM 200-MAIN-SYORI-SEC
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ����Խ�����
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    �ѥ�᥿���
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    ORCSPRGOPTION-IN-DATA
                                       TO  KANACHK-MAE-INPUT
           MOVE    30                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAX         TO  WRK-MAX
      *
      *    �ץ�����४�ץ���󸡺�
           PERFORM 1001-PRGOPTION-SET-SEC
           .
       100-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    �ץ�����४�ץ���󸡺�����
      *****************************************************************
       1001-PRGOPTION-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  PRGOPTION-REC
           INITIALIZE                      PRGOPTION-REC
           MOVE    SPA-HOSPNUM         TO  PRGOPTION-HOSPNUM
           MOVE    ORCSPRGOPTION-PRGID TO  PRGOPTION-PRGID
           MOVE    "*"                 TO  PRGOPTION-KBNCD
           MOVE    PRGOPTION-REC       TO  MCPDATA-REC
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prgoption"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PRGOPTION-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
           MOVE    "tbl_prgoption"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PRGOPTION       =   1
               MOVE    9               TO  ORCSPRGOPTION-RC
           END-IF
           .
       1001-PRGOPTION-SET-EXT.
           EXIT.
      *****************************************************************
      *    �� ����
      *****************************************************************
       200-MAIN-SYORI-SEC         SECTION.
      *
           MOVE    PRGOPTION-OPTION    TO  WRK-OPTION
           MOVE    SPACE               TO  WRK-PARA
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  FLG-CHKEND
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   44000 )
                          OR  (FLG-OK  NOT =   ZERO   )
                          OR  (FLG-CHKEND  =   1      )
      *        # �ʹߤϥ����ȤȤ���
               IF      WRK-OPTION(IDX:1)       =   "#"
                   MOVE    1                   TO  FLG-CHK
               END-IF
               IF      WRK-OPTION(IDX:1)       =   X"0A"
                   IF      FLG-CHK             =   1
                       MOVE    ZERO                TO  FLG-CHK
                   END-IF
               END-IF
      *!!!
               IF      FLG-CHK         =   ZERO
      *H28.5      ����ǽ�λ
                  IF    ( WRK-OPTION(IDX:)   =   SPACE )
                    OR  ((IDX +  WRK-MAX )   >   44000 )
                      MOVE   1                TO  FLG-CHKEND
                  END-IF
      *
                  IF     FLG-CHKEND       =   ZERO
      *
      *            # ���� ���ԤޤǤ��оݳ��Ȥ���
                   IF      WRK-OPTION(IDX:WRK-MAX)
                                       =   ORCSPRGOPTION-IN-DATA
                                                          (1:WRK-MAX)
                       MOVE    ZERO           TO  FLG-CHK2
      *                �������򤫲��ԤǤ��뤳��
                       COMPUTE IDX4        =   (IDX   -  1 )
                       IF     (IDX4        =   ZERO )
                         OR  ((IDX4        >   ZERO )
                         AND  (WRK-OPTION(IDX4:1)   =   SPACE
                                                OR  X"0A" ))
                           CONTINUE
                       ELSE
                           MOVE    1              TO  FLG-CHK2
                       END-IF
                       IF      FLG-CHK2        =   ZERO
      *
                       COMPUTE IDX2        =   (IDX   +  WRK-MAX)
                       IF      IDX2        >=  44000
                           MOVE    2           TO  FLG-OK
                       ELSE
                           IF      WRK-OPTION(IDX2:1)      =   "="
                               PERFORM 2001-PARA-HEN-SEC
                           END-IF
                       END-IF
                   END-IF
      *
                   END-IF
      *
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-PARA        NOT =   SPACE
      *        �ѥ�᥿���
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    WRK-PARA            TO  KANACHK-MAE-INPUT
               MOVE    100                 TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-PARA
               MOVE    KANACHK-MAX         TO  ORCSPRGOPTION-OT-SIZE
      *        ����
               IF      KANACHK-SYUBETU     =   1
                   MOVE    1               TO  ORCSPRGOPTION-OT-TYPE
               END-IF
      *        Ⱦ��
               IF      KANACHK-SYUBETU     =   2
                   MOVE    2               TO  ORCSPRGOPTION-OT-TYPE
               END-IF
      *        ����
               IF      KANACHK-RC          =   1
                   MOVE    3               TO  ORCSPRGOPTION-OT-TYPE
               END-IF
               MOVE    WRK-PARA            TO  ORCSPRGOPTION-OT-PARA
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    �ץ�����४�ץ����ޥ������ɹ�
      *****************************************************************
       2001-PARA-HEN-SEC           SECTION.
      *
           ADD     1                   TO  IDX2
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX3    FROM   IDX2   BY  1
                   UNTIL      (IDX3        >   44000 )
                          OR  (IDY         >=  100  )
                          OR  (FLG-OK  NOT =   ZERO   )
               IF      WRK-OPTION(IDX3:1)  =   X"0A"
                                           OR  "#"
                   MOVE    1                   TO  FLG-OK
               ELSE
                   ADD     1                   TO  IDY
                   MOVE    WRK-OPTION(IDX3:1)  TO  WRK-PARA(IDY:1)
               END-IF
           END-PERFORM
           .
       2001-PARA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �ץ�����४�ץ����ޥ������ɹ�
      *****************************************************************
       900-PRGOPTION-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRGOPTION-REC
               MOVE    ZERO                TO  FLG-PRGOPTION
           ELSE
               INITIALIZE                      PRGOPTION-REC
               MOVE    1                   TO  FLG-PRGOPTION
           END-IF
      *
           .
       900-PRGOPTION-READ-EXT.
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
      *    �ơ��֥��ɹ�����
      *****************************************************************
       920-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
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
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *