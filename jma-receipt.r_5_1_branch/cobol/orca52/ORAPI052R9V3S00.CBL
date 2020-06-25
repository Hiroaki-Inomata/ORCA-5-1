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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI052R9V3S00.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : �����
      *  ����ݡ��ͥ��̾  : �ޥ�������
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  16/10/31    NACL�����ġ���������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-MOTOPG                  PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-EDTYMD1                 PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY             PIC 9(04).
               05  WRK-DATE-FL1            PIC X(01).
               05  WRK-DATE-MM             PIC 9(02).
               05  WRK-DATE-FL2            PIC X(01).
               05  WRK-DATE-DD             PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH              PIC 9(02).
               05  WRK-HMS-MM              PIC 9(02).
               05  WRK-HMS-SS              PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH             PIC 9(02).
               05  WRK-TIME-FL1            PIC X(01).
               05  WRK-TIME-MM             PIC 9(02).
               05  WRK-TIME-FL2            PIC X(01).
               05  WRK-TIME-SS             PIC 9(02).
      *
      *    �����оݥޥ�������
           COPY    "CPMASTERINF.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ����
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1101.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *    API XML�������
           COPY    "CPAPIV3REQ.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  SCRAREA.
           COPY    "ORCA51SCRAREA.INC".
           COPY    "CPMEV3REQ.INC".
           COPY    "CPMEV3RES.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               MEREQ-AREA
                                               MERES-AREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    �ֵ��ΰ��Խ�
           PERFORM 300-END-SEC
      *
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   MASTERINF-MAX )
      *
               MOVE    MASTERINF-ID (IDX0)
                                   TO  MERES-MASTER-ID (IDX0)
               MOVE    MASTERINF-NAME (IDX0)
                                   TO  MERES-MASTER-NAME (IDX0)
               MOVE    MASTERINF-EXISTS-UPYMD (IDX0)
                                   TO  MERES-EXISTS-UPYMD (IDX0)
      *
      D        DISPLAY IDX0 ":" MERES-MASTER-ID (IDX0)
      D        DISPLAY IDX0 ":" MERES-MASTER-ID (IDX0)
      D        DISPLAY IDX0 ":" MERES-MASTER-NAME (IDX0)
      D        DISPLAY "*"
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                         SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ������������
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO            TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               INITIALIZE              SYSKANRI-REC
               MOVE    1           TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
      *    �ģ¸���������FETCH�Ϥ��ʤ���
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.