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
       PROGRAM-ID.             ORCHCN03S03.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�
      *  ����ݡ��ͥ��̾  : �������μ�����������Խ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  2013/06/03  NACL������   ��������
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
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDXD                        PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-SRYYMD.
               05  WRK-SRYYM               PIC X(06).
               05  WRK-SRYDD               PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-SYU-MAX               PIC 9(03)   VALUE 10.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ��Ǽ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    �ݸ��ȹ礻
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                        SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
           COPY    "CPORCHCN03S03.INC".
      *
       PROCEDURE                       DIVISION    USING
                                       SPA-AREA
                                       ORCHCN03S03AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
      *                                WRK-AREA
                                       ORCHCN03S03-OT
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           MOVE    "99999999"          TO  ORCHCN03S03-SKYSTYMD
           MOVE    "00000000"          TO  ORCHCN03S03-SKYEDYMD
      *
      *    ��������Խ�����
           PERFORM 2001-SEIKYU-KIKAN-EDIT-SEC
      *
           IF    ( ORCHCN03S03-SYUSTYMD    >   ORCHCN03S03-SKYSTYMD )
               MOVE    ORCHCN03S03-SYUSTYMD
                                           TO  ORCHCN03S03-SKYSTYMD
           END-IF
      *
           IF    ( ORCHCN03S03-SYUEDYMD    <   ORCHCN03S03-SKYEDYMD )
               MOVE    ORCHCN03S03-SYUEDYMD
                                           TO  ORCHCN03S03-SKYEDYMD
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������Խ�����
      *****************************************************************
       2001-SEIKYU-KIKAN-EDIT-SEC      SECTION.
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   CONST-SYU-MAX )
      *
               IF    ( ORCHCN03S03-DENPNUM (IDXD)  NOT =   ZERO )
                   PERFORM 20011-SEIKYU-KIKAN-EDIT-SEC
               END-IF
      *
           END-PERFORM
      *
           .
       2001-SEIKYU-KIKAN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ���Ųʡ��ݸ��ȹ礻�̽��׽���
      *****************************************************************
       20011-SEIKYU-KIKAN-EDIT-SEC     SECTION.
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
      *    PERFORM 900-HKNCOMBI-KEY-SEL-SEC
      *    IF    ( FLG-HKNCOMBI    =   ZERO )
      *        IF    ( COMB-TEKSTYMD   <   ORCHCN03S03-SKYSTYMD )
      *            MOVE    COMB-TEKSTYMD   TO  ORCHCN03S03-SKYSTYMD
      *        END-IF
      *
      *        IF    ( COMB-TEKEDYMD   >   ORCHCN03S03-SKYEDYMD )
      *            MOVE    COMB-TEKEDYMD   TO  ORCHCN03S03-SKYEDYMD
      *        END-IF
      *    END-IF
      *
           MOVE    SYU-SRYYMD          TO  WRK-SRYYMD
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
      *
             IF  ( SYU-DAY (IDX0)  >   ZERO )
      *
                 MOVE    IDX0              TO  WRK-SRYDD
      *
                 IF    ( WRK-SRYYMD    <   ORCHCN03S03-SKYSTYMD )
                     MOVE    WRK-SRYYMD    TO  ORCHCN03S03-SKYSTYMD
                 END-IF
      *
                 IF    ( WRK-SRYYMD    >   ORCHCN03S03-SKYEDYMD )
                     MOVE    WRK-SRYYMD    TO  ORCHCN03S03-SKYEDYMD
                 END-IF
      *
             END-IF
      *
           END-PERFORM
      *
           .
       20011-SEIKYU-KIKAN-EDIT-EXT.
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
      *
      *****************************************************************
      *    ��Ǽ�ơ��֥븡������
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    ORCHCN03S03-PTID    TO  SYU-PTID
           MOVE    ORCHCN03S03-DENPNUM (IDXD)
                                       TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-SYUNOU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ȹ礻��������(KEY)
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SYU-PTID            TO  COMB-PTID
           MOVE    SYU-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
            ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
            END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
           .
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
           .