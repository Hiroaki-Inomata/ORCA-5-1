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
       IDENTIFICATION                      DIVISION.
       PROGRAM-ID.                         ORCSBTUSENTEI.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : 
      *  ����ݡ��ͥ��̾  : ������������������ô��������
      *  �ֵѥ��顼������  : 00:���99:���顼
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/02/06    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  00.00.01    NACL-��Ʀ��  05/01/14  ���꼫����ô��ۻ��˰��̤�
      *                                     Ϸ�ʹ��Ԥλ���ۤ������ǽ���ѹ�
      *  03.05.00    NACL-����    07/05/09  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                         DIVISION.
       CONFIGURATION                       SECTION.
       INPUT-OUTPUT                        SECTION.
       FILE-CONTROL.
      *
      *
       DATA                                DIVISION.
      *FILE                                SECTION.
      *
      *
       WORKING-STORAGE                     SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-ERR                     PIC 9(01).
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTNYUINRRK              PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-YMD                     PIC X(10). 
           03  WRK-HENYMDG                 PIC X(09).
           03  WRK-SYMD.
               05  WRK-SYY                 PIC 9(04).
               05  WRK-SMM                 PIC 9(02).
               05  WRK-SDD                 PIC 9(02).
           03  WRK-STYMD                   PIC X(08).
           03  WRK-EDYMD                   PIC X(08).
           03  WRK-NISSU1                  PIC 9(05).
           03  WRK-NISSU2                  PIC 9(05).
           03  WRK-MMSTYMD.
               05  WRK-MMSTYM              PIC X(06).
               05  WRK-MMSTDD              PIC 9(02).
           03  WRK-MMEDYMD.
               05  WRK-MMEDYM              PIC X(06).
               05  WRK-MMEDDD              PIC 9(02).
           03  WRK-STIDX                   PIC 9(02).
           03  WRK-EDIDX                   PIC 9(02).
           03  WRK-CHK-BTUNUM              PIC X(02).
           03  WRK-CHK-YMD                 PIC X(08).
           03  WRK-SRYYMD.
               05  WRK-SRYYM               PIC X(06).
               05  WRK-SRYDD               PIC 9(02).
      *
       01  SRH-5001-AREA.
           03  SRH-5001-OCC                OCCURS  300
                                           INDEXED BY SRH-5001-IDX.
               05  SRH-5001-BTUNUM         PIC X(02).
               05  SRH-5001-STYUKYMD       PIC X(08).
               05  SRH-5001-EDYUKYMD       PIC X(08).
               05  SRH-5001-FTNKBN         PIC X(01).
               05  SRH-5001-TAXKBN         PIC X(01).
               05  SRH-5001-TANKA          PIC 9(02).
               05  SRH-5001-KINGAKU        PIC 9(04).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *    ����������
           COPY    "CPSK5001.INC".
      *
      *    ������������
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
           COPY    "CPORCSGDAY.INC".
      *    �ݸ�������ǯ�𡦳��׻�����
           COPY    "CPORCSAGECHK.INC".
      *
      *    �����ΰ�
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                             SECTION.
      *
           COPY    "CPORCSBTUSENTEI.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                           DIVISION    USING
           ORCSBTUSENTEIAREA
           SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                        SECTION.
      *
           INITIALIZE                      WRK-AREA
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      SBTUSENTEI-OUT-AREA
           INITIALIZE                      SBTUSENTEI-RC
      *
           MOVE    ZERO                TO  FLG-ERR
      *
      *    �ѥ�᡼�������å�
           PERFORM 100-PRM-CHK-SEC
           IF    ( SBTUSENTEI-RC        NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
      *    �����
           PERFORM 200-MAIN-SEC
           IF    ( SBTUSENTEI-RC        NOT =   ZERO )
               GO  TO  000-PROC-EXT
           END-IF
      *
           .
       000-PROC-EXT.
           EXIT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    �ѥ�᡼�������å�
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
           IF    ( SBTUSENTEI-SRYYM    =   SPACE )
               MOVE    01              TO  SBTUSENTEI-RC
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           MOVE    SBTUSENTEI-SRYYM    TO  WRK-YMD
           MOVE    "01"                TO  WRK-YMD (7:2)
           PERFORM 800-HIZUKE-CHK-SEC
           IF    ( SBTUSENTEI-RC       NOT =   ZERO )
               GO  TO  100-PRM-CHK-EXT
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                        SECTION.
      *
      *    ������Խ�
           MOVE    SBTUSENTEI-SRYYM    TO  WRK-MMSTYMD
           MOVE    "01"                TO  WRK-MMSTYMD (7:2)
      *
      *    ��������������
           MOVE    SBTUSENTEI-SRYYM    TO  WRK-YMD
           PERFORM 2100-GETUMATU-GET-SEC
           MOVE    WRK-YMD             TO  WRK-MMEDYMD
      *
      *
           PERFORM 900-PTNYUINRRK-KEY41-SEL-SEC
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM  910-DBCLOSECURSOR-SEC
      *
      *    �������Ԥ�Ϸ��Ƚ��
           INITIALIZE   ORCSAGECHKAREA
           MOVE    SBTUSENTEI-HOSPNUM  TO  AGECHK-HOSPNUM
           MOVE    SBTUSENTEI-PTID     TO  AGECHK-PTID
           MOVE    WRK-MMSTYMD         TO  AGECHK-SRYYMD
           IF    ( WRK-MMSTYMD         <   PTNYUINRRK-NYUINYMD )
               MOVE    PTNYUINRRK-NYUINYMD
                                       TO  AGECHK-SRYYMD
           END-IF
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      ( AGECHK-RC    =    ZERO )
               CONTINUE
           ELSE
               INITIALIZE    ORCSAGECHKAREA
           END-IF
      *    ����������
           PERFORM 2100-SYS-5001-GET-SEC
           IF    ( SBTUSENTEI-RC       NOT =   ZERO )
               DISPLAY  "SBTUSENTEI-PTID1 = " SBTUSENTEI-PTID
               DISPLAY  "SBTUSENTEI-RC1 = " SBTUSENTEI-RC
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    ���������Խ�����
           PERFORM 2100-PTNYUINRRK-EDIT-SEC
           IF    ( SBTUSENTEI-RC       NOT =   ZERO )
               GO  TO  200-MAIN-EXT
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��������������
      *****************************************************************
       2100-GETUMATU-GET-SEC           SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY1-AREA
           MOVE    "71"                TO   LNK-DAY7-IRAI
           MOVE    WRK-YMD (1:6)       TO   LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                       LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-YMD
      *
           .
       2100-GETUMATU-GET-EXT.
            EXIT.
      *****************************************************************
      *    ����������
      *****************************************************************
       2100-SYS-5001-GET-SEC               SECTION.
      *
           INITIALIZE                      SRH-5001-AREA
      *
           INITIALIZE                      SYSKANRI-REC
           MOVE    SBTUSENTEI-HOSPNUM  TO  SYS-HOSPNUM
           MOVE    "5001"              TO  SYS-KANRICD
           MOVE    WRK-MMEDYMD         TO  SYS-STYUKYMD
           MOVE    WRK-MMSTYMD         TO  SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY2-SEL-SEC
           IF    ( FLG-SYSKANRI         =  ZERO )
               MOVE    SYSKANRI-REC    TO  SYS-5001-REC
           ELSE
               INITIALIZE                  SYS-5001-REC
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                   OR    ( IDX1    >       300 )
      *
               MOVE    SYS-5001-KBNCD      TO  SRH-5001-BTUNUM  (IDX1)
               MOVE    SYS-5001-STYUKYMD   TO  SRH-5001-STYUKYMD(IDX1)
               MOVE    SYS-5001-EDYUKYMD   TO  SRH-5001-EDYUKYMD(IDX1)
      *
               EVALUATE SYS-5001-SENTEI-FTNKBN
               WHEN    "1"
                   MOVE    SYS-5001-SENTEI-FTNKBN
                                           TO  SRH-5001-FTNKBN (IDX1)
                   MOVE    SYS-5001-SENTEI-TANKA
                                           TO  SRH-5001-TANKA  (IDX1)
                   MOVE    SYS-5001-SENTEI-TAXKBN
                                           TO  SRH-5001-TAXKBN (IDX1)
                   MOVE     ZERO           TO  SRH-5001-KINGAKU(IDX1)
               WHEN    "2"
                   MOVE    SYS-5001-SENTEI-FTNKBN
                                           TO  SRH-5001-FTNKBN (IDX1)
                   MOVE    ZERO            TO  SRH-5001-TANKA  (IDX1)
                   MOVE    SYS-5001-SENTEI-TAXKBN
                                           TO  SRH-5001-TAXKBN (IDX1)
                   IF   (SYS-5001-SENTEI-KINGAKU-X NOT = SPACE) AND
                        (SYS-5001-SENTEI-KINGAKU-X NOT = "0000")
                       MOVE    SYS-5001-SENTEI-KINGAKU
                                           TO  SRH-5001-KINGAKU(IDX1)
                   ELSE
                       IF   AGECHK-ROUJIN2    =   0
                           IF  SYS-5001-SENTEI-IPN-KINGAKU-X  =  SPACE
                               MOVE   ZERO TO  SRH-5001-KINGAKU(IDX1)
                           ELSE
                               MOVE    SYS-5001-SENTEI-IPN-KINGAKU
                                           TO  SRH-5001-KINGAKU(IDX1)
                           END-IF
                       ELSE
                           IF  SYS-5001-SENTEI-RJN-KINGAKU-X  =  SPACE
                               MOVE   ZERO TO  SRH-5001-KINGAKU(IDX1)
                           ELSE
                               MOVE    SYS-5001-SENTEI-RJN-KINGAKU
                                           TO  SRH-5001-KINGAKU(IDX1)
                           END-IF
                       END-IF
                   END-IF
               WHEN    OTHER
                   MOVE    "1"             TO  SRH-5001-FTNKBN (IDX1)
                   MOVE     10             TO  SRH-5001-TANKA  (IDX1)
                   MOVE     "0"            TO  SRH-5001-TAXKBN (IDX1)
                   MOVE     ZERO           TO  SRH-5001-KINGAKU(IDX1)
               END-EVALUATE
      *
               PERFORM 900-SYSKANRI-KEY2-FET-SEC
               IF    ( FLG-SYSKANRI         =  ZERO )
                   MOVE    SYSKANRI-REC    TO  SYS-5001-REC
               ELSE
                   INITIALIZE                  SYS-5001-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM  910-DBCLOSECURSOR-SEC
      *
           .
       2100-SYS-5001-GET-EXT.
           EXIT.
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       2100-PTNYUINRRK-EDIT-SEC            SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY41-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTNYUINRRK  NOT =   ZERO )
      *
      *        ��ñ�̤��������Ƥ�������ξ�����Խ�����
               PERFORM 2100-DAY-BTUINF-EDIT-SEC
               IF    ( SBTUSENTEI-RC       NOT =   ZERO )
                   DISPLAY "SBTUSENTEI-PTID2 = " SBTUSENTEI-PTID
                   DISPLAY "SBTUSENTEI-RC2 = " SBTUSENTEI-RC
                   MOVE    1               TO  FLG-PTNYUINRRK
               END-IF
      *
               IF    ( FLG-PTNYUINRRK      =   ZERO )
                   PERFORM 900-PTNYUINRRK-KEY41-FET-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM  910-DBCLOSECURSOR-SEC
      *
           .
       2100-PTNYUINRRK-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ������������Խ�����
      *****************************************************************
       2100-DAY-BTUINF-EDIT-SEC             SECTION.
      *
           PERFORM VARYING IDX2    FROM    WRK-MMSTDD  BY  1
                   UNTIL ( IDX2    >   WRK-MMEDDD )
      *
               MOVE    SBTUSENTEI-SRYYM    TO  WRK-SRYYM
               MOVE    IDX2                TO  WRK-SRYDD
      *
               IF    ( PTNYUINRRK-TENNYUYMD    <=    WRK-SRYYMD )
                AND  ( PTNYUINRRK-TENSTUYMD    >=    WRK-SRYYMD )
      *
      *            ���������������������
                   PERFORM 2100-SENTEI-SET-SEC
                   IF    ( SBTUSENTEI-RC       NOT =   ZERO )
                       GO  TO  2100-DAY-BTUINF-EDIT-EXT
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2100-DAY-BTUINF-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ���������������������
      *****************************************************************
       2100-SENTEI-SET-SEC                 SECTION.
      *
           MOVE    PTNYUINRRK-BTUNUM   TO  WRK-CHK-BTUNUM
           MOVE    SBTUSENTEI-SRYYM    TO  WRK-CHK-YMD
           MOVE    IDX2 (4:2)          TO  WRK-CHK-YMD (7:2)
      *
           SET     SRH-5001-IDX        TO  1
           SEARCH  SRH-5001-OCC        VARYING SRH-5001-IDX
           AT      END
               MOVE    02              TO  SBTUSENTEI-RC
               GO  TO  2100-SENTEI-SET-EXT
           WHEN  ( SRH-5001-BTUNUM (SRH-5001-IDX)
                                       =   SPACE )
               MOVE    03              TO  SBTUSENTEI-RC
               GO  TO  2100-SENTEI-SET-EXT
           WHEN  ( SRH-5001-BTUNUM (SRH-5001-IDX)
                                       =   WRK-CHK-BTUNUM )
            AND  ( SRH-5001-STYUKYMD (SRH-5001-IDX)
                                       <=  WRK-CHK-YMD )
            AND  ( SRH-5001-EDYUKYMD (SRH-5001-IDX)
                                       >=  WRK-CHK-YMD )
                                       
               MOVE    SRH-5001-BTUNUM (SRH-5001-IDX)
                                       TO  SBTUSENTEI-DAY-BTUNUM (IDX2)
               MOVE    SRH-5001-FTNKBN (SRH-5001-IDX)
                                       TO  SBTUSENTEI-DAY-FTNKBN (IDX2)
               MOVE    SRH-5001-TAXKBN (SRH-5001-IDX)
                                       TO  SBTUSENTEI-DAY-TAXKBN (IDX2)
               MOVE    SRH-5001-TANKA  (SRH-5001-IDX)
                                       TO  SBTUSENTEI-DAY-TANKA  (IDX2)
               MOVE    SRH-5001-KINGAKU(SRH-5001-IDX)
                                       TO  SBTUSENTEI-DAY-KINGAKU(IDX2)
           END-SEARCH
      *
           .
       2100-SENTEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    ���ե����å����Խ�����
      *****************************************************************
       800-HIZUKE-CHK-SEC                  SECTION.
      *
           MOVE    SPACE                   TO  WRK-HENYMDG
                                               WRK-SYMD
      *
           IF    ( WRK-YMD                 =   SPACE )
               GO  TO  800-HIZUKE-CHK-EXT
           END-IF
      *
           INITIALIZE                          ORCSGDAYAREA
           MOVE    WRK-YMD                 TO  SGDAY-INDAY
           CALL    "ORCSGDAY"                  USING
                                               ORCSGDAYAREA
           IF  ( SGDAY-RC                  =   ZERO )
               MOVE    SGDAY-OTDAY         TO  WRK-HENYMDG
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    04                  TO  SBTUSENTEI-RC
           END-IF
           .
       800-HIZUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���ֻ��н���
      *****************************************************************
       800-KIKAN-CALC-SEC                  SECTION.
      *
           INITIALIZE                      WRK-NISSU1
                                           WRK-NISSU2
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY5-AREA
           MOVE    "51"                TO  LNK-DAY5-IRAI
           MOVE    WRK-STYMD           TO  LNK-DAY5-START
           MOVE    WRK-EDYMD           TO  LNK-DAY5-END
           CALL    "ORCSDAY"               USING
                                           STS-AREA-DAY
                                           LNK-DAY5-AREA
           IF    ( STS-DAY-RC1     NOT =   ZERO )
               MOVE    05              TO  SBTUSENTEI-RC
               GO  TO                      800-KIKAN-CALC-EXT
           END-IF
      *
           MOVE    LNK-DAY5-NISSU1     TO  WRK-NISSU1
           MOVE    LNK-DAY5-NISSU2     TO  WRK-NISSU2
      *
           .
       800-KIKAN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ������������(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE     SYSKANRI-REC       TO  MCPDATA-REC
           PERFORM  910-DBSELECT-SEC
           IF    ( MCP-RC           =  ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ����FETCH����(KEY2)
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM  910-DBFETCH-SEC
           IF    ( MCP-RC           =  ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    �����������򸡺�����(KEY11)
      *****************************************************************
       900-PTNYUINRRK-KEY41-SEL-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE  PTNYUINRRK-REC
           MOVE    SBTUSENTEI-HOSPNUM  TO  PTNYUINRRK-HOSPNUM
           MOVE    SBTUSENTEI-PTID     TO  PTNYUINRRK-PTID
           MOVE    WRK-MMEDYMD         TO  PTNYUINRRK-TENNYUYMD
           MOVE    WRK-MMSTYMD         TO  PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY41-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ������������FETCH����(KEY11)
      *****************************************************************
       900-PTNYUINRRK-KEY41-FET-SEC    SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key41"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           .
       900-PTNYUINRRK-KEY41-FET-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC                     SECTION.
      *
           MOVE    "DBSELECT"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
           IF    ( MCP-RC          =    ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                     SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL "ORCDBMAIN" USING MCPAREA
                                  MCPDATA-REC
grpsys                            SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.