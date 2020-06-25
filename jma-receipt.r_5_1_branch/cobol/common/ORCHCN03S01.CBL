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
       PROGRAM-ID.                     ORCHCN03S01.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �����������μ����������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/02/09    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����    07.05.17  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *    �����ѥǡ���
           SELECT  IN01-FILE   ASSIGN  ASGN0001
                               ORGANIZATION    IS  LINE SEQUENTIAL 
                               FILE    STATUS  IS  STS-IN01.
      *
      *    �����ѥǡ���
           SELECT  PRT-FILE    ASSIGN  ASGN0002
                               ORGANIZATION    IS  SEQUENTIAL 
                               FILE    STATUS  IS  STS-PARA.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       FD  IN01-FILE.
       01  IN01-REC.
           03  IN01-SRYYMD             PIC X(08).
           03  IN01-PTID               PIC 9(10).
           03  IN01-PRTID              PIC X(30).
           03  IN01-PRTNM              PIC X(20).
           03  IN01-SITEKBN            PIC X(01).
           03  IN01-PRTDATA            PIC X(10000).
      *
      *    �����ѥǡ���
       FD  PRT-FILE.
       01  PRT-REC                     PIC X(20000).
      *
       WORKING-STORAGE                 SECTION.
      *
           COPY    "CPCOMMONPRT.INC"   REPLACING   //HC01PARA//
                                       BY          //HC03TEMP//.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //ASGN0001//.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //ASGN0002//.
      *
      *    ���������ΰ�
           COPY    "CPSHELLTBL.INC".
      *
      *    ���ơ������ΰ�
       01  STS-AREA.
           03  STS-IN01                PIC X(02).
           03  STS-PARA                PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-IN01                PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX1                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-REDFILE             PIC X(20).
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //WRK-SPA-//.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "CPSK1044.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *    �ե�����ǥ��쥯�ȥ���ֻ��ꥵ��
           COPY    "CPMKPASS.INC".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *    �����ģ����極��
           COPY    "CPORCSPRT.INC".
      *
           COPY  "CPORCSMKPRT.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "CPORCHCN03.INC".
           COPY    "CPASGNPARA.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           ORCHCN03AREA
           ASGNPARA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       STS-AREA
                                       WRK-AREA
                                       IDX-AREA
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 200-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    ASGNPARA        TO  ASGN0001
      *
           INITIALIZE                  HC03TEMP
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    ASGNPARA        TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)(SGETTEMP-ST :)
                                   TO  HC03TEMP-BASENAME
      *
           COMPUTE HC03TEMP-CNT    =   HC03TEMP-CNT    +   1
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
           IF    ( ORCHCN03-PRTKBN     =   "1" )
               PERFORM 2001-BATCH-PRINT-SEC
           ELSE
               PERFORM 2001-ONLINE-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Хå����Ͻ���
      *****************************************************************
       2001-BATCH-PRINT-SEC            SECTION.
      *
           MOVE    ZERO                    TO  ORCHCN03-OUTCNT
      *
           OPEN    INPUT   IN01-FILE
      *
           PERFORM 900-IN01-READ-SEC
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
      *
               COMPUTE ORCHCN03-OUTCNT
                   =   ORCHCN03-OUTCNT     +   1
      *
               MOVE    ORCHCN03-PRTAREA    TO  ORCSPRTAREA
               MOVE    IN01-SRYYMD (1:6)   TO  SPRT-SRYYM
               MOVE    IN01-PTID           TO  SPRT-PTID
               MOVE    IN01-PRTID          TO  SPRT-PRTID
               MOVE    IN01-PRTNM          TO  SPRT-PRTNM
               MOVE    IN01-PRTDATA        TO  SPRT-PRTDATA
               MOVE    IN01-SITEKBN        TO  SPRT-SITEKBN
      *
               CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
               IF    ( SPRT-RETURN         =   ZERO )
                   CONTINUE
               ELSE
                   DISPLAY "ORCSPRT ERR !! " SPRT-RETURN
               END-IF
      *
               PERFORM 900-IN01-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   IN01-FILE
      *
           .
       2001-BATCH-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ����饤����Ͻ���
      *****************************************************************
       2001-ONLINE-PRINT-SEC           SECTION.
      *
      *
           OPEN    INPUT   IN01-FILE
      *
           PERFORM 900-IN01-READ-SEC
      *
           PERFORM UNTIL ( FLG-IN01    NOT =   ZERO )
      *
               COMPUTE HC03TEMP-CNT    =   HC03TEMP-CNT    +   1
      *
               INITIALIZE                  SGETTEMP-AREA
               MOVE    HC03TEMP
                                       TO  SGETTEMP-BASENAMES (1)
               CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
               MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASGN0002
      *
               OPEN    OUTPUT  PRT-FILE
      *
               MOVE    IN01-PRTDATA        TO  PRT-REC
      *
               WRITE   PRT-REC
      *
               CLOSE   PRT-FILE
      *
               MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
               MOVE    SPA-SRYYMD          TO  WRK-SPA-SRYYMD
               MOVE    IN01-PTID           TO  WRK-SPA-PTID
      *
               INITIALIZE                  ORCSMKPRTAREA
               MOVE   "HC01.sh"            TO  MKPRT-ID
               MOVE   "�����������μ���"
                                           TO  MKPRT-INFO
               MOVE   IN01-PRTID           TO  MKPRT-DIA
               MOVE   SPACE                TO  MKPRT-DEF
               MOVE   ASGN0002             TO  MKPRT-DAT
               MOVE   IN01-PRTNM           TO  MKPRT-PRTNM
               IF  (  IN01-SITEKBN     =   "1" )
                   CALL   "ORCSMKPRT1"        USING
                                              ORCSMKPRTAREA
                                              WRK-SPA-AREA
                   MOVE   WRK-SPA-PUTTYPE  TO SPA-PUTTYPE
               ELSE
                   CALL   "ORCSMKPRTSITE1"     USING
                                               ORCSMKPRTAREA
                                               WRK-SPA-AREA
                   MOVE   WRK-SPA-PUTTYPE  TO SPA-PUTTYPE
               END-IF
      *
               PERFORM 900-IN01-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   IN01-FILE
      *
           .
       2001-ONLINE-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       200-END-SEC                 SECTION.
      *
           MOVE    ZERO        TO  IF-RESULT
           MOVE    ASGN0001    TO  IF-FILENAME
           CALL    "orcfiledel"    USING
                                   ORCSFDELAREA
      *
           .
       200-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ե��åȼ�������
      *****************************************************************
       800-OFFSET-GET-SEC              SECTION.
      *
           INITIALIZE                  SYS-1044-REC
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    IN01-PRTID          TO  SYS-1044-FORMID
           STRING  IN01-PRTNM          DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
           INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE  MCPDATA-REC       TO  SYS-1044-REC
           ELSE
               INITIALIZE                  SYS-1044-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF   (  SYS-1044-FORMID  =   IN01-PRTID  )
            AND (  SYS-1044-PRTNM   =   IN01-PRTNM  )
               CONTINUE
           ELSE
               INITIALIZE  SYS-1044-REC
           END-IF
      *
           .
       800-OFFSET-GET-EXT.
            EXIT.
      *****************************************************************
      *    �����ѥǡ����ɹ�����
      *****************************************************************
       900-IN01-READ-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-IN01
      *
           READ    IN01-FILE
           AT  END
               MOVE    1           TO  FLG-IN01
           END-READ
      *
           .
       900-IN01-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
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
      *    �ģ¸���������FHETCH�ϹԤ�ʤ�)
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
       911-DBSELECT-EXT.
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
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.