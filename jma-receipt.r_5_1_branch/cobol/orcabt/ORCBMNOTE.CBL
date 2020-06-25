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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBMNOTE.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ģ�ǡ�������
      *  ����ݡ��ͥ��̾  : ������Ģ�ǡ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  13/07/12    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ��¸�ѥǡ���
           SELECT  SAVE-FILE       ASSIGN  ASS-SAVE
                                   ORGANIZATION    IS  LINE SEQUENTIAL
                                   FILE    STATUS  IS  STS-SAVE.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    ��¸�ѥǡ���
       FD  SAVE-FILE.
       01  SAVE-REC                PIC X(1000).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //ASS-SAVE//.
      *
       01  STS-AREA.
           03  STS-SAVE            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SAVE            PIC 9(01).
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ������Ģ���極��
           COPY    "CPORCSMNOTE.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *
       LINKAGE                     SECTION.
           COPY    "CPCOMMAND-PARAM.INC".
      *
      *****************************************************************
       PROCEDURE                   DIVISION
                                   USING
                                   COMMAND-PARAM
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
      *
       000-PROC-EXT.
           STOP    RUN.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE              STS-AREA
                                   FLG-AREA
                                   SPA-AREA
                                   ORCSMNOTEAREA
                                   ASS-SAVE
      *
           UNSTRING    COMMAND-PARAM   DELIMITED   BY  ","
           INTO                        SPA-TERMID
                                       SPA-HOSPNUM
                                       SMNOTE-MODE
                                       SMNOTE-TBL-KEY
                                       SMNOTE-RENNUM
                                       SMNOTE-TBL-GROUP
                                       SMNOTE-SHORI-RENNUM
                                       SMNOTE-SHELLID
                                       SMNOTE-SRYYM
                                       SMNOTE-SRYYMD
                                       SMNOTE-PUTFLG
                                       SMNOTE-NYUGAIKBN
                                       SMNOTE-PTID
                                       SMNOTE-DRCD
                                       SMNOTE-TO-FOLDER
                                       SMNOTE-TO-DATA
                                       SMNOTE-CODE-TYPE
                                       SMNOTE-TITLE
                                       ASS-SAVE
                                       SMNOTE-TBL-UUID
                                       SMNOTE-GYOUMU-CD
                                       SMNOTE-LOCATION-CD
                                       SMNOTE-DATA-VERSION
                                       SPA-OPID
           END-UNSTRING
      *
           PERFORM 100-DBOPEN-SEC
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    ASS-SAVE            TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ASS-SAVE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    INPUT           SAVE-FILE
      *
           PERFORM 900-SAVE-READ-SEC
      *
           PERFORM UNTIL ( FLG-SAVE    NOT =  ZERO )
      *
               MOVE    SAVE-REC        TO  SMNOTE-CSVDATA
      *
               CALL    "ORCSCTRLMNOTE"     USING
                                           ORCSMNOTEAREA
                                           SPA-AREA
      *
               PERFORM 900-SAVE-READ-SEC
      *
           END-PERFORM
      *
           CLOSE   SAVE-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    ��¸�ѥǡ����ɹ�����
      *****************************************************************
       900-SAVE-READ-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-SAVE
      *
           READ    SAVE-FILE
           AT  END
               MOVE    1           TO  FLG-SAVE
           END-READ
           IF    ( STS-SAVE    =   "00" OR "10" )
               CONTINUE
           ELSE
               DISPLAY "READ ERROR[" STS-SAVE "]" ASS-SAVE
               MOVE    1           TO  FLG-SAVE
           END-IF
      *
           .
       900-SAVE-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¡������ץ����
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBSTART"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡����ǽ���
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    LOW-VALUE       TO  MCP-TABLE
                                       MCP-PATHNAME
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.