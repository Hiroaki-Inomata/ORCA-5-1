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
       PROGRAM-ID.             ORCBMNT01.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ޥ�������
      *  ����ݡ��ͥ��̾  : �ޥ��������¹��������å�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  08/08/29    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  MNTERRFL
                                   FILE    STATUS  IS  STS-RECEERR.
      *
      *    HOSPID�ե�����
           SELECT  HOSPID-FILE     ASSIGN  HOSPIDFL
                                   FILE    STATUS  IS  STS-HOSPIDFL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
      *    ���顼�ե�����
       FD  HOSPID-FILE.
       01  HOSPID-REC              PIC X(24).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPRECEERR.INC"     REPLACING   //RECEERR//
                                       BY          //MNTERRFL//.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //HOSPIDFL//.
      *
      *    ���ơ������ΰ�
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
           03  STS-HOSPIDFL        PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JOBRUNNING      PIC 9(01).
           03  FLG-JOBMNT          PIC 9(01).
           03  FLG-JOBMLI          PIC 9(01).
      *
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
      *
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "COMMON-SPA".
     *
        01  WRK-PARA.
           03  WRK-PARA-HOSPNUM    PIC 9(02).
      *
       01  CONST-AREA.
           03  CONST-MNTJOBID      PIC 9(07)  VALUE    1.
           03  CONST-MNTSHELLID    PIC X(08)  VALUE    "MSTMNT".
           03  CONST-MLIJOBID      PIC 9(07)  VALUE    1.
           03  CONST-MLISHELLID    PIC X(08)  VALUE    "MSTMLI".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1001.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(400).
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING
                                       COMMAND-PARAM.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =       ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
      *
           .
       000-PROC-EXT.
      *
           STOP    RUN
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
      *
           INITIALIZE                  MNTERRFL
                                       HOSPIDFL
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                               INTO    WRK-PARA-HOSPNUM
                                       MNTERRFL
                                       HOSPIDFL
           END-UNSTRING
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           MOVE    "T"                 TO  SPA-SUPERVISOR
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    MNTERRFL        TO  SGETTEMP-BASENAMES (1)
           MOVE    HOSPIDFL        TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  MNTERRFL
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  HOSPIDFL
      *
           PERFORM 100-DBOPEN-SEC
      *
           PERFORM 800-MCNDATE-SEC
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
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1001"          TO  SYS-KANRICD
           MOVE    "*"             TO  SYS-KBNCD
           MOVE    SMCNDATE-YMD    TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
      *
           PERFORM 800-CHK-ORCSJOB-SEC
           IF    ( FLG-JOBRUNNING  =   1 )
               MOVE    "�ޥ��������¹���Ǥ���"
                                   TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 2001-JOBSTART-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ����ּ¹Խ���
      *****************************************************************
       2001-JOBSTART-SEC               SECTION.
      *
           PERFORM 800-CHK-ORCSJOB-SEC
      *
           IF    ( FLG-JOBMNT     =   ZERO )
               MOVE    "DEL"               TO  SJOBKANRI-MODE
               PERFORM 800-MNT-ORCSJOB-SEC
           END-IF
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           PERFORM 800-MNT-ORCSJOB-SEC
      *
           IF    ( FLG-JOBMLI     =   ZERO )
               MOVE    "DEL"               TO  SJOBKANRI-MODE
               PERFORM 800-MLI-ORCSJOB-SEC
           END-IF
      *
           MOVE    "JBS"               TO  SJOBKANRI-MODE
           PERFORM 800-MLI-ORCSJOB-SEC
      *
           .
      *
       2001-JOBSTART-EXT.
           EXIT.
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC              SECTION.
      *
           OPEN    INPUT               RECEERR-FILE
           IF    ( STS-RECEERR     =   ZERO )
               CONTINUE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR     TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
           END-IF
      *
      *
      *    DISPLAY WRK-RECEERR
      *
           MOVE    1                   TO  FLG-END
           MOVE    1                   TO  FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ��������å�����
      *****************************************************************
       800-CHK-ORCSJOB-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-JOBRUNNING
                                       FLG-JOBMNT
                                       FLG-JOBMLI
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           PERFORM 800-MNT-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDTIME  NOT =   SPACE )
                AND  ( JOB-ENDYMD   NOT =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-JOBRUNNING
               END-IF
           ELSE
                   MOVE    1       TO  FLG-JOBMNT
           END-IF
      *
           MOVE    "CHK"               TO  SJOBKANRI-MODE
           PERFORM 800-MLI-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               IF    ( JOB-ENDTIME  NOT =   SPACE )
                AND  ( JOB-ENDYMD   NOT =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1       TO  FLG-JOBRUNNING
               END-IF
           ELSE
                   MOVE    1       TO  FLG-JOBMLI
           END-IF
      *
           .
       800-CHK-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    ����ִ����Խ�����
      *****************************************************************
       800-MNT-ORCSJOB-SEC             SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MNTJOBID      TO  JOB-JOBID
           MOVE    CONST-MNTSHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       800-MNT-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    ����ִ����Խ�����
      *****************************************************************
       800-MLI-ORCSJOB-SEC             SECTION.
      *
           INITIALIZE                  JOBKANRI-REC
           MOVE    CONST-MLIJOBID      TO  JOB-JOBID
           MOVE    CONST-MLISHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       800-MLI-ORCSJOB-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF    ( FLG-ERR         =   ZERO )
               OPEN    OUTPUT      HOSPID-FILE
               MOVE    SYS-1001-HOSPID TO  HOSPID-REC
               WRITE   HOSPID-REC
               CLOSE   HOSPID-FILE
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
      
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ޥ������ռ�������
      *****************************************************************
       800-MCNDATE-SEC         SECTION.
      *
           CALL    "ORCSMCNDATE"   USING   ORCSMCNDATEAREA
      *
           .
      *
       800-MCNDATE-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ�����ޥ�����������
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC    TO  MCPDATA-REC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC      =   ZERO )
               MOVE    MCPDATA-REC TO  SYSKANRI-REC
           ELSE
               MOVE    1           TO  FLG-SYSKANRI
               INITIALIZE              SYSKANRI-REC
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
      *    �ģ¸�������
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
       910-DBCLOSECURSOR-SEC                SECTION.
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
      *
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