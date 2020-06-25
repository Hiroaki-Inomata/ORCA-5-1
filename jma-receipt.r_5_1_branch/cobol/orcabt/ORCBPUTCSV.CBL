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
       PROGRAM-ID.             ORCBPUTCSV.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �
      *  ����ݡ��ͥ��̾  : ���ףãӣ֥ǡ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  09/10/01    NACL��ƣ��    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.01    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    �ãӣ֥ǡ���
           SELECT  CSV-FILE   ASSIGN  CSVPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-CSV.
      *    ���顼�ե�����
           SELECT  CSVERR-FILE  ASSIGN  CSVERR
                                   FILE    STATUS  IS  STS-CSVERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    �ãӣ֥ǡ���
       FD  CSV-FILE.
       01  CSV-REC                 PIC X(20000). 
      *
      *    ���顼�ե�����
       FD  CSVERR-FILE.
       01  CSVERR-REC              PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ãӣ֥ǡ���
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //CSVPARA//.
      *01  CSVPARA.
      *****03  FILLER              PIC X(05)   VALUE   "/tmp/".
       01  CSVPARA-BASENAME.
           03  CSVPARA-HOSPNUM     PIC 9(02).
           03  FILLER              PIC X(06)   VALUE 
                                                       "PUTCSV".
           03  CSVPARA-RENNUM      PIC 9(02)   VALUE   1.
           03  FILLER              PIC X(04)   VALUE   ".txt".
      *
           COPY    "CPRECEERR.INC" REPLACING  //RECEERR//
                                   BY         //CSVERR//.
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-CSV             PIC X(02).
           03  STS-CSVERR          PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TOUKEICSV-B     PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-CSV             PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03). 
           03  IDY                 PIC 9(03). 
           03  IDZ                 PIC 9(03).
      *
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-HOSPNUM        PIC 9(02).
               05  WRK-PARA-TBL-KEY        PIC X(8).
               05  WRK-PARA-TBL-GROUP      PIC X(14).
               05  WRK-PARA-SHORI-RENNUM   PIC 9(04).
               05  WRK-PARA-RENNUM         PIC 9(04).
               05  WRK-PARA-TO-DATA        PIC X(200).
               05  WRK-PARA-JOBID          PIC 9(07).
               05  WRK-PARA-SHELLID        PIC X(08).
               05  WRK-PARA-TITLE          PIC X(100).
      *
           03  WRK-CSVERR          PIC X(200). 
           03  WRK-ERRMSG          PIC X(300).
      *
           03  WRK-JOBNAME         PIC X(40).
      *   
           COPY    "CMJOBNAME.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
       01  TOUKEICSV-B-REC.
           COPY    "CPTOUKEICSV-B.INC".          
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    ���ףãӣ����極��
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(1000).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC    UNTIL   FLG-END =   1 
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
                                       FLG-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA-HOSPNUM
                                               WRK-PARA-TBL-KEY
                                               WRK-PARA-TBL-GROUP
                                               WRK-PARA-SHORI-RENNUM
                                               WRK-PARA-RENNUM
                                               WRK-PARA-TO-DATA 
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-TITLE
                                               CSVERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM
                                   TO  SPA-HOSPNUM
      *
           MOVE    SPA-HOSPNUM     TO  CSVPARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    CSVERR              TO  SGETTEMP-BASENAMES  (1)
           MOVE    CSVPARA-BASENAME    TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  CSVERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  CSVERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  CSVPARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBPUTCSV"    TO  JOB-PGID
      *
           IF      WRK-PARA-TITLE   =   SPACE
               MOVE    SPACE         TO  WRK-JOBNAME
               SET     TBL-IDX             TO  1
               SEARCH      MNAME-TBL-OCC      VARYING   TBL-IDX
                   AT   END
                     MOVE    SPACE         TO  WRK-JOBNAME
                   WHEN    WRK-PARA-TBL-KEY    =   MNAME-SHELLID
                                                      (TBL-IDX)
                       MOVE    MNAME-JOBNAME (TBL-IDX)
                                           TO  WRK-JOBNAME
               END-SEARCH
               STRING  WRK-JOBNAME     DELIMITED  BY  SPACE 
                       "�ãӣֺ���"    DELIMITED  BY  SIZE
                                       INTO       JOB-SHELLMSG
               END-STRING
           ELSE
               MOVE    WRK-PARA-TITLE  TO  JOB-SHELLMSG
           END-IF
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           OPEN    OUTPUT              CSV-FILE
      *
           PERFORM 900-TOUKEICSV-B-START-SEC   
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
           MOVE    TOUKEICSV-B-CSVDATA
                                   TO  CSV-REC
      *
           WRITE   CSV-REC
           IF      STS-CSV         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-CSVERR
               STRING "�ãӣ֥ǡ����ե�����񤭹��ߥ��顼 STS="
                                                   DELIMITED  BY  SIZE
                       STS-CSV                     DELIMITED  BY  SIZE
                                           INTO    WRK-CSVERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           ADD     1                   TO  CNT-CSV
      *
           PERFORM 900-TOUKEICSV-B-READ-SEC   
           .
       200-MAIN-EXT.
           EXIT. 
      *
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   CSVERR-FILE
           IF      STS-CSVERR         =   ZERO
               CLOSE   CSVERR-FILE
           ELSE
               OPEN    OUTPUT              CSVERR-FILE
      *
               MOVE    WRK-CSVERR         TO  CSVERR-REC
               WRITE   CSVERR-REC
               CLOSE   CSVERR-FILE
      *         
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CSVERR      TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼����λ����
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBPUTCSV "       DELIMITED  BY  SIZE
                   WRK-CSVERR          DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           MOVE    "tbl_toukeicsv_b"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           CLOSE   CSV-FILE
      *
           IF      CNT-CSV    =   ZERO  
               MOVE    "�����оݤΥǡ���������ޤ���Ǥ���"
                                           TO  WRK-CSVERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
      *        ���ףãӣֹ�������
               PERFORM 3001-TOUKEICSV-UPDATE-SEC
           END-IF
      *
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-CSV         TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *     
           DISPLAY "CSV  CNT-OUT:" CNT-CSV
           DISPLAY "*** ORCBPUTCSV END ***"
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ףãӣֹ�������
      *****************************************************************
       3001-TOUKEICSV-UPDATE-SEC           SECTION.
      *
           INITIALIZE                  ORCSTOUKEICSVAREA
           MOVE    "UPD"           TO  STOUKEICSV-MODE
           MOVE    WRK-PARA-TBL-KEY
                                   TO  STOUKEICSV-TBL-KEY
           MOVE    WRK-PARA-RENNUM TO  STOUKEICSV-RENNUM
           MOVE    WRK-PARA-TBL-GROUP
                                   TO  STOUKEICSV-TBL-GROUP
           MOVE    WRK-PARA-SHORI-RENNUM
                                   TO  STOUKEICSV-SHORI-RENNUM
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "���ףãӣ֣ģ¤˹����Ǥ��ޤ���Ǥ���"
                                      TO  WRK-CSVERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       3001-TOUKEICSV-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ףãӣ����٣ģ��ɹ�
      *****************************************************************
       900-TOUKEICSV-B-START-SEC   SECTION.
      *
           MOVE    SPACE               TO  TOUKEICSV-B-REC
           INITIALIZE                      TOUKEICSV-B-REC
           MOVE    SPA-HOSPNUM         TO  TOUKEICSV-B-HOSPNUM
           MOVE    WRK-PARA-TBL-KEY    TO  TOUKEICSV-B-TBL-KEY
           MOVE    WRK-PARA-TBL-GROUP  TO  TOUKEICSV-B-TBL-GROUP
           MOVE    WRK-PARA-SHORI-RENNUM
                                       TO  TOUKEICSV-B-SHORI-RENNUM
           MOVE    WRK-PARA-RENNUM     TO  TOUKEICSV-B-RENNUM
      *
           MOVE    TOUKEICSV-B-REC     TO  MCPDATA-REC
           MOVE    "tbl_toukeicsv_b"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 900-TOUKEICSV-B-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TOUKEICSV-B
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-TOUKEICSV-B-START-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ףãӣ����٣ģ��ɹ�
      *****************************************************************
       900-TOUKEICSV-B-READ-SEC             SECTION.
      *
           MOVE    "tbl_toukeicsv_b"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TOUKEICSV-B-REC
               MOVE    ZERO                TO  FLG-TOUKEICSV-B
           ELSE
               MOVE    1                   TO  FLG-TOUKEICSV-B
               MOVE    1                   TO  FLG-END
           END-IF
           .
       900-TOUKEICSV-B-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ����ģ��������
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ƣţԣãȽ���
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    �ģ¥����ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.