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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCBPRT.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : Ģɼ�ģ¤ΰ���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/04/16    MCC-ƣ��      ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     02/05/20  �ư������̤���ΰ����ˤ�뽤��  
      *  01.00.02    NACL-ƣ��    02/09/12  �ǥ��쥯�ȥ�򥵥֥ץ���
      *  01.01.01    NACL-ƣ��    02/10/10  �����ѥ��󥹥�����ΰ���ɲ�
      *                                     �����ѥե�����˥����ȶ�ʬ���ɲ�
      *  01.01.02    NACL-ƣ��    03/03/04  �����ģ¤ؤΥץ��̾�ɲäˤ��
      *                                     ����
      *  01.01.03    NACL-ƣ��    04/11/29  �����˥��顼�ե�����̾���ɲ�
      *  01.01.04    NACL-��ƣ    06/03/08  ���ե��å��б�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *  04.05.00    NACL-����    10/01/28  ��ĥ�����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    �����ѥǡ���
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL 
                                   FILE    STATUS  IS  STS-PRT.
      *
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    �����ѥǡ���
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTID           PIC X(30).
           03  PRT-PRTNM           PIC X(20).
           03  PRT-OFFSET-X        PIC X(05).
           03  PRT-OFFSET-Y        PIC X(05).
           03  PRT-SITEKBN         PIC X(01).
           03  PRT-PRTDATA         PIC X(20000).
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    �����ѥǡ��� ̾���ΰ� 
           COPY    "CPCOMMONPRT.INC".
           COPY    "CPASGNPARA.INC".
      *
      *    ���顼�ե����� ̾���ΰ� 
           COPY    "CPRECEERR.INC".
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).    
      *
      *    �����ΰ�
       01  STS-AREA.
           03  STS-PRT             PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-PRTDATA         PIC 9(01).
           03  FLG-PRTKANRI        PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN              PIC 9(04).
           03  CNT-OUT             PIC 9(10).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-HC01PARA-CNT    PIC  9(03).
      *
           03  WRK-RECEERR         PIC X(200). 
           03  WRK-ENCODING            PIC X(01).
           03  WRK-ENCODING9           REDEFINES WRK-ENCODING
                                       PIC 9(01).
      *
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-STPAGE     PIC 9(05).
           03  WRK-PARA-EDPAGE     PIC 9(05).
           03  WRK-PARA-HOSPNUM    PIC X(02).
      *
      *    �����ѥ��󥹥�����ΰ� 
           COPY    "CPPRTCONST.INC".          
      *
      *    �ե�����ǥ��쥯�ȥ���ֻ��ꥵ��
           COPY  "CPMKPASS.INC".
      *
           COPY    "CPSHELLTBL.INC".
      *     
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    monpe�����ǡ����㤭�̤콤������
           COPY  "CPORCSFIXREDDATA.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ���ե��åȾ���
           COPY    "CPSK1044.INC".
      *
      *    ���������ģ�
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    �����ģ�
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE           DIVISION
               USING
           COMMAND-PARAM.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      * 
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
                                       RECEERR
      *
           ACCEPT  SYS-TIME            FROM    TIME     
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               RECEERR    
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-STPAGE
                                               WRK-PARA-EDPAGE
                                               WRK-PARA-HOSPNUM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
           CALL    "ORCSENCODING"          USING
                                           MCPAREA
                                           SPA-AREA
           MOVE    SPA-ENCODING        TO  WRK-ENCODING9
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCBPRT"       TO  JOB-PGID
           MOVE    "Ģɼ��������"  TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    ���������ģ��ɤ߹��߽���
           PERFORM 900-PRTKANRI-SELECT-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    �����ѥե��������  
           MOVE    "HC99"          TO  HC01PARA-FORM-ID
grpsys     MOVE    WRK-PARA-HOSPNUM
                                   TO  HC01PARA-HOSPNUM
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    1               TO  HC01PARA-CNT
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    HC01PARA-BASENAME
                                   TO  SGETTEMP-BASENAMES (1)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  ASGNPARA
           MOVE    SGETTEMP-ST     TO  ASGNPARA-ST
      *
           OPEN    OUTPUT              PRT-FILE
      *
           PERFORM                     UNTIL   FLG-PRTKANRI =   1
                                       OR      FLG-END      =   1   
      *        �����ģ��Խ�����
               PERFORM 900-PRTDATA-SELECT-SEC
               PERFORM                     UNTIL   FLG-PRTDATA  =   1
                                           OR      FLG-END      =   1
                   PERFORM 2001-PRTDATA-PRINT-SEC
                   PERFORM 900-PRTDATA-FET-SEC
               END-PERFORM    
      *
               MOVE    "tbl_prtdata"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               PERFORM 2003-PRTKANRI-UPD-SEC
               PERFORM 900-PRTKANRI-FET-SEC
           END-PERFORM                                
      *
           CLOSE                       PRT-FILE
      *     
      *   �����ѥ�����ץȽ���
      *
           IF      CNT-OUT             >   ZERO
           AND     FLG-ERR             =   ZERO 
      *
               MOVE    SPACE           TO  SHELLTBL
               MOVE    "HCALL2.sh"     TO  SHELLTBL-NAME
      *         
               MOVE    ASGNPARA (ASGNPARA-ST:)
                                       TO  SHELLTBL-ARG1
               MOVE    WRK-ENCODING    TO  SHELLTBL-ARG2
               MOVE    SHELLTBL        TO  MCPDATA-REC
               MOVE    "SHELL"         TO  MCP-FUNC
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    "daily"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
           END-IF                                                
      *
           MOVE    1                   TO  FLG-END     
           .
       200-MAIN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    Ģɼ��������
      *****************************************************************
       2001-PRTDATA-PRINT-SEC          SECTION.
      *
           MOVE    SPACE               TO  PRT-REC 
           MOVE    PRTDATA-PRTID       TO  PRT-PRTID
           IF      PRTDATA-PRTNM       =   SPACE
               MOVE    PRTKANRI-PRTNM      TO  PRT-PRTNM
           ELSE
               MOVE    PRTDATA-PRTNM       TO  PRT-PRTNM
           END-IF
      *    ���ե��å�
           PERFORM 2002-OFFSET-GET-SEC
           MOVE    SYS-1044-OFFSET-X   TO  PRT-OFFSET-X
           MOVE    SYS-1044-OFFSET-Y   TO  PRT-OFFSET-Y
      *
           EVALUATE    PRTDATA-SITEKBN
               WHEN    "1"
               WHEN    "2"               
                   MOVE    PRTDATA-SITEKBN     TO  PRT-SITEKBN
               WHEN    OTHER
                   SET     PRTCONST-IDX        TO  1
                   SEARCH  PRTCONST-TX VARYING PRTCONST-IDX
                       AT  END
                           MOVE    "2"         TO  PRT-SITEKBN
                       WHEN    PRTCONST-PRTID (PRTCONST-IDX)
                                               =   PRTDATA-PRTID
                           MOVE    "1"         TO  PRT-SITEKBN
                   END-SEARCH
           END-EVALUATE                                                                
           INITIALIZE                  SFIXREDDATA-AREA
           MOVE    PRT-SITEKBN     TO  SFIXREDDATA-SITEKBN
           MOVE    PRT-PRTID       TO  SFIXREDDATA-FORMID
           MOVE    PRTDATA-PRTDATA TO  SFIXREDDATA-DATA
           CALL    "ORCSFIXREDDATA"    USING
                                       SFIXREDDATA-AREA
                                       SPA-AREA
           MOVE    SFIXREDDATA-DATA
                                   TO  PRT-PRTDATA
           WRITE   PRT-REC
      *
           ADD     1                   TO  CNT-OUT     
      *
           PERFORM 2002-PRTDATA-UPD-SEC
           .
       2001-PRTDATA-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ե��åȼ�������
      *****************************************************************
       2002-OFFSET-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYS-1044-REC
           INITIALIZE                  SYS-1044-REC
           MOVE    "1044"              TO  SYS-1044-KANRICD
           MOVE    PRT-PRTID           TO  SYS-1044-FORMID
           STRING  PRT-PRTNM           DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                       INTO    SYS-1044-TBL(21:)
           END-STRING
           MOVE    "99999999"          TO  SYS-1044-STYUKYMD
           MOVE    "99999999"          TO  SYS-1044-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1044-HOSPNUM
           MOVE    SYS-1044-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE  MCPDATA-REC   TO  SYS-1044-REC
                   IF   (SYS-1044-FORMID  =   PRT-PRTID)  AND
                        (SYS-1044-PRTNM   =   PRT-PRTNM)
                         CONTINUE
                   ELSE
                         INITIALIZE    SYS-1044-REC
                   END-IF
               ELSE
                   INITIALIZE          SYS-1044-REC
               END-IF
           ELSE
               INITIALIZE              SYS-1044-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       2002-OFFSET-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ե饰���åȽ���
      *****************************************************************
       2002-PRTDATA-UPD-SEC          SECTION.
      *
      *    �����ե饰���å�
           MOVE    "1"                 TO  PRTDATA-PRTFLG
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC             =   ZERO
               CONTINUE
           ELSE
               MOVE    "�����ģ¤ι����ǥ��顼�ˤʤ�ޤ���"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
          END-IF    
           .
       2001-PRTDATA-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ե饰���åȽ���
      *****************************************************************
       2003-PRTKANRI-UPD-SEC          SECTION.
      *
           MOVE    "1"                 TO  PRTKANRI-PRTFLG
grpsys*    MOVE SPA-HOSPNUM TO -HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC               =   ZERO
               CONTINUE
           ELSE
               MOVE    "���������ģ¤ι����ǥ��顼�ˤʤ�ޤ���"
                                        TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF    
           .
       2003-PRTKANRI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
                                           FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *     
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT                        
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBPRT IN  "  CNT-IN
           DISPLAY "*** ORCBPRT OUT "  CNT-OUT
      **** DISPLAY "*** ORCBPRT END ***"
      *****ACCEPT WRK-IN
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *     ���������ģ��ɤ߹���
      *****************************************************************
       900-PRTKANRI-SELECT-SEC          SECTION.
      *
           INITIALIZE                      PRTKANRI-REC
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  PRTKANRI-TBL-GROUP
grpsys     MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRTKANRI-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRTKANRI
                                               FLG-END
           END-IF
           .
       900-PRTKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     ���������ģ��ɤ߹���
      *****************************************************************
       900-PRTKANRI-FET-SEC            SECTION.
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRTKANRI-REC
               IF      LNK-PRTKANRI-RENNUM  =   ZERO
                   CONTINUE
               ELSE
                   IF      LNK-PRTKANRI-RENNUM   NOT = 
                                           PRTKANRI-RENNUM
                       GO  TO  900-PRTKANRI-FET-SEC
                   END-IF
               END-IF                                             
               IF      LNK-PRTKANRI-SHORI-RENNUM   =   ZERO
                   CONTINUE
               ELSE
                   IF      LNK-PRTKANRI-SHORI-RENNUM   NOT = 
                                           PRTKANRI-SHORI-RENNUM
                       GO  TO  900-PRTKANRI-FET-SEC
                   END-IF
               END-IF                                             
               MOVE    ZERO                TO  FLG-PRTKANRI
                                               FLG-END
           ELSE
               MOVE    1                   TO  FLG-PRTKANRI
                                               FLG-END 
           END-IF
           .
       900-PRTKANRI-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *     �����ģ��ɤ߹���
      *****************************************************************
       900-PRTDATA-SELECT-SEC          SECTION.
      *
           INITIALIZE                      PRTDATA-REC
           MOVE    PRTKANRI-RENNUM     TO  PRTDATA-RENNUM
           MOVE    PRTKANRI-SHORI-RENNUM
                                       TO  PRTDATA-SHORI-RENNUM
           MOVE    PRTKANRI-TBL-GROUP  TO  PRTDATA-TBL-GROUP
           MOVE    PRTKANRI-TBL-KEY    TO  PRTDATA-TBL-KEY
           MOVE    WRK-PARA-STPAGE     TO  PRTDATAXX-STPAGE
           MOVE    WRK-PARA-EDPAGE     TO  PRTDATAXX-EDPAGE
grpsys     MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
      *
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRTDATA-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRTDATA
           END-IF
           .
       900-PRTDATA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *     �����ģ��ɤ߹���
      *****************************************************************
       900-PRTDATA-FET-SEC            SECTION.
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRTDATA-REC
               ADD     1                   TO  CNT-IN
               MOVE    ZERO                TO  FLG-PRTDATA
           ELSE  
               MOVE    1                   TO  FLG-PRTDATA
           END-IF 
           .
       900-PRTDATA-FET-EXT.
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
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.