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
       PROGRAM-ID.             ORCBPRTDBDEL.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �Ģɼ
      *  ����ݡ��ͥ��̾  : Ģɼ�ģ¤κ��
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/05/25    NACL-ƣ��     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.08.00    NACL-����    15/03/06  �������������ɲ�
      *                                     �����ǡ����������ǽ����
      *                                     ���ɲý������ɲ�
      *  04.08.00    NACL-����    16/11/30  MONBLOB�����ǽ�ɲ�
      *
      *  05.00.01    NACL-ƣ��    19/01/18  tbl_onrece_kanri tbl_onrece_body
      *                                     �κ����ǽ�ɲ�
      *                                     �ʥ쥻�Ÿ��̺����ѤΤߡ�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-ONRECE-K        PIC 9(01).
           03  FLG-ONRECE-B        PIC 9(01).
      *
           03  FLG-SYS-8000-DLT    PIC 9(01).
      *
       01  WRK-PARA.
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM   PIC  9(02).
      *
       01  WRK-AREA.
           03  WRK-SMCNDATE-YMD.
               05  WRK-SMCNDATE-YM PIC X(06).
               05  WRK-SMCNDATE-DD PIC X(02).
      *
       01  CONST-AREA.
           03  CONST-DAILY         PIC 9(03)  VALUE  1.
           03  CONST-MONTHLY       PIC 9(03)  VALUE 36.
           03  CONST-NORDER        PIC 9(03)  VALUE 36.
      *
      *    ��������ǡ�������������
           COPY    "CPSK8000.INC"  REPLACING  //SYS-8000//
                                   BY         //WRK-SYS-8000//.
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ���������ǡ���
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    �����ǡ���
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *    �ץ�ӥ塼�����Ѵ����ǡ���
           COPY    "CPPRVKANRI.INC".
      *
      *    �����ƥ����
           COPY    "CPSYSKANRI.INC".
      *
      *    �����ǡ�������������
           COPY    "CPSK8000.INC".
      *
      *    ������������
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    ����������������
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
       01  ONRECE-K-REC.
           COPY    "CPONRECE-KANRI.INC".
      *
       01  ONRECE-B-REC.
           COPY    "CPONRECE-BODY.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
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
           DISPLAY "*** ORCBPRTDBDEL START ***"
      * 
           INITIALIZE                  FLG-AREA
                                       SPA-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ���ƥå״������Ͻ���
      *****MOVE    "STS"           TO  SJOBKANRI-MODE
      *    INITIALIZE                  JOBKANRI-REC
      *    MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
      *    MOVE    WRK-PARA-SHELLID
      *                            TO  JOB-SHELLID
      *    MOVE    "ORCBPRTDBDEL"  TO  JOB-PGID
      *    MOVE    "Ģɼ�ģº������"
      *                            TO  JOB-SHELLMSG
      *    CALL    "ORCSJOB"       USING
      *                            ORCSJOBKANRIAREA  
      *****                            JOBKANRI-REC
      *
      *    �ޥ������ռ���
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SMCNDATE-YMD
           IF      WRK-SMCNDATE-DD     >=  "20" 
               INITIALIZE                     STS-AREA-DAY
               INITIALIZE                     LNK-DAY6-AREA
               MOVE  "61"                     TO  LNK-DAY6-IRAI
               MOVE  WRK-SMCNDATE-YMD         TO  LNK-DAY6-KIJUN
               MOVE  "2"                      TO  LNK-DAY6-ZOGENPTN
               MOVE  1                        TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                                      LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN        TO  WRK-SMCNDATE-YMD
           END-IF
      *
           DISPLAY "SMCNDATE-YMD=" SMCNDATE-YMD " WRK=" WRK-SMCNDATE-YMD
      *
      *    �����ǡ�������������ι���
           MOVE    SPACE               TO  SYS-8000-REC
           INITIALIZE                      SYS-8000-REC
           MOVE    "8000"              TO  SYS-8000-KANRICD
           MOVE    "*"                 TO  SYS-8000-KBNCD
           MOVE    SMCNDATE-YMD        TO  SYS-8000-STYUKYMD
                                           SYS-8000-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-8000-HOSPNUM
           MOVE    SYS-8000-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-8000-REC
                                               WRK-SYS-8000-REC
      *
               IF    ( SYS-8000-NORDER     IS  NOT NUMERIC )
                   MOVE    CONST-NORDER    TO  SYS-8000-NORDER
               END-IF
      *
           ELSE
               MOVE    SPACE               TO  SYS-8000-REC
               INITIALIZE                      SYS-8000-REC
               MOVE    SPA-HOSPNUM         TO  SYS-8000-HOSPNUM
               MOVE    "8000"              TO  SYS-8000-KANRICD
               MOVE    "*"                 TO  SYS-8000-KBNCD
               MOVE    "00000000"          TO  SYS-8000-STYUKYMD
               MOVE    "99999999"          TO  SYS-8000-EDYUKYMD
               MOVE    CONST-DAILY         TO  SYS-8000-DAILY
               MOVE    CONST-MONTHLY       TO  SYS-8000-MONTHLY
               MOVE    CONST-NORDER        TO  SYS-8000-NORDER
               MOVE    SMCNDATE-YMD        TO  SYS-8000-CREYMD
                                               SYS-8000-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-8000-UPHMS
               MOVE    SYS-8000-REC        TO  SYSKANRI-REC
                                               WRK-SYS-8000-REC
      *
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC          NOT =   ZERO 
                   DISPLAY "SYS 8000 INSERT ERR RC=" MCP-RC
                   MOVE    1               TO  FLG-END
               END-IF
           END-IF
      *
           IF      FLG-END                 =   ZERO
      *
      *        ����������
               IF    ( SYS-8000-DAILY-YM   =   WRK-SMCNDATE-YM )
               OR    ( SYS-8000-DAILY      =   999             )
                   CONTINUE
               ELSE
                   MOVE    WRK-SMCNDATE-YM TO  SYS-8000-DAILY-YM
                   MOVE    SMCNDATE-YMD    TO  SYS-8000-DAILY-UPYMD
                   MOVE    1               TO  FLG-SYS-8000-DLT
               END-IF    
      *        �������
               IF    ( SYS-8000-MONTHLY-YM =   WRK-SMCNDATE-YM )
               OR    ( SYS-8000-MONTHLY    =   999             )
                   CONTINUE
               ELSE
                   MOVE    WRK-SMCNDATE-YM TO  SYS-8000-MONTHLY-YM
                   MOVE    SMCNDATE-YMD    TO  SYS-8000-MONTHLY-UPYMD
                   MOVE    1               TO  FLG-SYS-8000-DLT
               END-IF
      *        ���������������
               IF    ( SYS-8000-NORDER-YM  =   WRK-SMCNDATE-YM )
               OR    ( SYS-8000-NORDER     =   999             )
                   CONTINUE
               ELSE
                   MOVE    WRK-SMCNDATE-YM TO  SYS-8000-NORDER-YM
                   MOVE    SMCNDATE-YMD    TO  SYS-8000-NORDER-UPYMD
                   MOVE    1               TO  FLG-SYS-8000-DLT
               END-IF
      *
      *        �����ƥ������������
               IF      FLG-SYS-8000-DLT    =   1
                   MOVE    SMCNDATE-YMD        TO  SYS-8000-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYS-8000-UPHMS
                   MOVE    SYS-8000-REC        TO  MCPDATA-REC
      *
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
      *
                   IF      MCP-RC          NOT =   ZERO 
                       DISPLAY "SYS 8000 UPDATE ERR RC=" MCP-RC
                       MOVE    1                   TO  FLG-END
                   ELSE 
                       DISPLAY "SYS 8000 TAISYO"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                   END-IF      
               ELSE
                   DISPLAY "SYS 8000 EXIST DELETE SKIP"
                   MOVE    1                   TO  FLG-END
               END-IF
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *    
      *    ����������
      *
           DISPLAY "DAILY       YM=" WRK-SYS-8000-DAILY-YM 
                                "   DAILY=" WRK-SYS-8000-DAILY
      *
           IF    ( WRK-SYS-8000-DAILY-YM
                                       =   WRK-SMCNDATE-YM )
           OR    ( WRK-SYS-8000-DAILY  =   999             )
               CONTINUE
           ELSE  
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-SMCNDATE-YMD    TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN  =   WRK-SYS-8000-DAILY  *   (-1)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
      *
               DISPLAY "DAILY   KEISAN="  LNK-DAY6-KEISAN (1:6)
      *                 
               INITIALIZE                  PRTKANRI-REC
               MOVE    LNK-DAY6-KEISAN (1:6)
                                       TO  PRTKANRI-SRYYM
               MOVE    WRK-PARA-HOSPNUM
                                       TO  PRTKANRI-HOSPNUM
               MOVE    PRTKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_prtkanri"  TO  MCP-TABLE
               MOVE    "del16"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               INITIALIZE                  PRTDATA-REC
               MOVE    LNK-DAY6-KEISAN(1:6)
                                       TO  PRTDATA-SRYYM
               MOVE    WRK-PARA-HOSPNUM
                                       TO  PRTDATA-HOSPNUM
               MOVE    PRTDATA-REC     TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_prtdata"   TO  MCP-TABLE
               MOVE    "delmb3"        TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *       
               MOVE    PRTDATA-REC     TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_prtdata"   TO  MCP-TABLE
               MOVE    "del16"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *       
               INITIALIZE                  PRVKANRI-REC
               MOVE    LNK-DAY6-KEISAN(1:6)
                                       TO  PRVKANRI-SRYYM
               MOVE    WRK-PARA-HOSPNUM
                                       TO  PRVKANRI-HOSPNUM
               MOVE    PRVKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_prvkanri"  TO  MCP-TABLE
               MOVE    "del14"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
           END-IF    
      *    
           PERFORM 900-DBCOMMIT-SEC
           PERFORM 900-DBSTART-SEC
      *    
      *    �������
      *
           DISPLAY "MONTHLY     YM=" WRK-SYS-8000-MONTHLY-YM
                                " MONTHLY="WRK-SYS-8000-MONTHLY
      *
           IF    ( WRK-SYS-8000-MONTHLY-YM
                                       =   WRK-SMCNDATE-YM )
           OR    ( WRK-SYS-8000-MONTHLY
                                       =   999             )
               CONTINUE
           ELSE  
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-SMCNDATE-YMD    TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN  =   WRK-SYS-8000-MONTHLY *  (-1)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
      *
               DISPLAY "MONTHLY KEISAN="  LNK-DAY6-KEISAN (1:6)
      *
               INITIALIZE                  PRTKANRI-REC
               MOVE    LNK-DAY6-KEISAN (1:6)
                                       TO  PRTKANRI-SRYYM
               MOVE    WRK-PARA-HOSPNUM
                                       TO  PRTKANRI-HOSPNUM
               MOVE    PRTKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_prtkanri"  TO  MCP-TABLE
               MOVE    "del15"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               INITIALIZE                  PRTDATA-REC
               MOVE    LNK-DAY6-KEISAN (1:6)
                                       TO  PRTDATA-SRYYM
               MOVE    WRK-PARA-HOSPNUM
                                       TO  PRTDATA-HOSPNUM
               MOVE    PRTDATA-REC     TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_prtdata"   TO  MCP-TABLE
               MOVE    "delmb4"        TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               MOVE    PRTDATA-REC     TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_prtdata"   TO  MCP-TABLE
               MOVE    "del15"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               INITIALIZE                  PRVKANRI-REC
               MOVE    LNK-DAY6-KEISAN (1:6)
                                       TO  PRVKANRI-SRYYM
               MOVE    WRK-PARA-HOSPNUM
                                       TO  PRVKANRI-HOSPNUM
               MOVE    PRVKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_prvkanri"  TO  MCP-TABLE
               MOVE    "del15"         TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               INITIALIZE                  ONRECE-K-REC
               MOVE    LNK-DAY6-KEISAN (1:6)
                                       TO  ONRECE-K-SRYYM
               MOVE    WRK-PARA-HOSPNUM
                                       TO  ONRECE-K-HOSPNUM
               MOVE    ONRECE-K-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_onrece_kanri"
                                       TO  MCP-TABLE
               MOVE    "del1"          TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
      *
               INITIALIZE                  ONRECE-B-REC
               MOVE    LNK-DAY6-KEISAN (1:6)
                                       TO  ONRECE-B-SRYYM
               MOVE    WRK-PARA-HOSPNUM
                                       TO  ONRECE-B-HOSPNUM
               MOVE    ONRECE-B-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_onrece_body"
                                       TO  MCP-TABLE
               MOVE    "del1"          TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
           END-IF    
      *    
           PERFORM 900-DBCOMMIT-SEC
           PERFORM 900-DBSTART-SEC
      *    
           PERFORM 2001-NORDER-DEL-SEC
      *
           PERFORM 900-DBCOMMIT-SEC
           PERFORM 900-DBSTART-SEC
      *
           MOVE    1                   TO  FLG-END     
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������������������
      *****************************************************************
       2001-NORDER-DEL-SEC         SECTION.
      *
      *    ���������������
           DISPLAY "NORDER      YM=" WRK-SYS-8000-NORDER-YM
                                " NORDER="WRK-SYS-8000-NORDER
      *
           IF    ( WRK-SYS-8000-NORDER-YM
                                       =   WRK-SMCNDATE-YM )
           OR    ( WRK-SYS-8000-NORDER
                                       =   999             )
               CONTINUE
           ELSE  
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-SMCNDATE-YMD    TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               COMPUTE LNK-DAY6-ZOGEN  =   WRK-SYS-8000-NORDER *  (-1)
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
      *
               DISPLAY "NORDER KEISAN="  LNK-DAY6-KEISAN (1:6)
      *
               INITIALIZE                  ODRITEM-REC
               MOVE    LNK-DAY6-KEISAN (1:6)
                                       TO  ODRITEM-CREYMD
               MOVE    "01"            TO  ODRITEM-CREYMD(7:2)
               MOVE    WRK-PARA-HOSPNUM
                                       TO  ODRITEM-HOSPNUM
               MOVE    ODRITEM-REC     TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_odritem"   TO  MCP-TABLE
               MOVE    "del2"          TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               INITIALIZE                  ORDER-REC
               MOVE    LNK-DAY6-KEISAN (1:6)
                                       TO  ORDER-UKEYMD
               MOVE    "01"            TO  ORDER-UKEYMD(7:2)
               MOVE    WRK-PARA-HOSPNUM
                                       TO  ORDER-HOSPNUM
               MOVE    ORDER-REC       TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_order"     TO  MCP-TABLE
               MOVE    "del2"          TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
           END-IF    
           .
      *
       2001-NORDER-DEL-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
      *     
      *****���ƥå״�����λ����
      *    MOVE    "STE"           TO  SJOBKANRI-MODE
      *    INITIALIZE                  JOBKANRI-REC
      *    MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
      *    MOVE    WRK-PARA-SHELLID
      *                            TO  JOB-SHELLID
      *    CALL    "ORCSJOB"       USING
      *                            ORCSJOBKANRIAREA  
      *****                        JOBKANRI-REC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBPRTDBDEL END ***"
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ������ɹ��ʥ�����
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
      *
      *****************************************************************
      *    �ģ¡��ãϣͣͣɣԽ���
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¡��ӣԣ��ңԽ���
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.