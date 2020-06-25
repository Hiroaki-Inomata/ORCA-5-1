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
       PROGRAM-ID.             ORCSJOB.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����ִ���
      *  ����ݡ��ͥ��̾  : ����ִ����ģ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/01/20    MCC��ƣ����   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    MCC-ƣ��     02/05/15  DELALL���ɲ�  
      *  01.00.02    NACL-ƣ��    02/08/30  ���ƥå׷�����ɲ�
      *  01.00.03    NACL-ƣ��    03/02/24  �ǿ��Υ����å�
      *  01.00.04    NACL-����    03/08/11  ����вṹ���������ɲ�
      *  01.00.05    NACL-ƣ��    04/09/28  STP,CAN���ɲ�
      *  01.00.06    NACL-ƣ��    04/12/20  CN2���ɲ�
      *                                     JBE�ΤȤ�YOBI������ʳ��ΤȤ�
      *                                     �����Խ���Ԥ�
      *  01.00.07    NACL-����    05/10/21  MONFUNC�б�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *
      *  04.05.01    NACL-����    10/02/27  �ƻ���������б�
      *
      *  04.07.01    NACL-ƣ��    12.09.07  ��������ʸ��̡ˤ��ɲ�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
       DATA                    DIVISION.
       WORKING-STORAGE         SECTION.
      *
      *    �ե饰�ΰ�
      *
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-JOBKANRI                            PIC S9(03).
           03  FLG-MONBATCH                            PIC 9(01).
           03  FLG-LOOPEND                             PIC 9(01).
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC 9(03). 
      *
      *
       01  WRK-AREA.
           03  WRK-SYSYMD-8.
               05  WRK-SYSYY           PIC 9(02).
               05  WRK-SYSYMD          PIC 9(06).  
           03  WRK-SYSTIME             PIC X(08).
      *������ɣ�
           03  WRK-SHELLID             PIC X(08).
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
       01  WRK-JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC"    REPLACING   //JOB-//
                                       BY          //WRK-JOB-//.
           COPY    "CPSHELLTBL.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *     
           COPY    "CPORCSLOCK.INC".
      *
      **************************************************************************
      *
       LINKAGE                 SECTION.
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
grpsys*
grpsys     COPY    "COMMON-SPA".
      *
      **************************************************************************
       PROCEDURE           DIVISION
               USING
               ORCSJOBKANRIAREA
               JOBKANRI-REC
grpsys         SPA-AREA
               .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE     SPACE              TO  WRK-SYSTIME
           ACCEPT   WRK-SYSTIME        FROM      TIME
      *
      *???
           DISPLAY "ORCSJOB ST =" SJOBKANRI-MODE "## "
                   SMCNDATE-YMD (1:4) "/"
                   SMCNDATE-YMD (5:2) "/"
                   SMCNDATE-YMD (7:2) " "
                   WRK-SYSTIME  (1:2) ":"
                   WRK-SYSTIME  (3:2) ":"
                   WRK-SYSTIME  (5:2) "."
                   WRK-SYSTIME  (7:2) " "
      *???     
           INITIALIZE                  CNT-AREA
                                       WRK-AREA
      *    �����⡼�ɤΥ����å�
           EVALUATE    SJOBKANRI-MODE
               WHEN    "JBS"
                       PERFORM 110-JBS-SEC
               WHEN    "JBE"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 120-JBE-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "STS"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 130-STS-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "STM"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 180-STM-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "STE"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 160-STE-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "DEL"
                       PERFORM 140-DEL-SEC
               WHEN    "ALL"
                       PERFORM 170-DELALL-SEC
               WHEN    "CHK"
               WHEN    "CH2"
                       PERFORM 150-CHK-SEC
               WHEN    "STP"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 190-STP-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "CAN"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 200-CANCEL-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    "CA2"
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
                       PERFORM 210-CANCEL2-SEC
                       PERFORM 900-DBCOMMIT-SEC
                       PERFORM 900-DBSTART-SEC
               WHEN    OTHER
                       MOVE  10            TO  SJOBKANRI-RETURN  
                       MOVE  1             TO  FLG-END
           END-EVALUATE
      *???
           DISPLAY "ORCSJOB ED =" SJOBKANRI-RETURN "##"
      *???     
      *
           .
       000-PROC-EXT.
           EXIT PROGRAM
           .
      *****************************************************************
      *    ����ֳ��ϥ��åȡ�����
      *****************************************************************
       110-JBS-SEC                 SECTION.
      *
           PERFORM 500-SYSYMD-HENSYU-SEC
           MOVE    WRK-SYSYMD-8        TO  JOB-STARTYMD
      *****ACCEPT  JOB-STARTTIME       FROM TIME
           MOVE    SMCNDATE-HMS        TO  JOB-STARTTIME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "INSERT ERR KEY = [" MCP-RC JOB-KEY "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
      *    �ƻ�������� 
           CALL    "ORCSAUDIT"         USING
                                      "2"
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
                                       ORCSLOCKAREA
                                       JOBKANRI-REC
      *
           .
       110-JBS-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ����ֽ�λ���åȡ�����
      *****************************************************************
       120-JBE-SEC                 SECTION.
      *
      *    �쥳����¸�ߥ����å�
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF      FLG-JOBKANRI   NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO  TO  120-JBE-EXT
           END-IF
      *    ���顼������
           IF      WRK-SHELLID(7:2)    NOT =   SPACE
               MOVE    WRK-SHELLID(7:2)    TO  JOB-ERRCD(1:2)
           END-IF
      *
      *    �쥳���ɹ���
           PERFORM 500-SYSYMD-HENSYU-SEC
           MOVE    WRK-SYSYMD-8        TO  WRK-JOB-ENDYMD
      *****ACCEPT  WRK-JOB-ENDTIME     FROM TIME
           MOVE    SMCNDATE-HMS        TO  WRK-JOB-ENDTIME
           IF      JOB-YOBI            =   SPACE
               CONTINUE
           ELSE
               MOVE    JOB-YOBI            TO  WRK-JOB-YOBI
           END-IF
           MOVE    JOB-ERRCD           TO  WRK-JOB-ERRCD
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       120-JBE-EXT.
           EXIT.
      * 
      * 
      *****************************************************************
      *    ���ƥå׳��ϥ��åȡ�����
      *****************************************************************
       130-STS-SEC                 SECTION.
      *
      *    �쥳����¸�ߥ����å�
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO    TO                130-STS-EXT
           END-IF
      *
      *    �쥳���ɹ���
           MOVE    ZERO                TO  WRK-JOB-STEPENDTIME
           PERFORM 500-SYSYMD-HENSYU-SEC
      *****ACCEPT  WRK-JOB-STEPSTARTTIME
      *****                            FROM TIME
           MOVE    SMCNDATE-HMS        TO  WRK-JOB-STEPSTARTTIME
           MOVE    JOB-PGID            TO  WRK-JOB-PGID
           MOVE    JOB-SHELLMSG        TO  WRK-JOB-SHELLMSG
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       130-STS-EXT.
           EXIT.
      * 
      * 
      *****************************************************************
      *    ���ƥå׽�λ���åȡ�����
      *****************************************************************
       160-STE-SEC                 SECTION.
      *
      *    �쥳����¸�ߥ����å�
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO    TO                160-STE-EXT
           END-IF
      *
      *    �쥳���ɹ���
           PERFORM 500-SYSYMD-HENSYU-SEC
      *****ACCEPT  WRK-JOB-STEPENDTIME
      *****                            FROM TIME
           MOVE    SMCNDATE-HMS        TO  WRK-JOB-STEPENDTIME
           IF      JOB-UPDCNT          NOT NUMERIC
               MOVE    ZERO                TO   JOB-UPDCNT
           END-IF
           IF      JOB-UPDCNT1         NOT NUMERIC
               MOVE    ZERO                TO   JOB-UPDCNT1
           END-IF
           IF      JOB-UPDCNT2         NOT NUMERIC
               MOVE    ZERO                TO   JOB-UPDCNT2
           END-IF
           COMPUTE WRK-JOB-UPDCNT1     =   WRK-JOB-UPDCNT1  +
                                           JOB-UPDCNT1
           COMPUTE WRK-JOB-UPDCNT2     =   WRK-JOB-UPDCNT2  +
                                           JOB-UPDCNT2
           COMPUTE WRK-JOB-UPDCNT      =   WRK-JOB-UPDCNT  +
                                           JOB-UPDCNT                            
           COMPUTE WRK-JOB-STEPCNT     =   WRK-JOB-STEPCNT +
                                           1                            
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       160-STE-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �������
      *****************************************************************
       140-DEL-SEC                 SECTION.
      *
      *    �쥳����¸�ߥ����å�
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO    TO                140-DEL-EXT
           END-IF
      *
      *    �쥳���ɹ���
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "del11"
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "DELETE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       140-DEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ����������оݤΥ�����ɣĤ����ơ�
      *****************************************************************
       170-DELALL-SEC                 SECTION.
      *
      *    �쥳����¸�ߥ����å�
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  ZERO              TO  SJOBKANRI-RETURN
               GO    TO                170-DELALL-EXT
           END-IF
      *
      *    �쥳���ɹ���
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "del12"
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "DELETE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       170-DELALL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ����ִ��������å�����
      *****************************************************************
       150-CHK-SEC                 SECTION.
      *
      *    �쥳����¸�ߥ����å�
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
           ELSE
               MOVE    ZERO            TO  SJOBKANRI-RETURN
               MOVE    WRK-JOBKANRI-REC
                                       TO  JOBKANRI-REC
           END-IF
      *
           IF    ( SJOBKANRI-MODE  =   "CH2" )
               PERFORM 910-MONBATCH-SEL-SEC
               IF    ( FLG-MONBATCH    =   ZERO )
                   MOVE    ZERO        TO  SJOBKANRI-RETURN
                   MOVE    SPACE       TO  JOB-ENDYMD
                                           JOB-ENDTIME
               END-IF
           END-IF
      *
           .
       150-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ƥå׳��ϥ��åȡ�����
      *****************************************************************
       180-STM-SEC                 SECTION.
      *
      *    �쥳����¸�ߥ����å�
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF       FLG-JOBKANRI       NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO    TO                180-STM-EXT
           END-IF
      *
      *    �쥳���ɹ���
           IF      JOB-UPDCNT          NOT NUMERIC
               MOVE    ZERO                TO   JOB-UPDCNT
           END-IF
           COMPUTE WRK-JOB-UPDCNT      =   WRK-JOB-UPDCNT  +
                                           JOB-UPDCNT                            
           MOVE    JOB-YOBI            TO  WRK-JOB-YOBI
           IF    ( JOB-SHELLMSG    NOT =   SPACE )
               MOVE    JOB-SHELLMSG    TO  WRK-JOB-SHELLMSG
           END-IF
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       180-STM-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ��ߥե饰���åȽ���
      *****************************************************************
       190-STP-SEC                 SECTION.
      *
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF      FLG-JOBKANRI   NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO  TO  190-STP-EXT
           END-IF
      *
      *    �쥳���ɹ���
           IF      WRK-JOB-STOPFLG     =   2   OR  3
               CONTINUE
           ELSE
               MOVE    JOB-STOPFLG         TO  WRK-JOB-STOPFLG
               MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               IF      MCP-RC     =   ZERO
                   MOVE  MCP-RC            TO  SJOBKANRI-RETURN
               ELSE
                   DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
                   MOVE  MCP-RC            TO  SJOBKANRI-RETURN
               END-IF
           END-IF    
      *
           .
       190-STP-EXT.
           EXIT.
      * 
      * 
      *****************************************************************
      *    �������߽���
      *****************************************************************
       200-CANCEL-SEC                 SECTION.
      *
      *    �쥳����¸�ߥ����å�
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF      FLG-JOBKANRI   NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO  TO  200-CANCEL-EXT
           END-IF
      *
      *    �쥳���ɹ���
           PERFORM 500-SYSYMD-HENSYU-SEC
           MOVE    WRK-SYSYMD-8        TO  WRK-JOB-ENDYMD
      *****ACCEPT  WRK-JOB-ENDTIME     FROM TIME
           MOVE    SMCNDATE-HMS        TO  WRK-JOB-ENDTIME
           MOVE    JOB-YOBI            TO  WRK-JOB-YOBI
           MOVE    JOB-ERRCD           TO  WRK-JOB-ERRCD
           MOVE    JOB-STOPFLG         TO  WRK-JOB-STOPFLG
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"
                                       TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       200-CANCEL-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �������߽����ʽ�λ������λ���֤�����ʤ���
      *****************************************************************
       210-CANCEL2-SEC                 SECTION.
      *
      *    �쥳����¸�ߥ����å�
           MOVE    JOBKANRI-REC        TO  MCPDATA-REC
           PERFORM 910-JOBKANRI-INV2-SEC
           IF      FLG-JOBKANRI   NOT  =  ZERO
               MOVE  FLG-JOBKANRI      TO  SJOBKANRI-RETURN
               GO  TO  210-CANCEL2-EXT
           END-IF
      *
      *    �쥳���ɹ���
           MOVE    JOB-YOBI            TO  WRK-JOB-YOBI
           MOVE    JOB-ERRCD           TO  WRK-JOB-ERRCD
           MOVE    JOB-STOPFLG         TO  WRK-JOB-STOPFLG
           MOVE    WRK-JOBKANRI-REC    TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           ELSE
               DISPLAY "UPDATE ERR KEY = [" MCP-RC JOB-SHELLID "]"   
               MOVE  MCP-RC            TO  SJOBKANRI-RETURN
           END-IF
      *
           .
       210-CANCEL2-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �����ƥ������Խ�����
      *****************************************************************
       500-SYSYMD-HENSYU-SEC       SECTION.
      *
      **** ACCEPT  WRK-SYSYMD          FROM DATE
      **** MOVE    20                  TO  WRK-SYSYY
      *
      *    �����ƥ����ռ���
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYSYMD-8
      *
           .
       500-SYSYMD-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    ����ִ����ޥ����ɹ�
      *****************************************************************
       910-JOBKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jobkanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  WRK-JOBKANRI-REC
                   MOVE    ZERO                TO  FLG-JOBKANRI
               ELSE
                   MOVE    MCP-RC              TO  FLG-JOBKANRI
               END-IF
           ELSE
               MOVE    MCP-RC                  TO  FLG-JOBKANRI
           END-IF
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-CLOSE-SEC
      *
           .
       910-JOBKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ����ޥ����ɹ��ʣˣţ٣���
      *****************************************************************
       910-JOBKANRI-INV2-SEC         SECTION.
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jobkanri"  TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  WRK-JOBKANRI-REC
                   MOVE    ZERO                TO  FLG-JOBKANRI
               ELSE
                   MOVE    MCP-RC              TO  FLG-JOBKANRI
               END-IF
           ELSE
               MOVE    MCP-RC                  TO  FLG-JOBKANRI
           END-IF
      *
           MOVE    "tbl_jobkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-CLOSE-SEC
      *
           .
       910-JOBKANRI-INV2-EXT.
           EXIT.
      *
      *****************************************************************
      *    MONBATCH��������
      *****************************************************************
       910-MONBATCH-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MONBATCH
      *
           INITIALIZE                      SHELLTBL
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               PERFORM 910-MONBATCH-FET-SEC
           ELSE
               MOVE    1               TO  FLG-MONBATCH
           END-IF
      *
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-MONBATCH-SEL-EXT.
           EXIT.
      *****************************************************************
      *    MONBATCH FETCH����
      *****************************************************************
       910-MONBATCH-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-MONBATCH
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-MONBATCH    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "shell"         TO  MCP-TABLE
               MOVE    LOW-VALUE       TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
      *
                   MOVE    MCPDATA-REC TO  SHELLTBL
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( SHELLTBL-HOSPNUM    =  SPA-HOSPNUM )
                    AND  ( SHELLTBL-SHELLID    =  JOB-SHELLID )
                    AND  ( SHELLTBL-JOBID      =  JOB-JOBID )
                       CONTINUE
                   ELSE
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-MONBATCH
               END-IF
      *
           END-PERFORM
      *
           .
       910-MONBATCH-FET-EXT.
           EXIT.
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
      *    �ģ¡��ãϣͣͣɣԽ���
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
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
           MOVE    "DBSTART"           TO  MCP-FUNC
           MOVE    LOW-VALUE           TO  MCP-TABLE
           MOVE    LOW-VALUE           TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.