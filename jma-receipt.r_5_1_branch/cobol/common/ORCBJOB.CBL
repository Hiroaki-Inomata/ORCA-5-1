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
       PROGRAM-ID.             ORCBJOB.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����ִ���
      *  ����ݡ��ͥ��̾  : ��λ����񤭹���
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/01/20    MCC��ƣ����   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-��ƣ    04/02/17  ���顼�����ɥ��å�
      *
      *  03.05.01    NACL-ƣ��    07/04/19  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       WORKING-STORAGE             SECTION.
grpsys*
grpsys COPY    "COMMON-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                                 PIC 9(01).
           03  FLG-JOBKANRI                            PIC 9(01).
           03  FLG-RETURN                              PIC S9(03).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-IN                                  PIC 9(06).
      *
       01  INDEX-AREA.
           03  IDX                                     PIC 9(03). 
      *
      *    ����ΰ�
      *
       01  WRK-PARA.
           03  WRK-PARA-MODE             PIC X(03). 
           03  WRK-PARA-JOBID            PIC 9(07).
           03  WRK-PARA-SHELLID          PIC X(08).
           03  WRK-PARA-ERRCD            PIC X(04).
grpsys 01  WRK-PARA-HOSPNUM              PIC 9(02).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
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
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *    COPY    "ORCA-DBPATH".
      *
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      **************************************************************************
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
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-TERM-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *???
           DISPLAY "ORCBJOB=" WRK-PARA-MODE "##"
      *???          
      *
           PERFORM 100-DBOPEN-SEC
      *
           MOVE    ZERO                TO  FLG-END
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
           IF      WRK-PARA-MODE   =   "JBS"
      *        ����ִ������
               MOVE    "DEL"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF                            
      *
      *   �ƥ��ȳ���
           MOVE    WRK-PARA-MODE   TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           IF      WRK-PARA-ERRCD  =   SPACE    OR
                   WRK-PARA-ERRCD  =   X"00000000"
             MOVE  SPACE           TO  JOB-ERRCD
           ELSE
             MOVE  WRK-PARA-ERRCD  TO  JOB-ERRCD
           END-IF
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
           DISPLAY  "ORCBJOB INSERT = " "[" SJOBKANRI-RETURN "]"
      *
           .
       200-MAIN-EXT.
           EXIT.             
      * 
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-TERM-EXT.
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