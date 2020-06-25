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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGS99.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ��Ǽ
      *  ����ݡ��ͥ��̾  : �������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  04/11/25    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *  03.04.01    NACL-����     06/12/19  �ץ�ӥ塼�ɲ�
      *  03.05.00    NACL-����     07/05/20  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *----(03.04.01)--ADD-START---
           COPY    "S01COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *----(03.04.01)--ADD-END-----
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-PAGE            PIC ZZZZZZZZZZ.
      *----(03.04.01)--ADD-START---
           03  WRK-PRT-PAGE        PIC 9(10).
      *----(03.04.01)--ADD-END-----
      *     
       01  WRK-HEN-AREA.
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-CONS-AREA.
      *    �����ƥ�����ģ¡ʣ��������ˤΥ�����  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "023".
      *    ���������ģ¤Υ�����  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "ORCBS01".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ����ִ����ģ¤Υ�����  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "ORCBS01".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "CPSK3003.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *     
      *     COPY    "CPORCMCP.INC".  
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA23SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
      *----(03.04.01)--ADD-START---
           MOVE    SPA-WORK        TO  SPA-S01KYOUTU
      *----(03.04.01)--ADD-END-----
      * 
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN    OTHER
                   PERFORM 200-GMNSENI
           END-EVALUATE
      *
      *----(03.04.01)--ADD-START---
           MOVE    SPA-S01KYOUTU   TO  SPA-WORK
      *----(03.04.01)--ADD-END----
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      * 
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *    ���
               WHEN    "CLICKED"   ALSO    "B01"
                   MOVE    SPACE           TO  SPA-S99-FLG
                   PERFORM 210-BACK
      *    ������
               WHEN    "CLICKED"   ALSO    "B01S"
                   MOVE    "DEL"           TO  SPA-S99-FLG
                   PERFORM 210-BACK
      *    ��������
               WHEN    "CLICKED"   ALSO    "pandatimer1"
               WHEN    "CLICKED"   ALSO    "B11"
                   PERFORM 490-KAKUNIN-SEC
      *    �ץ�ӥ塼���̤�
               WHEN    "CLICKED"   ALSO    "B12"
                   MOVE    "OK"            TO  SPA-S99-FLG
                   PERFORM 210-BACK
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-MOTOPG      TO  SPA-SAKIPG
           MOVE    "S99"           TO  SPA-MOTOPG
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ǧ����
      *****************************************************************
       490-KAKUNIN-SEC             SECTION.
      *     
           MOVE    SPACE               TO  S99
           INITIALIZE                      S99
           MOVE    ZERO                TO  S99-COUNT
      *----(03.04.01)--ADD-START---
           MOVE    WIDGET-INSENSITIVE  TO  S99-B12-STATE
           MOVE    ZERO                TO  WRK-PRT-PAGE
      *----(03.04.01)--ADD-END----
      *
      *----(03.04.01)--UPD-START---
           IF      SPA-MOTOPG          =   "S02"
               MOVE    SPACE               TO  SYS-3003-REC 
               INITIALIZE                      SYS-3003-REC
               MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
               MOVE    "3003"              TO  SYS-3003-KANRICD
               MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                               SYS-3003-EDYUKYMD
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-3003-REC 
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SYS-3003-JOBID
      *                ����ִ��������å�����
                       MOVE    "CHK"           TO  SJOBKANRI-MODE
                       INITIALIZE                  JOBKANRI-REC
                       MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                       MOVE    IDX             TO  JOB-JOBID
                       MOVE    WRK-CONS-JOB-SHELLID
                                               TO  JOB-SHELLID
                       CALL    "ORCSJOB"       USING
                                               ORCSJOBKANRIAREA  
                                               JOBKANRI-REC 
                                               SPA-AREA
                       IF      SJOBKANRI-RETURN    =   ZERO
                           PERFORM 4901-KAKUNIN-HYOUJI-SEC
                       END-IF
                   END-PERFORM
               END-IF
           ELSE
      *        ����ִ��������å�����
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
               MOVE    WRK-CONS-JOB-JOBID
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID
                                       TO  JOB-SHELLID
               CALL    "ORCSJOB"       USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
               MOVE    1               TO  IDX 
               IF      SJOBKANRI-RETURN    =   ZERO
                   PERFORM 4901-KAKUNIN-HYOUJI-SEC
               END-IF
           END-IF
      *
           MOVE    "B01"                TO  MCP-WIDGET
           IF    ( S99-MSG              =   SPACE )
               IF    ( S99-COUNT        >   ZERO  )
                   MOVE    "����������˽�λ���ޤ���"
                                                TO  S99-MSG
               ELSE
                   MOVE    "������������Ϥ���ޤ���"
                                                TO  S99-MSG
               END-IF 
           END-IF                                 
      *
           MOVE    SPACE               TO  LINKAREA
      *----(03.04.01)--UPD-END-----        
      *
      *----(03.04.01)--ADD-START---
           IF    ( SPA-S01-SHR-ORDER   =   "PV" )
               MOVE    "F12:�ץ�ӥ塼"    TO  S99-B12-LABEL
               IF    ( WRK-PRT-PAGE    >   ZERO )
                       MOVE    WIDGET-NORMAL    TO  S99-B12-STATE
                       MOVE    "B12"            TO  MCP-WIDGET
               END-IF
           END-IF
      *----(03.04.01)--ADD-END-----
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "S99    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-START---
      *****************************************************************
      *    ������ǧ����
      *****************************************************************
       4901-KAKUNIN-HYOUJI-SEC     SECTION.
      *
           MOVE    JOB-JOBID       TO  S99-RENNUM   (IDX)
           MOVE    JOB-SHELLMSG    TO  S99-SHORINM  (IDX)
           MOVE    JOB-STARTTIME(1:2)
                                   TO  WRK-HH
           MOVE    JOB-STARTTIME(3:2)
                                   TO  WRK-MM
           MOVE    JOB-STARTTIME(5:2)
                                   TO  WRK-SS
           MOVE    WRK-HENTIME     TO  S99-STARTTIME(IDX)
           MOVE    JOB-ENDTIME(1:2)
                                   TO  WRK-HH
           MOVE    JOB-ENDTIME(3:2)
                                   TO  WRK-MM
           MOVE    JOB-ENDTIME(5:2)
                                   TO  WRK-SS
           MOVE    WRK-HENTIME     TO  S99-ENDTIME  (IDX)
           MOVE    JOB-UPDCNT      TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  S99-PAGE     (IDX)
           MOVE    JOB-YOBI        TO  S99-ERRMSG   (IDX)
           IF      JOB-ENDYMD      =   SPACE
               MOVE    "������Ǥ�"    TO  S99-MSG
           ELSE
               IF      JOB-ERRCD       =   SPACE
                   CONTINUE
               ELSE
                   INITIALIZE          S99-MSG
                   STRING  JOB-YOBI    DELIMITED  BY  SPACE
                           "��"        DELIMITED  BY  SIZE
                          JOB-SHELLMSG DELIMITED  BY  SPACE
                          "��"         DELIMITED  BY  SIZE      
                                            INTO  S99-MSG
                   END-STRING
               END-IF                          
           END-IF  
          MOVE    IDX                     TO  S99-COUNT    
          IF    ( JOB-JOBID   =   SPA-S02-JOB-JOBID )
              MOVE    JOB-UPDCNT  TO  WRK-PRT-PAGE
          END-IF  
           .
       4901-KAKUNIN-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å��������åȽ���
      *****************************************************************
       4901-JOB-ERRSET-SEC         SECTION.
      *
           EVALUATE    JOB-ERRCD
               WHEN    "0001"
                   MOVE    "�ڥե����뤬����ޤ����" 
                                               TO  S99-MSG
               WHEN    "0002"
                   MOVE    "�ڣ��䤬��ư����Ƥ��ޤ����" 
                                               TO  S99-MSG
               WHEN    "0003"
               WHEN    "0004"
                   MOVE    "�ڣ���ǽ��������Ԥ��ޤ�����" 
                                               TO  S99-MSG
               WHEN    "0011"
                   MOVE    "�����Τ��ޥ���ȤǤ��ޤ���Ǥ�����" 
                                               TO  S99-MSG
               WHEN    "0012"
                   MOVE    "�����Τ�����ޥ���ȤǤ��ޤ���Ǥ�����" 
                                               TO  S99-MSG
               WHEN    "0021"
                   MOVE    "�ڥե����륳�ԡ��ǥ��顼��ȯ�����ޤ�����" 
                                               TO  S99-MSG
               WHEN    "0127"
                   MOVE    "�ڥ�����ץȤ��¹ԤǤ��ޤ���Ǥ�����" 
                                               TO  S99-MSG
           END-EVALUATE
      *
           .
       4901-JOB-ERRSET-EXT.
           EXIT.
      *
      *----(03.04.01)--ADD-END-----
      *****************************************************************
      *    �����ޥ������ɹ��ʥ�����
      *****************************************************************
       900-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE    "PUTWINDOW"     TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *