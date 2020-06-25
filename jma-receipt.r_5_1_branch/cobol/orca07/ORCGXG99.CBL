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
       PROGRAM-ID.                 ORCGXG99.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ףãӣֽ���
      *  ����ݡ��ͥ��̾  : �������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  09/09/30    NACL-ƣ��     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.06.00    NACL-����    10/11/25  ���饤�������¸��ĥ�б�
      *
      *  04.08.01    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
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
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
           COPY    "XG01COMMON-SPA".
      *
      *    �����ѣӣУ�
           COPY    "XG01SCR-SPA".
      *   
           COPY    "ENUM-VALUE".
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
           03  WRK-PAGE            PIC ZZZZZZZZZ9.
           03  WRK-KENSU-Z         PIC ZZZZZZ9.
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
      *    ����ִ����ģ¤Υ�����  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "PUTCSV".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   1.
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
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA07SCRAREA.INC".
      *
      ******************************************************************
       PROCEDURE                   DIVISION    USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-XG01FREE
           MOVE    SPA-WORK        TO  SPA-XG01KYOUTU
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN      OTHER
                   PERFORM 200-GMNSENI
           END-EVALUATE
      *
           MOVE    SPA-XG01KYOUTU  TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XG01FREE    TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *****MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XG99"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      * 
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    ��������
               WHEN    "CLICKED"       ALSO    "pandatimer1"
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-KAKUNIN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ������
      *****************************************************************
       210-BACK                    SECTION.
      *
      *    ���饤�������¸�Ѻ�ȥե�������
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-SFILESV-MAX
               INITIALIZE              ORCSFDELAREA
               STRING  SPA-SFILESV-FOR-FOLDER (IDX)
                                           DELIMITED  BY  SPACE
                       SPA-SFILESV-FOR-DATA   (IDX) 
                                           DELIMITED  BY  SPACE
                                           INTO  IF-FILENAME
               END-STRING
               CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-XG99-FLG
      *
           MOVE    SPA-MOTOPG          TO  SPA-SAKIPG
           MOVE    "XG99"              TO  SPA-MOTOPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ǧ����
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *     
           INITIALIZE                      XG99
           MOVE    ZERO                TO  XG99-COUNT
      *
           MOVE    5                   TO  XG99-DURATION
      *
      *   ����ִ��������å�����
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
           IF      SJOBKANRI-RETURN    =   ZERO
               PERFORM 4901-KAKUNIN-HYOUJI-SEC
           END-IF
      *
           MOVE    "B11"               TO  MCP-WIDGET
           IF    ( XG99-MSG             =   SPACE )
               MOVE    "B01"           TO  MCP-WIDGET
               IF    ( XG99-COUNT       >   ZERO )
                   MOVE    "����������˽�λ���ޤ���"
                                       TO  XG99-MSG
               ELSE
                   MOVE    "������������Ϥ���ޤ���"
                                       TO  XG99-MSG
               END-IF
           END-IF
      *
           IF    ( SPA-XGID1-FLG   =   "SV" )
               IF      JOB-ENDYMD  NOT =   SPACE
                   IF      JOB-ERRCD       =   "9999"
                       CONTINUE
                   ELSE
                       IF      SPA-XG99-FILESV-CHK-FLG  =   ZERO
                           PERFORM 4902-FILE-INFO-CHK-SEC
                       ELSE
      *                    �ե����륨��ȥ����(���饤�������¸)
                           MOVE    1               TO  SPA-SFILESV-IDX 
                           PERFORM 3001-FILEENTRY-SEC
                           IF      FLG-END         =   1
                               GO  TO  490-KAKUNIN-EXT
                           END-IF
                       END-IF
                   END-IF
               END-IF    
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "XG99"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ǧ����
      *****************************************************************
       4901-KAKUNIN-HYOUJI-SEC            SECTION.
      *
           MOVE    1               TO  IDX     
           MOVE    JOB-JOBID       TO  XG99-RENNUM   (IDX)
           MOVE    JOB-SHELLMSG    TO  XG99-SHORINM  (IDX)
           MOVE    JOB-STARTTIME(1:2)
                                   TO  WRK-HH
           MOVE    JOB-STARTTIME(3:2)
                                   TO  WRK-MM
           MOVE    JOB-STARTTIME(5:2)
                                   TO  WRK-SS
           MOVE    WRK-HENTIME     TO  XG99-STARTTIME(IDX)
           MOVE    JOB-ENDTIME(1:2)
                                   TO  WRK-HH
           MOVE    JOB-ENDTIME(3:2)
                                   TO  WRK-MM
           MOVE    JOB-ENDTIME(5:2)
                                   TO  WRK-SS
           MOVE    WRK-HENTIME     TO  XG99-ENDTIME  (IDX)
           MOVE    JOB-UPDCNT      TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  XG99-PAGE     (IDX)
           MOVE    JOB-YOBI        TO  XG99-ERRMSG   (IDX)
           IF      JOB-ENDYMD      =   SPACE
               MOVE    "������Ǥ�"    TO  XG99-MSG
           ELSE
              IF      JOB-ERRCD       =   SPACE
                  CONTINUE
              ELSE
                  MOVE    SPACE       TO  XG99-MSG
                  PERFORM 4901-JOB-ERRSET-SEC
                  IF      XG99-MSG     =   SPACE
                       STRING  JOB-YOBI        DELIMITED BY SPACE
                               "��"            DELIMITED BY SIZE
                               JOB-SHELLMSG    DELIMITED BY SPACE
                               "��"            DELIMITED BY SIZE
                                               INTO  XG99-MSG
                       END-STRING   
                   END-IF
              END-IF                          
           END-IF    
           MOVE    IDX                     TO  XG99-COUNT    
           .
       4901-KAKUNIN-HYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���饤�������¸��������å�����
      *****************************************************************
       4902-FILE-INFO-CHK-SEC      SECTION.
      *
           MOVE    1               TO  IDX
           MOVE    SPA-GMN-TPRT-TITLE (SPA-NAI-SELNUM)
                                   TO  SPA-SFILESV-TITLE     (IDX)
           MOVE    SPA-GMN-TKENSU     (SPA-NAI-SELNUM)
                                   TO  SPA-SFILESV-CNT-ALL   (IDX)
      *****MOVE    "/tmp/"         TO  SPA-SFILESV-FOR-FOLDER(IDX)
           STRING  MCP-TEMPDIR         DELIMITED  BY  SPACE
                   "/"                 DELIMITED  BY  SIZE
                                   INTO    SPA-SFILESV-FOR-FOLDER (IDX)
           END-STRING 
           STRING  SPA-HOSPNUM             DELIMITED  BY  SIZE  
                   SPA-NAI-XG02-TO-DATA    DELIMITED  BY  SPACE
                                   INTO  SPA-SFILESV-FOR-DATA  (IDX)
           END-STRING
           STRING  SPA-NAI-XG02-TO-FOLDER
                                       DELIMITED   BY  SPACE
                   "/"                 DELIMITED   BY  SIZE 
                                       INTO  SPA-SFILESV-TO-FOLDER(IDX)
           END-STRING
           MOVE    SPA-NAI-XG02-TO-DATA
                                   TO  SPA-SFILESV-TO-DATA   (IDX)
           MOVE    SPA-GMN-XG02-CDKBN
                                   TO  SPA-SFILESV-DATA-TYPE (IDX)
           MOVE    1               TO  SPA-SFILESV-MAX
      *
           MOVE    1               TO  SPA-XG99-FILESV-CHK-FLG
      *
           MOVE    1               TO  XG99-DURATION
      *
           .
       4902-FILE-INFO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ե����륨��ȥ����(���饤�������¸)
      *****************************************************************
       3001-FILEENTRY-SEC          SECTION.
      *
           INITIALIZE                      ORCA-BLOB
           MOVE    "BLOBIMPORT"        TO  MCP-FUNC
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "blob"              TO  MCP-TABLE
           STRING  SPA-SFILESV-FOR-FOLDER (SPA-SFILESV-IDX)
                                       DELIMITED  BY  SPACE
                   SPA-SFILESV-FOR-DATA   (SPA-SFILESV-IDX) 
                                       DELIMITED  BY  SPACE
                                       INTO  ORCA-BLOB-FILE
           END-STRING
           CALL   "ORCDBMAIN"          USING
                                       MCPAREA
                                       ORCA-BLOB
                                       SPA-AREA
      *
           MOVE    ORCA-BLOB-OBJECT    TO  XG99-OBJECTDATA
           MOVE    SPA-SFILESV-TO-DATA   (SPA-SFILESV-IDX)
                                       TO  XG99-FILENAME
           MOVE    ZERO                TO  XG99-DURATION
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       3001-FILEENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å��������åȽ���
      *****************************************************************
       4901-JOB-ERRSET-SEC             SECTION.
      *
           EVALUATE    JOB-ERRCD
               WHEN    "0001"
                   MOVE    "�ڥե�����κ����˼��Ԥ��ޤ�����" 
                                               TO  XG99-MSG
               WHEN    "0002"
                   MOVE    "�ڣ��䤬��ư����Ƥ��ޤ����" 
                                               TO  XG99-MSG
               WHEN    "0003"
               WHEN    "0004"
                   MOVE    "�ڣ���ǽ��������Ԥ��ޤ�����" 
                                               TO  XG99-MSG
               WHEN    "0011"
                   MOVE    "�����Τ��ޥ���ȤǤ��ޤ���Ǥ�����" 
                                               TO  XG99-MSG
               WHEN    "0012"
                   MOVE    "�����Τ�����ޥ���ȤǤ��ޤ���Ǥ�����" 
                                               TO  XG99-MSG
               WHEN    "0021"
                   MOVE    "�ڥե����륳�ԡ��ǥ��顼��ȯ�����ޤ�����" 
                                               TO  XG99-MSG
               WHEN    "0127"
                   MOVE    "�ڥ�����ץȤ��¹ԤǤ��ޤ���Ǥ�����" 
                                               TO  XG99-MSG
           END-EVALUATE
      *
           .
       4901-JOB-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ������ɹ�
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
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
      *    �УգԽ���
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *