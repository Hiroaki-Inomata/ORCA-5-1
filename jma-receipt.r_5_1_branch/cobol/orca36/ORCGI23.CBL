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
       PROGRAM-ID.                 ORCGI23.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���ԾȲ�
      *  ����ݡ��ͥ��̾  : �������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  02/06/26    MCC�����ġ�   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  04.01.01    NACL-ƣ��    07/10/19  ���饤�������¸�б�
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
      *
      *    �������ԾȲ��̣ӣУ�
           COPY    "I2SCR-SPA".
      *
      *    �������ԾȲ��Ѷ��̥ѥ�᥿
           COPY    "I2COMMON-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
      *
       01  WRK-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-SYMD-TAIHI      PIC X(08).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-PAGE            PIC ZZ,ZZZ,ZZ9.
           03  WRK-KENSU-Z         PIC ZZZZZZ9.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-SS          PIC X(02).
      *
       01  CONST-AREA.
           03  CONST-JOBID         PIC 9(07)   VALUE   1.
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
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    ���饤�������¸�ģ����極��
           COPY    "CPORCSFILESV.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01 SCRAREA.
           COPY    "ORCA36SCRAREA.INC".
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
           MOVE    SPA-FREE        TO  SPA-I2FREE
           MOVE    SPA-WORK        TO  SPA-I2KYOUTU
      * 
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN      OTHER
                   PERFORM 200-GMNSENI
           END-EVALUATE
      *
           MOVE    SPA-I2KYOUTU    TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-I2FREE      TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           PERFORM 490-KAKUNIN-SEC
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "I23"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
      *     
      *****************************************************************
      *    ��롡����
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
           MOVE    SPA-I23-MOTOPG      TO  SPA-SAKIPG
           MOVE    "I23"               TO  SPA-MOTOPG
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
           MOVE    SPACE               TO  I23
           INITIALIZE                      I23
           MOVE    ZERO                TO  I23-COUNT
      *
           MOVE    5                   TO  I23-DURATION
      *
      *    ����ִ��������å�����
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    CONST-JOBID     TO  JOB-JOBID
           MOVE    SPA-I23-SHELLID TO  JOB-SHELLID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA
                                   JOBKANRI-REC
                                   SPA-AREA
           IF    ( SJOBKANRI-RETURN    =   ZERO )
               CONTINUE
           ELSE
               MOVE    "������������Ϥ���ޤ���"
                                               TO  I23-MSG
               MOVE    "B01"               TO  MCP-WIDGET
               PERFORM 4901-PUT-CURRENT-WINDOW-SEC
               GO  TO  490-KAKUNIN-EXT
           END-IF
      *
           MOVE    JOB-JOBID       TO  I23-RENNUM   (1)
           MOVE    JOB-SHELLMSG    TO  I23-SHORINM  (1)
      *
           MOVE    JOB-STARTYMD    TO  WRK-SYMD
           PERFORM 700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  I23-STARTDATE (1)
      *
           IF    ( JOB-STARTTIME   =   SPACE )
               MOVE    SPACE   TO  I23-STARTTIME(1)
           ELSE
               MOVE    JOB-STARTTIME(1:2)
                                   TO  WRK-HH
               MOVE    JOB-STARTTIME(3:2)
                                   TO  WRK-MM
               MOVE    JOB-STARTTIME(5:2)
                                   TO  WRK-SS
               MOVE    WRK-HENTIME TO  I23-STARTTIME(1)
           END-IF
      *
           MOVE    JOB-ENDYMD      TO  WRK-SYMD
           PERFORM 700-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  I23-ENDDATE (1)
      *
           IF    ( JOB-ENDTIME =   SPACE )
               MOVE    SPACE       TO  I23-ENDTIME  (1)
           ELSE
               MOVE    JOB-ENDTIME(1:2)
                                   TO  WRK-HH
               MOVE    JOB-ENDTIME(3:2)
                                   TO  WRK-MM
               MOVE    JOB-ENDTIME(5:2)
                                   TO  WRK-SS
               MOVE    WRK-HENTIME TO  I23-ENDTIME  (1)
           END-IF
           MOVE    JOB-UPDCNT      TO  WRK-PAGE
           MOVE    WRK-PAGE        TO  I23-PAGE     (1)
           MOVE    JOB-YOBI        TO  I23-ERRMSG   (1)
      *
           IF    ( JOB-ENDYMD      =   SPACE )
               MOVE    "������Ǥ�"
                                   TO  I23-MSG
           ELSE
               IF    ( JOB-ERRCD   =   SPACE )
                   MOVE    "����������˽�λ���ޤ���"
                                               TO  I23-MSG
               ELSE
                   STRING  JOB-YOBI    DELIMITED  BY  SPACE
                           "��"        DELIMITED  BY  SIZE
                          JOB-SHELLMSG DELIMITED  BY  SPACE
                          "��"         DELIMITED  BY  SIZE
                   INTO  I23-MSG
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    1                TO  I23-COUNT
      *
           MOVE    "B11"            TO  MCP-WIDGET
      *
           IF    ( SPA-I2ID1-FLG    =   "SV" )
               IF      JOB-ENDYMD  NOT =   SPACE
                   IF      JOB-ERRCD       =   "9999"
                       CONTINUE
                   ELSE
                       IF      SPA-I23-FILESV-CHK-FLG  =   ZERO
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
           PERFORM 4901-PUT-CURRENT-WINDOW-SEC
      *
           .
       490-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����̺�ɽ������
      *****************************************************************
       4901-PUT-CURRENT-WINDOW-SEC     SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "I23    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
      *
       4901-PUT-CURRENT-WINDOW-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���饤�������¸��������å�����
      *****************************************************************
       4902-FILE-INFO-CHK-SEC          SECTION.
      *
           INITIALIZE                  ORCSFILESVAREA
           MOVE    "R"             TO  SFILESV-MODE
           MOVE    SPA-I23-SHELLID TO  SFILESV-TBL-KEY
           MOVE    SPA-I23-SELHMS  TO  SFILESV-SRYYM(1)
           CALL    "ORCSFILESV"    USING
                                       ORCSFILESVAREA
                                       SPA-AREA  
           IF      SFILESV-RETURN  =   ZERO
               INITIALIZE               SPA-SFILESV-AREA
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       100
                       OR      SFILESV-SHELLID (IDX)
                                   =       SPACE
                   MOVE    SFILESV-TITLE        (IDX)
                                       TO  SPA-SFILESV-TITLE     (IDX)
                   MOVE    SFILESV-CNT-ALL      (IDX)
                                       TO  SPA-SFILESV-CNT-ALL   (IDX)
                   MOVE    SFILESV-FOR-FOLDER   (IDX)
                                       TO  SPA-SFILESV-FOR-FOLDER(IDX)
                   MOVE    SFILESV-FOR-DATA     (IDX)
                                       TO  SPA-SFILESV-FOR-DATA  (IDX)
                   MOVE    SFILESV-TO-FOLDER    (IDX)
                                       TO  SPA-SFILESV-TO-FOLDER (IDX)
                   MOVE    SFILESV-TO-DATA      (IDX)
                                       TO  SPA-SFILESV-TO-DATA   (IDX)
                   MOVE    SFILESV-CODE-TYPE    (IDX)
                                       TO  SPA-SFILESV-CODE-TYPE (IDX)
                   MOVE    SFILESV-DATA-TYPE    (IDX)
                                       TO  SPA-SFILESV-DATA-TYPE (IDX)
                   ADD     1           TO  SPA-SFILESV-MAX
               END-PERFORM
      *
               MOVE    1                   TO  SPA-I23-FILESV-CHK-FLG
      *
               MOVE    1                   TO  I23-DURATION
           END-IF
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
           MOVE    ORCA-BLOB-OBJECT    TO  I23-OBJECTDATA
           MOVE    SPA-SFILESV-TO-DATA   (SPA-SFILESV-IDX)
                                       TO  I23-FILENAME
           MOVE    ZERO                TO  I23-DURATION
      *
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
           .
       3001-FILEENTRY-EXT.
           EXIT.
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       700-SEIWA-HEN-SEC             SECTION.
      *
           IF    ( WRK-SYMD    NOT =   SPACE)
            AND  ( WRK-SYMD (7:2)  =   SPACE)
               MOVE    "20"            TO  WRK-SYMD-TAIHI(1:2)
               MOVE    WRK-SYMD (1:6)  TO  WRK-SYMD-TAIHI(3:6)
           ELSE
               MOVE    WRK-SYMD        TO  WRK-SYMD-TAIHI
           END-IF
      *
           MOVE    WRK-SYMD-TAIHI      TO  WRK-SYMD
      *
           IF    ( WRK-SYMD        =   ALL "9"  OR   SPACE )
               MOVE    SPACE           TO  WRK-HENYMDG
           ELSE
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"            TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1
                                       TO  WRK-HENYMDG
           END-IF.
      *
       700-SEIWA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *