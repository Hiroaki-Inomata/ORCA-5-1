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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSMILOAD.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���޾�������
      *  ����ݡ��ͥ��̾  : ���ޥ��᡼���ɹ�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  14/07/29    NACL�ݰ�ƣ    ��������
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
      *    ���᡼���ǡ����ե�����
           SELECT  IMAGE-FILE      ASSIGN  IMAGE-FILENAME
                                   ORGANIZATION    IS  LINE  SEQUENTIAL
                                   FILE    STATUS  IS  STS-IMAGE.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���᡼���ǡ����ե�����
       FD  IMAGE-FILE.
       01  IMAGE-REC               PIC X(500000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-IMAGE           PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-MEDIMAGE        PIC 9(01).
      *
      *    ���᡼���ǡ����ե�����̾
       01  IMAGE-FILENAME          PIC X(256).
       01  MED-FILENAME            PIC X(512).
      *
      *    ���ޥ��
       01  COMMAND                 PIC X(512).
      *
      *    �ֵ���
       01  RESULT                  PIC S9(09).
      *
      *    �����ե�����ե����̾
       01  WRK-CONS-AREA.
           03  WRK-CONS-MEDIMGDIR  PIC X(08)   VALUE   "medimage".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "CPMEDIMAGE.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �ե�����¸�ߥ����å�
           COPY    "CPORCSACCESS.INC".
      *
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
       01  MCPDATA.
           03  MCPREC              PIC X(510000).
      *
      ****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSMIAREA.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION
                                       USING
                                       ORCSMIAREA
                                       SPA-AREA
           .
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
           .
       000-PROC-EXT.
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
      *
      *    ���᡼���ǡ����ե�����̾
           MOVE    SPACE           TO  IMAGE-FILENAME
           STRING  ORCSMI-TEMPDIR      DELIMITED BY SPACE
                   "/"                 DELIMITED BY SIZE
                   ORCSMI-SRYCD        DELIMITED BY SPACE
                   ".txt"              DELIMITED BY SIZE
                               INTO    IMAGE-FILENAME
           END-STRING
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    ���᡼���ǡ����ɤ߹���
           PERFORM 900-MEDIMAGE-READ-SEC
      *
      *    ���᡼���ǡ����������
           IF      FLG-MEDIMAGE        =   ZERO
      *
      *        ���᡼���ǡ����ե��������
               OPEN    OUTPUT                  IMAGE-FILE
               IF      STS-IMAGE       NOT =   ZERO
                   DISPLAY "ORCSMILOAD IMAGE FILE OPEN ERROR"
                   MOVE    1                   TO  ORCSMI-RETCD
                   MOVE    1                   TO  FLG-END
                   GO  TO  200-MAIN-EXT
               ELSE
                   MOVE    MEDIMG-IMAGEDATA
                                           TO  IMAGE-REC
                   WRITE   IMAGE-REC
                   CLOSE   IMAGE-FILE
               END-IF
      *
      *        �����ե�����ե�����������ޥ��
               MOVE    LOW-VALUE       TO  COMMAND
               STRING  "mkdir -p "         DELIMITED BY SIZE
                       ORCSMI-TEMPDIR      DELIMITED BY SPACE
                       "/"                 DELIMITED BY SIZE
                       WRK-CONS-MEDIMGDIR  DELIMITED BY SIZE
                                   INTO    COMMAND
               END-STRING
      *        ���ޥ�ɼ¹�
               CALL    "system"            USING
                                           COMMAND
      *        ���᡼���ǡ�����������ե�����������ޥ��
               MOVE    SPACE           TO  MED-FILENAME
               STRING  ORCSMI-TEMPDIR      DELIMITED BY SPACE
                       "/"                 DELIMITED BY SIZE
                       WRK-CONS-MEDIMGDIR  DELIMITED BY SIZE
                       "/"                 DELIMITED BY SIZE
                       ORCSMI-FILENAME     DELIMITED BY SPACE
               INTO    MED-FILENAME
               END-STRING
      *
               MOVE    LOW-VALUE       TO  COMMAND
               STRING  "base64 -d "        DELIMITED BY SIZE
                       IMAGE-FILENAME      DELIMITED BY SPACE
                       " > "               DELIMITED BY SIZE
                       MED-FILENAME        DELIMITED BY SPACE
                       " || rm -f "        DELIMITED BY SIZE
                       MED-FILENAME        DELIMITED BY SPACE
               INTO    COMMAND
               END-STRING
      *        ���ޥ�ɼ¹�
               CALL    "system"            USING
                                           COMMAND
      *
               INITIALIZE                  SACCESS-AREA
               MOVE    MED-FILENAME    TO  SACCESS-IN-FILENAME
               CALL    "ORCSACCESS"        USING
                                           SACCESS-AREA
               IF    ( SACCESS-RC      NOT =   ZERO )
                   MOVE    1           TO  ORCSMI-RETCD
               END-IF
           END-IF
      *
           MOVE    1               TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
      *    ���᡼���ǡ����ե����������ޥ��
           MOVE    LOW-VALUE       TO  COMMAND
           STRING  "rm -f "            DELIMITED BY SIZE
                   IMAGE-FILENAME      DELIMITED BY SPACE
                               INTO    COMMAND
           END-STRING
      *    ���ޥ�ɼ¹�
           CALL    "system"            USING
                                       COMMAND
           .
       300-TERM-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���᡼���ǡ����ɤ߹���  ����
      *****************************************************************
       900-MEDIMAGE-READ-SEC       SECTION.
      *
           INITIALIZE                  MEDIMAGE-REC
           MOVE    SPA-HOSPNUM     TO  MEDIMG-HOSPNUM
           MOVE    ORCSMI-SRYCD    TO  MEDIMG-SRYCD
           MOVE    MEDIMAGE-REC    TO  MCPREC
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_med_image" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPREC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_med_image" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "MONFUNC"           USING
                                           MCPAREA
                                           MCPREC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-MEDIMAGE
                   MOVE    MCPREC          TO  MEDIMAGE-REC
               ELSE
                   MOVE    1               TO  FLG-MEDIMAGE
               END-IF
           ELSE
               MOVE    1               TO  FLG-MEDIMAGE
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_med_image" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       MCPREC
           .
       900-MEDIMAGE-READ-EXT.
           EXIT.