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
       PROGRAM-ID.                 ORCGW62.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �����ƥ������������
      *  ����ݡ��ͥ��̾  : Ģɼ�ƥ��Ȱ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/04/01    NACL�ݿ���    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      *  Maj/Min/Rev ������        ����      ����
      *  00.00.01    NACL-����     06.08.02  ϫ�ҡ�����Υץ���ɲ�
      *  03.05.00    NACL-¿��Ǽ  07/05/02  ���롼�׿����б�
      *  04.08.01    NACL-����     16/09/06  GINBEE�б�
      *  05.00.00    NACL-����     17/11/29  ���饤����Ȱ����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ƥ�������������Ѷ��̥ѥ�᥿
           COPY    "W01COMMON-SPA".
      *    �����ѣӣУ�
           COPY    "W11SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(02).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-WIDGET          PIC X(64).
           03  WRK-WIDMSG          PIC X(70).
      *
           03  WRK-PRTNUM          PIC X(17).
           03  WRK-PRTNAME-G.
               05  WRK-PRTNAME     PIC X(16)   OCCURS  16.
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-FILENAME   PIC X(20)   VALUE
                                   "data/test.pdf".
      *    �����ƥ�����ģ¡ʣ��������ˤΥ�����  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "101".
      *    ���������ģ¤Υ�����  
           03  WRK-CONS-PREVIEW-TBL-KEY
                                   PIC X(08)   VALUE   "TSTPRINT".
           03  WRK-CONS-PREVIEW-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ����ִ����ģ¤Υ�����  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "TSTPRINT".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY  "CPSYSKANRI.INC".
      *    ����ץ�󥿾���
           COPY  "CPSK1031.INC".
      *    �����ץ�󥿾���
           COPY  "CPSK1034.INC".
      *    ���硢�쥻�ץ�󥿾���
           COPY  "CPSK2005.INC".
      *    ����ִ�������
          COPY    "CPSK3003.INC".
      *
      *01  PRVKANRI-REC.
           COPY    "CPPRVKANRI.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCSPRTLNK.INC".
      *    �ե�����ǥ��쥯�ȥ���ֻ��ꥵ��
           COPY    "CPMKPASS.INC".
           COPY    "CPSHELLTBL.INC".
      *
      *    �ե�����¸�ߥ����å�
           COPY    "CPORCSFILECHK.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *   ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    COPY    "ORCA-DBPATH".
           COPY    "ORCA-BLOB".
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
       01  SCRAREA.
           COPY    "ORCA101SCRAREA.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-W01KYOUTU
           MOVE    SPA-FREE        TO  SPA-W11
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    ����
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    �������ܤ��ʤ���������ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-W11         TO  SPA-FREE
           MOVE    SPA-W01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "WERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ��������Խ�
               PERFORM 300-SCREEN-SEC
      *
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE           TO  LINKAREA
               MOVE   "NEW"            TO  MCP-PUTTYPE
               MOVE   "W62"            TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1               TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    ¾�Σ̣Ĥ��
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU      TO  SPA-KYOUTU
           END-IF
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-W62-AREA
      *
      *    �ե�������ַ���
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    WRK-CONS-FILENAME
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
      *
      *    �ե�����¸�ߥ����å�
           INITIALIZE                  ORCSFILECHKAREA
           MOVE    MKPASS-OT-01    TO  FILECHK-NAME
           CALL    "ORCSFILECHK"       USING
                                       ORCSFILECHKAREA
           IF          FILECHK-RC      =   0
               MOVE    "BLOBIMPORT"    TO  MCP-FUNC
               MOVE    "blob"          TO  MCP-TABLE
               MOVE    "primary"       TO  MCP-PATHNAME
               MOVE    MKPASS-OT-01    TO  ORCA-BLOB-FILE
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   ORCA-BLOB
                                                   SPA-AREA
               MOVE    ORCA-BLOB-OBJECT    TO  SPA-W62-OBJECT
           END-IF
      *
           MOVE    ZERO            TO  SPA-W62-PRTNUM-MAX
      *
      *    ����ץ�󥿥��å�
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    "1031"              TO  SYS-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-SELECT-SEC
           PERFORM UNTIL   FLG-SYSKANRI    NOT =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1031-REC
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       20
                   MOVE    ZERO            TO  FLG-OK
                   IF      SYS-1031-PRTNM(IDX)     =   SPACE
                       MOVE    1           TO  FLG-OK
                   END-IF
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       20
                           OR      FLG-OK  =   1
                       IF      SPA-W62-PRTNUML(IDY)
                                             =   SYS-1031-PRTNM(IDX)
                           MOVE    1           TO  FLG-OK
                       ELSE
                           IF      SPA-W62-PRTNUML(IDY)    =   SPACE
                               MOVE     IDY    TO  SPA-W62-PRTKBNL(IDY)
                               MOVE     SYS-1031-PRTNM(IDX)
                                               TO  SPA-W62-PRTNUML(IDY)
                               ADD     1       TO  SPA-W62-PRTNUM-MAX
                               MOVE    1       TO  FLG-OK
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           PERFORM 900-SYSKANRI-CLOSE-SEC
      *
      *    ���硢�쥻�ץ�󥿥��å�
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "2005"              TO  SYS-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-SELECT-SEC
           PERFORM UNTIL   FLG-SYSKANRI    NOT =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2005-REC
               MOVE    SPACE           TO  WRK-PRTNAME-G
               MOVE    SYS-2005-SYAPRTNM           TO  WRK-PRTNAME(1)
               MOVE    SYS-2005-KKHPRTNM           TO  WRK-PRTNAME(2)
               MOVE    SYS-2005-SYAPRTNM-NEXT      TO  WRK-PRTNAME(3)
               MOVE    SYS-2005-KKHPRTNM-NEXT      TO  WRK-PRTNAME(4)
               MOVE    SYS-2005-SYAPRTNM-N         TO  WRK-PRTNAME(5)
               MOVE    SYS-2005-KKHPRTNM-N         TO  WRK-PRTNAME(6)
               MOVE    SYS-2005-SYAPRTNM-NEXT-N    TO  WRK-PRTNAME(7)
               MOVE    SYS-2005-KKHPRTNM-NEXT-N    TO  WRK-PRTNAME(8)
               MOVE    SYS-2005-ROUPRTNM           TO  WRK-PRTNAME(9)
               MOVE    SYS-2005-ROUPRTNM-NEXT      TO  WRK-PRTNAME(10)
               MOVE    SYS-2005-ROUPRTNM-N         TO  WRK-PRTNAME(11)
               MOVE    SYS-2005-ROUPRTNM-NEXT-N    TO  WRK-PRTNAME(12)
               MOVE    SYS-2005-JBAIPRTNM          TO  WRK-PRTNAME(13)
               MOVE    SYS-2005-JBAIPRTNM-NEXT     TO  WRK-PRTNAME(14)
               MOVE    SYS-2005-JBAIPRTNM-N        TO  WRK-PRTNAME(15)
               MOVE    SYS-2005-JBAIPRTNM-NEXT-N   TO  WRK-PRTNAME(16)
      *
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       16
                   MOVE    ZERO            TO  FLG-OK
                   IF      WRK-PRTNAME(IDX)    =   SPACE
                       MOVE    1           TO  FLG-OK
                   END-IF
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       20
                           OR      FLG-OK  =   1
                       IF      SPA-W62-PRTNUML(IDY)
                                             =   WRK-PRTNAME(IDX)
                           MOVE    1           TO  FLG-OK
                       ELSE
                           IF      SPA-W62-PRTNUML(IDY)    =   SPACE
                               MOVE     IDY    TO  SPA-W62-PRTKBNL(IDY)
                               MOVE     WRK-PRTNAME(IDX)
                                               TO  SPA-W62-PRTNUML(IDY)
                               ADD     1       TO  SPA-W62-PRTNUM-MAX
                               MOVE    1       TO  FLG-OK
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           PERFORM 900-SYSKANRI-CLOSE-SEC
      *
           IF    ( SPA-BEDSU           >   ZERO )
               CONTINUE
           ELSE
               GO  TO  310-SPASET-EXT
           END-IF
      *
      *    �����ץ�󥿥��å�
           MOVE    SPACE               TO  SYSKANRI-REC
           INITIALIZE                      SYSKANRI-REC
           MOVE    "1034"              TO  SYS-KANRICD
           MOVE    SPA-W01-STYUKYMD    TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-SELECT-SEC
           PERFORM UNTIL FLG-SYSKANRI  NOT =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1034-REC
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       20
                   MOVE    ZERO            TO  FLG-OK
                   IF      SYS-1034-PRTNM(IDX)     =   SPACE
                       MOVE    1           TO  FLG-OK
                   END-IF
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY     >       20
                           OR      FLG-OK  =   1
                       IF      SPA-W62-PRTNUML(IDY)
                                               =   SYS-1034-PRTNM(IDX)
                           MOVE    1           TO  FLG-OK
                       ELSE
                           IF      SPA-W62-PRTNUML(IDY)    =   SPACE
                               MOVE     IDY    TO  SPA-W62-PRTKBNL(IDY)
                               MOVE     SYS-1034-PRTNM(IDX)
                                               TO  SPA-W62-PRTNUML(IDY)
                               ADD     1       TO  SPA-W62-PRTNUM-MAX
                               MOVE    1       TO  FLG-OK
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           PERFORM 900-SYSKANRI-CLOSE-SEC
      *
           MOVE    1               TO  SPA-W62-CUR
      *
          .
       310-SPASET-EXT.
           EXIT.
      *
     *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE.
           MOVE    "W62    "       TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE           TO  W62
           INITIALIZE                  W62
      *
      *----(04.08.01)--ADD-START---
      *    �ե�������ַ���
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    WRK-CONS-FILENAME
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
      *
      *    �ե�����¸�ߥ����å�
           INITIALIZE                  ORCSFILECHKAREA
           MOVE    MKPASS-OT-01    TO  FILECHK-NAME
           CALL    "ORCSFILECHK"       USING
                                       ORCSFILECHKAREA
           IF          FILECHK-RC      =   0
               MOVE    "BLOBIMPORT"    TO  MCP-FUNC
               MOVE    "blob"          TO  MCP-TABLE
               MOVE    "primary"       TO  MCP-PATHNAME
               MOVE    MKPASS-OT-01    TO  ORCA-BLOB-FILE
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   ORCA-BLOB
                                                   SPA-AREA
               MOVE    ORCA-BLOB-OBJECT    TO  SPA-W62-OBJECT
           END-IF
      *----(04.08.01)--ADD-END-----
      *
           MOVE    SPA-W62-OBJECT  TO  W62-PSAREA
           MOVE    SPA-W62-PRTNUM-G    TO  W62-PRTNUM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-W62-PRTNUM-MAX
               MOVE    SPA-W62-PRTNUMLST(IDX)
                                       TO  W62-PRTNUMLST(IDX)
           END-PERFORM
           MOVE    SPA-W62-PRTNUM-MAX  TO  W62-PRTNUM-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-W62-CUR     =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-W62-CUR
               WHEN    1
                   MOVE    "PRTNUM"    TO   MCP-WIDGET
               WHEN    99
                   MOVE    "B12"       TO   MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-WIDGET
               WHEN    "PRTNUM"
                   MOVE    99          TO  SPA-W62-CUR
               WHEN    "B12"
                   MOVE    1           TO  SPA-W62-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                 SECTION.
      *
           EVALUATE    MCP-EVENT   ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"   ALSO    "B01"
                   PERFORM 210-BACK
      *        ����
               WHEN    "CLICKED"   ALSO    "B12"
                   PERFORM 220-KAKUNIN-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    �ץ�󥿥��å�
           PERFORM 2001-PRTNUM-SYORI-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ץ��̾���Ͻ���
      *****************************************************************
       2001-PRTNUM-SYORI-SEC       SECTION.
      *
           MOVE    W62-PRTNUM      TO  SPA-W62-PRTNUM-G
      *
           MOVE    ZERO            TO  FLG-OK
           MOVE    SPACE           TO  WRK-PRTNUM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-W62-PRTNUM-MAX
               IF      SPA-W62-PRTNUM  =   SPA-W62-PRTNUML(IDX)
                   MOVE    SPA-W62-PRTNUMLST(IDX)
                                           TO  SPA-W62-PRTNUM-G
                   MOVE    SPA-W62-PRTNUM-MAX  TO  IDX
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    SPA-W62-PRTNUM-G    TO  WRK-PRTNUM
           ELSE
               MOVE    SPA-W62-PRTNUM      TO  WRK-PRTNUM
           END-IF
      *
           IF      WRK-PRTNUM          =   SPACE
               MOVE    "0001"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-W62-CUR
           ELSE
               MOVE    99              TO  SPA-W62-CUR
           END-IF
      *
           .
       2001-PRTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "W11"           TO  SPA-SAKIPG
           MOVE    "W62"           TO  SPA-MOTOPG
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
           MOVE    "W11"           TO  MCP-WINDOW
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
      *    ��ǧ������
      *****************************************************************
       220-KAKUNIN-SEC             SECTION.
      *
           PERFORM 2001-PRTNUM-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  220-KAKUNIN-EXT
           END-IF
      *
           PERFORM 230-PRINT-SEC
      *
           .
       220-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����������
      *****************************************************************
       230-PRINT-SEC               SECTION.
      *
      *    �ʣϣ£ɣġʣ��������˺������
           MOVE    SPACE           TO  SYS-3003-REC
           INITIALIZE                  SYS-3003-REC
           MOVE    "3003"          TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD
                                   TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-3003-STYUKYMD
                                       SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-3003-REC
               MOVE    SYS-3003-REC    TO  MCPDATA-REC
               MOVE    "DBDELETE"      TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   CONTINUE
               ELSE
                   MOVE    "1005"          TO  SPA-ERRCD
                   GO  TO  230-PRINT-EXT
               END-IF
           END-IF
      *
      *    ����ִ����������
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"          TO  SPA-ERRCD
               GO  TO  230-PRINT-EXT
           END-IF
      *
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM          TO  WRK-SYSMM
           MOVE    SYS-DD          TO  WRK-SYSDD
           ACCEPT  SYS-TIME        FROM    TIME
      *
           MOVE    ZERO            TO  WRK-CONS-JOB-JOBID
           MOVE    SPACE           TO  WRK-CONS-PREVIEW-TBL-GROUP
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
           MOVE    SPACE           TO  SHELLTBL
      *
           MOVE    "preprint.sh"   TO  SHELLTBL-NAME
           MOVE    SPACE           TO  LNK-PRTKANRI-AREA
           INITIALIZE                  LNK-PRTKANRI-AREA
           MOVE    WRK-CONS-PREVIEW-TBL-KEY
                                   TO  LNK-PRTKANRI-TBL-KEY
           MOVE    WRK-CONS-PREVIEW-TBL-GROUP
                                   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    ZERO            TO  LNK-PRTKANRI-RENNUM
           MOVE    ZERO            TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    "000000"        TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD      TO  LNK-PRTKANRI-SKYYMD
           MOVE    "preprint.sh"   TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO            TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID      TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID        TO  LNK-PRTKANRI-OPID
           MOVE    WRK-PRTNUM      TO  LNK-PRTKANRI-PRTNM
      *
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD
                                   TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                   TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID
                                   TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID
                                   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM
                                   TO  SHELLTBL-ARG11
      *
           MOVE    SPACE           TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SHELLTBL-ARG13
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  SHELLTBL-ARG14
      *
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    WRK-CONS-FILENAME
                                   TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01    TO  SHELLTBL-ARG15
           MOVE    WRK-PRTNUM      TO  SHELLTBL-ARG16
      *
           IF    SPA-CLIENT-PRT-FLG  =  2
               MOVE    "1"         TO  SHELLTBL-ARG17
           END-IF
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SHELLTBL        TO  MCPDATA-REC
           MOVE    "SHELL"         TO  MCP-FUNC
           MOVE    "shell"         TO  MCP-TABLE
           MOVE    "allways"       TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
      *    �ʣϣ£ɣĹ�������
           PERFORM 45012-JOBID-UPD-SEC
      *
           .
       230-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "�ץ��̾�����Ϥ��Ʋ�������"
                                           TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "����ִ����ģ¤�����Ǥ��ޤ���Ǥ���"
                                           TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE
                   "�����ƥ�����ģ¡ʣ��������ˤ�����Ǥ��ޤ���Ǥ���"
                                           TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  WERR
           MOVE    SPA-ERRCD       TO  WERR-ERRCODE
           MOVE    SPA-ERRMSG      TO  WERR-ERRMSG
           MOVE    "B01"           TO  MCP-WIDGET
      *
           MOVE    "W62"           TO  SPA-MOTOPG
           MOVE    "WERR"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE.
           MOVE    "WERR"          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1               TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ֳ��ϥ��åȽ���
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC   SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID
           CALL    "ORCSJOB"           USING
                                       ORCSJOBKANRIAREA
                                       JOBKANRI-REC
                                       SPA-AREA
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ʣϣ£ɣĹ�������
      *****************************************************************
       45012-JOBID-UPD-SEC         SECTION.
      *
           MOVE    SPACE           TO  SYS-3003-REC
           INITIALIZE                  SYS-3003-REC
           MOVE    "3003"          TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD
                                   TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-3003-STYUKYMD
                                       SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *
           MOVE    SPACE           TO  SYS-3003-REC
           INITIALIZE                  SYS-3003-REC
           MOVE    "3003"          TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD
                                   TO  SYS-3003-KBNCD
           MOVE    ZERO            TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"         TO  SYS-3003-EDYUKYMD
           MOVE    WRK-CONS-PREVIEW-TBL-KEY
                                   TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PREVIEW-TBL-GROUP
                                   TO  SYS-3003-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  SYS-3003-JOBID
      *
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC    TO  MCPDATA-REC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"      TO  MCP-FUNC
           END-IF
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ�����������
      *****************************************************************
       900-SYSKANRI-SELECT-SEC     SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          =   ZERO
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɤ߹��߽���
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ���������������
      *****************************************************************
       900-SYSKANRI-CLOSE-SEC      SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-SYSKANRI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ������ɹ��ʥ�����
      *****************************************************************
       900-SYSKANRI-INV-SEC        SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
           MOVE   "PUTWINDOW"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *