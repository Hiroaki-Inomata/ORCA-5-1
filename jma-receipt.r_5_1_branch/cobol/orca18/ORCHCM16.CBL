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
       PROGRAM-ID.             ORCHCM16.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ���հ���ɽ����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  03/06/10    NACL�ݿ���    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     NACL-¿��Ǽ  03/11/06  ������ץ�󥿥����ɲ�
      *                                     �������ͽ������ʣ��ˤ�
      * 01.00.01     NACL-¿��Ǽ  05/12/07  MONFUNC�б�
      *  03.05.00    NACL-¿��Ǽ  07/05/07  ���롼�׿����б�
      *  04.08.00    NACL-¿��Ǽ  14/08/01  ����ǥ��쥯�ȥ��б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    �����ѥǡ���
      **** SELECT  PRT-FILE        ASSIGN  HC01PARA
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    �����ѥǡ���
       FD  PRT-FILE.
           COPY    "HCM16.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ǡ���̾���ΰ�
           COPY    "CPCOMMONPRT.INC".
      *
      *v4.8
      *    ����ǥ��쥯�ȥ��ΰ� 
           COPY    "CPASGNPARA.INC".
      *
      *    ���ơ������ΰ�
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(02).
           03  CNT-HCM16           PIC 9(04).
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-DATE            PIC 9(08).
           03  SYS-TIME            PIC 9(08).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-ZZZ9            PIC ZZZ9.
      *
      *    �Խ��Ѻ���ΰ�
       01  WRK-HEN-AREA.
           03  WRK-HEN-DATE.
               05  WRK-HDYY        PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDMM        PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDDD        PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   "/".
           03  WRK-HEN-TIME.
               05  WRK-HTHH        PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ":".
               05  WRK-HTMM        PIC 9(02).
      *
      ****************************************************************
      *    �ե�����쥤������
      ****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    ���Ų��ܾ���
           COPY    "CPSK1005.INC".
      *
      *    ��������
           COPY    "CPSK1010.INC".
      *
      *    �������ƾ���
           COPY    "CPSK1012.INC".
      *
      *    ������ץ��̾������ƾ���
           COPY    "CPSK1031.INC".
      *
      *    ����
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      ****************************************************************
      *    ���֥ץ����ΰ�
      ****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
      *    �����ѥ�᥿
           COPY    "CPORCSMKPRT.INC".
      *
      *    ����饤��Ģɼ̾��������ץ��̾�����ѥ�
           COPY    "CPORCSPRTNM.INC".
      *
      *v4.8
      *    ����ե�����̾����
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      ****************************************************************
      *    Ϣ���ΰ�
      ****************************************************************
       LINKAGE                     SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE               DIVISION    USING
                                           SPA-AREA.
      *
      ****************************************************************
      *    �����
      ****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    �������
           PERFORM 100-INIT-SEC
      *    �硡������
           PERFORM 200-MAIN-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM.
      *
      ****************************************************************
      *    �������
      ****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           ACCEPT  SYS-DATE    FROM    DATE
           ACCEPT  SYS-TIME    FROM    TIME
      *
      *    ������ץ��̾������ƾ����Խ�
           PERFORM 900-SYS1031-READ-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      ****************************************************************
      *    �硡������
      ****************************************************************
       200-MAIN-SEC                SECTION.
      *
           MOVE    ZERO            TO  FLG-UKETUKE
           MOVE    ZERO            TO  CNT-AREA
      *
      *    �����ɤ߹���
           PERFORM 900-UKETUKE-SELECT-SEC
      *
           PERFORM UNTIL   FLG-UKETUKE =   1
      *        Ģɼ�Խ�<���Ф�>����
               PERFORM 310-HEAD-HEN-SEC
      *        Ģɼ�Խ�<����>����
               PERFORM 320-BODY-HEN-SEC
      *        Ģɼ��������
               PERFORM 390-PRINT-OUT-SEC
           END-PERFORM
      *
      *    �����ɤ߹���
           PERFORM 900-UKETUKE-CLOSE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      ****************************************************************
      *    Ģɼ�Խ�<�إå�����>����
      ****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
           MOVE    ZERO            TO  CNT-LINE
           ADD     1               TO  CNT-PAGE
      *
           INITIALIZE                  HCM16
           MOVE    SPACE           TO  HCM16
      *
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  HCM16-SRYYMD
      *
           MOVE    SYS-DATE(3:2)   TO  WRK-HDYY
           MOVE    SYS-DATE(5:2)   TO  WRK-HDMM
           MOVE    SYS-DATE(7:2)   TO  WRK-HDDD
           MOVE    SYS-TIME(1:2)   TO  WRK-HTHH
           MOVE    SYS-TIME(3:2)   TO  WRK-HTMM
           MOVE    WRK-HEN-AREA    TO  HCM16-SAKUSEI
      *
           MOVE    CNT-PAGE        TO  WRK-ZZ9
           MOVE    WRK-ZZ9         TO  HCM16-PAGE
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    Ģɼ�Խ�<���ٹ�>����
      ****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
           PERFORM UNTIL   FLG-UKETUKE =   1
                   OR      CNT-LINE    >=  35
               ADD     1               TO  CNT-LINE
               ADD     1               TO  CNT-HCM16
      *
      *        �ֹ�
               MOVE    CNT-HCM16       TO  WRK-ZZZ9
               MOVE    WRK-ZZZ9        TO  HCM16-NUM(CNT-LINE)
      *
      *        ����̾��
               MOVE    UKE-NAME        TO  HCM16-NAME(CNT-LINE)
      *
      *        �����ֹ��Ѵ�
               IF      UKE-PTID        NOT =   ZERO
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                   MOVE    UKE-PTID        TO  SPTID-PTID
                   CALL    "ORCSPTID"          USING
                                               ORCSPTIDAREA
                                               SPA-AREA
                   IF      SPTID-RC    =   ZERO
                       MOVE    SPTID-PTNUM     TO  HCM16-PTNUM(CNT-LINE)
                   END-IF
               END-IF
      *
      *        ���Ų��ɤ߹���
               PERFORM 900-SYS1005-READ-SEC
               MOVE    SYS-1005-SRYKANAME1     TO  HCM16-SRYKA(CNT-LINE)
      *
      *        ���������ɤ߹���
               PERFORM 900-SYS1012-READ-SEC
               MOVE    SYS-1012-SRYNAIYODSP    TO  HCM16-NAIYO(CNT-LINE)
      *
      *        ���������ɤ߹���
               IF      UKE-DRCD(2:4)   NOT =   SPACE
                   PERFORM 900-SYS1010-READ-SEC
                   MOVE    SYS-1010-NAME   TO  HCM16-DRNAME(CNT-LINE)
               END-IF
      *
      *        ���ջ���
               MOVE    UKE-UKETIME(1:2)    TO  WRK-HTHH
               MOVE    UKE-UKETIME(3:2)    TO  WRK-HTMM
               MOVE    WRK-HEN-TIME    TO  HCM16-UKETUKE(CNT-LINE)
      *
      *        ��׻���
               MOVE    UKE-KAIKEITIME(1:2) TO  WRK-HTHH
               MOVE    UKE-KAIKEITIME(3:2) TO  WRK-HTMM
               MOVE    WRK-HEN-TIME    TO  HCM16-KAIKEI(CNT-LINE)
      *
      *        �����ɤ߹���
               MOVE    ZERO            TO  FLG-UKETUKE
               MOVE    ZERO            TO  UKE-UKEID
               PERFORM 900-UKETUKE-FETCH-SEC
                       UNTIL   (FLG-UKETUKE   =  1  )  OR
                               (UKE-UKEID     >  ZERO)
      *
           END-PERFORM
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    Ģɼ��������
      ****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
      *    �ե�����ϣУţ�
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC16"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    CNT-PAGE        TO  HC01PARA-CNT
      *v4.8
      *    ����ե�����̾����
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
           WRITE   HCM16
           CLOSE                       PRT-FILE
      *
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCM16.red"     TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
      *****MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    ASGNPARA        TO  MKPRT-DAT
      *****MOVE    SYS-1031-PRTNM(9)   TO  MKPRT-PRTNM
           MOVE    SYS-1031-PRTNM(7)   TO  MKPRT-PRTNM
           MOVE    "���հ���"          TO  MKPRT-INFO
           CALL    "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      ****************************************************************
      *    �������ܸ��Ѵ�����
      ****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    �����ƥ�����ޥ����ɤ߹���
      ****************************************************************
       800-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       800-SYSKANRI-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    �����ƥ�������������ɤ߹���
      ****************************************************************
       900-SYS1005-READ-SEC        SECTION.
      *
           MOVE    SPACE           TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"          TO  SYS-1005-KANRICD
           MOVE    UKE-SRYKA       TO  SYS-1005-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1005-REC
           ELSE
               INITIALIZE                  SYS-1005-REC
           END-IF
      *
           .
       900-SYS1005-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    �����ƥ�������������ɤ߹���
      ****************************************************************
       900-SYS1010-READ-SEC        SECTION.
      *
           MOVE    SPACE           TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    UKE-DRCD        TO  SYS-1010-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
           END-IF
      *
           .
       900-SYS1010-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    �����ƥ�������������ɤ߹���
      ****************************************************************
       900-SYS1012-READ-SEC        SECTION.
      *
           MOVE    SPACE           TO  SYS-1012-REC
           INITIALIZE                  SYS-1012-REC
           MOVE    "1012"          TO  SYS-1012-KANRICD
           MOVE    UKE-SRYNAIYO    TO  SYS-1012-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1012-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1012-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1012-HOSPNUM
           MOVE    SYS-1012-REC    TO  MCPDATA-REC
           PERFORM 800-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1012-REC
           ELSE
               INITIALIZE                  SYS-1012-REC
           END-IF
      *
           .
       900-SYS1012-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    �����ƥ�������������ɤ߹���
      ****************************************************************
       900-SYS1031-READ-SEC        SECTION.
      *
      **** MOVE    SPACE           TO  SYS-1031-REC
      *    INITIALIZE                  SYS-1031-REC
      *    MOVE    "1031"          TO  SYS-1031-KANRICD
      *    MOVE    "*"             TO  SYS-1031-KBNCD
      *    MOVE    SYS-1031-REC    TO  MCPDATA-REC
      *    PERFORM 800-SYSKANRI-READ-SEC
      *    IF      FLG-SYSKANRI    =   ZERO
      *        MOVE    MCPDATA-REC     TO  SYS-1031-REC
      *    ELSE
      *        INITIALIZE                  SYS-1031-REC
      **** END-IF
      *
      *    ������ץ��̾������ƾ����Խ�
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           .
       900-SYS1031-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    �����ɤ߹���
      ****************************************************************
       900-UKETUKE-SELECT-SEC      SECTION.
      *
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM     TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD      TO  UKE-UKEYMD
           MOVE    UKETUKE-REC     TO  MCPDATA-REC
      *
           MOVE    "tbl_uketuke"   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC      =   ZERO
               MOVE    ZERO            TO  FLG-UKETUKE
               PERFORM 900-UKETUKE-FETCH-SEC
                       UNTIL   (FLG-UKETUKE   =  1  )  OR
                               (UKE-UKEID     >  ZERO)
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       900-UKETUKE-SELECT-EXT.
           EXIT.
      *
      ****************************************************************
      *    �����ɤ߹���
      ****************************************************************
       900-UKETUKE-FETCH-SEC       SECTION.
      *
           MOVE    "tbl_uketuke"   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC      =   ZERO
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
               MOVE    ZERO            TO  FLG-UKETUKE
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       900-UKETUKE-FETCH-EXT.
           EXIT.
      *
      ****************************************************************
      *    ���գạ̃ϣӣ�
      ****************************************************************
       900-UKETUKE-CLOSE-SEC       SECTION.
      *
           MOVE    "tbl_uketuke"   TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC 
      *
           .
       900-UKETUKE-CLOSE-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *