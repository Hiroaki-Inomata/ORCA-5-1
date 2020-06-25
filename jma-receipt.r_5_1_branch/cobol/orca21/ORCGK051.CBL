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
       PROGRAM-ID.                 ORCGK051.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ���Ź԰٥��åȥ��������ϡʣˣ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  01/04/23    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  05/11/17  MONFUNC �б�
      *  03.05.00    NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      *  05.00.00    NACL-¿��Ǽ  18/10/XX  ��Ͽ�Ѥߥ��顽ɽ���б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ��׾Ȳ��̥ѥ�᥿
           COPY    "K01COMMON-SPA".
      *
      *    ���̥����ΰ�
           COPY    "K02SCR-SPA".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-KANJI           PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX-C               PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-ENDDD           PIC 9(02).
      *
           03  WRK-CNT             PIC 9(04).
      *
           03  WRK-CDSYU           PIC X(01).
      *
           03  WRK-K051-TBL.
               05  WRK-K051-TBL-G      OCCURS  5.
                   07  WRK-K051-INPUTCD    PIC X(20).
                   07  WRK-K051-CDSYU      PIC X(01).
      *
           03  WRK-PATH            PIC X(64).
           03  WRK-WIDGET          PIC X(20).
      *    ����̵ͭ
           03  FLG-IN-INPUTCD      PIC 9(01)   OCCURS   5.
      *
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���ϥ����ɥޥ�����
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    ���ϥ��åȥ�����
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *    �����ޥ�����
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *    ���ѥ����å��ѥ�᥿
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ģ¸���
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
      *
       01  SCRAREA.
           COPY    "ORCA21SCRAREA.INC".
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    SPACE           TO  SPA-ERRMSG2
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
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
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
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "KERR"
                                       OR  "KERR2"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ��������Խ�
      *D       PERFORM 300-SCREEN-SEC
      *
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "K051"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
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
           INITIALIZE                  SPA-K051-AREA
      *
           PERFORM                     310-SPA-SET-SEC
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  K051
           INITIALIZE                  K051
      *
      *    �����Խ�
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               MOVE    SPA-GMN-K051-INPUTCD(IDX)   TO  K051-INPUTCD
                                                                 (IDX)
           END-PERFORM
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
           IF      SPA-K051-CUR        =   ZERO
               EVALUATE    WRK-WIDGET
                   WHEN    "INPUTCD1"
                       MOVE    2                   TO  SPA-K051-CUR
                   WHEN    "INPUTCD2"
                       MOVE    3                   TO  SPA-K051-CUR
                   WHEN    "INPUTCD3"
                       MOVE    4                   TO  SPA-K051-CUR
                   WHEN    "INPUTCD4"
                       MOVE    5                   TO  SPA-K051-CUR
                   WHEN    "INPUTCD5"
                       MOVE    6                   TO  SPA-K051-CUR
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-K051-CUR
               WHEN    1
                   MOVE    "INPUTCD1"          TO  MCP-WIDGET
               WHEN    2
                   MOVE    "INPUTCD2"          TO  MCP-WIDGET
               WHEN    3
                   MOVE    "INPUTCD3"          TO  MCP-WIDGET
               WHEN    4
                   MOVE    "INPUTCD4"          TO  MCP-WIDGET
               WHEN    5
                   MOVE    "INPUTCD5"          TO  MCP-WIDGET
               WHEN    6
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        ��Ͽ
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 220-KAKUTEI-SEC
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
      *    ���ϸĽ�Υ��å�
           PERFORM 210-GMN-INPUT-SEC
      *
      *    ���ϥ����å�����
           PERFORM 220-INPUT-CHK-SEC
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϸĽ�Υ��åȽ���
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-K051-CUR
      *    ���Ϲ��ܤη���
           MOVE    MCP-WIDGET          TO  WRK-WIDGET
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    ���̤�ӣУ����Խ�
           PERFORM 2002-GMNSPA-SET-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   5   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               PERFORM 2001-INPUTCD-SEC
           END-PERFORM
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���̹��ܢ��ӣУ��Խ�����
      *****************************************************************
       2002-GMNSPA-SET-SEC               SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               IF      K051-INPUTCD (IDX)  =   SPA-GMN-K051-INPUTCD
                                                              (IDX)
                   MOVE    ZERO            TO  FLG-IN-INPUTCD (IDX)
               ELSE
                   MOVE    1               TO  FLG-IN-INPUTCD (IDX)
               END-IF
               MOVE    K051-INPUTCD (IDX)  TO  SPA-GMN-K051-INPUTCD
                                                              (IDX)
           END-PERFORM
      *
           .
       2002-GMNSPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    ���åȥ��������Ͻ���
      *****************************************************************
       2001-INPUTCD-SEC                SECTION.
      *
      *    ����λ��������ϥ��顼�Ȥ���
           IF      FLG-IN-INPUTCD(IDX) =   1
               IF      SPA-K05-SYORI       =   "3"
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    IDX                 TO  SPA-K051-CUR
                   GO      TO      2001-INPUTCD-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN-K051-INPUTCD (IDX)  =   SPACE
      *
      *        ����Ժ��
               PERFORM 20011-SPACE-DEL-SEC
               GO      TO      2001-INPUTCD-EXT
           END-IF
      *
      *    ��������å�
           MOVE    ZERO                TO  WRK-CNT
      *
      *    �������ϥ����å�
           MOVE    ZERO               TO  FLG-KANJI
           MOVE    ZERO               TO  FLG-KANA
           MOVE    ZERO               TO  FLG-SUJI
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-K051-INPUTCD (IDX)
                                       TO  KANACHK-MAE-INPUT
           MOVE    20                  TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-K051-INPUTCD (IDX)
           IF      KANACHK-RC      NOT =   ZERO
      *        ����Ⱦ�Ѻ��ߥ��顼
               MOVE    "0005"          TO  SPA-ERRCD
           ELSE
      *        �֤˶��򤬤����
               IF      KANACHK-RC2         =   8
                   MOVE    "0010"              TO  SPA-ERRCD
               END-IF
               MOVE    KANACHK-MAX     TO  WRK-CNT
      *        ����
               IF      KANACHK-SYUBETU     =   2
                   MOVE    1               TO  FLG-KANJI
                   IF      KANACHK-MAX         >   10
                       MOVE    "0004"              TO  SPA-ERRCD
                   END-IF
                   IF    KANACHK-RC3   NOT =   ZERO
      *                ���Ѷ��򤢤�
                       MOVE    "0011"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        Ⱦ��
               IF      KANACHK-SYUBETU     =   1
                   IF      SPA-GMN-K051-INPUTCD (IDX)(1:WRK-CNT)
                                                   NUMERIC
      *                ������������å�
                       IF      KANACHK-MAX         <   4   OR  >  6
                           MOVE    "0006"              TO  SPA-ERRCD
                       END-IF
                   ELSE
      *                S,P�����å�
                       IF      SPA-GMN-K051-INPUTCD (IDX) (1:1)
                                                   =   "S"   OR  "P"
                           MOVE    "0007"      TO  SPA-ERRCD
                       END-IF
      *                ʣ���ʡ��ݸ������å�
                       IF      SPA-GMN-K051-INPUTCD (IDX) (1:1)
                                                   =   "#"   OR  "$"
                           MOVE    "0008"      TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    IDX                 TO  SPA-K051-CUR
               GO      TO      2001-INPUTCD-EXT
           END-IF
      *    ����������ԲĤȤ���
           IF      FLG-KANJI           =   1
               MOVE    2                   TO  IDX-C
           ELSE
               MOVE    1                   TO  IDX-C
           END-IF
           PERFORM VARYING     IDY     FROM    1   BY  IDX-C
                   UNTIL      (IDY         >   20  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      IDX-C               =   1
                   IF      SPA-GMN-K051-INPUTCD (IDX)(IDY:1)
                                               =   "+"   OR  "-"
                                               OR  "/"   OR  "!"
                                               OR  "."   OR  "*"
                                               OR  "_"
                       MOVE    "0008"      TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF    SPA-GMN-K051-INPUTCD (IDX) (IDY:2)
                                               =   "��"   OR  "��"
                                               OR  "��"   OR  "��"
                                               OR  "��"   OR  "��"
                       MOVE    "0008"      TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    IDX                 TO  SPA-K051-CUR
               GO      TO      2001-INPUTCD-EXT
           END-IF
      *
      *    Ʊ�쥳���ɥ����å�
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY         >   5   )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      IDX             NOT =   IDY
                   IF      SPA-GMN-K051-INPUTCD (IDX)
                                           =   SPA-GMN-K051-INPUTCD
                                                                (IDY)
                       MOVE    "0002"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    IDX                 TO  SPA-K051-CUR
               GO      TO      2001-INPUTCD-EXT
           END-IF
      *
      *    ���ϥ����ɥ����å�
           INITIALIZE                      INPUTCD-REC
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
      *    ��������
           IF      FLG-KANJI           =   1
               MOVE    "J"                 TO  ICD-CDSYU
           ELSE
      *       ����������ɣС��ӡɤǻϤޤ��Τϡ������ɼ��̤����Ƿ���
               IF     (SPA-GMN-K051-INPUTCD (IDX)(1:WRK-CNT)
                                                   NUMERIC)  OR
                      (SPA-GMN-K051-INPUTCD (IDX)(1:1) =   "P"  OR "S")
                   EVALUATE    WRK-CNT
                       WHEN    4
                           MOVE    "4"                 TO  ICD-CDSYU
                       WHEN    5
                           MOVE    "5"                 TO  ICD-CDSYU
                       WHEN    6
                           MOVE    "6"                 TO  ICD-CDSYU
                   END-EVALUATE
               ELSE
                   MOVE    "A"                 TO  ICD-CDSYU
               END-IF
           END-IF
           MOVE    ICD-CDSYU           TO  WRK-CDSYU
           MOVE    SPA-GMN-K051-INPUTCD (IDX)
                                       TO  ICD-INPUTCD
           IF      WRK-CNT             =   20
               MOVE    "INPUTCD-KEY2"      TO  WRK-PATH
           ELSE
               MOVE    "INPUTCD-KEY"       TO  WRK-PATH
           END-IF
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM                 920-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-INPUTCD         =   ZERO  )  AND
                  (SPA-GMN-K051-INPUTCD (IDX)
                                       =   ICD-INPUTCD )
               IF      ICD-SRYCD       NOT =   SPA-GMN-SETCODE
                   MOVE    "0003"              TO  SPA-ERRCD
                   MOVE    IDX                 TO  SPA-K051-CUR
      *!!
      *H30.10
      *            ��Ͽ�Ѥ߿��ť�����
                   PERFORM 20012-CDERR-MSG-SEC
                   GO      TO      2001-INPUTCD-EXT
               END-IF
           END-IF
      *
      *    ����Ժ��
      *    PERFORM 20011-SPACE-DEL-SEC
      *
           MOVE    WRK-CDSYU           TO  SPA-GMN-K051-CDSYU (IDX)
      *
           .
       2001-INPUTCD-EXT.
           EXIT.
      *
      *H30.10
      *****************************************************************
      *    ��Ͽ�Ѥ߿��ť����ɽ���
      *****************************************************************
       20012-CDERR-MSG-SEC        SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG2
           IF      ICD-SRYCD(1:1)      =   "P"
                                       OR  "S"
               STRING  "���åȥ����ɡ�"
                       ICD-SRYCD
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG2
               END-STRING
               GO      TO      20012-CDERR-MSG-EXT
           END-IF
      *
           MOVE    "0013"              TO  SPA-ERRCD
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    ICD-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
      *    �������ι߽�˸���
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
               MOVE    SPACE               TO  SPA-ERRMSG2
               MOVE    TNS-YUKOEDYMD       TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               STRING  "���ť����ɡ�"
                       TNS-SRYCD
                       " ��λ����"
                       WRK-HENYMDG     DELIMITED  BY  SIZE
                       "��"
                       TNS-NAME (1:36) DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG2
               END-STRING
           END-IF
           .
       20012-CDERR-MSG-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ����Ժ������
      *****************************************************************
       20011-SPACE-DEL-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-K051-TBL
           MOVE    SPA-GMN-K051-TBL    TO  WRK-K051-TBL
           MOVE    SPACE               TO  SPA-GMN-K051-TBL
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   5
               IF      WRK-K051-INPUTCD (IDX1) NOT =   SPACE
                   ADD     1               TO  IDX2
                   MOVE    WRK-K051-TBL-G   (IDX1)
                                           TO  SPA-GMN-K051-TBL-G(IDX2)
               END-IF
           END-PERFORM
      *
           .
       20011-SPACE-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��Ͽ������
      *****************************************************************
       220-KAKUTEI-SEC                   SECTION.
      *
      *    ���ϥ����å�����
           PERFORM 220-INPUT-CHK-SEC
      **** PERFORM VARYING     IDX     FROM    1   BY 1
      *            UNTIL      (IDX         >   5   )   OR
      *                       (SPA-ERRCD   NOT =   SPACE )
      *        PERFORM 2001-INPUTCD-SEC
      *****END-PERFORM
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      220-KAKUTEI-EXT
           END-IF
      *
      *    �������Ƥ�ӣУ���
           MOVE    SPA-GMN-K051-TBL    TO  SPA-K051-TBL
      *
      *    ���β��̤�
           PERFORM 210-BACK
      *
           .
       220-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "K05"               TO  SPA-SAKIPG
           MOVE    "K051"              TO  SPA-MOTOPG
      *
      *****MOVE    "BACK"              TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "K05"               TO  MCP-WINDOW
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
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
            PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K051"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "���ϥ��顼"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "Ʊ�쥳���ɤ�ͭ��ޤ�"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "¾�Υ��åȤ�Ʊ�쥳���ɤ�ͭ��ޤ�"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "���������ɤϣ�ʸ���ޤǤǤ�"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "�����Ȥκ��ߤϤǤ��ޤ���"
                                                 TO  SPA-ERRMSG
      ******** WHEN    "0006"
      *            MOVE    "���ϥ����ɤϣ��夫�飶������Ϥ��Ƥ�������"
      ********                                   TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE
              "�����Τߤ����ϥ����ɤϣ��夫�飶������Ϥ��Ƥ�������"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "���ϥ����ɤϻϤޤ꤬�ӡ��аʳ��Υ�����"
                                                 TO  SPA-ERRMSG
                   MOVE    "�ˤ��Ʋ�������"
                                                 TO  SPA-ERRMSG(39:)
               WHEN    "0008"
                   MOVE
                   "���ϥ����ɤˤϻ��ѤǤ��ʤ����椬����ޤ���"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "ʸ���δ֤˶�������ϤǤ��ޤ���"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   STRING  "���Ѷ��򤬤���ޤ���"
                           "���Ѷ�������ϤǤ��ޤ���"
                                        DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
      *H30.10
               WHEN    "0013"
                   MOVE    "������Ͽ�ѤΥ����ɤǤ���"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
      *H30.10
           IF      SPA-ERRMSG2     NOT =   SPACE
      *        ���顼���̣�
               PERFORM 5101-KERR2-SET-SEC
               GO      TO      510-ERRSET-EXT
           END-IF
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K051"              TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å�����ɽ��������
      *****************************************************************
       5101-KERR2-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KERR2
           INITIALIZE                      KERR2
           MOVE    SPA-ERRCD           TO  KERR2-ERRCODE
           MOVE    SPA-ERRMSG2         TO  KERR2-ERRMSG(1:80)
           MOVE    SPA-ERRMSG          TO  KERR2-ERRMSG2
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K051"              TO  SPA-MOTOPG
           MOVE    "KERR2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KERR2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       5101-KERR2-SET-EXT.
           EXIT.
      *****************************************************************
      *    ���������Խ�����
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����ɥޥ��������ɹ�
      *****************************************************************
       920-INPUTCD-NEXT-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-INPUTCD-NEXT-EXT.
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
      *****************************************************************
      *    �Уգԡ�����
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *