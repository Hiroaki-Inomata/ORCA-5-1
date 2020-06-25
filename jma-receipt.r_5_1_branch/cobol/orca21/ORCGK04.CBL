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
       PROGRAM-ID.             ORCGK04.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�����
      *  ����ݡ��ͥ��̾  : ����ʣˣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    ���Ź԰��Ѷ��̥ѥ�᥿
           COPY    "K01COMMON-SPA".
      *
      *    ���̥����ΰ�
           COPY    "K02SCR-SPA".
      *
      *
      *    03  SPA-K04-AREA.
      *        05  SPA-GMN-AREA.
      *            07  SPA-GMN-PTNUM           PIC X(22).
      *            07  SPA-GMN-PTID           PIC X(10).
      *            07  SPA-GMN-KANANAME        PIC X(30).
      *            07  SPA-GMN-SEIBET          PIC X(02).
      *            07  SPA-GMN-JSNYMD          PIC X(09).
      *            07  SPA-GMN-KNJNAME         PIC X(40).
      *            07  SPA-GMN-BIRTH           PIC X(09).
      *            07  SPA-GMN-SINRYOU         PIC X(10).
      *            07  SPA-GMN-TBL             OCCURS   20.
      *                09  SPA-GMN-KOUMOKU-G.
      *                    11  SPA-GMN-SRYCD       PIC X(09).
      *                    11  SPA-GMN-FIL1        PIC X(01).
      *                    11  SPA-GMN-KOUMOKU     PIC X(40).
      *                09  SPA-GMN-KAMEI           PIC X(16).
      *                09  SPA-GMN-SYOKAIYMD       PIC X(09).
      *                09  SPA-GMN-SANTEIYMD       PIC X(09).
      *                09  SPA-GMN-KAISU1          PIC 9(02).
      *                09  SPA-GMN-YY1             PIC 9(02).
      *                09  SPA-GMN-MM1             PIC 9(02).
      *                09  SPA-GMN-DD1             PIC 9(02).
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDD                 PIC 9(04).
      *
      *
       01  WRK-HOZON-AREA.
           03  WRK-TBL-OCC1            OCCURS  20.
               05  WRK-SRYCD           PIC X(09).
               05  WRK-SRYCDMEI        PIC X(40).
               05  WRK-TBL-OCC2            OCCURS   20.
                   07  WRK-SRYKA           PIC X(02).
                   07  WRK-SRYKAMEI        PIC X(16).
                   07  WRK-SYOKAIYMD       PIC X(08).
                   07  WRK-SANTEIYMD       PIC X(08).
                   07  WRK-KAISU           PIC 9(04).
                   07  WRK-NEN             PIC 9(04).
                   07  WRK-TUKI            PIC 9(04).
                   07  WRK-HI              PIC 9(04).
                   07  WRK-SYOKAIYMDH      PIC X(09).
                   07  WRK-SANTEIYMDH      PIC X(09).
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-YMD.
               05  WRK-YY            PIC 9(04).
               05  WRK-MM            PIC 9(02).
               05  WRK-DD            PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-ZZ              PIC ZZ.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HYMD            PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSK1005.INC".
      *    ���Ź԰٥ޥ�����
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ���Ų�ץޥ�����
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    �����ޥ�����
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
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
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
      *    �ģ¸���
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    ��������
               WHEN     "PUTG"         ALSO   "CLICKED"
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
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
      *D       PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
      *
      *        �����Խ�
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "K04"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *D   INITIALIZE              SPA-K04-AREA
      *
           PERFORM 310-SPASET-SEC
      *
      *    MOVE    1               TO  SPA-GMN-CUR
      *    MOVE    1               TO  SPA-GMN-PAGE
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  WRK-HOZON-AREA
      *
      *    ������ܡ��оݤο��ť����ɤ��Խ������
           MOVE    "111003610"         TO  WRK-SRYCD (01)
           MOVE    "112003350"         TO  WRK-SRYCD (02)
           MOVE    "112000210"         TO  WRK-SRYCD (03)
           MOVE    "112005610"         TO  WRK-SRYCD (04)
           MOVE    "112701510"         TO  WRK-SRYCD (05)
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   20  )   OR
                              (WRK-SRYCD (IDX) =   SPACE )
      *
      *        ���ť����ɤ����Ź԰�̾�Τ����
               MOVE    WRK-SRYCD (IDX)     TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
                   MOVE    TNS-NAME            TO  WRK-SRYCDMEI(IDX)
               END-IF
      *
           END-PERFORM
      *
           PERFORM 3101-DBSET-SEC
      *
           PERFORM 3102-TBLHEN-SEC
      *
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¤���Խ�����
      *****************************************************************
       3101-DBSET-SEC              SECTION.
      *
      *    ���Ź԰٥ǡ�������
           INITIALIZE                      SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL       FLG-SRYACT  =   1
      *        �������Խ�
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL      (IDX         >   20  )   OR
                                      (WRK-SRYCD (IDX)     =   SPACE)
                       IF      SRY-SRYCD(IDZ)    =   WRK-SRYCD (IDX)
                           PERFORM 31011-SRYACT-SET-SEC
                           MOVE    20            TO  IDX 
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3101-DBSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ų���Խ�����
      *****************************************************************
       31011-SRYACT-SET-SEC              SECTION.
      *
      *���Ų�ץޥ������ɤ߹���
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
     *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYACCT         NOT =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
               GO      TO     31011-SRYACT-SET-EXT
           END-IF
      *    ����������λ����оݳ�
           IF      ACCT-ZAIKAISU       =   ZERO
               GO      TO     31011-SRYACT-SET-EXT
           END-IF
      *
      *    �����
           MOVE    ZERO                TO  IDX3
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   20
               IF      WRK-SRYKA (IDX IDX2)    =   ACCT-SRYKA
                   MOVE    IDX2                TO  IDX3
                   MOVE    20                  TO  IDX2
               ELSE
                   IF      WRK-SRYKA (IDX IDX2)    =   SPACE
                       MOVE    ACCT-SRYKA      TO  WRK-SRYKA (IDX IDX2)
                       MOVE    IDX2            TO  IDX3
                       MOVE    20              TO  IDX2
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDX3                =   ZERO
               GO      TO     31011-SRYACT-SET-EXT
           END-IF
      *
           PERFORM VARYING  IDD        FROM  1  BY  1
                   UNTIL    IDD        >   31
               IF      ACCT-DAY (1 IDD)    NOT =   ZERO
                   MOVE    ACCT-SRYYM          TO  WRK-YMD(1:6)
                   MOVE    IDD                 TO  WRK-DD
      *            �����
                   IF      WRK-SYOKAIYMD(IDX IDX3) =   SPACE
                       MOVE    WRK-YMD         TO  WRK-SYOKAIYMD
                                                       (IDX IDX3)
                   END-IF
      *            ������
                   IF      WRK-SANTEIYMD(IDX IDX3) <   WRK-YMD
                       MOVE    WRK-YMD         TO  WRK-SANTEIYMD
                                                       (IDX IDX3)
                   END-IF
      *            ���
                   ADD     1               TO  WRK-KAISU (IDX IDX3)
      *            ǯ���
                   IF      ACCT-SRYYM(1:4)     =   SPA-SRYYMD(1:4)
                       ADD     1               TO  WRK-NEN (IDX IDX3)
                   END-IF
      *            ����
                   IF      ACCT-SRYYM(1:6)     =   SPA-SRYYMD(1:6)
                       ADD     1               TO  WRK-TUKI(IDX IDX3)
                   END-IF
      *            �����
                   IF      WRK-YMD             =   SPA-SRYYMD
                       ADD     1               TO  WRK-HI  (IDX IDX3)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       31011-SRYACT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       3102-TBLHEN-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   20  )
                         OR   (WRK-SRYCD (IDX) =   SPACE )
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   20  ) 
                             OR   (WRK-SRYKA (IDX IDX2)  =   SPACE )
      *        ���Ų�
               MOVE    SPACE               TO  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    WRK-SRYKA (IDX IDX2)
                                           TO  SYS-1005-KBNCD
               MOVE    WRK-SYOKAIYMD  (IDX IDX2)
                                           TO  SYS-1005-STYUKYMD
               IF      WRK-SYOKAIYMD  (IDX IDX2)   =   SPACE
                   MOVE    WRK-SANTEIYMD  (IDX IDX2)
                                           TO  SYS-1005-STYUKYMD
               END-IF
               MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
grpsys*        MOVE    SPA-HOSPNUM         TO  -HOSPNUM
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               ELSE
                   INITIALIZE                  SYS-1005-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYS-1005-SRYKANAME1     TO  WRK-SRYKAMEI
                                                         (IDX IDX2)
               ELSE
                   MOVE    WRK-SRYKA (IDX IDX2)    TO  WRK-SRYKAMEI
                                                         (IDX IDX2)
               END-IF
      *
      *    �������륯������
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
      *        �����Ѵ�
               MOVE    WRK-SYOKAIYMD (IDX IDX2)
                                           TO  WRK-SYMD
               PERFORM 410114-SE-WA-YMDEDIT-SEC
               MOVE    WRK-HYMD            TO  WRK-SYOKAIYMDH(IDX IDX2)
      *
               MOVE    WRK-SANTEIYMD (IDX IDX2)
                                           TO  WRK-SYMD
               PERFORM 410114-SE-WA-YMDEDIT-SEC
               MOVE    WRK-HYMD            TO  WRK-SANTEIYMDH(IDX IDX2)
             END-PERFORM
           END-PERFORM
      *
           .
       3102-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  K04
      *���ԣɣ�
           INITIALIZE                      K04
           MOVE    SPA-PTNUM           TO  K04-PTNUM
      *���ԥ��ʻ�̾
           MOVE    SPA-KANANAME        TO  K04-KANANAME
      *����
           MOVE    SPA-SEX             TO  K04-SEX
      *������
           MOVE    SPA-SRYYMDW         TO  K04-SRYYM
      *���Դ�����̾
           MOVE    SPA-NAME            TO  K04-NAME
      *��ǯ����
           MOVE    SPA-BIRTHDAYW       TO  K04-BIRTHDAY
      *���Ų�
           MOVE    SPA-SRYKAMEI        TO  K04-SRYKA
      *
      *ǯ��
           MOVE    SPA-NENREI-HEN      TO  K04-NENREI
      *
      *�������
           MOVE    ZERO                TO  IDX3
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   20  )
                         OR   (WRK-SRYCD (IDX) =   SPACE )
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2    >   20  ) 
                             OR   (WRK-SRYKA (IDX IDX2)  =   SPACE )
                   ADD     1                       TO  IDX3
                   IF      IDX2               =    1
                       MOVE    WRK-SRYCD (IDX)     TO  K04-SANTEINAME
                                                            (IDX3)(1:9)
                       MOVE    WRK-SRYCDMEI (IDX)  TO  K04-SANTEINAME
                                                            (IDX3)(11:)
                   END-IF
                   MOVE    WRK-SRYKAMEI (IDX IDX2) TO  K04-SRYKAMEI
                                                            (IDX3)
                   MOVE    WRK-SYOKAIYMDH(IDX IDX2)
                                                   TO  K04-SYOKAIYMD
                                                            (IDX3)
                   MOVE    WRK-SANTEIYMDH(IDX IDX2)
                                                   TO  K04-SANTEIYMD
                                                            (IDX3)
                   MOVE    WRK-KAISU    (IDX IDX2) TO  WRK-ZZZ
                   MOVE    WRK-ZZZ                 TO  K04-KAISU(IDX3)
                   MOVE    WRK-NEN      (IDX IDX2) TO  WRK-ZZZ
                   MOVE    WRK-ZZZ                 TO  K04-YY   (IDX3)
                   MOVE    WRK-TUKI     (IDX IDX2) TO  WRK-ZZ
                   MOVE    WRK-ZZ                  TO  K04-MM   (IDX3)
                   MOVE    WRK-HI       (IDX IDX2) TO  WRK-ZZ
                   MOVE    WRK-ZZ                  TO  K04-DD   (IDX3)
               END-PERFORM
           END-PERFORM
      *
           MOVE    IDX3                TO  K04-SANTEILIST-COUNT
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܽ���
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    ���á�����
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    �������������Խ�
      *****************************************************************
       410114-SE-WA-YMDEDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    ZERO                TO  LNK-DAY2-YMD
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HYMD
           .
       410114-SE-WA-YMDEDIT-EXT.
           EXIT.
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K04"               TO  SPA-MOTOPG
      *
      *****MOVE    "BACK"              TO  MCP-PUTTYPE
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
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
      *D   PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K04"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���顼��å�����ɽ����
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "���ϥ��顼"        TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K04"               TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ޥ������ɹ�
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
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
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ź԰٥ޥ������ɹ�
      *****************************************************************
       930-SRYACT-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ų�ץޥ������ɹ�
      *****************************************************************
       940-SRYACCT-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ����   �ɹ�
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
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
      *    �ģ£ạ̃ϣӣŽ���
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