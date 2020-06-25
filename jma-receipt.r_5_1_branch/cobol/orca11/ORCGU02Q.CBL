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
       PROGRAM-ID.             ORCGU02Q.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �象����
      *  ����ݡ��ͥ��̾  : �ѣҼ��հ������̡ʣգ����ѡ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/02/13    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-¿��Ǽ  07/05/07  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
      *    �����ѣӣУ�
           COPY    "U02COMMON-SPA".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-QRCDHKN         PIC 9(01).
           03  FLG-PTHKNINF         PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
     *
           03  FLG-CHK             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-QROK            PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-TIME.
               05  WRK-THH             PIC 9(02).
               05  WRK-TMM             PIC 9(02).
               05  WRK-TSS             PIC 9(02).
      *    03  WRK-TIME-H.
      *        05  WRK-THH-H           PIC Z9.
      *        05  WRK-K1              PIC X(01).
      *        05  WRK-TMM-H           PIC Z9.
      *        05  WRK-K2              PIC X(01).
      *        05  WRK-TSS-H           PIC Z9.
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ZZZ             PIC ZZZ.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH-H       PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM-H       PIC Z9.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    QR������(���ݸ��Ծ�)
       01  QRCDHKN-REC.
           COPY    "CPQRCDHKN.INC".
      *    ���Ծ���
       01  PTINF-REC.
           COPY "CPPTINF.INC".
      *    �ݸ��ޥ���
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *    �ݸ��ֹ�
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *    ����ʸ���ɲ�
           COPY    "CPORCSADDSIGN.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
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
           COPY    "U02.INC".
           COPY    "U02Q.INC".
           COPY    "U02X.INC".
           COPY    "U2ERR.INC".
           COPY    "U2ID1.INC".
           COPY    "CPAPIMODV2.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-U02
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
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
           END-EVALUATE
      *
      *    �����Խ��塢ɽ����
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-U02                       TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "U2ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        ��������Խ�
               PERFORM 300-SCREEN-SEC
      *
               IF      FLG-END             =   ZERO
      *            �����Խ�
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "U02Q"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           PERFORM 3000-SCREEN-INIT-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����Խ�����
      *****************************************************************
       3000-SCREEN-INIT-SEC            SECTION.
      *
      *    �ѣ�������β��
           IF      (SPA-U02Q-REGISTID  NOT =   ZERO  )
               PERFORM 30009-QRCDHKN-CR-SEC
           END-IF
      *
           INITIALIZE                      SPA-U02Q-AREA
      *
      *    �ѣ��Խ�
           PERFORM 3001-QRCDHKN-SYORI-SEC
           IF      SPA-U02Q-MAX        =   ZERO
               MOVE    99                  TO  SPA-U02Q-CUR
           ELSE
               MOVE    1                   TO  SPA-U02Q-CUR
           END-IF
           .
       3000-SCREEN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ѣ��Խ�����
      *****************************************************************
       3001-QRCDHKN-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-U02Q-MAX
           INITIALIZE                      SPA-U02Q-QRLIST
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  QRCDHKN-REGISTYMD
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL  ( FLG-QRCDHKN    =   1  )
                   OR     ( IDX            >=  50 )
               IF     (QRCDHKN-PTID    NOT =   ZERO ) AND
                      (QRCDHKN-MOD-FLG2    =   ZERO )
                   ADD    1                    TO  IDX
                   PERFORM 30011-QRCDHKN-HEN-SEC
               END-IF
      *
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           END-PERFORM
           MOVE    IDX                 TO  SPA-U02Q-MAX
      *
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3001-QRCDHKN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ɽ�Խ�����
      *****************************************************************
       30011-QRCDHKN-HEN-SEC              SECTION.
      *
           MOVE    IDX                 TO  SPA-U02Q-TNUM (IDX)
      *
           PERFORM 300111-PTNUM-HEN-SEC
      *    ���Ծ���
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    QRCDHKN-PTID        TO  PTINF-PTID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    PTINF-NAME          TO  SPA-U02Q-TNAME (IDX)
      *    ����
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "��"            TO  SPA-U02Q-TSEX (IDX)
               WHEN    "2"
                   MOVE    "��"            TO  SPA-U02Q-TSEX (IDX)
           END-EVALUATE
      *    ��ǯ����
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    PTINF-BIRTHDAY      TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  SPA-U02Q-TBIRTHDAY (IDX)
      *
           MOVE    QRCDHKN-REGISTID    TO  SPA-U02Q-TREGISTID (IDX)
      *    ���ջ���
           MOVE    QRCDHKN-REGISTTIME  TO  WRK-TIME
           MOVE    WRK-THH             TO  WRK-THH-H
           MOVE    WRK-TMM             TO  WRK-TMM-H
           MOVE    WRK-HENTIME         TO  SPA-U02Q-TTIME (IDX)
      *    ������ݸ��ɣ�
           PERFORM 300112-HKNID-HEN-SEC
      *    �ݸ�̾��
           IF      FLG-OK              =   1
               PERFORM 300113-HKNNUM-MEI-SEC
           END-IF
           .
       30011-QRCDHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �ѣҲ������
      *****************************************************************
       30009-QRCDHKN-CR-SEC              SECTION.
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  QRCDHKN-REGISTYMD
           MOVE    SPA-U02Q-REGISTID   TO  QRCDHKN-REGISTID
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-QRCDHKN         =   ZERO
               MOVE    0                   TO  QRCDHKN-MOD-FLG2
      *
               MOVE    QRCDHKN-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "U02Q QRCDHKN KEY UPD ERR:"  MCP-RC
                           ",KEY:" QRCDHKN-KEY
               END-IF
           END-IF
           .
       30009-QRCDHKN-CR-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ��Խ�����
      *****************************************************************
       300111-PTNUM-HEN-SEC              SECTION.
      *
      *    �����ֹ��Ѵ�(�����ˤ򸡺���������ID��괵��NO�����
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    QRCDHKN-PTID        TO  SPTID-PTID
           CALL    "ORCSPTID"          USING
                                           ORCSPTIDAREA
                                           SPA-AREA
           IF      SPTID-RC            =   ZERO
               MOVE    SPTID-PTNUM     TO  SPA-U02Q-TPTNUM (IDX)
               MOVE    QRCDHKN-PTID    TO  SPA-U02Q-TPTID  (IDX)
           END-IF
           .
       300111-PTNUM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ݸ��ɣĽ���
      *****************************************************************
       300112-HKNID-HEN-SEC              SECTION.
      *
      *    �ݸ����󸡺�
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    QRCDHKN-PTID        TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM UNTIL  (FLG-PTHKNINF    =   1   )   OR
                          (FLG-OK          =   1   )
               IF     (PTHKN-SAKJOKBN      NOT =   SPACE )  OR
                      (PTHKN-HKNNUM(1:1)       =   "9"   )
                   CONTINUE
               ELSE
                   IF     (PTHKN-HKNJANUM      =   QRCDHKN-HKNJANUM) 
                      AND (PTHKN-KIGO          =   QRCDHKN-KIGO )
                      AND (PTHKN-NUM           =   QRCDHKN-NUM )
                      AND (PTHKN-HONKZKKBN     =   QRCDHKN-HONKZKKBN )
                      AND (PTHKN-TEKSTYMD      <=  SPA-SYSYMD )
                      AND (PTHKN-TEKEDYMD      >=  SPA-SYSYMD )
                       MOVE    PTHKN-HKNID         TO  SPA-U02Q-THKNID
                                                                 (IDX)
                       MOVE    PTHKN-HKNNUM        TO  SPA-U02Q-THKNNUM
                                                                 (IDX)
                       MOVE    1                   TO  FLG-OK
                   END-IF
               END-IF
      *
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       300112-HKNID-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ֹ��Խ�����
      *****************************************************************
       300113-HKNNUM-MEI-SEC              SECTION.
      *
      *    �ݸ�����
           MOVE    SPACE               TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-U02Q-THKNNUM(IDX)
                                       TO  HKN-HKNNUM
           MOVE    "00"                TO  HKN-PAYKBN 
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-HKNNUM-READ-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNNUM          =   ZERO
               MOVE    HKN-TANSEIDONAME    TO  SPA-U02Q-THKNNUMMEI (IDX)
           END-IF
           .
       300113-HKNNUM-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  U02Q
           PERFORM   VARYING    IDX   FROM    1   BY  1
                     UNTIL      IDX      >    SPA-U02Q-MAX
               MOVE    SPA-U02Q-TNUM   (IDX)   TO  WRK-ZZZ
               MOVE    WRK-ZZZ                 TO  U02Q-TNUM  (IDX)
               MOVE    SPA-U02Q-TPTNUM (IDX)   TO  U02Q-TPTNUM (IDX)
               MOVE    SPA-U02Q-TNAME  (IDX)   TO  U02Q-TNAME  (IDX)
               MOVE    SPA-U02Q-TBIRTHDAY(IDX)
                                               TO  U02Q-TBIRTHDAY (IDX)
               MOVE    SPA-U02Q-TSEX   (IDX)   TO  U02Q-TSEX   (IDX)
               MOVE    SPA-U02Q-THKNNUMMEI(IDX)
                                               TO  U02Q-THKNNUMMEI(IDX)
               MOVE    SPA-U02Q-TTIME  (IDX)   TO  U02Q-TTIME (IDX)
               IF      IDX                 =   SPA-U02Q-SELNUM
                   MOVE    "T"                 TO  U02Q-SELECT
                                                               (IDX)
               ELSE
                   MOVE    "F"                 TO  U02Q-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-U02Q-MAX        TO  U02Q-COUNT
      *
           MOVE    SPA-U02Q-SELNUM     TO  U02Q-SELNUM
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-U02Q-CUR
               WHEN    01
                   MOVE    "SELNUM"    TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"       TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
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
      *    ���ꥢ
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 402-CLEAR-SEC
      *    ���Գ���
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 412-KAKUTEI-SYORI-SEC
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
           MOVE    ZERO                TO  SPA-U02Q-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        �ꥹ������
               WHEN    ANY             ALSO    "QRLIST"
                   PERFORM 4001-QRLIST-SEL-SEC
               WHEN    OTHER
      *            ���ϥ����å�����
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ɽ�������
      *****************************************************************
       4001-QRLIST-SEL-SEC          SECTION.
      *
      *    ������
           MOVE    ZERO                TO  SPA-U02Q-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-U02Q-MAX
               IF      U02Q-SELECT (IDX)   =   "T"
                   MOVE    IDX             TO  SPA-U02Q-SELNUM
               END-IF
           END-PERFORM
      *
           IF      SPA-U02Q-SELNUM    NOT =   ZERO
               PERFORM 4102-SELNUM-CHK-SEC
           END-IF
      *
           .
       4001-QRLIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ϥ����å�����
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    ���̤�ӣУ����å�
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           PERFORM 4102-SELNUM-CHK-SEC
           .
      *
       410-INPUT-CHK-EXT.
      *
      *****************************************************************
      *    ���̣ӣУ��Խ�����
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    U02Q-SELNUM         TO  SPA-U02Q-SELNUM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ����
      *****************************************************************
       4102-SELNUM-CHK-SEC                 SECTION.
      *
           MOVE    1                   TO  SPA-U02Q-CUR
      *
           IF     (SPA-U02Q-SELNUM     =   ZERO )  OR
                  (SPA-U02Q-SELNUM     >   SPA-U02Q-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
      *        ��ǧ
               MOVE    99             TO  SPA-U02Q-CUR
           END-IF
           .
      *
       4102-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ꥢ����
      *****************************************************************
       402-CLEAR-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-U02Q-SELNUM
           MOVE    1                   TO  SPA-U02Q-CUR
           .
      *
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Գ������
      *****************************************************************
       412-KAKUTEI-SYORI-SEC             SECTION.
      *
           IF     (SPA-U02Q-SELNUM     =   ZERO )  OR
                  (SPA-U02Q-SELNUM     >   SPA-U02Q-MAX)
               MOVE    "0001"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        ���Գ���
               PERFORM 490-KAKUTEI-SYORI-SEC
           END-IF
           .
      *
       412-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Գ������
      *****************************************************************
       490-KAKUTEI-SYORI-SEC           SECTION.
      *
           INITIALIZE                      SPA-U02Q-DATA-G
           MOVE    SPA-U02Q-SELNUM         TO  IDX
           MOVE    SPA-U02Q-TPTID (IDX)    TO  SPA-U02Q-PTID
           MOVE    SPA-U02Q-TPTNUM(IDX)    TO  SPA-U02Q-PTNUM
           MOVE    SPA-U02Q-THKNID(IDX)    TO  SPA-U02Q-HKNID
           MOVE    SPA-U02Q-TREGISTID (IDX)
                                           TO  SPA-U02Q-REGISTID
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  QRCDHKN-REGISTYMD
           MOVE    SPA-U02Q-REGISTID   TO  QRCDHKN-REGISTID
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           IF      FLG-QRCDHKN         =   ZERO
               MOVE    9                   TO  QRCDHKN-MOD-FLG2
               MOVE    SMCNDATE-YMD        TO  QRCDHKN-UPYMD
               MOVE    SMCNDATE-HMS        TO  QRCDHKN-UPHMS
      *
               MOVE    QRCDHKN-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "U02Q QRCDHKN KEY UPD ERR:"  MCP-RC
                       ",KEY:" QRCDHKN-KEY
               END-IF
           END-IF
      *
           PERFORM 210-BACK
           .
      *
       490-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "U02Q"              TO  SPA-MOTOPG
           MOVE    "U02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "U02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    �������Խ�����
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "U02Q"              TO  MCP-WINDOW.
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "�����ֹ����ϥ��顼"    TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "�ѣҥǡ����������Ǥ��ޤ���Ǥ�����"
                                                   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  U2ERR
           MOVE    SPA-ERRCD           TO  U2ERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  U2ERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "U02Q"              TO  SPA-MOTOPG
           MOVE    "U2ERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "U2ERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    QR������(���ݸ��Ծ�)�ɹ�
      *****************************************************************
       900-QRCDHKN-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  QRCDHKN-REC
               MOVE    ZERO            TO  FLG-QRCDHKN
           ELSE
               INITIALIZE                  QRCDHKN-REC
               MOVE    1               TO  FLG-QRCDHKN
           END-IF
           .
       900-QRCDHKN-READ-EXT.
           EXIT.
      *****************************************************************
      **    ���ԥޥ������ɤ߹���
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ޥ������ɤ߹���
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ֹ�ޥ������ɤ߹���
      *****************************************************************
       920-HKNNUM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       920-HKNNUM-READ-EXT.
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