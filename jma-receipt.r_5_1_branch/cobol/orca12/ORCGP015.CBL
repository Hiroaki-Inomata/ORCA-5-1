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
       PROGRAM-ID.             ORCGP015.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : �����ֹ������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  04/10/XX    NACL-¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  05/11/11  MONFUNC �б� ¾
      *  03.05.00    NACL-¿��Ǽ  07/05/XX  ���롼�׿����б�
      *  04.07.00    NACL-¿��Ǽ  11/09/XX  ��������ơ��֥����б��ɲ�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
      *INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *    ������Ͽ�����ΰ�
           COPY    "P02COMMON-SPA".
      *    �У������̥����ΰ�
           COPY    "P02SCR-SPA".
      *    ������ɽ��
           COPY    "ENUM-VALUE".
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-PIDMSG          PIC X(100).
      *
           03  WRK-WAYMD.
               05  WRK-WAGO         PIC 9(01).
               05  WRK-WAYY         PIC 9(02).
               05  WRK-WAMM         PIC 9(02).
               05  WRK-WADD         PIC 9(02).
      *
           03  WRK-SEYMD.
               05  WRK-SEYY         PIC 9(04).
               05  WRK-SEMM         PIC 9(02).
               05  WRK-SEDD         PIC 9(02).
      *
           03  WRK-KETA             PIC 9(04).
           03  FLG-KETA             PIC 9(01).
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
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1009.INC".
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    �����ֹ��Ѵ��ʴ�����
       01  PTNUM-REC.
           COPY  "CPPTNUM.INC".
      *
      *    �������������
           COPY    "CPKOHSKY.INC".
      *
      *    �������
           COPY    "CPRCF010.INC".
      *
      *    ����������
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    ����������
       01  TEIKIPRT-REC.
           COPY    "CPTEIKIPRT.INC".
      *
      *    ʬ�ڴ����ơ��֥�
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
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
      *   �����å��ǥ��å�
           COPY    "CPORCCHKDGT.INC".
      *   �����ֹ��Ѵ�����
           COPY    "CPORCSPTNUM.INC".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *   �����ֹ�ȯ������
           COPY    "CPORCSP01.INC".
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
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
      *
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
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
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
      *
           MOVE    SPA-P02KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA  TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "PERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        ������̥��å�
               PERFORM 300-SCREEN-SEC
      *        �����Խ�
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "P015"               TO  MCP-WINDOW
      *
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
           EVALUATE    SPA-MOTOPG
               WHEN    "PID1"
                   IF      SPA-PID1-FLG        =   SPACE
                       CONTINUE
                   ELSE
                       PERFORM 4901-KOUSIN-SYORI-SEC
                   END-IF
                   MOVE    SPACE           TO  SPA-PIDCD
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ѽ���Խ�����
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                      SPA-P015-AREA
           MOVE    1                   TO  SPA-GMN015-CUR
      *
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                      P015
      *
           MOVE    SPA-GMN-PTNUM       TO  P015-PTNUM1
           MOVE    SPA-GMN015-PTNUM    TO  P015-PTNUM2
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
         EVALUATE    SPA-GMN015-CUR
              WHEN    01
                  MOVE    "PTNUM2"       TO  MCP-WIDGET
              WHEN    02
                  MOVE    "B12"          TO  MCP-WIDGET
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
      *        ���
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    SPACE       TO  SPA-GMN015-PTNUM
                   PERFORM 210-BACK
      *        ����
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-KAKUTEI-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "PTNUM2"
                   PERFORM 4001-PTNUM-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ����
      *****************************************************************
       4001-PTNUM-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN015-CUR
           MOVE    P015-PTNUM2         TO  SPA-GMN015-PTNUM
      *
           IF     (SPA-GMN015-PTNUM        =   SPACE )  OR
                  (SPA-GMN015-PTNUM(1:1)   =   "*" AND
                   SPA-GMN015-PTNUM(2:)    =   SPACE )
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               INITIALIZE                      ORCSPTNUMAREA
               MOVE    SPA-GMN015-PTNUM    TO  SPTNUM-PTNUM
               CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                                   SPA-AREA
               EVALUATE    SPTNUM-RC
                   WHEN    00
      *            ���Ԥ���
                       MOVE    SPTNUM-PTNUM        TO  SPA-GMN015-PTNUM
                       MOVE    "0003"              TO  SPA-ERRCD
                   WHEN    20
      *            ����
                       MOVE    "0001"              TO  SPA-ERRCD
                   WHEN    10
      *            ���Թ��������å�
                       MOVE    SPTNUM-PTNUM        TO  SPA-GMN015-PTNUM
                       PERFORM 40011-PTNUM-CHK-SEC
                   WHEN    OTHER
                       MOVE    "0001"              TO  SPA-ERRCD
               END-EVALUATE
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    2                   TO  SPA-GMN015-CUR
           END-IF
      *
           .
       4001-PTNUM-EXT.
           EXIT.
      *****************************************************************
      *    ���Թ��������å�����
      *****************************************************************
       40011-PTNUM-CHK-SEC             SECTION.
      *
      *H30.10
      *    ��������å�
           MOVE    ZERO                TO  WRK-KETA
           PERFORM   VARYING   IDX     FROM    20  BY  -1
                     UNTIL    (IDX         <   1 )
                           OR (WRK-KETA     >   ZERO )
               IF      SPA-GMN015-PTNUM(IDX:1)   NOT =   SPACE
                   MOVE    IDX               TO  WRK-KETA
               END-IF
           END-PERFORM
      *    �֤˶��򤬤�������顼
           MOVE    ZERO                TO  FLG-KETA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   WRK-KETA  )
                         OR   (SPA-ERRCD   NOT =   SPACE)
               IF      SPA-GMN015-PTNUM(IDX:1) =   SPACE
                   IF      FLG-KETA            =   1
                       MOVE    "1005"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-KETA
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  40011-PTNUM-CHK-EXT
           END-IF
      *
      *    ���Թ��������å�
           INITIALIZE                  ORCSP01AREA
           MOVE    "2"                 TO  ORCSP01-SYORI
           MOVE    SPA-GMN015-PTNUM    TO  ORCSP01-PTNUM
           MOVE    SPA-GMN-PTID        TO  ORCSP01-PTID
      *
           CALL    "ORCSP01"           USING
                                       ORCSP01AREA
                                       SPA-AREA
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    ORCSP01-PTNUM       TO  SPA-GMN015-PTNUM
           END-IF
      *
           .
       40011-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       490-KAKUTEI-SEC             SECTION.
      *
      *    �����������å�
           PERFORM 4001-PTNUM-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"          TO  SPA-PIDCD
           END-IF
      *
          .
       490-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������
      *****************************************************************
       4901-KOUSIN-SYORI-SEC           SECTION.
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-GMN015-PTNUM    TO  PTNUM-PTNUM
           IF      SPA-GMN015-PTNUM(20:1) =   SPACE
               STRING  SPA-GMN015-PTNUM    DELIMITED   BY  SPACE
                       "%"                 DELIMITED   BY  SIZE
                       INTO                    PTNUM-PTNUM
               END-STRING
           END-IF
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 950-PTNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTNUM           =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               PERFORM 49011-PTNUM-UPDATE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 210-BACK
           ELSE
               MOVE    1                   TO  SPA-GMN015-CUR
           END-IF
          .
       4901-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������
      *****************************************************************
       49011-PTNUM-UPDATE-SEC           SECTION.
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 950-PTNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
           END-IF
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF     (FLG-PTNUM           =   1  )  OR
                  (PTNUM-PTNUM     NOT =   SPA-GMN-PTNUM)
               MOVE    "0004"              TO  SPA-ERRCD
           ELSE
      *        �����ֹ湹��
               MOVE    SPA-GMN015-PTNUM    TO  PTNUM-PTNUM
      *
               MOVE    PTNUM-REC           TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P015 PTNUM UPDATE:" MCP-RC
                           ",KEY:" PTNUM-KEY
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    ��������ơ��֥�
           INITIALIZE                      KOHSKY-REC
           MOVE    SPA-HOSPNUM         TO  KOHSKY-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  KOHSKY-PTID
           MOVE    SPA-GMN015-PTNUM    TO  KOHSKY-PTNUM
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P015 KOHSKY UPDATE:" MCP-RC
                           ",KEY:" KOHSKY-KEY
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *
      *    �������
           INITIALIZE                      RECE10-REC
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  RECE10-PTID
           MOVE    SPA-GMN015-PTNUM    TO  RECE10-PTNUM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P015 RECE10 UPDATE:" MCP-RC
                           ",KEY:" RECE10-KEY
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *    ���������������
           INITIALIZE                      TEIKIPRT-REC
           MOVE    SPA-HOSPNUM         TO  TEIKIPRT-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  TEIKIPRT-PTID
           MOVE    SPA-GMN015-PTNUM    TO  TEIKIPRT-PTNUM
           MOVE    TEIKIPRT-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_teikiprt"      TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P015 TEIKIPRT UPDATE:" MCP-RC
                           ",KEY:" TEIKIPRT-KEY
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *
      *    ʬ�ڴ����ơ��֥�
           INITIALIZE                      BUNBEN-REC
           MOVE    SPA-HOSPNUM         TO  BUNBEN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  BUNBEN-PTID
           MOVE    SPA-GMN015-PTNUM    TO  BUNBEN-PTNUM
           MOVE    BUNBEN-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_bunben"        TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P015 BUNBEN UPDATE:" MCP-RC
                           ",KEY:" BUNBEN-KEY
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *
      *    �����������ơ��֥빹��
           INITIALIZE                      ORDER-REC
           MOVE    SPA-HOSPNUM         TO  ORDER-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  ORDER-PTID
           MOVE    SPA-GMN015-PTNUM    TO  ORDER-PTNUM
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "upd2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P015 ORDER UPDATE:" MCP-RC
                           ",KEY:" ORDER-KEY
               MOVE    "0009"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-PTNUM           =   SPA-GMN-PTNUM
                   MOVE    SPA-GMN015-PTNUM    TO  SPA-PTNUM
               END-IF
               IF      SPA-GMN-PTNUM       =   SPA-LAST-PTNUM
                   MOVE    SPA-GMN015-PTNUM    TO  SPA-LAST-PTNUM
               END-IF
               MOVE    SPA-GMN015-PTNUM    TO  SPA-GMN-PTNUM
           END-IF
      *
           .
       49011-PTNUM-UPDATE-EXT.
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
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-PIDCD       NOT =   SPACE
               PERFORM 520-JIDSET-SEC
               GO      TO              500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "P015"              TO  MCP-WINDOW.
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
                   MOVE    "�����ֹ椬�㤤�ޤ���"
                                           TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "������Ͽ�Ѥδ����ֹ�Ǥ�"
                                           TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "���ԣɣĤȴ����ֹ椬�㤤�ޤ���"
                                           TO  SPA-ERRMSG
                   MOVE    "�����Ǥ��ޤ���"
                                           TO  SPA-ERRMSG(31:)
               WHEN    "0009"
                   MOVE    "�ޥ����������Ǥ��ޤ���Ǥ�����"
                                           TO  SPA-ERRMSG
      *H30.10
               WHEN    "1013"
                   MOVE    "�����ֹ�˻��ѤǤ��ʤ�ʸ��������ޤ���"
                                               TO  SPA-ERRMSG
      *
      *        ���֥��顼
               WHEN    "1005"
                   MOVE    "�����ֹ�η�����㤤�ޤ���"
                                               TO  SPA-ERRMSG
               WHEN    "1006"
                   MOVE    "�����ֹ�ι������㤤�ޤ���"
                                               TO  SPA-ERRMSG
               WHEN    "1007"
                   MOVE    "�����ֹ�θ����ֹ椬�㤤�ޤ���"
                                               TO  SPA-ERRMSG
               WHEN    "1008"
                   MOVE    "�����ֹ��Ϣ�֤ϴ���Ϣ�ְʲ��ˤ��Ʋ�����"
                                               TO  SPA-ERRMSG
      *        �����ֹ�������顼
               WHEN    "8000"
                   MOVE    "��ư���֤δ����ֹ椬�����ͤǤ���"
                                               TO  SPA-ERRMSG
                   MOVE    "����ֹ椫����֤��ޤ�����"
                                               TO  SPA-ERRMSG(33:)
               WHEN    "8001"
                   MOVE    "���ԣɣĤ������ͤǤ���"
                                               TO  SPA-ERRMSG
                   MOVE    "����ֹ椫����֤��ޤ�����"
                                               TO  SPA-ERRMSG(23:)
               WHEN    "8002"
                   MOVE    "��ư���֤δ����ֹ椬�����ͤˤʤ�ޤ�����"
                                               TO  SPA-ERRMSG
                   MOVE    "Ϣ�ֹ������ѹ����Ʋ�������"
                                               TO  SPA-ERRMSG(41:)
               WHEN    "8003"
                   MOVE    "���ԣɣĤ���Ͽ�ѤǤ�����Ͽ�Ǥ��ޤ���"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  PERR
           MOVE    SPA-ERRCD           TO  PERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  PERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "P015"              TO  SPA-MOTOPG
           MOVE    "PERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PERR"              TO  MCP-WINDOW.
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
      *    ���顼��å�����ɽ����
      *****************************************************************
       520-JIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0101"
                   MOVE    "�����ֹ���ѹ����ޤ���"
                                               TO  WRK-PIDMSG
                   MOVE    "���ߤδ����ֹ�Ϸ��֤Ȥʤ�ޤ���"
                                               TO  WRK-PIDMSG(23:)
                   MOVE    "��������Ǥ�����"
                                               TO  WRK-PIDMSG(55:)
               WHEN    OTHER
                   MOVE    SPA-PIDCD
                                       TO  WRK-PIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PID1-FLG
      *
           MOVE    SPACE               TO  PID1
           INITIALIZE                      PID1
           MOVE    SPA-PIDCD           TO  PID1-ID1CODE
           MOVE    WRK-PIDMSG          TO  PID1-ID1MSG
           MOVE    "���"              TO  PID1-B01
           MOVE    "�ϣ�"              TO  PID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "P015"              TO  SPA-MOTOPG
           MOVE    "PID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "PID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-JIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
      *
           MOVE    "P015"              TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "P02"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ��Ѵ��ɤ߹���
      *****************************************************************
       950-PTNUM-READ-SEC           SECTION.
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNUM-REC
               MOVE    ZERO            TO  FLG-PTNUM
           ELSE
               MOVE    1               TO  FLG-PTNUM
           END-IF
      *
           .
       950-PTNUM-READ-EXT.
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