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
       PROGRAM-ID.                 ORCGK021.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���Ź԰�
      *  ����ݡ��ͥ��̾  : �ش����顼���� �ʣˣ�������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  00/12/01    MCC��¿��Ǽ   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    NACL-¿��Ǽ  05/11/17  MONFUNC �б�
      *  03.05.00    NACL-¿��Ǽ  07/04/XX  ���롼�׿����б�
      *  04.07.00    NACL-¿��Ǽ  13/07/XX  ��߾ɾ������б�
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
      *    ���Ź԰��Ѷ��̥ѥ�᥿
           COPY    "K01COMMON-SPA".
      *
      *    ���̥����ΰ�
           COPY    "K02SCR-SPA".
      *
      *    ���̿���
           COPY    "ENUM-VALUE".
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SSKIJYO         PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-ENT             PIC 9(06).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ZZ                  PIC ZZZ.
           03  WRK-SELNUM              PIC 9(03).
      *
           03  WRK-ERRMSG          PIC X(40).
      *
           03  WRK-K021-INPUTAREA.
                   07  WRK-K021-KNKMEIFLG          PIC X(001).
                   07  WRK-K021-TOUMEI             PIC X(100).
                   07  WRK-K021-KNKMEI             PIC X(100).
                   07  WRK-K021-SYOJYOU            PIC X(2048).
                   07  WRK-K021-SAYOKIJYO          PIC X(2048).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ɾ����ֵ����ޥ���
       01  SSKIJYO-REC.
           COPY    "CPSSKIJYO.INC".
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
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
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    ���顼���̤��
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
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
           MOVE   "NEW"                TO  MCP-PUTTYPE.
           MOVE   "K021"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������̽���
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           INITIALIZE                  SPA-K021-INPUTAREA
      *
           MOVE    1                   TO  SPA-K021-SELNUM
           MOVE    SPA-K021-SELNUM     TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  SPA-K021-GMN-SELNUM
      *
           MOVE    1                   TO  IDX
           PERFORM 4101-SELNUM-SYORI-SEC
      *
           MOVE    2                   TO  SPA-K021-CUR
           .
       300-SCREEN-EXT.
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
                   MOVE    ZERO            TO  SPA-K021-CHK
                   PERFORM 210-BACK
      *        ��
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 260-UP-SYROI-SEC
      *        ��
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 270-DW-SYROI-SEC
      *        ��Ͽ
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    1               TO  SPA-K021-CHK
                   PERFORM 210-BACK
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
      *        �����ֹ�
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 410-SELNUM-CHK-SEC
      *        �ꥹ������
               WHEN    ANY             ALSO    "MEDLIST"
                   PERFORM 420-LIST-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ����ϥ����å�����
      *****************************************************************
       410-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-K021-CUR
      *
           MOVE    K021-SELNUM         TO  SPA-K021-GMN-SELNUM
           IF      K021-SELNUM         =   SPACE
               MOVE    ZERO                TO  SPA-K021-SELNUM
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    K021-SELNUM             TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0001"              TO  SPA-ERRCD
                   GO      TO      410-SELNUM-CHK-EXT
               ELSE
                   MOVE    SNUM-OUTNUM       TO  SPA-K021-SELNUM
               END-IF
           END-IF
           MOVE    SPA-K021-SELNUM     TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  SPA-K021-GMN-SELNUM
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K021-MAX
               IF      SPA-K021-SELNUM     =   SPA-K021-TSEQNO (IDX)
                   MOVE    1               TO  FLG-CHK
                   PERFORM 4101-SELNUM-SYORI-SEC
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    2                   TO  SPA-K021-CUR
           END-IF
      *
           .
       410-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ɾ������Խ�����
      *****************************************************************
       4101-SELNUM-SYORI-SEC           SECTION.
      *
           IF     (SPA-K021-TTOUMEI (IDX)  =   SPACE    )
               PERFORM VARYING     IDY     FROM    IDX  BY  -1
                       UNTIL       IDY     <   1
                   IF      SPA-K021-TTOUMEI (IDY)  =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    SPA-K021-TTOUMEI (IDY)
                                               TO  WRK-K021-TOUMEI
                       MOVE    1               TO  IDY
                    END-IF
               END-PERFORM
           ELSE
               MOVE    SPA-K021-TTOUMEI (IDX)  TO  WRK-K021-TOUMEI
           END-IF
           IF      SPA-K021-TKNKMEI (IDX)(1:2) =   "��"
                                               OR  "��"
               MOVE    SPA-K021-TKNKMEI(IDX)(3:)   TO  WRK-K021-KNKMEI
           ELSE
               MOVE    SPA-K021-TKNKMEI(IDX)       TO  WRK-K021-KNKMEI
           END-IF
           MOVE    "1"                     TO  WRK-K021-KNKMEIFLG
      *    �ɾ��Խ�
           INITIALIZE                  SSKIJYO-REC
           MOVE    SPA-K021-TSYOJYOUCD (IDX)
                                       TO  SSKIJYO-SYOJYOUCD
      *
           MOVE    SSKIJYO-REC         TO  MCPDATA-REC
           MOVE    "tbl_sskijyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sskijyo"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-SSKIJYO-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SSKIJYO
           END-IF
           MOVE    "tbl_sskijyo"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SSKIJYO         =   ZERO
               MOVE    SSKIJYO-SYOJYOU     TO  WRK-K021-SYOJYOU
               MOVE    SSKIJYO-SAYOKIJYO   TO  WRK-K021-SAYOKIJYO
           ELSE
               MOVE    SPACE               TO  WRK-K021-SYOJYOU
               MOVE    SPACE               TO  WRK-K021-SAYOKIJYO
           END-IF
      *
           .
       4101-SELNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������
      *****************************************************************
       420-LIST-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-K021-MAX
              IF      K021-SELECT (IDX)       =  "T"
                  IF      IDX                 =   SPA-K021-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-K021-TSEQNO(IDX)    TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-K021-SELNUM
           END-IF
      *
           MOVE    SPA-K021-SELNUM     TO  K021-SELNUM
           PERFORM 410-SELNUM-CHK-SEC
      *
           .
       420-LIST-CHK-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ξɾ��������
      *****************************************************************
       270-DW-SYROI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-K021-MAX
              IF      SPA-K021-SELNUM    =   SPA-K021-TSEQNO (IDX)
                  MOVE    1              TO  FLG-OK
              ELSE
                  IF     (FLG-OK             =   1     )  AND
                         (SPA-K021-TSEQNO (IDX)    NOT =   ZERO)
                      MOVE    SPA-K021-TSEQNO(IDX)    TO  WRK-SELNUM
                      MOVE    SPA-K021-MAX         TO  IDX
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  K021-SELNUM
               PERFORM 410-SELNUM-CHK-SEC
           END-IF
      *
           .
       270-DW-SYROI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ξɾ��������
      *****************************************************************
       260-UP-SYROI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL      (IDX       >   SPA-K021-MAX )  OR
                              (FLG-OK    =   1        )
              IF      SPA-K021-SELNUM    =   SPA-K021-TSEQNO (IDX)
                  MOVE    1              TO  FLG-OK
              ELSE
                  IF     (FLG-OK             =   ZERO  )  AND
                         (SPA-K021-TSEQNO (IDX)    NOT =   ZERO)
                      MOVE    SPA-K021-TSEQNO(IDX)    TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  K021-SELNUM
               PERFORM 410-SELNUM-CHK-SEC
           END-IF
      *
           .
       260-UP-SYROI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��롡����
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-HZN-MOTOPG      =   SPACE
               MOVE    "K02"               TO  SPA-SAKIPG
           ELSE
               MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           END-IF
           MOVE    "K021"              TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
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
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K021"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           IF      WRK-K021-KNKMEIFLG  =   SPACE
               MOVE    K021-TOUMEI     TO  WRK-K021-TOUMEI
               MOVE    K021-KNKMEI     TO  WRK-K021-KNKMEI
               MOVE    K021-SYOJYOU    TO  WRK-K021-SYOJYOU
               MOVE    K021-SAYOKIJYO  TO  WRK-K021-SAYOKIJYO
           END-IF
           MOVE    SPACE               TO  K021
      *
           INITIALIZE                      K021
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K021-MAX
               MOVE    SPA-K021-TSEQNO (IDX)   TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  K021-TSEQNO (IDX)
               MOVE    SPA-K021-TTOUMEI (IDX)  TO  K021-TTOUMEI (IDX)
               MOVE    SPA-K021-TKNKMEI (IDX)  TO  K021-TKNKMEI (IDX)
               MOVE    SPA-K021-TTOUYMDW(IDX)  TO  K021-TTOUYMD (IDX)
               MOVE    SPA-K021-TSYOJYOUCD (IDX)
                                               TO  K021-TSYOJYOUCD(IDX)
               IF      SPA-K021-TSEQNO(IDX)    =   SPA-K021-SELNUM
                   MOVE    "T"             TO  K021-SELECT  (IDX)
               ELSE
                   MOVE    "F"             TO  K021-SELECT  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-K021-MAX        TO  K021-COUNT
      *
           MOVE    SPA-K021-GMN-SELNUM TO  K021-SELNUM
           MOVE    WRK-K021-TOUMEI     TO  K021-TOUMEI
           MOVE    WRK-K021-KNKMEI     TO  K021-KNKMEI
           MOVE    WRK-K021-SYOJYOU    TO  K021-SYOJYOU
           MOVE    WRK-K021-SAYOKIJYO  TO  K021-SAYOKIJYO
      *
      *    ���������
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  K021-B12-STATE
      *
      *    ��Ͽ���ʳ��ϡ������
           IF      SPA-K021-SYORI      =   1
               MOVE    WIDGET-INSENSITIVE      TO  K021-B12-STATE
           END-IF
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ���̥������륻�åȽ���
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           EVALUATE    SPA-K021-CUR
               WHEN    1
                   MOVE    "SELNUM"          TO  MCP-WIDGET
               WHEN    2
                   MOVE    "B01"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
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
                   MOVE    "�ֹ��������ϥ��顼"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K021"              TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *****************************************************************
      *    ��ǧ��å�����ɽ����
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ɾ����ֵ����ޥ������ɹ�
      *****************************************************************
       910-SSKIJYO-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SSKIJYO-REC
               MOVE    ZERO            TO  FLG-SSKIJYO
           ELSE
               INITIALIZE                  SSKIJYO-REC
               MOVE    1               TO  FLG-SSKIJYO
           END-IF
      *
           .
       910-SSKIJYO-READ-EXT.
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