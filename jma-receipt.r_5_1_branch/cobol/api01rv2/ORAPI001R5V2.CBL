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
       PROGRAM-ID.             ORAPI001R5V2.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : APIϢ���ѥ⥸�塼��
      *  ����ݡ��ͥ��̾  : ����ƣ����������˰���
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  18/07/26    NACL�����ġ���������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-ORCSNUM             PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXD                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
      *
       01  API-AREA.
           03  API-NYUINYMD            PIC X(08).
           03  API-TAIINYMD            PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-TRUE              PIC X(05)   VALUE "True".
           03  CONST-FALSE             PIC X(05)   VALUE "False".
      *
           COPY    "CPRPKEY.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    ����饤��Ģɼ�ɣ�Ϣ��
           COPY    "CPORCSRPLNK.INC".
      *
      *    ����饤��Ģɼ̾��������ץ��̾����
           COPY    "CPORCSPRTNM.INC".
      *
      *    �����Ѵ��ΰ�
           COPY    "CPORCSNUM.INC".
      *
      *    ����ƣ�����������
           COPY    "CPORCHCN01.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ���饤����Ȱ�������
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    API XML�ɤ߹����������
           COPY    "CPKARTE1HV2REQ.INC".
      *
      *    API XML�ɤ߹��߶������
           COPY    "CPAPIV2REQ.INC".
      *
      *    ����ǥ��쥯�ȥ��������
           COPY    "CPORCSTEMPDIR.INC".
      *
      *    API��Ģɼ�ǡ����쥹�ݥ󥹺���
           COPY    "CPORCSAPIFM.INC".
      *
      *    �����ֹ��Ѵ�����
           COPY    "CPORCSPTID.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ����
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1005.INC".
           COPY    "CPSK1010.INC".
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    �ݸ��ȹ礻
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY     "API01RV2SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "*******************"
           DISPLAY   "* karte_no1_h start *"
           DISPLAY   "*******************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  API-AREA
           INITIALIZE                  KARTE1HREQ-AREA
      *
      *    �������
           PERFORM 100-INIT-SEC
      *
      *    �����
           PERFORM 200-MAIN-SEC
      *
      *    �ֵ��ΰ��Խ�
           PERFORM 300-END-SEC
      *
           DISPLAY   "*****************"
           DISPLAY   "* karte_no1_h end *"
           DISPLAY   "*****************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
      *    ���ŵ��ؼ����ֹ楻�å�
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "0099"       TO  SPA-ERRCD
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *
      *        �ӣУ���������
               INITIALIZE              SYS-1010-REC
               INITIALIZE              ORCSCOMMONAREA
               MOVE    "M00"       TO  ORCSCOMMON-PG
               CALL    "ORCSCOMMON"    USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                           SYS-1010-REC
      *        IF    ( SPA-ERRCD   NOT =   SPACE )
      *            MOVE   "0089"   TO  SPA-ERRCD
      *        END-IF
           END-IF
      *
           INITIALIZE                  STEMPDIR-AREA
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           IF    ( SPA-ERRCD       =   SPACE )
      *        XML������Ф�
               PERFORM 900-XML-READ-SEC
               MOVE    LOW-VALUE   TO  APILST40-BODY
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-REQUEST-CHK-SEC
      *
           IF    ( SPA-ERRCD   =   SPACE )
               PERFORM 2001-PRINT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    �ꥯ�����ȹ��ܽ���
      *****************************************************************
       2001-REQUEST-CHK-SEC            SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-REQNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-PRINT-MODE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-HAKKOKBN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-PTNUM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-NYUINYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-TAIINYMD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-SRYKA-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-DRCD-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 2001-HKNCOMBINUM-CHK-SEC
           END-IF
      *
           .
       2001-REQUEST-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ꥯ�������ֹ�����å�����
      *****************************************************************
       2001-REQNUM-CHK-SEC             SECTION.
      *
           IF    ( KARTE1HREQ-REQNUM    =   "01" OR "02" )
               CONTINUE
           ELSE
               MOVE    "0101"      TO  SPA-ERRCD
           END-IF
      *
           .
       2001-REQNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����⡼�ɥ����å�����
      *****************************************************************
       2001-PRINT-MODE-CHK-SEC         SECTION.
      *
           EVALUATE    KARTE1HREQ-REQNUM
           WHEN    "01"
               MOVE    FUNCTION UPPER-CASE(KARTE1HREQ-PRINT-MODE)
                                   TO  KARTE1HREQ-PRINT-MODE
               EVALUATE    KARTE1HREQ-PRINT-MODE
               WHEN    SPACE
                   CONTINUE
               WHEN    "PDF"
                   INITIALIZE                  SRPLNK-AREA
                   MOVE    "CHK_MONBLOB"   TO  SRPLNK-MODE
                   CALL    "ORCSRPLNK"         USING
                                               SRPLNK-AREA
                                               SPA-AREA
                   IF    ( SRPLNK-RC       NOT =   ZERO )
                       MOVE    "0128"      TO  SPA-ERRCD
                   END-IF
               WHEN    OTHER
                   MOVE    "0127"      TO  SPA-ERRCD
               END-EVALUATE
      *
           END-EVALUATE
      *
           .
       2001-PRINT-MODE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ȯ�Զ�ʬ�����å�����
      *****************************************************************
       2001-HAKKOKBN-CHK-SEC           SECTION.
      *
           IF    ( KARTE1HREQ-HAKKOKBN  =   SPACE )
               MOVE    "1"             TO  KARTE1HREQ-HAKKOKBN
           END-IF
      *
           IF    ( KARTE1HREQ-HAKKOKBN  =   "1" OR "2" )
               CONTINUE
           ELSE
               MOVE    "0107"          TO  SPA-ERRCD
           END-IF
      *
           .
       2001-HAKKOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ�����å�����
      *****************************************************************
       2001-PTNUM-CHK-SEC               SECTION.
      *
           IF    ( KARTE1HREQ-PTNUM     =   SPACE )
               MOVE    "0102"      TO  SPA-ERRCD
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    KARTE1HREQ-PTNUM     TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        NOT =   ZERO )
                   MOVE    "0103"      TO  SPA-ERRCD
               END-IF
      *
               MOVE    SPTID-PTNUM     TO  SPA-PTNUM
               MOVE    SPTID-PTID      TO  SPA-PTID
      *
           END-IF
      *
           IF    ( SPA-ERRCD               =   SPACE )
               PERFORM 900-PTINF-KEY-SEL-SEC
               IF    ( FLG-PTINF       NOT =   ZERO )
                   MOVE    "0103"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����������å�����
      *****************************************************************
       2001-NYUINYMD-CHK-SEC           SECTION.
      *
           IF    ( KARTE1HREQ-NYUINYMD =    SPACE )
               CONTINUE
           ELSE
               MOVE    KARTE1HREQ-NYUINYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0110"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  API-NYUINYMD
               END-IF
           END-IF
      *
           .
       2001-NYUINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ౡ�������å�����
      *****************************************************************
       2001-TAIINYMD-CHK-SEC           SECTION.
      *
           IF    ( KARTE1HREQ-TAIINYMD =    SPACE )
               CONTINUE
           ELSE
               MOVE    KARTE1HREQ-TAIINYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "0111"      TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-SYMD    TO  API-TAIINYMD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF    ( API-NYUINYMD    NOT =   SPACE )
                AND  ( API-TAIINYMD    NOT =   SPACE )
                AND  ( API-NYUINYMD    >   API-TAIINYMD )
                       MOVE    "0112"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-TAIINYMD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ���Ųʥ����å�����
      *****************************************************************
       2001-SRYKA-CHK-SEC              SECTION.
      *
           IF    ( KARTE1HREQ-SRYKA     =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    SPA-HOSPNUM     TO  SYS-HOSPNUM
               MOVE    "1005"          TO  SYS-KANRICD
               MOVE    KARTE1HREQ-SRYKA TO  SYS-KBNCD
               MOVE    SPA-SYSYMD      TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYSKANRI-REC TO SYS-1005-REC
                   MOVE    KARTE1HREQ-SRYKA
                                       TO  SPA-SRYKA
                   MOVE    SYS-1005-SRYKANAME1
                                       TO  SPA-SRYKAMEI
               ELSE
                   MOVE    "0108"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ô��������å�����
      *****************************************************************
       2001-DRCD-CHK-SEC               SECTION.
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   3 )
                    OR   ( SPA-ERRCD   NOT =   SPACE )
      *
               IF    ( KARTE1HREQ-DRCD (IDXD)  NOT =   SPACE )
                   INITIALIZE              SYSKANRI-REC
                   MOVE    SPA-HOSPNUM TO  SYS-HOSPNUM
                   MOVE    "1010"      TO  SYS-KANRICD
                   MOVE    KARTE1HREQ-DRCD (IDXD)
                                       TO  SYS-KBNCD
                   MOVE    SPA-SYSYMD  TO  SYS-STYUKYMD
                                               SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY10-SEL-SEC
                   IF    ( FLG-SYSKANRI    =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    "0113"  TO  SPA-ERRCD
                   END-IF
               END-IF
      *
           END-PERFORM
      *
           .
       2001-DRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ȹ礻�����å�����
      *****************************************************************
       2001-HKNCOMBINUM-CHK-SEC        SECTION.
      *
           IF    ( KARTE1HREQ-HKNCOMBINUM   =   SPACE )
               CONTINUE
           ELSE
               IF    ( FUNCTION TRIM(KARTE1HREQ-HKNCOMBINUM)
                                       IS  NOT NUMERIC )
                   MOVE    "0109"      TO  SPA-ERRCD
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   PERFORM 900-HKNCOMBI-KEY-SEL-SEC
                   IF    ( FLG-HKNCOMBI    =   ZERO )
                       MOVE    COMB-HKNCOMBINUM
                                       TO  SPA-HKNCOMBINUM
                   ELSE
                       MOVE    "0109"  TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-HKNCOMBINUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �������ǡ�����������
      *****************************************************************
       2001-PRINT-SEC                  SECTION.
      *
           PERFORM 20011-PRE-PRINT-SEC
      *
      *    ����ư���
           INITIALIZE                      ORCHCN01AREA
           IF    ( KARTE1HREQ-HAKKOKBN  =   "1" )
               MOVE    "2"             TO  ORCHCN01-SYORIKBN
               MOVE    "1"             TO  ORCHCN01-PRTKBN
           ELSE
               MOVE    "1"             TO  ORCHCN01-SYORIKBN
               MOVE    "2"             TO  ORCHCN01-PRTKBN
           END-IF
      *
           MOVE    API-NYUINYMD        TO  ORCHCN01-NYUINYMD
           MOVE    API-TAIINYMD        TO  ORCHCN01-TAIINYMD
      *
           MOVE    KARTE1HREQ-DRCD (1) TO  ORCHCN01-DR (1)
           MOVE    KARTE1HREQ-DRCD (2) TO  ORCHCN01-DR (2)
           MOVE    KARTE1HREQ-DRCD (3) TO  ORCHCN01-DR (3)
      *
      *    ���饤����Ȱ���
           MOVE    "01"                TO  SPA-PRT-FLG
           INITIALIZE                  SRPLNK-AREA
           MOVE    "INS"           TO  SRPLNK-MODE
           MOVE    RPKEY-KARTE-NO1-N
                                   TO  SRPLNK-VAL
           CALL    "ORCSRPLNK"         USING
                                       SRPLNK-AREA
                                       SPA-AREA
      *
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "00000001"          TO  ORCSPRTNM-KBNCD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYSKANRI-REC
      *
           CALL    ORCSPRTNM-PRTPG     USING
                                       SPA-AREA
                                       ORCHCN01AREA
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           .
       2001-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    �������ǡ�����������
      *****************************************************************
       20011-PRE-PRINT-SEC             SECTION.
      *
           IF    ( KARTE1HREQ-REQNUM    =   "01" )
               MOVE    2           TO  SPA-API-FLG
               IF    ( KARTE1HREQ-PRINT-MODE   =   "PDF" )
                   MOVE    "3"     TO  SPA-PRTCONF
               END-IF
           ELSE
               MOVE    3           TO  SPA-API-FLG
           END-IF
      *
           CALL    "ORCSONPRTSET"      USING
                                       SPA-AREA
      *
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING
                                       SPA-AREA
                                       ORCSPRTCTRLAREA
                                       MCPAREA
      *
           .
       20011-PRE-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD    =  "0099" )
               MOVE    404             TO  APILST40-HTTPSTATUS
           ELSE
               INITIALIZE                  SAPIFM-AREA
               MOVE    SPA-ERRCD       TO  SAPIFM-ERRCD
               CALL    "ORCSAPIFM"         USING
                                           SAPIFM-AREA
                                           SPA-AREA
               IF    ( SAPIFM-OBJECT   NOT =   LOW-VALUE )
                   MOVE    SAPIFM-OBJECT
                                       TO  APILST40-BODY
                   MOVE    "application/json"
                                       TO  APILST40-CONTENT-TYPE
               END-IF
           END-IF
      *
           INITIALIZE                  STEMPDIR-AREA
           MOVE    "DEL"           TO  STEMPDIR-MODE
           CALL    "ORCSTEMPDIR"       USING
                                       STEMPDIR-AREA
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           INITIALIZE              STS-AREA-DAY
           INITIALIZE              LNK-DAY2-AREA
           MOVE    "21"        TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING
                                   STS-AREA-DAY
                                   LNK-DAY2-AREA
           IF    ( STS-DAY-RC1     =   ZERO )
               MOVE    LNK-DAY2-EDTYMD1
                               TO  WRK-EDTYMD1
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    ���Ծ��󸡺�����
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF     ( MCP-RC             =  ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ƥ������������
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ȹ礻��������
      *****************************************************************
       900-HKNCOMBI-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBI
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    FUNCTION NUMVAL(KARTE1HREQ-HKNCOMBINUM)
                                       TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNCOMBI-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    XML���󤫤����Ƥ���Ф�
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( APILST40-BODY    NOT =   LOW-VALUE )
               DISPLAY "karte_no1_h object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_karte_no1_hv2req"  TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    APILST40-BODY           TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "karte_no1_hv2req"      TO  APIREQ-RECORDNAME
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "0098"            TO  SPA-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE            TO  APILST40-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XML�ե��륿��󥰽���
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  KARTE1HREQ-DATA-AREA
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸�������
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FETCH�Ϥ��ʤ���
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ģ��ɤ߹��߽���
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    �أͣ��ɹ�����
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.