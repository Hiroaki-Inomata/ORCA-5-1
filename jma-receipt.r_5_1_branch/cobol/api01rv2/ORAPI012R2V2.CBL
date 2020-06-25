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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORAPI012R2V2.
      *****************************************************************
      *  �����ƥ�̾        : ����ɸ��쥻�ץȥ��ե�
      *  ���֥����ƥ�̾    : APIϢ���ѥ⥸�塼��
      *  ����ݡ��ͥ��̾  : �����ֹ������������
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  12/06/XX    NACL��¿��Ǽ����������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *
      *****************************************************************
      *
       ENVIRONMENT           DIVISION.
       CONFIGURATION         SECTION.
       INPUT-OUTPUT          SECTION.
      *
       FILE-CONTROL.
       DATA                DIVISION.
      *FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
      *
           03  FLG-SYORI               PIC 9(01).
           03  FLG-SYORI-END           PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(04).
               05  WRK-SMM             PIC X(02).
               05  WRK-SDD             PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    �����Խ�
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *    ���顼������
           03  WRK-ERRCD               PIC X(02).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-KEICD               PIC X(02).
      *
           03  WRK-MCP-PATHNAME        PIC X(64).
           03  WRK-ZZ9                 PIC ZZZ9.
      *
           03  WRK-CNT                 PIC 9(04).
      *
           03  WRK-START-YMD           PIC X(08).
           03  WRK-END-YMD             PIC X(08).
      *
       01  WRK-XML-AREA.
           03  WRK-START-DATE          PIC X(10).
           03  WRK-END-DATE            PIC X(10).
      *
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *   �����ֹ��Ѵ�����
      *    COPY    "CPORCSPTID.INC".
      *   �����ֹ��Ѵ�����
      *    COPY    "CPORCSPTNUM.INC".
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    API XML�ɤ߹����������
           COPY    "CPPATIENTLST1V2REQ.INC".
      *    API XML�ɤ߹����������
           COPY    "CPPATIENTLST1V2RES.INC".
      *ver.4.7
      *    API XML�ɤ߹��߶������
           COPY    "CPAPIV2REQ.INC".
      *
      *    �ݸ��ȹ礻���å��ΰ�
      ***  COPY    "CPORCSAPIHKN.INC".
      *
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �����ƥ�����ޥ���
           COPY    "CPSYSKANRI.INC".
      *
      *    ��������
           COPY  "CPSK1010.INC".
      *
      *    ���ԥޥ�����
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "ORCA-BLOB".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
       01  SCRAREA.
           COPY    "API01RV2SCRAREA.INC".
      *    COPY    "CPAPIPTAV2.INC".
      *    COPY    "CPAPILSTV2.INC".
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST1-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST2-//.
      *    COPY    "CPAPILSTV2.INC"  REPLACING
      *                              //APILST-// BY //APILST3-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST4-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST5-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST6-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                              //APILST-// BY //APILST7-//.
      *    COPY    "CPAPILSTV2.INC"    REPLACING
      *                                //APILST-// BY //APILST8-//.
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "********************"
           DISPLAY   "* patientlst1 start*"
           DISPLAY   "********************"
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      ***  INITIALIZE                      CNT-AREA
           INITIALIZE                      SPA-AREA
      *
      *    �������
           PERFORM 100-INIT-SEC
      *    �����
           IF      WRK-ERRCD           =   SPACE
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *
           IF      WRK-ERRCD           =   "99"
      *        �桼���ɣĥ��顼
               MOVE   404                  TO  APILST2-HTTPSTATUS
           ELSE
      *        �ֵ��ΰ��Խ�
               PERFORM 300-PTXMLMAKE-SEC
           END-IF
      *
           DISPLAY   "*******************"
           DISPLAY   "* patientlst1 end *"
           DISPLAY   "*******************"
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
           INITIALIZE                      WRK-XML-AREA
           INITIALIZE                      XML-PATIENTLST1RES
      *    ����������
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE   "Patient Info"       TO  PATLSTRES-RESKEY
      *
           MOVE    MCP-USER            TO  SPA-OPID
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *    ���ա����ֽ����Խ�
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PATLSTRES-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  PATLSTRES-INFORMATION-TIME
      *
      *    ���ŵ��ؼ����ֹ楻�å� 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "99"             TO  WRK-ERRCD
               GO  TO         100-INIT-EXT
           END-IF
      *    �ӣУ���������
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE   "89"             TO  WRK-ERRCD
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    XML������Ф�
           PERFORM 1000-APTLSTREAD-SEC
           IF      WRK-ERRCD       NOT =   SPACE
               GO      TO      200-MAIN-EXT
           END-IF
      *
           MOVE    ZERO                TO  PATLSTRES-API-RESULT
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-KEICD
      *
           MOVE    ZERO                TO  FLG-SYORI
           EVALUATE    APILST2-CLASS
           WHEN    "01"
      *        ����������
               MOVE    1                   TO  FLG-SYORI
           WHEN    "02"
      *        ����
               MOVE    2                   TO  FLG-SYORI
           WHEN    OTHER
               MOVE    "91"                TO  WRK-ERRCD
           END-EVALUATE
      *    ���Ϲ��ܥ����å�����
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
      *    ���԰�������
           IF      WRK-ERRCD           =   SPACE
               PERFORM 2002-PTINF-LIST-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML���󤫤����Ƥ���Ф�
      *****************************************************************
       1000-APTLSTREAD-SEC   SECTION.
      *
           IF      APILST2-BODY        NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
           END-IF
           INITIALIZE                      XML-APIREQ
      * XML������Ф�
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientlst1v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST2-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientlst1req"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
               MOVE   "97"             TO  WRK-ERRCD
               GO     TO      1000-APTLSTREAD-EXT
           END-IF
      *
           MOVE    "XMLREAD"           TO  MCP-FUNC
           MOVE    "xml_patientlst1v2req"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    APILST2-BODY        TO  APIREQ-OBJECT
           MOVE    ZERO                TO  APIREQ-MODE
           MOVE    "patientlst1req"    TO  APIREQ-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-APIREQ
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    XML-APIREQ          TO  XML-PATIENTLST1REQ
               PERFORM 10001-XML-REMAKE-SEC
           ELSE
               MOVE   "98"             TO  WRK-ERRCD
               DISPLAY "XMLREAD failure = " MCP-RC
           END-IF
      *
           .
       1000-APTLSTREAD-EXT.
           EXIT.
      *****************************************************************
      *    �ɤ߹����XML��LOW-VALUE ��  SPACE ���ִ�
      *****************************************************************
       10001-XML-REMAKE-SEC        SECTION.
      *
           IF      PATLSTREQ-START-DATE    =   LOW-VALUE
               MOVE  SPACE                 TO  PATLSTREQ-START-DATE
           END-IF
           IF      PATLSTREQ-END-DATE      =   LOW-VALUE
               MOVE  SPACE                 TO  PATLSTREQ-END-DATE
           END-IF
           .
       10001-XML-REMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ƥ����å�
      *****************************************************************
       2001-INPUT-CHK-SEC      SECTION.
      *
           MOVE    PATLSTREQ-START-DATE    TO  WRK-START-DATE
           MOVE    PATLSTREQ-END-DATE      TO  WRK-END-DATE
      *    ������
           MOVE    PATLSTREQ-START-DATE    TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-START-YMD
      *    ��λ��
           MOVE    PATLSTREQ-END-DATE      TO  WRK-HEN-DATE
           MOVE    SPACE                   TO  WRK-HEN-TIME
           PERFORM 802-DAYHEN02-SEC
           MOVE    WRK-SYMD                TO  WRK-END-YMD
      *    ������̤����ϥ����ƥ�����
           IF      WRK-START-DATE      =   SPACE
               MOVE    SPA-SYSYMD          TO  WRK-START-YMD
               MOVE    WRK-START-YMD       TO  WRK-SYMD
               MOVE    ZERO                TO  WRK-THMS
               PERFORM 801-DAYHEN01-SEC
      ***      MOVE    WRK-HEN-DATE        TO  PATLSTRES-START-DATE
           END-IF
      *    ��λ��̤����Ͻ�λ�ʤ�
           IF      WRK-END-DATE        =   SPACE
               MOVE    "99999999"          TO  WRK-END-YMD
           END-IF
      *    �������佪λ���ϥ��顼
           IF      WRK-START-YMD       >   WRK-END-YMD
               MOVE    "01"                TO  WRK-ERRCD
           END-IF
      *    �ƥ��ȴ��Զ�ʬ
           IF      WRK-ERRCD           =   SPACE
               IF      PATLSTREQ-TEST-KBN  =   "1"
                                           OR  "0"
                                           OR  SPACE
                   CONTINUE
               ELSE
                   MOVE    "02"                TO  WRK-ERRCD
               END-IF
           END-IF
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ��������
      *****************************************************************
       2002-PTINF-LIST-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
      *    ����ǯ����
           MOVE    WRK-START-YMD       TO  PTNUM-CREYMD
      *    ����ǯ����
           MOVE    WRK-START-YMD       TO  PTNUM-UPYMD
           IF      FLG-SYORI           =   1
      *        ��������Ͽ
               MOVE    "key6"              TO  WRK-MCP-PATHNAME
           ELSE
      *        ��Ͽ
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
           END-IF
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTNUM-READ-SEC
           END-IF
           MOVE    ZERO                TO  FLG-SYORI-END
           MOVE    ZERO                TO  WRK-CNT
           PERFORM UNTIL     (FLG-PTNUM    NOT =   ZERO )
     ****              OR    (FLG-SYORI-END    =   1    )
                       OR    (WRK-ERRCD    NOT =   SPACE)
               IF      FLG-SYORI           =   1
      *            ��������ϿȽ��
      *            IF     (PTNUM-CREYMD        >   WRK-END-YMD)
      *            AND    (PTNUM-UPYMD         >   WRK-END-YMD)
      *                MOVE    1               TO  FLG-SYORI-END
      *            END-IF
      *
      *H28.7       ��������Ͽ�����о�Ƚ��
                   MOVE    1               TO  FLG-SYORI-END
                   IF     (PTNUM-CREYMD    NOT =   SPACE      )
                      IF      (PTNUM-CREYMD    >=  WRK-START-YMD)
                        AND   (PTNUM-CREYMD    <=  WRK-END-YMD  )
                           MOVE    ZERO            TO  FLG-SYORI-END
                      END-IF
                   END-IF
                   IF     (PTNUM-UPYMD         >=  WRK-START-YMD)
                     AND  (PTNUM-UPYMD         <=  WRK-END-YMD  )
                       MOVE    ZERO            TO  FLG-SYORI-END
                   END-IF
               ELSE
      *            ��ϿȽ��
                   IF     (PTNUM-CREYMD        >   WRK-END-YMD)
                       MOVE    1               TO  FLG-SYORI-END
                   END-IF
               END-IF
      *
               IF      FLG-SYORI-END       =   ZERO
                   PERFORM 20021-PTINF-CHK-SEC
               END-IF
      *
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PTNUM-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    "20"            TO  WRK-ERRCD
           END-IF
           .
       2002-PTINF-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ծ����Խ�����
      *****************************************************************
       20021-PTINF-CHK-SEC     SECTION.
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTNUM-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *    �ƥ��ȴ����оݳ�Ƚ��
           MOVE    ZERO                TO  FLG-CHK
           IF      PATLSTREQ-TEST-KBN  =   "1"
               IF      PTINF-TSTPTNUMKBN   =   "1"
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-IF
           IF      FLG-CHK             =   ZERO
               ADD     1               TO  IDX
               IF      IDX             >   1000
                   MOVE    "10"            TO  WRK-ERRCD
      *            1000�索���С�
                   MOVE    9999            TO  WRK-CNT
               ELSE
                   PERFORM 20021-PTINF-HEN-SEC
               END-IF
           END-IF
           .
       20021-PTINF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ծ����Խ�����
      *****************************************************************
       20021-PTINF-HEN-SEC     SECTION.
      *
           ADD     1                   TO  WRK-CNT
      *
           MOVE    PTNUM-PTNUM         TO  PATLSTRES-PATIENTID (IDX)
           MOVE    PTINF-NAME          TO  PATLSTRES-NAME      (IDX)
           MOVE    PTINF-KANANAME      TO  PATLSTRES-KANANAME  (IDX)
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           MOVE    ZERO                TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PATLSTRES-BIRTHDAY  (IDX)
           MOVE    PTINF-SEX           TO  PATLSTRES-SEX       (IDX)
      *    ��Ͽ��
           MOVE    PTNUM-CREYMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PATLSTRES-CREYMD  (IDX)
      *    ������
           MOVE    PTNUM-UPYMD         TO  WRK-SYMD
           MOVE    PTNUM-UPHMS         TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PATLSTRES-UPYMD  (IDX)
      *H29.12 ����
           MOVE    WRK-HEN-TIME        TO  PATLSTRES-UPTIME (IDX)
      *    �ƥ��ȴ���
           MOVE    PTINF-TSTPTNUMKBN   TO  PATLSTRES-TSTPTNUMKBN(IDX)
           .
       20021-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML����񤭹��߽���
      *****************************************************************
       300-PTXMLMAKE-SEC               SECTION.
      *
      *    �����оݷ��
           MOVE    WRK-CNT             TO  PATLSTRES-PATIENTID-CNT
      *    ���顼��å������Խ�
           IF      WRK-ERRCD           =   SPACE
               MOVE    WRK-KEICD           TO  WRK-ERRCD
           END-IF
           IF      WRK-ERRCD           =   SPACE
      *        ���ｪλ
               MOVE    "������λ"          TO  PATLSTRES-API-RESULT-MSG
           ELSE
      *        ���顼���ٹ��å�����
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD           TO  PATLSTRES-API-RESULT
               MOVE    WRK-ERRMSG          TO  PATLSTRES-API-RESULT-MSG
           END-IF
      *
           IF      APILST2-BODY     NOT =   LOW-VALUE
               DISPLAY "ACPLSTRES-OBJECT not low_value"
           END-IF
      *
           MOVE    "XMLOPEN"           TO  MCP-FUNC
           MOVE    "xml_patientlst1v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PATLSTRES-MODE
           MOVE    "patientlst1res"    TO  PATLSTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTLST1RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               CONTINUE
           ELSE
               DISPLAY "XMLOPEN failure = " MCP-RC
           END-IF
      *
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           MOVE    "xml_patientlst1v2res"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  PATLSTRES-MODE
           MOVE    "patientlst1res"    TO  PATLSTRES-RECORDNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       XML-PATIENTLST1RES
                                       SPA-AREA
           IF     (MCP-RC              =   ZERO    OR  1  )
               DISPLAY "XMLWRITE OK      = " MCP-RC
               CONTINUE
               MOVE    PATLSTRES-OBJECT    TO  APILST2-BODY 
               MOVE    "application/xml"   TO  APILST2-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       300-PTXMLMAKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    �����Ѵ��Խ�����
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           EVALUATE    WRK-ERRCD
           WHEN    "01"
               MOVE    "�������ա佪λ���դǤ���"
                                               TO  WRK-ERRMSG
           WHEN    "02"
               MOVE    "�ƥ��ȴ��Զ�ʬ������ޤ���"
                                               TO  WRK-ERRMSG
           WHEN    "10"
               MOVE    "�������Ԥ�����������ʾ�Ȥʤ�ޤ���"
                                               TO  WRK-ERRMSG
           WHEN    "20"
               MOVE    "�������Ԥ�����ޤ���"
                                               TO  WRK-ERRMSG
      *���̥��顼
           WHEN    "89"
      *        ORCSCOMMON �Υ��顼
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "�������󤬼����Ǥ��ޤ���"
                                               TO  WRK-ERRMSG
               WHEN    "1002"
                   MOVE    "���ŵ��ؾ��󤬼����Ǥ��ޤ���"
                                               TO  WRK-ERRMSG
               WHEN    "1003"
                   MOVE    "�����ƥ����դ������Ǥ��ޤ���"
                                               TO  WRK-ERRMSG
               WHEN    "1005"
                   MOVE    "�����ֹ湽�����󤬼����Ǥ��ޤ���"
                                               TO  WRK-ERRMSG
               WHEN    "1015"
                   STRING  "���롼�װ��ŵ��ؤ�������Ǥ���"
                           "������λ���Ʋ�������"
                                               DELIMITED  BY  SIZE
                                               INTO    WRK-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    "�����ƥ���ܤ�����Ǥ��ޤ���"
                                               TO  WRK-ERRMSG
               END-EVALUATE
           WHEN    "90"
               MOVE    "¾ü��������"          TO  WRK-ERRMSG
           WHEN    "91"
               MOVE    "������ʬ̤����"        TO  WRK-ERRMSG
           WHEN    "97"
               MOVE    "�������Ƥ˸��꤬����ޤ���"
                                               TO  WRK-ERRMSG
           WHEN    "98"
               MOVE    "�������Ƥ��ɹ����Ǥ��ޤ���Ǥ���"
                                               TO  WRK-ERRMSG
           WHEN    "99"
               MOVE    "�桼���ɣ�̤��Ͽ��"
                                               TO  WRK-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ�ޥ������ɹ�����
      *****************************************************************
       900-PTNUM-READ-SEC            SECTION.
      *
           PERFORM 910-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTNUM
               MOVE    MCPDATA-REC         TO  PTNUM-REC
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                      PTNUM-REC
           END-IF
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Դ��ܾ������
      *****************************************************************
       900-PTINF-READ-SEC              SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *