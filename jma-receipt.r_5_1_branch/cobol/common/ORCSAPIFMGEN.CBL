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
       PROGRAM-ID.             ORCSAPIFMGEN.
      *****************************************************************
      *  �����ƥ�̾        : ORCA
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : API��Ģɼ�ǡ����쥹�ݥ󥹾������
      *  ��������    ��ȼ�        ����
      *  19/09/20    NMED-����     ��������
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
      *    FORM�ǡ���
           SELECT  FORM-FILE       ASSIGN  FORMPARA
                                   ORGANIZATION    IS  LINE SEQUENTIAL 
                                   FILE    STATUS  IS  STS-FORM.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
       FD  FORM-FILE.
       01  FORM-REC                    PIC X(20000).
      *
       WORKING-STORAGE                 SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGN//
                                       BY          //FORM//.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGN//
                                       BY          //UTF//.
      *
       01  STS-AREA.
           03  STS-FORM                PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PRTDATA             PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXR                    PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-YMD                 PIC X(10). 
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-TEXT                PIC X(1024).
           03  WRK-ELEMENT             PIC X(128).
           03  WRK-JSON                PIC X(20000).
           03  WRK-JSON-NEW            PIC X(20000).
           03  WRK-LENGTH              PIC 9(08).
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *   �գɣļ����ѥ��ꥢ
       01 UIDIO-AREA.
           03  UIDIO-RET   PIC X(2).
           03  UIDIO-UID   PIC X(36).
       01  UIDIO-ST        PIC 9(2).
      *
      *    �ޥ������ռ�������
           COPY    "CPORCSMCNDATE.INC".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    �����ֹ�
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    �����ǡ���
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    Ϣ���ΰ�
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  LNKPRTKANRI-AREA.
           COPY    "CPPRTKANRI.INC"    REPLACING    //PRTKANRI-//
                                       BY           //LNKPRTKANRI-//.
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           LNKPRTKANRI-AREA
           SPA-AREA.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-MAIN-SEC                    SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPACE           TO  SPA-WORK
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
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *
           INITIALIZE                  UIDIO-AREA.
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           MOVE    SPACE           TO  WRK-TEXT
           STRING  SMCNDATE-YMD        DELIMITED BY SPACE
                   SMCNDATE-TIME       DELIMITED BY SPACE
                   UIDIO-UID           DELIMITED BY SPACE
                   ".dat"              DELIMITED BY SIZE
           INTO    WRK-TEXT
           END-STRING
      *
           MOVE    SPACE           TO  FORMPARA
           STRING  "RPF"           DELIMITED BY SIZE
                   WRK-TEXT        DELIMITED BY SPACE
           INTO    FORMPARA
           END-STRING
      *
           MOVE    SPACE           TO  UTFPARA
           STRING  "RPU"           DELIMITED BY SIZE
                   WRK-TEXT        DELIMITED BY SPACE
           INTO    UTFPARA
           END-STRING
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    FORMPARA        TO  SGETTEMP-BASENAMES (1)
           MOVE    UTFPARA         TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                   TO  FORMPARA
           MOVE    SGETTEMP-FULLNAMES (2)
                                   TO  UTFPARA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �硡����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           OPEN    OUTPUT              FORM-FILE
      *
           MOVE    "{"             TO  FORM-REC
           WRITE   FORM-REC
      *
           PERFORM 2001-HEADER-SEC
           MOVE    WRK-JSON        TO  FORM-REC
           WRITE   FORM-REC
      *
           IF    ( SPA-ERRCD               =   SPACE )
            AND  ( LNKPRTKANRI-TBL-KEY NOT =   SPACE )
               PERFORM 2001-BODY-SEC
           END-IF
      *
           MOVE    "}"             TO  FORM-REC
           WRITE   FORM-REC
      *
           CLOSE   FORM-FILE
      *
           CALL    "SYSTEM"    USING
                               FUNCTION CONCATENATE
                               ("iconv -f EUC-JISX0213 "
                               "-t UTF-8 "
                               FORMPARA
                               " > "
                               UTFPARA
                               )
      *
           MOVE    UTFPARA         TO  SPA-WORK
      *
           CALL    "CBL_DELETE_FILE"   USING
                                       FORMPARA
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �إå����Խ�����
      *****************************************************************
       2001-HEADER-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-JSON
      *
           INITIALIZE                  PTINF-REC
           INITIALIZE                  PTNUM-REC
      *
           PERFORM 890-ERRCD-MSG-SEC
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 900-PTINF-KEY-SEL-SEC
               PERFORM 900-PTNUM-KEY-SEL-SEC
           END-IF
      *
           MOVE    "Information_Date"  TO  WRK-ELEMENT
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  WRK-TEXT
           PERFORM 910-JSON-WRITE-SEC
      *
           MOVE    "Information_Time"  TO  WRK-ELEMENT
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  WRK-TEXT
           PERFORM 910-JSON-WRITE-SEC
      *
           MOVE    "Api_Result"        TO  WRK-ELEMENT
           IF    ( SPA-ERRCD           =   SPACE )
               MOVE    "0000"          TO  WRK-TEXT
           ELSE
               MOVE    SPA-ERRCD       TO  WRK-TEXT
           END-IF
           PERFORM 910-JSON-WRITE-SEC
      *
           MOVE    "Api_Result_Message"
                                       TO  WRK-ELEMENT
           MOVE    SPA-ERRMSG          TO  WRK-TEXT
           PERFORM 910-JSON-WRITE-SEC
      *
           IF    ( LNKPRTKANRI-API-ID
                                   NOT =   SPACE )
               MOVE    "Form_ID"
                                       TO  WRK-ELEMENT
               MOVE    LNKPRTKANRI-API-ID
                                       TO  WRK-TEXT
               PERFORM 910-JSON-WRITE-SEC
           END-IF
      *
           IF    ( LNKPRTKANRI-TITLE
                                   NOT =   SPACE )
               MOVE    "Form_Name"
                                       TO  WRK-ELEMENT
               MOVE    LNKPRTKANRI-TITLE
                                       TO  WRK-TEXT
               PERFORM 910-JSON-WRITE-SEC
           END-IF
      *
           IF    (LNKPRTKANRI-TBL-GROUP (1:8)
                                   NOT =   SPACE )
               MOVE    "Print_Date"
                                       TO  WRK-ELEMENT
               MOVE    LNKPRTKANRI-TBL-GROUP (1:8)
                                       TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE    TO  WRK-TEXT
               PERFORM 910-JSON-WRITE-SEC
           END-IF
      *
           IF    (LNKPRTKANRI-TBL-GROUP (9:6)
                                   NOT =   SPACE )
               MOVE    "Print_Time"
                                       TO  WRK-ELEMENT
               MOVE    LNKPRTKANRI-TBL-GROUP (9:6)
                                       TO  WRK-HMS
               PERFORM 800-TIME-SEC
               MOVE    WRK-TIME        TO  WRK-TEXT
               PERFORM 910-JSON-WRITE-SEC
           END-IF
      *
           IF    ( PTINF-PTID      NOT =   ZERO )
      *
               PERFORM 9001-JSON-WRITE-SEC
      *
               MOVE FUNCTION CONCATENATE (
                       FUNCTION TRIM (WRK-JSON TRAILING)
                       '"Patient":{'
                       )               TO  WRK-JSON-NEW
               MOVE    WRK-JSON-NEW    TO  WRK-JSON
      *
               IF    ( PTNUM-PTNUM
                                   NOT =   SPACE )
                   MOVE    "ID"
                                       TO  WRK-ELEMENT
                   MOVE    PTNUM-PTNUM
                                       TO  WRK-TEXT
                   PERFORM 910-JSON-WRITE-SEC
               END-IF
      *
               IF    ( PTINF-NAME
                                   NOT =   SPACE )
                   MOVE    "Name"
                                       TO  WRK-ELEMENT
                   MOVE    PTINF-NAME
                                       TO  WRK-TEXT
                   PERFORM 910-JSON-WRITE-SEC
               END-IF
      *
               IF    ( PTINF-KANANAME
                                   NOT =   SPACE )
                   MOVE    "KanaName"
                                       TO  WRK-ELEMENT
                   MOVE    PTINF-KANANAME
                                       TO  WRK-TEXT
                   PERFORM 910-JSON-WRITE-SEC
               END-IF
      *
               IF    ( PTINF-BIRTHDAY
                                   NOT =   SPACE )
                   MOVE    "BirthDate"
                                       TO  WRK-ELEMENT
                   MOVE    PTINF-BIRTHDAY
                                       TO  WRK-SYMD
                   PERFORM 800-DATE-SEC
                   MOVE    WRK-DATE    TO  WRK-TEXT
                   PERFORM 910-JSON-WRITE-SEC
               END-IF
      *
               IF    ( PTINF-SEX
                                   NOT =   SPACE )
                   MOVE    "Sex"
                                       TO  WRK-ELEMENT
                   MOVE    PTINF-SEX
                                       TO  WRK-TEXT
                   PERFORM 910-JSON-WRITE-SEC
               END-IF
      *
               PERFORM 9003-JSON-WRITE-SEC
      *
           END-IF
      *
           IF    ( LNKPRTKANRI-TBL-UUID
                                   NOT =   SPACE )
            AND  ( SPA-PRTCONF         =   "2" OR "3" OR "A" )
      *
               PERFORM 9001-JSON-WRITE-SEC
      *
               MOVE FUNCTION CONCATENATE (
                       FUNCTION TRIM (WRK-JSON TRAILING)
                       '"Data_Id_Information":{'
                       )               TO  WRK-JSON-NEW
               MOVE    WRK-JSON-NEW    TO  WRK-JSON
      *
               MOVE    "Data_Id"
                                       TO  WRK-ELEMENT
               MOVE    LNKPRTKANRI-TBL-UUID
                                       TO  WRK-TEXT
               PERFORM 910-JSON-WRITE-SEC
      *
               PERFORM 9003-JSON-WRITE-SEC
      *
           END-IF
      *
           .
       2001-HEADER-EXT.
           EXIT.
      *****************************************************************
      *    Ģɼ�ǡ����Խ�����
      *****************************************************************
       2001-BODY-SEC                 SECTION.
      *
           MOVE    ZERO                TO  IDXR
      *
           PERFORM 900-PRTDATA-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PRTDATA NOT =   ZERO )
      *
               COMPUTE IDXR    =   IDXR    +   1
      *
               IF    ( IDXR    =   1 )
      *
                   MOVE    ',"Forms":['
                                       TO  FORM-REC
                   WRITE   FORM-REC
               END-IF
      *
               MOVE    PRTDATA-PRTDATA TO  FORM-REC
               WRITE   FORM-REC
      *
               PERFORM 900-PRTDATA-KEY3-FET-SEC
      *
           END-PERFORM
      *
           IF    ( IDXR        >   ZERO )
               MOVE    "]"             TO  FORM-REC
               WRITE   FORM-REC
           END-IF
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-BODY-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
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
               IF    ( WRK-SYMD            =   "99999999" )
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-IF
           END-IF
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼��å������Խ�����
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
           EVALUATE    SPA-ERRCD
           WHEN    SPACE
      *
               MOVE    "������λ"      TO  SPA-ERRMSG
      *
           WHEN    "0001"
               MOVE    "Ģɼ�ǡ�����¸�ߤ��ޤ���Ǥ�����"
                                       TO  SPA-ERRMSG
           WHEN    "0101"
               MOVE    "�ꥯ�������ֹ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0102"
               MOVE    "�����ֹ椬̤���ϤǤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0103"
               MOVE    "�����ֹ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0104"
               MOVE    "��ɼ�ֹ椬̤���ϤǤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0105"
               MOVE    "��ɼ�ֹ�˳��������������¸�ߤ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0106"
               MOVE    "���⡦������ʬ�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0107"
               MOVE    "�����ؼ������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0108"
               MOVE    "���Ųʤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0109"
               MOVE    "�ݸ��ȹ礻�����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0110"
               MOVE    "�����������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0111"
               MOVE    "�ౡ�������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0112"
               MOVE    "���������ౡ���Ǥ���"
                                       TO  SPA-ERRMSG
           WHEN    "0113"
               MOVE    "ô��������Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0114"
               MOVE    "����ǯ�̤���ϤǤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0115"
               MOVE    "����ǯ������Ϥ˸��꤬����ޤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0116"
               MOVE    "���������������Ϥ˸��꤬����ޤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0117"
               MOVE    "�ǽ������Ǥ����Ϥ˸��꤬����ޤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0118"
               MOVE    "�ǽ������Ԥ����Ϥ˸��꤬����ޤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0119"
               MOVE    "�ǽ������Ԥϣ�����̤���ˤ��Ʋ�������" 
                                       TO  SPA-ERRMSG
           WHEN    "0120"
               STRING  "�ݸ��ȹ礻�Ǥΰ����Ȥʤ�ޤ���"
                                               DELIMITED  BY  SIZE
                       "�ݸ��ȹ礻���ǧ���Ʋ�������"
                                               DELIMITED  BY  SIZE
               INTO  SPA-ERRMSG
               END-STRING
           WHEN    "0121"
               MOVE    "����ƣ����ΰ����оݤ�����ޤ���Ǥ�����" 
                                       TO  SPA-ERRMSG
           WHEN    "0122"
               MOVE    "������ʬ�����Ϥ˸��꤬����ޤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0123"
               MOVE    "�������¸�ߤ��ޤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0124"
               MOVE    "������ʬ��̤���ϤǤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0125"
               MOVE    "���پ���¸�ߤ��ޤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0126"
               MOVE    "��ɼ�ֹ�����Ϥ˸��꤬����ޤ���" 
                                       TO  SPA-ERRMSG
           WHEN    "0127"
               MOVE    "�����⡼�ɤ����Ϥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0128"
               MOVE    SPACE           TO  SPA-ERRMSG
               STRING  "�����⡼�ɤ�ͭ���ˤ����硢"
                                           DELIMITED BY SIZE
                       "�ߥɥ륦�����򹹿����Ƥ���������"
                                           DELIMITED BY SIZE
               INTO    SPA-ERRMSG
               END-STRING
      *���̥��顼
           WHEN    "0097"
               MOVE   "�������Ƥ˸��꤬����ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "0098"
               MOVE   "�������Ƥ��ɹ����Ǥ��ޤ���Ǥ�����"
                                       TO  SPA-ERRMSG
           WHEN    "0099"
               MOVE    "�桼���ɣĤ�̤��Ͽ�Ǥ���"
                                       TO  SPA-ERRMSG
           WHEN    "1001"
      *
               MOVE    "0089"          TO  SPA-ERRCD
      *
               MOVE    "�������󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "1002"
      *
               MOVE    "0089"          TO  SPA-ERRCD
      *
               MOVE    "���ŵ��ؾ��󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "1003"
      *
               MOVE    "0089"          TO  SPA-ERRCD
      *
               MOVE    "�����ƥ����դ������Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "1005"
      *
               MOVE    "0089"          TO  SPA-ERRCD
      *
               MOVE    "�����ֹ湽�����󤬼����Ǥ��ޤ���"
                                       TO  SPA-ERRMSG
           WHEN    "1015"
      *
               MOVE    "0089"          TO  SPA-ERRCD
      *
               STRING  "���롼�װ��ŵ��ؤ�������Ǥ���"
                                           DELIMITED  BY  SIZE
                       "������λ���Ʋ�������"
                                           DELIMITED  BY  SIZE
               INTO    SPA-ERRMSG
               END-STRING
           WHEN    OTHER
               MOVE    "�ֵѾ�����Խ��ǥ��顼��ȯ�����ޤ�����"
                                       TO  SPA-ERRMSG
           END-EVALUATE
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    JSON�����񤭽Ф�
      *****************************************************************
       910-JSON-WRITE-SEC             SECTION.
      *
           PERFORM 9001-JSON-WRITE-SEC
           PERFORM 9002-JSON-WRITE-SEC
      *
           .
       910-JSON-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    JSON�����񤭽Ф�
      *****************************************************************
       9001-JSON-WRITE-SEC             SECTION.
      *
           IF    ( WRK-JSON        =   SPACE )
               CONTINUE
           ELSE
               MOVE    FUNCTION LENGTH(FUNCTION TRIM(WRK-JSON TRAILING))
                                       TO  WRK-LENGTH
               IF    ( WRK-JSON (WRK-LENGTH : 1)   =   "{" OR "[" )
                   MOVE    WRK-JSON    TO  WRK-JSON-NEW
               ELSE
                   MOVE FUNCTION CONCATENATE (
                       FUNCTION TRIM (WRK-JSON TRAILING)
                       ',' )           TO  WRK-JSON-NEW
               END-IF
      *
               MOVE    WRK-JSON-NEW    TO  WRK-JSON
           END-IF
      *
           .
       9001-JSON-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    JSON�����񤭽Ф�
      *****************************************************************
       9002-JSON-WRITE-SEC             SECTION.
      *
           IF    ( WRK-TEXT            =   SPACE )
               CONTINUE
           ELSE
               MOVE FUNCTION CONCATENATE (
                       FUNCTION TRIM (WRK-JSON TRAILING)
                       ' "'
                       FUNCTION TRIM (WRK-ELEMENT TRAILING)
                       '": "'
                       FUNCTION TRIM (WRK-TEXT  TRAILING)
                       '"'
                       )               TO  WRK-JSON-NEW
               MOVE    WRK-JSON-NEW    TO  WRK-JSON
           END-IF
      *
      *
           .
       9002-JSON-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    JSON�����񤭽Ф�
      *****************************************************************
       9003-JSON-WRITE-SEC             SECTION.
      *
           MOVE FUNCTION CONCATENATE (
                       FUNCTION TRIM (WRK-JSON TRAILING)
                       '}' )           TO  WRK-JSON-NEW
      *
           MOVE    WRK-JSON-NEW        TO  WRK-JSON
      *
           .
       9003-JSON-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    ���Ծ��󸡺�����
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    SPA-PTID        TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
               MOVE    ZERO        TO  FLG-PTINF
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-EXT.
           EXIT.
      *****************************************************************
      *    �����ֹ渡������
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
               MOVE    ZERO        TO  FLG-PTNUM
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-EXT.
           EXIT.
      *****************************************************************
      *    Ģɼ�ǡ�����������
      *****************************************************************
       900-PRTDATA-KEY3-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-PRTDATA
      *
           INITIALIZE                      PRTDATA-REC
           MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
           MOVE    LNKPRTKANRI-TBL-KEY TO  PRTDATA-TBL-KEY
           MOVE    LNKPRTKANRI-RENNUM  TO  PRTDATA-RENNUM
           MOVE    LNKPRTKANRI-TBL-GROUP
                                       TO  PRTDATA-TBL-GROUP
           MOVE    LNKPRTKANRI-TBL-UUID
                                       TO  PRTDATA-TBL-UUID
           MOVE    LNKPRTKANRI-SHORI-RENNUM
                                       TO  PRTDATA-SHORI-RENNUM
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PRTDATA-REC
           ELSE
               INITIALIZE                  PRTDATA-REC
               MOVE    1               TO  FLG-PRTDATA
           END-IF
      *
           .
       900-PRTDATA-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    Ģɼ�ǡ���FETCH����
      *****************************************************************
       900-PRTDATA-KEY3-FET-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-PRTDATA
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PRTDATA-REC
           ELSE
               INITIALIZE                  PRTDATA-REC
               MOVE    1               TO  FLG-PRTDATA
           END-IF
      *
           .
       900-PRTDATA-KEY3-FET-EXT.
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