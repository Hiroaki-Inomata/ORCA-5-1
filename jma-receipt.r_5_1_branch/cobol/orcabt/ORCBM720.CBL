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
       PROGRAM-ID.             ORCBM720.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �쥻�ץ�
      *  ����ݡ��ͥ��̾  : ���ǥ����������ս��ϫ�ҡ�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  08/11/21    NACL-ƣ��     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      *  Maj/Min/Rev  ������       ����      ����
      *  04.07.01    NACL-ƣ��    15/06/18  ���ǥ����������ս�����μ���
      *                                     �����б�
      *  04.07.02    NACL-ƣ��    15/08/18  ���ŵ����Խ�����ؤγ��߼�̾
      *                                     �ɲ��б�
      *
      *  04.08.01    NACL-ƣ��    14/07/07  ����ǥ��쥯�ȥ��б�
      *  04.08.02    NACL-ƣ��    17/04/12  ʿ������ǯ�������ʬ�����
      *                                     �ͼ��ѹ��б�
      *
      *  05.00.01    NACL-ƣ��    19/01/18  �ƣĵڤӣͣϤˤ������ν�λ
      *                                     ��ȼ���ͼ��ѹ��б�
      *                                     Warning �б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    ���顼�ե�����
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���顼�ե�����
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *    ���顼�ե����� ̾���ΰ� 
           COPY    "CPRECEERR.INC".
      *
      **** COPY    "HCM78.INC".
           COPY    "HCM78V02.INC"  REPLACING  //HCM78V02//
                                   BY         //HCM78//.
      *
      *    �����ΰ�
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(01).
      *
       01  INDEX-AREA.
           03  IDX                 PIC 9(03).
           03  IDY                 PIC 9(03).
           03  IDZ                 PIC 9(03).
      *
      *    �ѥ�᥿�ΰ�
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           05  WRK-PARA-TEISYUTUSAKI
                                   PIC X(01).
           05  WRK-PARA-DATAKBN    PIC X(01).
           05  WRK-PARA-JOBID      PIC 9(07).
           05  WRK-PARA-SHELLID    PIC X(08).
           05  WRK-PARA-HOSPNUM    PIC 9(02).
      *
      *    ����ΰ�
       01  WRK-AREA.
      *    ���顼��å�����
           03  WRK-RECEERR         PIC X(200).
           03  WRK-ERRMSG          PIC X(300).
      *
      *    ���ŵ��ؾ����Խ��ѥ��
           03  WRK-HOSPNAME        PIC X(60).
           03  WRK-ADRS.
               05  WRK-ADRS1       PIC X(40).
               05  WRK-ADRS2       PIC X(40).
               05  WRK-ADRS3       PIC X(20).
      *
           03  WRK-KAISETUNAME     PIC X(80).
      *
           03  WRK-RSIIRYOCD       PIC X(07).
      *
      *    ����Ⱦ�������Ѵ���
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-IDX    PIC 9(02).
               05  WRK-CHAR-FLG    PIC 9(01).
      *
               05  WRK-CHAR-X      PIC X(10).
               05  WRK-CHAR-N      PIC X(20).
               05  WRK-CHAR-CNT    PIC 9(02).
               05  WRK-CHAR-N-CNT  PIC 9(02).
      *
      *    �����Ѵ���
           03  WRK-9                   PIC 9(04).
           03  WRK-9-R                 REDEFINES   WRK-9.
               05  FILLER              PIC X(03).
               05  WRK-9-X             PIC X(01).
      *
      *    �����Խ��ѥ��
       01  WRK-HEN-AREA.
           03  WRK-SYMD            PIC X(08).
           03  WRK-HENYMDG.
               05  WRK-HENYMD-G    PIC X(04).
               05  WRK-HENYMD-Y    PIC X(04).
               05  WRK-HENYMD-FIL1 PIC X(02).
               05  WRK-HENYMD-M    PIC X(04).
               05  WRK-HENYMD-FIL2 PIC X(02).
               05  WRK-HENYMD-D    PIC X(04).
      *
      *    ���ѿ���
       01  TBL-SUJI-AREA.
           03  FILLER                  PIC X(20)   VALUE
               "��������������������".
       01  TBL-SUJI-R                  REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI                PIC X(02)   OCCURS  10.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���ŵ��ؾ���ʴ��ܡ�
           COPY    "CPSK1001.INC".
      *
      *    ���ŵ��ؾ���ʽ����
           COPY    "CPSK1002.INC".
      *
      *    ���ŵ����Խ�����
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
           COPY    "CPSK1903.INC".
      *
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
      *    ϫ�Ұ��ŵ��ؾ���
           COPY    "CPSK4001.INC".
      *
      *    ����ִ����ޥ���
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    ��������
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    �����ѥǡ���
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *    �����ѥ�᥿
           COPY    "CPORCSMKPRT.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ����ִ����ģ����極��
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    �����ģ¹�������
           COPY    "CPORCSPRT.INC".
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *    ��̾�����ѥ�᥿
           COPY    "CPORCSPREF.INC".
      *
      *    Ⱦ�ѥ����å�����
           COPY    "CPORCSKANACHK.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ���̥ѥ�᥿
           COPY    "MCPAREA".
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER                  PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE                   DIVISION
                                   USING
                                   COMMAND-PARAM.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           DISPLAY "*** ORCBM720 START ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
                                       SPA-AREA
      *
           PERFORM 100-DBOPEN-SEC
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-DATAKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    RECEERR         TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"   USING   SGETTEMP-AREA
           MOVE    SPACE           TO  RECEERR
           MOVE    SGETTEMP-FULLNAME
                                   TO  RECEERR
      *
      *    ���ƥå״������Ͻ���
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORCBM720"          TO  JOB-PGID
           MOVE    "���ǥ����������ս��ϫ�ҡ�"
                                       TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    �����ƥ�����ޥ�������      
           PERFORM 1001-SYSKANRI-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      * 
      *****************************************************************
      *    �����ƥ�����ޥ�������      
      *****************************************************************
       1001-SYSKANRI-HENSYU-SEC      SECTION.
      *
      *    ���ŵ��ؾ����Խ�
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1001-STYUKYMD (7:2)
           MOVE    SYS-1001-STYUKYMD   TO  SYS-1001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           ELSE
               MOVE    "���ŵ��ؾ���ʴ��ܡˤ������Ǥ��ޤ���" 
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           INITIALIZE                      SYS-1002-REC
           MOVE    "1002"              TO  SYS-1002-KANRICD
           MOVE    "*"                 TO  SYS-1002-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-1002-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-1002-STYUKYMD (7:2)
           MOVE    SYS-1002-STYUKYMD   TO  SYS-1002-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1002-REC
           ELSE
               MOVE    "���ŵ��ؾ���ʽ���ˤ������Ǥ��ޤ���" 
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
           MOVE    SYS-1001-KAISETUNAME
                                       TO  WRK-KAISETUNAME
           MOVE    SYS-1002-ADRS       TO  WRK-ADRS
      *
      *    ���ŵ����Խ��������
           PERFORM 900-1900-READ-SEC
      *
      *    ϫ�Ұ��ŵ��ؾ������
           INITIALIZE                      SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-4001-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-4001-STYUKYMD (7:2)
           MOVE    SYS-4001-STYUKYMD   TO  SYS-4001-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
           ELSE
               MOVE    "ϫ�һ�����ŵ��ؾ��󤬼����Ǥ��ޤ���"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
      *    ϫ�һ�����ŵ����ֹ������Ѵ�
           INITIALIZE                      ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SYS-4001-RSIIRYOCDN TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
           IF    ( KANACHK-RC          =   ZERO )
           AND   ( KANACHK-SYUBETU     =   "2"  )
               INITIALIZE                      WRK-CHAR-HENKAN-AREA
               MOVE    SYS-4001-RSIIRYOCDN TO  WRK-CHAR-N
               MOVE    13                  TO  WRK-CHAR-N-CNT
               IF      KANACHK-MAX         <   14
                   COMPUTE WRK-CHAR-N-CNT  =   KANACHK-MAX   -   1
               END-IF
               PERFORM 800-NUM-HANKAKU-HENKAN-SEC
               MOVE    WRK-CHAR-X          TO  WRK-RSIIRYOCD
           END-IF
      *
      *    �쥻�ץȡ������Խ�����
           INITIALIZE                      SYS-25-REC
           MOVE    "2005"              TO  SYS-25-KANRICD
           MOVE    "01"                TO  SYS-25-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM  TO  SYS-25-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-25-STYUKYMD (7:2)
           MOVE    SYS-25-STYUKYMD     TO  SYS-25-EDYUKYMD
           MOVE    WRK-PARA-HOSPNUM    TO  SYS-25-HOSPNUM
           MOVE    SYS-25-REC          TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-25-REC
           ELSE
               INITIALIZE                      SYS-25-REC
           END-IF
      *    ���ǥ����������ս�����μ����
           IF      SYS-25-RECEDEN-LST2-MEDIA
                                       =   SPACE
               MOVE    "0"             TO  SYS-25-RECEDEN-LST2-MEDIA
           END-IF
           DISPLAY "SYS-200501-RECEDEN-LST2-MEDIA="
                                       SYS-25-RECEDEN-LST2-MEDIA
           DISPLAY "WRK-PARA-DATAKBN             ="
                                       WRK-PARA-DATAKBN
      *
           .
       1001-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                      HCM78
      *
           ADD     1                   TO  CNT-PAGE
      *
      *    ǯ����
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD-G        TO  HCM78-PRTG
           MOVE    WRK-HENYMD-Y        TO  HCM78-PRTY
           MOVE    WRK-HENYMD-M        TO  HCM78-PRTM
           MOVE    WRK-HENYMD-D        TO  HCM78-PRTD
      *
           INITIALIZE                      SPREF-AREA
           CALL    "ORCSPREF"          USING   SPREF-AREA
                                               SPA-AREA
      *
           MOVE    SYS-1001-PREFNUM    TO  IDX
       *    �������塢���Ԥϡ��ܡ��ԡפ�õ�
           IF      IDX                 =   13  OR  26  OR  27
               MOVE    SPACE               TO  SPREF-PREFNAME2 (IDX)(5:)
           END-IF
           STRING  SPREF-PREFNAME2 (IDX)   DELIMITED    BY  SPACE
                   "ϫƯ�ɡ�����"          DELIMITED    BY  SIZE
                                           INTO    HCM78-TEISYUTUSAKI
           END-STRING
      *
      *    ����
           INSPECT WRK-ADRS            REPLACING   ALL "  "    BY  "��"
           MOVE    WRK-ADRS1           TO  HCM78-ADRS-S1
           MOVE    WRK-ADRS2           TO  HCM78-ADRS-S2
           MOVE    WRK-ADRS3           TO  HCM78-ADRS-S3
      *
      *    ���߼�
           MOVE    WRK-KAISETUNAME     TO  HCM78-KAISETUNAME
      *
      *    ���ŵ��إ�����
           MOVE    WRK-RSIIRYOCD       TO  HCM78-HOSPCD
      *
      *    ���ŵ���̾
           MOVE    WRK-HOSPNAME        TO  HCM78-HOSPNAME
           INSPECT HCM78-HOSPNAME      REPLACING   ALL "  "    BY  "��"
      *
      *    ����ɽ��ʬ
           MOVE    "��"                TO  HCM78-TENHYOKBN
      *
      *    ���š�Ĵ�ޡ˷�ʬ
           MOVE    LNK-PRTKANRI-SRYYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD-G        TO  HCM78-SRYYMG
           MOVE    WRK-HENYMD-Y        TO  HCM78-SRYYMY
           MOVE    WRK-HENYMD-M        TO  HCM78-SRYYMM
      *
      *    ���ǯ����
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMD-G        TO  HCM78-SKYYMG
           MOVE    WRK-HENYMD-Y        TO  HCM78-SKYYMY
           MOVE    WRK-HENYMD-M        TO  HCM78-SKYYMM
           MOVE    WRK-HENYMD-D        TO  HCM78-SKYYMD
      *
      *    ���μ���
           EVALUATE    SYS-25-RECEDEN-LST2-MEDIA
                                       ALSO    WRK-PARA-DATAKBN
      ****     WHEN    "0"             ALSO    "1"
      ****     WHEN    "0"             ALSO    "3"
      ****     WHEN    "1"             ALSO    ANY
      ****         MOVE    "��"                TO  HCM78-BAITAI (2)
      ****     WHEN    "0"             ALSO    "2"
      ****     WHEN    "0"             ALSO    "4"
      ****     WHEN    "2"             ALSO    ANY
      ****         MOVE    "��"                TO  HCM78-BAITAI (3)
               WHEN    "0"             ALSO    "6"
               WHEN    "3"             ALSO    ANY
                   MOVE    "��"                TO  HCM78-BAITAI (1)
           END-EVALUATE
      *
      *    ���
      *????MOVE    WRK-CNT             TO  HCM78-CNT
      *
      *    Ģɼ��������
           PERFORM 2001-PRINT-OUT-SEC
      *    
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ��������
      *****************************************************************
       2001-PRINT-OUT-SEC           SECTION.
      *
           INITIALIZE                      ORCSPRTAREA
           MOVE    "INS"               TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SPRT-TBL-GROUP
           MOVE    LNK-PRTKANRI-SRYYM  TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID 
                                       TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SPRT-PRIORITY
      **** IF      LNK-PRTKANRI-SRYYM  <   "201702"
      ****     MOVE    "HCM78.red"         TO  SPRT-PRTID
      **** ELSE
      ****     MOVE    "HCM78V01.red"      TO  SPRT-PRTID
      **** END-IF
           MOVE    "HCM78V02.red"      TO  SPRT-PRTID
           MOVE    "���ǥ����������ս��ϫ�ҡ�"
                                       TO  SPRT-TITLE
           MOVE    HCM78               TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID   TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM  TO  SPRT-PRTNM
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"               USING
                                           ORCSPRTAREA
                                           SPA-AREA 
           IF          SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "�����ģ¤˹����Ǥ��ޤ���Ǥ���"
                                          TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
       2001-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܸ��Ѵ�����
      *****************************************************************
       5002-HIZUKE-HEN-SEC        SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD2
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG     REPLACING   ALL "  "    BY  "��"
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼���Ͻ���
      *****************************************************************
       500-ERR-HENSYU-SEC                SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CLOSE   RECEERR-FILE
           ELSE
               OPEN    OUTPUT              RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *         
      *        ����ִ������Ͻ���
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���顼����λ����
      *****************************************************************
       500-COBABORT-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
           STRING  "ORCBM720 "         DELIMITED  BY  SIZE
                   WRK-RECEERR         DELIMITED  BY  SIZE
                   LOW-VALUE           DELIMITED  BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                 SECTION.
      *
      *    ���ƥå״�����λ����
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-PAGE        TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORCBM720 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������Ѣ�Ⱦ���Ѵ�����
      *****************************************************************
       800-NUM-HANKAKU-HENKAN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-CHAR-CNT
                                           WRK-CHAR-IDX
      *
           PERFORM VARYING     IDZ     FROM    1   BY  2
                   UNTIL       IDZ     >       WRK-CHAR-N-CNT
               IF      WRK-CHAR-N (IDZ:2)  =   SPACE
                   MOVE    WRK-CHAR-N-CNT      TO  IDZ
               ELSE
                   MOVE    ZERO                TO  WRK-CHAR-FLG
                   PERFORM VARYING IDY     FROM    1   BY  1
                           UNTIL   IDY             >   10
                           OR      WRK-CHAR-FLG    =   1
                       IF       WRK-CHAR-N (IDZ:2) =   TBL-SUJI (IDY)
                           ADD     1           TO  WRK-CHAR-IDX
                           IF      IDY         =   10
                               MOVE    "0"     TO
                                           WRK-CHAR-X (WRK-CHAR-IDX:1)
                           ELSE
                               MOVE    IDY         TO  WRK-9
                               MOVE    WRK-9-X     TO
                                           WRK-CHAR-X (WRK-CHAR-IDX:1)
                           END-IF
                           MOVE    1           TO  WRK-CHAR-FLG
                       END-IF
                   END-PERFORM
      *            �����ʳ��ΤȤ��Ѵ���λ
                   IF      WRK-CHAR-FLG        =   ZERO
                       MOVE    WRK-CHAR-N-CNT      TO  IDZ
                       MOVE    SPACE               TO  WRK-CHAR-X
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       800-NUM-HANKAKU-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɹ�
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ŵ����Խ������ɤ߹���
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    ���ŵ����Խ������ɤ߹���
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1900-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1900-STYUKYMD (7:2)
           MOVE    SYS-1900-STYUKYMD
                                   TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(21)  NOT =   SPACE
      *            ���ŵ���̾���Խ�����
                   PERFORM 900-1901-READ-SEC
      *            ���ŵ��ؽ����Խ�����
                   PERFORM 900-1902-READ-SEC
      *            ���ŵ���̾���Խ�����ʣ���
                   PERFORM 900-1903-READ-SEC
                END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
           .
      *
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ŵ���̾���Խ������ɤ߹���
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN (21)
                                   TO  SYS-1901-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1901-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1901-STYUKYMD (7:2)
           MOVE    SYS-1901-STYUKYMD
                                   TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1
                                       TO  WRK-HOSPNAME
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
           .
      *
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ŵ��ؽ����Խ������ɤ߹���
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(21)
                                   TO  SYS-1902-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1902-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1902-STYUKYMD (7:2)
           MOVE    SYS-1902-STYUKYMD
                                   TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-ADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-ADRS2
               MOVE    SYS-1902-ADRS3  TO  WRK-ADRS3
           ELSE
               INITIALIZE                      SYS-1902-REC
           END-IF
           .
      *
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ŵ���̾���Խ�����ʣ����ɤ߹���
      *****************************************************************
       900-1903-READ-SEC           SECTION.
      *
           MOVE    SPACE           TO  SYS-1903-REC
           INITIALIZE                  SYS-1903-REC
           MOVE    SPA-HOSPNUM     TO  SYS-1903-HOSPNUM
           MOVE    "1903"          TO  SYS-1903-KANRICD
           MOVE    SYS-1900-PRTKBN(21)
                                   TO  SYS-1903-KBNCD
           MOVE    LNK-PRTKANRI-SRYYM
                                   TO  SYS-1903-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-1903-STYUKYMD (7:2)
           MOVE    SYS-1903-STYUKYMD
                                   TO  SYS-1903-EDYUKYMD
           MOVE    SYS-1903-REC    TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1903-REC
               MOVE    SYS-1903-KAISETUNAME
                                           TO  WRK-KAISETUNAME
           ELSE
               INITIALIZE                      SYS-1903-REC
           END-IF
           .
      *
       900-1903-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ����ִ����ģ��������
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
           MOVE    WRK-PARA-JOBID  TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ӣţ̣ţãԽ���
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ£ƣţԣãȽ���
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륯����������
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ơ��֥륢����������
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *      
      *****************************************************************
      *    �ģ¥����ץ����
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ¥�����������
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBDISCONNECT-EXT.
           EXIT.