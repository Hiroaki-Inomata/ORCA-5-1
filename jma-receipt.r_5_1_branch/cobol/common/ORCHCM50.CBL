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
       PROGRAM-ID.             ORCHCM50.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ������Ͽ
      *  ����ݡ��ͥ��̾  : �����������ٽ�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  06/12/18    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 03.05.00     NACL-����    07.05.29 ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    ���Ź԰٥ե�����
           SELECT  HCM50SRY-FILE   ASSIGN   HCM50F01
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  HCM50SRY-ISM-KEY
                                   FILE    STATUS  IS  STS-HCM50F01.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ���Ź԰٥ե�����
       FD  HCM50SRY-FILE.
           COPY "CPORCHCM50SRY.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *    Ģɼ
           COPY    "HCM50.INC".
      *
      *    �����оݴ��ԥե�����
           COPY    "CPCOMMONDAT3.INC"
                                           REPLACING   //TMPFLPARA//
                                           BY          //HCM50F01//.
      *
      *    �ե饰�ΰ�
       01  STS-AREA.
           03  STS-HCM50F01        PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-HCM50SRY        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-END             PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX-LINE            PIC 9(05).
      *
      *    ��������ΰ�
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(03).
           03  CNT-OUT             PIC 9(03).
           03  CNT-MEISAI          PIC 9(03).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-SYMD            PIC X(08).
           03  WRK-EDTYMD-G.
               05  WRK-EDTYMD1     PIC X(10).
               05  WRK-EDTYMD3     PIC X(22).
           03  WRK-KANRICD         PIC X(04).
           03  WRK-KBNCD           PIC X(08).
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-ZZZZ9           PIC ZZ,ZZ9.
           03  WRK-ZZZZZZZZ9       PIC ZZZ,ZZZ,ZZ9.
           03  WRK-KINGAKU         PIC S9(11).
           03  WRK-LINE            PIC 9(05).
           03  WRK-HOSPINF-G.
               05  WRK-HOSP-1902   PIC X(01).
               05  WRK-HOSP-NAME   PIC X(80)   OCCURS 2.
               05  WRK-HOSP-ADRS   PIC X(100)  OCCURS 2.
               05  WRK-HOSP-TEL    PIC X(15).
      *
       01  KEY-AREA.
           03  KEY-NYUGAIKBN       PIC X(01).
           03  KEY-SKYSTYMD        PIC X(08).
           03  KEY-SKYEDYMD        PIC X(08).
      *
      *    �ץ�����४�ץ����
       01  PO-AREA.
           03  PO-IN               PIC X(25).
           03  PO-OT               PIC X(100).
           03  PO-SHADE            PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-LINE-MAX      PIC 9(05)   VALUE   69.
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *
      *    ���ŵ��ؾ���ݴ��ܾ��� 
           COPY    "CPSK1001.INC".
      *
      *    ���ŵ��ؾ���ݽ���ϡ�Ϣ����
           COPY    "CPSK1002.INC".
      *
      *    ������ץ��̾������ƾ���
           COPY    "CPSK1031.INC".
      *
      *    ���ŵ����Խ�����
           COPY    "CPSK1900.INC".
      *
      *    ���ŵ���̾���Խ�����
           COPY    "CPSK1901.INC".
      *
      *    ���ŵ��ؽ����Խ�����
           COPY    "CPSK1902.INC".
      *
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    �����ֹ�
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �ե��������ѥ�᥿
           COPY    "CPORCSFDEL.INC".
      *
      *    �ץ�����४�ץ�����Խ��ѥ�᥿
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ���������ΰ�
           COPY    "CPSHELLTBL.INC".
      *
           COPY    "CPORCHCM50S01.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "COMMON-SPA".
           COPY    "CPORCSPRT.INC".
           COPY    "CPORCHCM31.INC".
           COPY    "CPORCHCM50.INC".
      *
       PROCEDURE                   DIVISION    USING
                                               SPA-AREA
                                               ORCSPRTAREA
                                               ORCHCM31AREA
                                               ORCHCM50AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  ORCHCM50-OT-AREA
           INITIALIZE                  KEY-AREA
           INITIALIZE                  PO-AREA
      *
      *    ���ŵ��ؾ���ݴ��ܾ����ɤ߹���
           MOVE    "1001"          TO  WRK-KANRICD
           MOVE    "*"             TO  WRK-KBNCD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1001-REC
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSP-NAME (1)
      *
      *    ���ŵ��ؾ���ݽ���ϡ�Ϣ�����ɤ߹���
           MOVE    "1002"          TO  WRK-KANRICD
           MOVE    "*"             TO  WRK-KBNCD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1002-REC
           MOVE    SYS-1002-ADRS       TO  WRK-HOSP-ADRS (1)
           MOVE    SYS-1002-TEL        TO  WRK-HOSP-TEL
      *
      *    ���ŵ����Խ������ɤ߹���
           MOVE    "1900"          TO  WRK-KANRICD
           MOVE    "*"             TO  WRK-KBNCD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC    TO  SYS-1900-REC
           IF    ( SYS-1900-PRTKBN (24)    NOT =   SPACE )
      *        ���ŵ���̾���Խ������ɤ߹���
               MOVE    "1901"      TO  WRK-KANRICD
               MOVE    SYS-1900-PRTKBN (25)
                                   TO  WRK-KBNCD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYSKANRI-REC        TO  SYS-1901-REC
                   MOVE    SYS-1901-HOSPNAME1  TO  WRK-HOSP-NAME (1)
                   MOVE    SYS-1901-HOSPNAME2  TO  WRK-HOSP-NAME (2)
               END-IF
      *        ���ŵ��ؽ����Խ������ɤ߹���
               MOVE    "1902"      TO  WRK-KANRICD
               MOVE    SYS-1900-PRTKBN (25)
                                   TO  WRK-KBNCD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    SYSKANRI-REC    TO  SYS-1902-REC
                   MOVE    "1"             TO  WRK-HOSP-1902
                   MOVE    SYS-1902-ADRS1  TO  WRK-HOSP-ADRS (1)
                   MOVE    SYS-1902-ADRS2  TO  WRK-HOSP-ADRS (2)
                   MOVE    SYS-1902-TEL    TO  WRK-HOSP-TEL
               END-IF
           END-IF
      *
      *    ���Ծ����ɤ߹���
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
      *    �����ֹ��ɤ߹���
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           INITIALIZE                      HCM50F01-BASENAME
           MOVE    SPA-HOSPNUM         TO  HCM50F01-HOSPNUM
           MOVE    "HCM50F01"          TO  HCM50F01-FILE-ID
           MOVE    1                   TO  HCM50F01-SYOKBN1
           MOVE    SPA-TERMID          TO  HCM50F01-TERMID
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HCM50F01-BASENAME   TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  HCM50F01-FULLNAME
      *
           OPEN    OUTPUT  HCM50SRY-FILE
           CLOSE   HCM50SRY-FILE
      *
           PERFORM 1001-PROGRAM-OPTION-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ץ�����४�ץ�����Խ�����
      *****************************************************************
       1001-PROGRAM-OPTION-SEC     SECTION.
      *
           MOVE    "1"                 TO  PO-SHADE
      *
           MOVE    "SHADE"             TO  PO-IN
           PERFORM 10011-ORCSPRGOPTION-SEC
           IF    ( PO-OT  =   "0" OR "1" )
               MOVE    PO-OT           TO  PO-SHADE
           END-IF
      *
           .
       1001-PROGRAM-OPTION-EXT.
           EXIT.
      *****************************************************************
      *    �ץ�����४�ץ�����Խ�����
      *****************************************************************
       10011-ORCSPRGOPTION-SEC     SECTION.
      *
           MOVE    SPACE              TO  PO-OT
      *
            INITIALIZE                     ORCSPRGOPTIONAREA
            MOVE    "ORCHCM50"         TO  ORCSPRGOPTION-PRGID
      *
            MOVE    PO-IN              TO  ORCSPRGOPTION-IN-DATA
            CALL    "ORCSPRGOPTION"    USING
                                       SPA-AREA
                                       ORCSPRGOPTIONAREA
            MOVE    ORCSPRGOPTION-OT-PARA
                                       TO  PO-OT
      *
           .
       10011-ORCSPRGOPTION-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM 2001-ORCHCM50S01-CALL-SEC
      *
           OPEN    INPUT   HCM50SRY-FILE
      *
           PERFORM 900-HCM50SRY-SEQ-READ-SEC
      *
           PERFORM UNTIL ( FLG-HCM50SRY    NOT =   ZERO )
      *
               MOVE    HCM50SRY-NYUGAIKBN  TO  KEY-NYUGAIKBN
               MOVE    ZERO                TO  CNT-PAGE
               PERFORM UNTIL ( FLG-HCM50SRY    NOT =   ZERO )
                        OR   ( HCM50SRY-NYUGAIKBN
                                               NOT =   KEY-NYUGAIKBN )
                   PERFORM 2001-HEAD-SEC
      *
                   PERFORM UNTIL ( FLG-HCM50SRY
                                               NOT =   ZERO )
                            OR   ( HCM50SRY-NYUGAIKBN
                                               NOT =   KEY-NYUGAIKBN )
                            OR   ( CNT-LINE       >=   CONST-LINE-MAX )
                       PERFORM 2001-BODY-SEC
                   END-PERFORM
      *
                   PERFORM 2001-LASTMES-SEC
      *
                   PERFORM 2001-LAYLER-OPTION-SEC
      *
                   PERFORM 2001-PRINT-SEC
               END-PERFORM
      *
      *
           END-PERFORM
      *
           CLOSE   HCM50SRY-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������Խ����ָƽн���
      *****************************************************************
       2001-ORCHCM50S01-CALL-SEC   SECTION.
      *
           INITIALIZE                  HCM50S01-AREA
           MOVE    SPA-HOSPNUM         TO  HCM50S01-HOSPNUM
           MOVE    ORCHCM50-NYUGAI     TO  HCM50S01-NYUGAIKBN
           MOVE    SPA-PTID            TO  HCM50S01-PTID
           MOVE    ORCHCM50-SUMKBN     TO  HCM50S01-SUMKBN
           MOVE    SPA-SYSYMD          TO  HCM50S01-SRYYMD
           MOVE    ORCHCM50-STRSRYYMD  TO  HCM50S01-STYMD
           MOVE    ORCHCM50-ENDSRYYMD  TO  HCM50S01-EDYMD
           MOVE    HCM50F01            TO  HCM50S01-HCM50F01
           CALL    "ORCHCM50S01"       USING   ORCHCM31AREA
                                               HCM50S01-AREA
                                               SPA-AREA
      *
           .
       2001-ORCHCM50S01-CALL-EXT.
           EXIT.
      *****************************************************************
      *    �إå��ʥեå����Խ�����
      *****************************************************************
       2001-HEAD-SEC                   SECTION.
      *
           MOVE    ZERO                TO  CNT-LINE
           MOVE    ZERO                TO  CNT-MEISAI
      *
           INITIALIZE                      HCM50
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
      *
           IF    ( HCM50SRY-NYUGAIKBN  =   "1" )
               MOVE    "��������"      TO  HCM50-NYUGAI
           ELSE
               MOVE    "�ʳ����"      TO  HCM50-NYUGAI
           END-IF
      *
           MOVE    ORCHCM50-STRSRYYMD  TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3        TO  HCM50-STRSRYYMD
      *
           MOVE    ORCHCM50-ENDSRYYMD  TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3        TO  HCM50-ENDSRYYMD
      *
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-EDTYMD3        TO  HCM50-HAKYMD
      *
           MOVE    CNT-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCM50-PAGE
      *
           MOVE    PTINF-NAME          TO  HCM50-NAME
           MOVE    PTNUM-PTNUM         TO  HCM50-PTNUM
      *
           MOVE    WRK-HOSP-ADRS (1)   TO  HCM50-IRYOADRS (1)
           MOVE    WRK-HOSP-ADRS (2)   TO  HCM50-IRYOADRS (2)
           MOVE    WRK-HOSP-NAME (1)   TO  HCM50-IRYOMEI (1)
           MOVE    WRK-HOSP-NAME (2)   TO  HCM50-IRYOMEI (2)
           MOVE    WRK-HOSP-TEL        TO  HCM50-IRYOTEL
      *
           MOVE    HCM50SRY-SKYSTYMD   TO  KEY-SKYSTYMD
           MOVE    HCM50SRY-SKYEDYMD   TO  KEY-SKYEDYMD
      *
           IF    ( ORCHCM50-SUMKBN =   "1" )
               MOVE    "���ŷ�"        TO  HCM50-LBLSRYYMD
           ELSE
               MOVE    "������"        TO  HCM50-LBLSRYYMD
           END-IF
      *
           .
       2001-HEAD-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       2001-BODY-SEC                   SECTION.
      *
           IF   (  CNT-LINE           >=   1 )
            AND((  KEY-SKYSTYMD    NOT =   HCM50SRY-SKYSTYMD )
              OR(  KEY-SKYEDYMD    NOT =   HCM50SRY-SKYEDYMD ))
               MOVE    HCM50SRY-SKYSTYMD
                                       TO  KEY-SKYSTYMD
               MOVE    HCM50SRY-SKYEDYMD
                                       TO  KEY-SKYEDYMD
               MOVE    ZERO            TO  CNT-MEISAI
               COMPUTE     CNT-LINE    =   CNT-LINE    +   1
           ELSE
               PERFORM 20011-BODY-SEC
           END-IF
      *
           .
       2001-BODY-EXT.
           EXIT.
      *****************************************************************
      *    �����Խ�����
      *****************************************************************
       20011-BODY-SEC              SECTION.
      *
           COMPUTE CNT-LINE        =   CNT-LINE    +   1
           COMPUTE CNT-MEISAI      =   CNT-MEISAI  +   1
      *
           IF    ( CNT-MEISAI      =   1 )
               IF    ( ORCHCM50-SUMKBN =   "1" )
                   MOVE    HCM50SRY-SKYSTYMD   TO  WRK-SYMD
                   MOVE    "01"                TO  WRK-SYMD (7:2)
                   PERFORM 800-SEIWA-HEN-SEC
                   MOVE    WRK-EDTYMD3 (11:6) TO  HCM50-SRYYMD
                                                           (CNT-LINE)
               ELSE
                   IF    ( HCM50SRY-NYUGAIKBN  =   "1" )
                       MOVE    HCM50SRY-SKYSTYMD  TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD1 (5:2)  TO  HCM50-SRYYMD
                                                        (CNT-LINE)(1:2)
                       MOVE    "��"               TO  HCM50-SRYYMD
                                                        (CNT-LINE)(3:2)
                       MOVE    WRK-EDTYMD1 (8:2)  TO  HCM50-SRYYMD
                                                        (CNT-LINE)(5:2)
                       MOVE    "����"             TO  HCM50-SRYYMD
                                                        (CNT-LINE)(7:4)
                       MOVE    HCM50SRY-SKYEDYMD  TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD1 (8:2)  TO  HCM50-SRYYMD
                                                        (CNT-LINE)(11:2)
                       MOVE    "��"               TO  HCM50-SRYYMD
                                                        (CNT-LINE)(13:2)
                   ELSE
                       MOVE    HCM50SRY-SKYSTYMD  TO  WRK-SYMD
                       PERFORM 800-SEIWA-HEN-SEC
                       MOVE    WRK-EDTYMD3 (11: ) TO  HCM50-SRYYMD
                                                           (CNT-LINE)
                   END-IF
               END-IF
           END-IF
      *
           MOVE    HCM50SRY-NAME       TO  HCM50-SRYMEI    (CNT-LINE)
           MOVE    HCM50SRY-ZAIKAISU   TO  WRK-ZZZZ9
           MOVE    WRK-ZZZZ9           TO  HCM50-KAISU     (CNT-LINE)
      *
           COMPUTE WRK-KINGAKU
               =   HCM50SRY-KINGAKU    *   HCM50SRY-ZAIKAISU
           MOVE    WRK-KINGAKU         TO  WRK-ZZZZZZZZ9
           MOVE    WRK-ZZZZZZZZ9       TO  HCM50-KINGAKU(CNT-LINE)
      *
           PERFORM 900-HCM50SRY-SEQ-READ-SEC
      *
           .
       20011-BODY-EXT.
           EXIT.
      *****************************************************************
      *    �ǽ��ԥ�å������Խ�����
      *****************************************************************
       2001-LASTMES-SEC                SECTION.
      *
           IF    ( FLG-HCM50SRY        NOT =   ZERO )
            OR   ( HCM50SRY-NYUGAIKBN  NOT =   KEY-NYUGAIKBN )
               IF    ( CONST-LINE-MAX      >   CNT-LINE )
                   COMPUTE WRK-LINE    =   CNT-LINE        +   2
               ELSE
                   COMPUTE WRK-LINE    =   CONST-LINE-MAX  +   1
               END-IF
      *
               STRING  "��������������������"  DELIMITED   BY  SIZE
                       "��������������������"  DELIMITED   BY  SIZE
                       "�ʲ�;��"              DELIMITED   BY  SIZE
               INTO    HCM50-SRYMEI    (WRK-LINE)
               END-STRING
      *
           ELSE
               STRING  "��������������������"  DELIMITED   BY  SIZE
                       "��������������������"  DELIMITED   BY  SIZE
                       "���ڡ�����³��"        DELIMITED   BY  SIZE
               INTO    HCM50-SRYMEI    (CONST-LINE-MAX + 1)
               END-STRING
           END-IF
      *
           .
       2001-LASTMES-EXT.
           EXIT.
      *****************************************************************
      *    �쥤�䡼���ץ�����Խ�����
      *****************************************************************
       2001-LAYLER-OPTION-SEC          SECTION.
      *
           IF    ( PO-SHADE            =   ZERO )
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L SHADE "        DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCM50-LOPTION
               END-STRING
           END-IF
      *
           .
       2001-LAYLER-OPTION-EXT.
           EXIT.
      *****************************************************************
      *    Ģɼ���Ͻ���
      *****************************************************************
       2001-PRINT-SEC                  SECTION.
      *
           MOVE    "HCM50.red"     TO  SPRT-PRTID
           MOVE    HCM50           TO  SPRT-PRTDATA
           MOVE    "1"             TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF    ( SPRT-RETURN         =   ZERO )
               COMPUTE CNT-OUT =   CNT-OUT +   1
           ELSE
               DISPLAY "ORCSPRT ERR !!! PTID = " SPA-PTID
           END-IF
      *
           .
       2001-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    CNT-OUT         TO  ORCHCM50-OT-CNT
      *
      *    �ե�������
           MOVE    ZERO        TO  IF-RESULT
           MOVE    HCM50F01    TO IF-FILENAME
           CALL    "orcfiledel"    USING
                                       ORCSFDELAREA
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܸ��Ѵ�����
      *****************************************************************
       800-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  WRK-EDTYMD-G
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-EDTYMD1
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-EDTYMD3
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ֹ渡��
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-PTNUM
      *
           INITIALIZE                  PTNUM-REC
           MOVE    SPA-HOSPNUM      TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID        TO  PTNUM-PTID
           MOVE    PTNUM-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTNUM-REC
           ELSE
               MOVE    1           TO  FLG-PTNUM
               INITIALIZE              PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ծ��󸡺�
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
           ELSE
               MOVE    1           TO  FLG-PTINF
               INITIALIZE              PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ������������(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    WRK-KANRICD         TO  SYS-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
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
      *    ���Ź԰٥ե������ɹ�����
      *****************************************************************
       900-HCM50SRY-SEQ-READ-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-HCM50SRY
      *
           READ    HCM50SRY-FILE   NEXT
           AT  END
               MOVE    1           TO  FLG-HCM50SRY
           END-READ
      *
           .
       900-HCM50SRY-SEQ-READ-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥븡������
      *****************************************************************
       910-DBSELECT-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF        ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥��ɹ�����
      *****************************************************************
       910-DBFETCH-SEC            SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    �ơ��֥륯����������
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
       910-DBCLOSECURSOR-EXT.
           EXIT.