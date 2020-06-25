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
       PROGRAM-ID.             ORCHCN63.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ʬ���������ٽ�
      *  ����ݡ��ͥ��̾  : ʬ���������ٽ�ʣȣãΣ�����
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  09/08/26    NACL-����     ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������        ����      ����
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    �����ѥǡ���
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    �����ѥǡ���
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(20000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    �����ѥǡ��� ̾���ΰ� 
           COPY    "CPCOMMONPRT.INC".
           COPY    "CPASGNPARA.INC".
      *
      *    Ģɼ�ΰ�
           COPY    "HCN63V2.INC"   REPLACING  //HCN63V2//
                                   BY         //HCN63//.
      *
      *    �����ƥ��ΰ�
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    ���ơ������ΰ�
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-BUNBEN          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-HOSPNAME        PIC X(100).
           03  WRK-HOSPNAME-R      REDEFINES   WRK-HOSPNAME.
               05  WRK-HOSPNAME1   PIC X(50).
               05  WRK-HOSPNAME2   PIC X(50).
           03  WRK-HOSPADRS        PIC X(100).
           03  WRK-HOSPADRS-R      REDEFINES   WRK-HOSPADRS.
               05  WRK-HOSPADRS1   PIC X(50).
               05  WRK-HOSPADRS2   PIC X(50).
           03  WRK-HOSPTEL         PIC X(15).
           03  WRK-999             PIC 9(03).
      *
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-Z03             PIC ZZ9.
           03  WRK-Z10             PIC ZZ,ZZZ,ZZ9.
      *
           03  WRK-MOJI-IN         PIC X(100).
           03  WRK-MOJI-OUT        PIC X(100).
           03  WRK-PNT             PIC 9(05).
           03  WRK-SKYMONEY        PIC 9(10).
           03  WRK-STR-G.
               05  WRK-STR1        PIC X(100).
               05  WRK-STR2        PIC X(100).
      *
      *    �ץ�����४�ץ����
       01  PO-AREA.
           03  PO-IN               PIC X(25).
           03  PO-OT               PIC X(100).
           03  PO-BUNBEN-TITLE     PIC X(01).
           03  PO-KANANAME         PIC X(01).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    ���ŵ��ؾ���ݴ��ܾ���
           COPY    "CPSK1001.INC".
      *    ���ŵ��ؾ���ݽ���ϡ�Ϣ����
           COPY    "CPSK1002.INC".
      *    ������ץ��̾������ƾ���
           COPY    "CPSK1034.INC".
      *
      *    ���ŵ����Խ�����
           COPY    "CPSK1900.INC".
      *    ���ŵ���̾���Խ�����
           COPY    "CPSK1901.INC".
      *    ���ŵ��ؽ����Խ�����
           COPY    "CPSK1902.INC".
      *
      *    ʬ�ھ���
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *    ���Ծ���
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *    �����Ѵ�����
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    �����Ѵ�����
           COPY    "CPORCSKANACHK.INC".
      *
      *    �ģ¸���
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    ����饤��Ģɼ̾��������ץ��̾�����ѥ�
           COPY    "CPORCSPRTNM.INC".
      *
      *    ����ե�����̾�Խ�
           COPY    "CPORCSGETTEMP.INC".
      *
      *    �ץ�����४�ץ�����Խ��ѥ�᥿
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    �����ѥ�᥿
           COPY    "CPORCSMKPRT.INC".
      *
           COPY    "CPORCHCN63.INC".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPONPRTDATA.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   ONPRTDATA
                                   .
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
           EXIT    PROGRAM
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
           ACCEPT  SYS-TIME    FROM    TIME
      *
           MOVE    ONPRTDATA-DATA  TO  ORCHCN63AREA
      *
      *    ������ץ��̾������ƾ���
           PERFORM 900-1034-READ-SEC
      *
      *    ���ŵ��ؾ���ݴ��ܾ���
           PERFORM 900-1001-READ-SEC
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
      *
      *    ���ŵ��ؾ���ݽ���ϡ�Ϣ����
           PERFORM 900-1002-READ-SEC
           MOVE    SYS-1002-ADRS   TO  WRK-HOSPADRS
           MOVE    SYS-1002-TEL    TO  WRK-HOSPTEL
      *
      *    ���ŵ����Խ�����
           PERFORM 900-1900-READ-SEC
      *
      *    ʬ�ھ���
           PERFORM 900-BUNBEN-READ-SEC
      *    ���Ծ���
           PERFORM 900-PTINF-READ-SEC
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
           MOVE    "1"                 TO  PO-BUNBEN-TITLE
           MOVE    "0"                 TO  PO-KANANAME
      *
           MOVE    "BUNBEN_TITLE"      TO  PO-IN
           PERFORM 10011-ORCSPRGOPTION-SEC
           IF    ( PO-OT  =   "0" OR "1" OR "2" )
               MOVE    PO-OT           TO  PO-BUNBEN-TITLE
           END-IF
      *
           MOVE    "KANANAME"          TO  PO-IN
           PERFORM 10011-ORCSPRGOPTION-SEC
           IF    ( PO-OT  =   "0" OR "1" )
               MOVE    PO-OT           TO  PO-KANANAME
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
            MOVE    "ORCHCN63"         TO  ORCSPRGOPTION-PRGID
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
      *    �硡������
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  HCN63
      *
      *    ʬ�ڶ�ʬ
           EVALUATE    BUNBEN-BUNBENKBN
               WHEN    1
                   IF    ( PO-BUNBEN-TITLE     =   "1" OR "2" )
                       MOVE    "������ʬ�ڡ�"  TO  HCN63-BUNBENKBN
                   END-IF
      *            ʬ�ڲ����
                   MOVE    "�ݡ�������"        TO  HCN63-KAIJO-MONEY
      *            ʬ����
                   MOVE    BUNBEN-BUNBEN-MONEY TO  WRK-Z10
                   MOVE    WRK-Z10             TO  WRK-MOJI-IN
                   PERFORM 31011-KANA-HEN-SEC
                   MOVE    WRK-MOJI-OUT        TO  HCN63-BUNBEN-MONEY
               WHEN    2
                   IF    ( PO-BUNBEN-TITLE     =   "1" )
                       MOVE    "�ʰ۾�ʬ�ڡ�"  TO  HCN63-BUNBENKBN
                   END-IF
      *            ʬ�ڲ����
                   MOVE    BUNBEN-KAIJO-MONEY  TO  WRK-Z10
                   MOVE    WRK-Z10             TO  WRK-MOJI-IN
                   PERFORM 31011-KANA-HEN-SEC
                   MOVE    WRK-MOJI-OUT        TO  HCN63-KAIJO-MONEY
      *            ʬ����
                   MOVE    "�ݡ�������"        TO  HCN63-BUNBEN-MONEY
           END-EVALUATE
      *
      *    �����ֹ�
           MOVE    BUNBEN-PTNUM    TO  HCN63-PTNUM
      *
           IF    ( PO-KANANAME     =   "1" )
               MOVE    PTINF-KANANAME
                                   TO  HCN63-KANANAME
           END-IF
      *
      *    ���Ի�̾
           IF    ( PTINF-NAME (31:)    =   SPACE )
               MOVE    PTINF-NAME  TO  HCN63-NAME
           ELSE
               MOVE    PTINF-NAME  TO  HCN63-NAME2
           END-IF
      *
      *    ���ե��å�
           MOVE    SPA-SYSYMD      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  HCN63-PRTYMD
      *
      *    �л���
           MOVE    BUNBEN-BIRTHDAY TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  HCN63-BIRTHDAY
      *
      *    ľ�ܻ�ʧ����
           EVALUATE    BUNBEN-RIYOUKBN
               WHEN    "0"
                   MOVE    "���Ѥ��ʤ�"    TO  HCN63-RIYOUKBN
                   MOVE
        "�������ٽ�����ƤˤĤ��Ƥϡ�ľ�ܻ�ʧ���١פ����Ѥ��Ƥ��ޤ���"
                                           TO  HCN63-COMMENT
               WHEN    "1"
                   MOVE    "���Ѥ���"      TO  HCN63-RIYOUKBN
                   MOVE
              "�������ٽ�����Ƥϡ����������פ����Ƥ���㤢��ޤ���"
                                           TO  HCN63-COMMENT
           END-EVALUATE
      *
      *    ���ʰ���������ٶ�ʬ
           EVALUATE    BUNBEN-SANKAKBN
               WHEN    "1"
                   MOVE    "�о�"          TO  HCN63-SANKAKBN
               WHEN    "2"
                   MOVE    "�оݳ�"        TO  HCN63-SANKAKBN
               WHEN    "3"
                   MOVE    "����"          TO  HCN63-SANKAKBN
           END-EVALUATE
      *
      *    ��������
           IF    ( BUNBEN-NYUGAIKBN    =   "1" )
               MOVE    "��������"      TO  HCN63-LBLNISSU
           ELSE
               MOVE    "��������"      TO  HCN63-LBLNISSU
           END-IF
           MOVE    BUNBEN-NYUIN-DAY    TO  WRK-Z03
           MOVE    WRK-Z03             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-NYUIN-DAY
      *
      *    ������
           MOVE    BUNBEN-NYU-MONEY    TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-NYU-MONEY
      *
      *    ��������
           MOVE    BUNBEN-RMSGK-MONEY  TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-RMSGK-MONEY
      *
      *    �����������ݰ���
           MOVE    BUNBEN-SHIN-MONEY   TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-SHIN-MONEY
      *
      *    ������������
           MOVE    BUNBEN-KNS-MONEY    TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-KNS-MONEY
      *
      *    ���֡�������
           MOVE    BUNBEN-SYC-MONEY    TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-SYC-MONEY
      *
      *    ���ʰ������������
           MOVE    BUNBEN-SNK-MONEY    TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-SNK-MONEY
      *
      *    ����¾
           MOVE    BUNBEN-ETC-MONEY    TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-ETC-MONEY
      *
      *    ������ô����
           MOVE    BUNBEN-ICBFTN-MONEY TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-ICBFTN-MONEY
      *
      *    ǥ�ع����ô��
           MOVE    BUNBEN-NINPUFTN-MONEY
                                       TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-NINPUFTN-MONEY
      *
      *    ���������
           MOVE    BUNBEN-DAIRI-MONEY  TO  WRK-Z10
           MOVE    WRK-Z10             TO  WRK-MOJI-IN
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-DAIRI-MONEY
      *
      *    �����
           MOVE    WRK-HOSPADRS    TO  HCN63-HOSPADRS
           INSPECT HCN63-HOSPADRS      REPLACING ALL "  " BY "��"
      *    ���ŵ���̾��
           MOVE    WRK-HOSPNAME    TO  HCN63-HOSPNAME
           INSPECT HCN63-HOSPNAME      REPLACING ALL "  " BY "��"
      *    �����ֹ�
           MOVE    WRK-HOSPTEL     TO  HCN63-HOSPTEL
      *
      *    ʸ����
           MOVE    SPACE           TO  HCN63-MOJI1
      *    IF    ( BUNBEN-NINPUFTN-MONEY   >   BUNBEN-DAIRI-MONEY )
               MOVE    1           TO  WRK-PNT
               STRING  "���������"  DELIMITED   BY  SIZE
               INTO    HCN63-MOJI1
               WITH POINTER WRK-PNT
               END-STRING
               MOVE    BUNBEN-DAIRI-MONEY 
                                       TO  WRK-Z10
               MOVE    WRK-Z10         TO  WRK-MOJI-OUT
               PERFORM 31011-LTRIM-SEC
               MOVE    WRK-MOJI-OUT    TO  WRK-MOJI-IN
               PERFORM 31011-KANA-HEN-SEC
               STRING  WRK-MOJI-OUT    DELIMITED   BY  SPACE
                       "�ߤˤĤ�������������"
               INTO    HCN63-MOJI1
               WITH POINTER WRK-PNT
               END-STRING
               COMPUTE WRK-SKYMONEY
                   =   BUNBEN-NINPUFTN-MONEY
                   -   BUNBEN-DAIRI-MONEY
               MOVE    WRK-SKYMONEY   TO   WRK-Z10
               MOVE    WRK-Z10        TO   WRK-MOJI-OUT
               PERFORM 31011-LTRIM-SEC
               MOVE    WRK-MOJI-OUT   TO  WRK-MOJI-IN
               PERFORM 31011-KANA-HEN-SEC
               STRING  WRK-MOJI-OUT    DELIMITED   BY  SPACE
                       "��"
               INTO    HCN63-MOJI1
               WITH POINTER WRK-PNT
               END-STRING
      *    END-IF
      *
      *    �л�����
           MOVE    SPACE           TO  WRK-MOJI-IN
           STRING  "�ʽл�������"      DELIMITED BY SIZE
                   BUNBEN-BIRTHCNT     DELIMITED BY SIZE
                   "�͡�"              DELIMITED BY SIZE
                   INTO                WRK-MOJI-IN
           END-STRING
           PERFORM 31011-KANA-HEN-SEC
           MOVE    WRK-MOJI-OUT        TO  HCN63-BIRTHCNT
      *
           IF    ( BUNBEN-RIYOUKBN     =   "1" )
            AND  ( BUNBEN-BUNBENKBN    =   "1" )
      *        ʸ����
               STRING  "���嵭������ô�����ϡ�����������"
                                           DELIMITED   BY  SIZE
                       "�֤���¾�פ����ѤȤ��Ʒ׾夷�Ƥ��ޤ���"
                                           DELIMITED   BY  SIZE
               INTO    HCN63-MOJI2
               END-STRING
           END-IF
      *
      *    ����
      *     MOVE    BUNBEN-BIKOU        TO  WRK-MOJI-IN
      *     PERFORM 31011-KANA-HEN-SEC
      *     MOVE    WRK-MOJI-OUT        TO  HCN63-BIKOU
      *
           IF    ( PTINF-NAME (31:)    =   SPACE )
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L L2    "        DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCN63-LOPTION
               END-STRING
           ELSE
               STRING  "MonpeLayerIn"      DELIMITED  BY  SIZE
                       " -L L1    "        DELIMITED  BY  SIZE
                       "MonpeLayerOut"     DELIMITED  BY  SIZE
               INTO    HCN63-LOPTION
               END-STRING
           END-IF
      *    Ģɼ��������
           PERFORM 400-PRT-WRITE-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ģɼ���Ͻ���
      *****************************************************************
       400-PRT-WRITE-SEC           SECTION.
      *
      *    �����ѥե��������
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC63"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    1               TO  HC01PARA-CNT
      *
           INITIALIZE                  SGETTEMP-AREA
           MOVE    HC01PARA        TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP" USING SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME
                                   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    �����ѥǡ�������
           MOVE    HCN63           TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    Ģɼ��������
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCN63V2.red"   TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
           MOVE    ASGNPARA        TO  MKPRT-DAT
           MOVE    SYS-1034-PRTNM(13)
                                   TO  MKPRT-PRTNM
           MOVE   "ʬ���������ٽ�" TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��λ��������
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ͤ����
      *****************************************************************
       31011-LTRIM-SEC             SECTION.
      *
           MOVE    SPACE           TO  WRK-STR-G
           UNSTRING    WRK-MOJI-OUT    DELIMITED   BY  ALL " "
           INTO        WRK-STR1 ,
                       WRK-STR2
           END-UNSTRING
           IF    ( WRK-STR1    =   SPACE )
               MOVE    WRK-STR2    TO  WRK-STR1
           END-IF
      *
           MOVE    WRK-STR1        TO  WRK-MOJI-OUT
      *
           .
       31011-LTRIM-EXT.
           EXIT.
      *****************************************************************
      *    �����Ѵ�����
      *****************************************************************
       31011-KANA-HEN-SEC          SECTION.
      *
      *    �����Ѵ�
           MOVE    SPACE           TO  ORCSKANACHKAREA
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"             TO  KANACHK-SYORI
           MOVE    WRK-MOJI-IN     TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                   TO  WRK-MOJI-OUT
      *
           .
       31011-KANA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �������ܸ��Ѵ�����
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3
                                   TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "��"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    �����ƥ�����ޥ����ɤ߹���
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ŵ��ؾ���ݴ��ܾ����ɤ߹���
      *****************************************************************
       900-1001-READ-SEC           SECTION.
      *
      *    ���ŵ��ؾ���ݴ��ܾ����ɤ߹���
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
           .
       900-1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ŵ��ؾ���ݽ���ϡ�Ϣ�����ɤ߹���
      *****************************************************************
       900-1002-READ-SEC           SECTION.
      *
      *    ���ŵ��ؾ���ݽ���ϡ�Ϣ�����ɤ߹���
           MOVE    SPACE           TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1002-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1002-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
           .
       900-1002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ������ץ��̾������ƾ����ɤ߹���
      *****************************************************************
       900-1034-READ-SEC           SECTION.
      *
      *    ������ץ��̾������ƾ����Խ�
           INITIALIZE                  SYS-1034-REC
           INITIALIZE                  ORCSPRTNMAREA
           MOVE    "2"             TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD      TO  ORCSPRTNM-SRYYMD
           MOVE    "1"             TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1034-REC
           END-IF
      *
           .
       900-1034-READ-EXT.
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
           MOVE    SPA-SYSYMD      TO  SYS-1900-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(11) NOT =   SPACE
      *            ���ŵ���̾���Խ�����
                   PERFORM 900-1901-READ-SEC
      *            ���ŵ��ؽ����Խ�����
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
      *
           .
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ŵ���̾���Խ������ɤ߹���
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    ���ŵ���̾���Խ������ɤ߹���
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(11)
                                   TO  SYS-1901-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1901-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1
                                       TO  WRK-HOSPNAME1
               MOVE    SYS-1901-HOSPNAME2
                                       TO  WRK-HOSPNAME2
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
      *
           .
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���ŵ��ؽ����Խ������ɤ߹���
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    ���ŵ��ؽ����Խ������ɤ߹���
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(11)
                                   TO  SYS-1902-KBNCD
           MOVE    SPA-SYSYMD      TO  SYS-1902-STYUKYMD
           MOVE    SPA-SYSYMD      TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1902-REC
               MOVE    SYS-1902-ADRS1  TO  WRK-HOSPADRS1
               MOVE    SYS-1902-ADRS2  TO  WRK-HOSPADRS2
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
      *
           .
       900-1902-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ʬ�ھ���
      *****************************************************************
       900-BUNBEN-READ-SEC         SECTION.
      *
           MOVE    SPACE           TO  BUNBEN-REC
           INITIALIZE                  BUNBEN-REC
           MOVE    SPA-HOSPNUM     TO  BUNBEN-HOSPNUM
           MOVE    ORCHCN63-PTID   TO  BUNBEN-PTID
           MOVE    ORCHCN63-NYUGAIKBN
                                   TO  BUNBEN-NYUGAIKBN
           MOVE    ORCHCN63-RRKNUM TO  BUNBEN-RRKNUM
           MOVE    BUNBEN-REC      TO  MCPDATA-REC
           MOVE    "tbl_bunben"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_bunben"    TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-BUNBEN
                   MOVE    MCPDATA-REC     TO  BUNBEN-REC
               ELSE
                   MOVE    1               TO  FLG-BUNBEN
                   INITIALIZE                  BUNBEN-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-BUNBEN
               INITIALIZE                  BUNBEN-REC
           END-IF
      *
           MOVE    "tbl_bunben"   TO  MCP-TABLE
           MOVE    "key3"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-BUNBEN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ���Ծ���
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    SPACE           TO  PTINF-REC
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    ORCHCN63-PTID   TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ��ɹ����Ͻ���
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ��ɹ���������
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    �ģ��ɹ���λ����
      *****************************************************************
       930-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-DBCLOSE-EXT.
           EXIT.
      *