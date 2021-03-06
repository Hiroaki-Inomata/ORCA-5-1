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
       PROGRAM-ID.             ORCSDTCHK05.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : �ǡ��������å�
      *  ����ݡ��ͥ��̾  : �ݸ�������������
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  05/02/02    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  03.05.00    NACL-����    07/06/04  ���롼�׿����б�
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-END                     PIC 9(01).
           03  FLG-HKNNUM                  PIC 9(01).
           03  FLG-PTHKNINF                PIC 9(01).
           03  FLG-PTKOHINF                PIC 9(01).
           03  FLG-PTRSIINF                PIC 9(01).
           03  FLG-TARGET                  PIC 9(01).
      *
      *    ź���ΰ�
       01  IDX-AREA.
           03  IDX0                        PIC 9(05).
           03  IDX1                        PIC 9(05).
           03  IDX5                        PIC 9(05).
           03  IDX-HKNNUM                  PIC 9(05).
           03  IDX-HKNINF                  PIC 9(05).
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-HKNINF-G.
               05  WRK-HKNKOHKBN                   PIC X(01).
               05  WRK-HKNNUM                      PIC X(03).
               05  WRK-PAYKBN                      PIC X(02).
               05  WRK-HKNJANUM                    PIC X(08).
               05  WRK-HONKZKKBN                   PIC X(01).
               05  WRK-RSIHKNKBN                   PIC X(01).
      *
       01  SRH-AREA.
           03  SRH-IDX                             PIC 9(05).
           03  SRH-HKNNUM-DAT.
               05  SRH-HKNNUM                      PIC X(03).
               05  SRH-HKNNUMMEI                   PIC X(10).
               05  SRH-HKNNUM-MAX                  PIC 9(05).
               05  SRH-HKNNUM-OCC                  OCCURS  50.
                   07  SRH-HKNNUML                 PIC X(03).
                   07  SRH-HKNNUMMEIL              PIC X(10).
           03  SRH-KOHNUM-DAT.
               05  SRH-KOHNUM                      PIC X(03).
               05  SRH-KOHNUMMEI                   PIC X(10).
               05  SRH-PAYKBN                      PIC X(02).
               05  SRH-KOHNUM-MAX                  PIC 9(05).
               05  SRH-KOHNUM-OCC                  OCCURS  150.
                   07  SRH-KOHNUML                 PIC X(03).
                   07  SRH-PAYKBNL                 PIC X(02).
                   07  SRH-KOHNUMMEIL              PIC X(10).
      *
      *    �������ΰ�
       01  CONST-AREA.
           03  CONST-SDTCHK05-OT-MAX               PIC 9(05)
                                                   VALUE   50.
           03  CONST-SYAHO                         PIC X(01)
                                                   VALUE   "1".
           03  CONST-KOKUHO                        PIC X(01)
                                                   VALUE   "2".
           03  CONST-ROSAI                         PIC X(01)
                                                   VALUE   "3".
           03  CONST-JIBAI                         PIC X(01)
                                                   VALUE   "4".
           03  CONST-JIHI                          PIC X(01)
                                                   VALUE   "5".
           03  CONST-KOUGAI                        PIC X(01)
                                                   VALUE   "6".
           03  CONST-KOUKI                         PIC X(01)
                                                   VALUE   "7".
           03  CONST-CHIKEN                        PIC X(01)
                                                   VALUE   "8".
           03  CONST-DAISAN                        PIC X(01)
                                                   VALUE   "9".
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *    �ݸ��ֹ�
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    �ݸ��ޥ���
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    ����ϫ�ҥޥ���
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    ����ޥ���
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *
      *    �ݸ��ȹ礻��������
           COPY    "CPORCSDTCHK02.INC".
           01  LNK-HKNCOMBI-AREA.
             02  LNK-HKNCOMBI-OCC      OCCURS  1000.
           COPY    "CPHKNCOMBI.INC"    REPLACING  //COMB-//
                                       BY         //LNK-COMB-//.
      *
      *    �ģ¸���
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSDTCHK05.INC".
           COPY    "CPORCBSD01PARA.INC".
           COPY    "COMMON-SPA".
      *
      *
       PROCEDURE                    DIVISION    USING
       SDTCHK05AREA
       ORCBSD01PARA
       SPA-AREA.
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC.
      *
           PERFORM 200-MAIN-SEC.
      *
           PERFORM 300-END-SEC.
      *
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    �������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       IDX-AREA
                                       WRK-AREA
                                       SRH-AREA
                                       SDTCHK05-OT
      *
           PERFORM 1001-HKNKOH-HEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��μ��ࡦ����μ����Խ�����
      *****************************************************************
       1001-HKNKOH-HEN-SEC         SECTION.
      *
           INITIALIZE                  SRH-HKNNUM-DAT
           INITIALIZE                  SRH-KOHNUM-DAT
      *
      *    �ݸ��ֹ渡��
           PERFORM 900-HKNNUM-KEY3-SEL-SEC
      *
           PERFORM UNTIL ( FLG-HKNNUM      =   1 )
      *
               IF    ( HKN-HKNKOHSBTKBN    =   "1" OR  "4"
                                           OR  "8" OR  "9" )
                   MOVE    SRH-HKNNUM-MAX  TO  IDX5
      *            �ݸ�
                   IF    ( IDX5            >   ZERO )
                       IF    ( HKN-HKNNUM  NOT =   SRH-HKNNUML (IDX5))
      *                    �ݸ������Խ�
                           PERFORM 10011-HKN-HEN-SEC
                       END-IF
                   ELSE
      *                �ݸ������Խ�
                       PERFORM 10011-HKN-HEN-SEC
                   END-IF
      *
               ELSE
      *            ����
                   MOVE    SRH-KOHNUM-MAX  TO  IDX5
                   IF    ( IDX5            >   ZERO )
                       IF    ( HKN-HKNNUM  =   SRH-KOHNUML (IDX5))
                           IF    ( HKN-HKNNUM  =   "027"  ) 
                             AND ( HKN-PAYKBN  =   "01" OR "02" OR "03")
                               IF ( HKN-PAYKBN
                                           NOT =   SRH-PAYKBNL (IDX5))
      *                            ��������Խ�
                                   PERFORM 10011-KOH-HEN-SEC
                               END-IF
                           END-IF
                       ELSE
      *                    ��������Խ�
                           PERFORM 10011-KOH-HEN-SEC
                       END-IF
                   ELSE
      *                ��������Խ�
                       PERFORM 10011-KOH-HEN-SEC
                   END-IF
               END-IF
      *
      *        �ݸ��ֹ��ɤ߹���
               PERFORM 900-HKNNUM-KEY3-FET-SEC
      *
           END-PERFORM
      *
      *    �ݸ��ֹ楯������
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       1001-HKNKOH-CMB-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ������Խ�����
      *****************************************************************
       10011-HKN-HEN-SEC          SECTION.
      *
           IF    ( SRH-HKNNUM-MAX      >=  50 )
               GO  TO  10011-HKN-HEN-EXT
           END-IF
      *
           COMPUTE SRH-HKNNUM-MAX  =   SRH-HKNNUM-MAX   +   1
      *
           MOVE    SRH-HKNNUM-MAX      TO  IDX5
      *
           MOVE    HKN-HKNNUM          TO  SRH-HKNNUML   (IDX5)
           MOVE    HKN-TANSEIDONAME    TO  SRH-HKNNUMMEIL(IDX5)
      *
           .
       10011-HKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ��������Խ�����
      *****************************************************************
       10011-KOH-HEN-SEC           SECTION.
      *
           IF    ( SRH-KOHNUM-MAX      >=  150 )
               GO  TO  10011-KOH-HEN-EXT
           END-IF
      *
           COMPUTE SRH-KOHNUM-MAX  =   SRH-KOHNUM-MAX  +   1
      *
           MOVE    SRH-KOHNUM-MAX      TO  IDX5
      *
           MOVE    HKN-HKNNUM          TO  SRH-KOHNUML (IDX5)
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN      NOT =   "01" AND "02" AND "03" )
               MOVE    "Ϸ���ݷ�"      TO  SRH-KOHNUMMEIL (IDX5)
           ELSE
               MOVE    HKN-TANSEIDONAME
                                       TO  SRH-KOHNUMMEIL (IDX5)
           END-IF
      *
           IF    ( HKN-HKNNUM          =   "027" )
            AND  ( HKN-PAYKBN          =   "01" OR  "02" OR "03" )
               MOVE    HKN-PAYKBN
                                       TO  SRH-PAYKBNL (IDX5)
           ELSE
               MOVE    SPACE           TO  SRH-PAYKBNL (IDX5)
           END-IF
      *
           .
       10011-KOH-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM 2101-PTHKN-HEN-SEC
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    �����ݸ������Խ�����
      *****************************************************************
       2101-PTHKN-HEN-SEC              SECTION.
      *
      *    �ݸ��Ȥ߹�碌
           INITIALIZE                  SDTCHK02AREA
                                       LNK-HKNCOMBI-AREA
      *
           MOVE    SDTCHK05-HOSPNUM     TO  SDTCHK02-HOSPNUM
           MOVE    SDTCHK05-PTID       TO  SDTCHK02-PTID
           CALL    "ORCSDTCHK02"       USING   SDTCHK02AREA
                                               LNK-HKNCOMBI-AREA
                                               SPA-AREA
      *
           PERFORM 21011-PTHKNINF-HEN-SEC
      *
           PERFORM 21011-PTKOHINF-HEN-SEC
      *
           .
       2101-PTHKN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �����ݸ������Խ�����
      *****************************************************************
       21011-PTHKNINF-HEN-SEC          SECTION.
      *
           PERFORM 900-PTHKNINF-KEY6-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTHKNINF    NOT =   ZERO )
      *
               PERFORM 210111-PTHKNINF-CHK-SEC
      *
               PERFORM 900-PTHKNINF-KEY6-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       21011-PTHKNINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    �����ݸ������Խ�����
      *****************************************************************
       210111-PTHKNINF-CHK-SEC         SECTION.
      *
           IF    ( PTHKN-SAKJOKBN   =  SPACE )
            AND  ( ORCBSD01-SRYYM  >=  PTHKN-TEKSTYMD (1:6))
            AND  ( ORCBSD01-SRYYM  <=  PTHKN-TEKEDYMD (1:6))
               INITIALIZE                  WRK-HKNINF-G
               MOVE    "0"             TO  WRK-HKNKOHKBN
               MOVE    PTHKN-HKNNUM    TO  WRK-HKNNUM
               MOVE    PTHKN-HKNJANUM  TO  WRK-HKNJANUM
               MOVE    PTHKN-HONKZKKBN TO  WRK-HONKZKKBN
               IF    ( PTHKN-HKNNUM    =   "971"   OR  "973" )
                   PERFORM 900-PTRSIINF-KEY-SEL-SEC
                   MOVE    PTRSI-HKNKBN
                                       TO  WRK-RSIHKNKBN
               END-IF
               IF    ( CONST-SDTCHK05-OT-MAX   >   SDTCHK05-OT-MAX )
                   PERFORM 210111-HKNNUM-CHK-SEC
               END-IF
           END-IF
      *
           .
       210111-PTHKNINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ֹ�����å�����
      *****************************************************************
       210111-HKNNUM-CHK-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-TARGET
      *
           MOVE    ZERO            TO  IDX-HKNNUM
           PERFORM VARYING IDX0        FROM    1   BY  1
                   UNTIL ( IDX0    >   SDTCHK02-O-HKNINF-MAX )
                    OR   ( IDX-HKNNUM  NOT =   ZERO )
      *
               IF    ( SDTCHK02-O-HKNNUM (IDX0)
                                   =   WRK-HKNNUM )
                   MOVE    IDX0    TO  IDX-HKNNUM
               END-IF
      *
           END-PERFORM
      *
           IF    ( IDX-HKNNUM      NOT =   ZERO )
               IF    ( ORCBSD01-PRTSRTKBN  =   "3" OR  "4" )
                   EVALUATE    SDTCHK05-HKNKBN
                   WHEN    CONST-SYAHO
                       IF   ( SDTCHK02-O-SYAHO (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-KOKUHO
                       IF   ( SDTCHK02-O-KOKUHO(IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-ROSAI
                       IF   ( SDTCHK02-O-ROSAI (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-JIBAI
                       IF   ( SDTCHK02-O-JIBAI (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-KOUGAI
                       IF   ( SDTCHK02-O-KOUGAI(IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-KOUKI
                       IF   ( SDTCHK02-O-KOUKI(IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-JIHI
                       IF   ( SDTCHK02-O-JIHI  (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-CHIKEN
                       IF   ( SDTCHK02-O-CHIKEN  (IDX-HKNNUM)  =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   WHEN    CONST-DAISAN
                       IF   ( SDTCHK02-O-DAISAN  (IDX-HKNNUM)  =   1 )
                           MOVE    1       TO  FLG-TARGET
                       END-IF
                   END-EVALUATE
               ELSE
                   IF    ( ORCBSD01-SYAHOPROCKBN   =   "1" )
                    AND  ( SDTCHK02-O-SYAHO    (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-KOKUHOPROCKBN  =   "1" )
                    AND  ( SDTCHK02-O-KOKUHO   (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-RSIPROCKBN     =   "1" )
                    AND  ( SDTCHK02-O-ROSAI    (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-JBIPROCKBN     =   "1" )
                    AND  ( SDTCHK02-O-JIBAI    (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-KGIPROCKBN     =   "1" )
                    AND  ( SDTCHK02-O-KOUGAI   (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-KOUKIPROCKBN     =   "1" )
                    AND  ( SDTCHK02-O-KOUKI    (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-JIHIPROCKBN    =   "1" )
                    AND  ( SDTCHK02-O-JIHI     (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-CHIKENPROCKBN  =   "1" )
                    AND  ( SDTCHK02-O-CHIKEN   (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
                   IF    ( ORCBSD01-DAISANPROCKBN  =   "1" )
                    AND  ( SDTCHK02-O-DAISAN   (IDX-HKNNUM)    =   1 )
                           MOVE    1       TO  FLG-TARGET
                   END-IF
               END-IF
           END-IF
      *
           IF    ( FLG-TARGET              =   1 )
               MOVE    ZERO            TO  IDX-HKNINF
               PERFORM VARYING IDX1    FROM    1   BY  1
                       UNTIL ( IDX1    >   SDTCHK05-OT-MAX )
      *
                   IF    ( SDTCHK05-HKNKOHKBN (IDX1)
                                               =   WRK-HKNKOHKBN )
                    AND  ( SDTCHK05-HKNNUM    (IDX1)
                                               =   WRK-HKNNUM )
                    AND  ( SDTCHK05-PAYKBN    (IDX1)
                                               =   WRK-PAYKBN )
                    AND  ( SDTCHK05-HKNJANUM  (IDX1)
                                               =   WRK-HKNJANUM )
                    AND  ( SDTCHK05-HONKZKKBN (IDX1)
                                               =   WRK-HONKZKKBN )
                    AND  ( SDTCHK05-RSIHKNKBN (IDX1)
                                               =   WRK-RSIHKNKBN )
                       MOVE    IDX1    TO  IDX-HKNINF
                   END-IF
      *
               END-PERFORM
      *
               IF    ( IDX-HKNINF      =   ZERO )
      *
                   COMPUTE SDTCHK05-OT-MAX
                       =   SDTCHK05-OT-MAX
                       +   1
      *
                   MOVE    SDTCHK05-OT-MAX     TO  IDX-HKNINF
      *
                   MOVE    WRK-HKNKOHKBN       TO  SDTCHK05-HKNKOHKBN
                                                           (IDX-HKNINF)
                   MOVE    WRK-HKNNUM          TO  SDTCHK05-HKNNUM
                                                           (IDX-HKNINF)
      *
                   IF    ( WRK-HKNKOHKBN       =   ZERO )
                       IF    ( WRK-HKNNUM      =   "971" OR  "973" )
                           EVALUATE    WRK-HKNNUM  ALSO  WRK-RSIHKNKBN
                           WHEN    "971"       ALSO    "1"
                               MOVE    "ϫ���ݸ���û"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           WHEN    "971"       ALSO    "2"
                               MOVE    "ϫ���ݸ�����"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           WHEN    "971"       ALSO    "3"
                               MOVE    "ϫ���ݸ�����"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           WHEN    "971"       ALSO    "5"
                               MOVE    "ϫ���ݸ�����"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           WHEN    "971"       ALSO    ANY
                               MOVE    "ϫ���ݸ�"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           WHEN    "973"       ALSO    ANY
                               MOVE    "�������ݸ�"
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                           END-EVALUATE
                       ELSE
                           MOVE    WRK-HKNNUM  TO  SRH-HKNNUM
                           PERFORM 2111-HKNNUM-GET-SEC
                           MOVE    SRH-HKNNUMMEI
                                               TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                       END-IF
                   ELSE
                       MOVE    WRK-HKNNUM      TO  SRH-KOHNUM
                       MOVE    WRK-PAYKBN      TO  SRH-PAYKBN
                       PERFORM 2111-KOHNUM-GET-SEC
                       MOVE    SRH-KOHNUMMEI   TO  SDTCHK05-HKNNUMMEI
                                                           (IDX-HKNINF)
                   END-IF
                   MOVE    WRK-PAYKBN          TO  SDTCHK05-PAYKBN
                                                           (IDX-HKNINF)
                   MOVE    WRK-HKNJANUM        TO  SDTCHK05-HKNJANUM
                                                           (IDX-HKNINF)
                   MOVE    WRK-HONKZKKBN       TO  SDTCHK05-HONKZKKBN
                                                           (IDX-HKNINF)
                   MOVE    WRK-RSIHKNKBN       TO  SDTCHK05-RSIHKNKBN
                                                           (IDX-HKNINF)
               END-IF
      *
           END-IF
      *
           .
       210111-HKNNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �����ݸ������Խ�����
      *****************************************************************
       21011-PTKOHINF-HEN-SEC          SECTION.
      *
           PERFORM 900-PTKOHINF-KEY2-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTKOHINF    NOT =   ZERO )
      *
               PERFORM 210111-PTKOHINF-CHK-SEC
      *
               PERFORM 900-PTKOHINF-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       21011-PTKOHINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ���Ը�������Խ�����
      *****************************************************************
       210111-PTKOHINF-CHK-SEC         SECTION.
      *
           IF    ( PTKOH-SAKJOKBN   =  SPACE )
            AND  ( ORCBSD01-SRYYM  >=  PTKOH-TEKSTYMD (1:6))
            AND  ( ORCBSD01-SRYYM  <=  PTKOH-TEKEDYMD (1:6))
               INITIALIZE                  WRK-HKNINF-G
               MOVE    "1"             TO  WRK-HKNKOHKBN
               MOVE    PTKOH-KOHNUM    TO  WRK-HKNNUM
               IF    ( PTKOH-KOHNUM    =   "027" )
                AND  ( PTKOH-PAYKBN    =   "01" OR "02" OR "03" )
                   MOVE    PTKOH-PAYKBN
                                       TO  WRK-PAYKBN
               END-IF
               IF    ( CONST-SDTCHK05-OT-MAX   >   SDTCHK05-OT-MAX )
                   PERFORM 210111-HKNNUM-CHK-SEC
               END-IF
           END-IF
      *
           .
       210111-PTKOHINF-CHK-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��μ����������
      *****************************************************************
       2111-HKNNUM-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SRH-HKNNUMMEI
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-HKNNUM-MAX )
               IF    ( SRH-HKNNUM  =   SRH-HKNNUML (SRH-IDX) )
                   MOVE    SRH-HKNNUMMEIL (SRH-IDX)
                                       TO  SRH-HKNNUMMEI
                   MOVE    SRH-HKNNUM-MAX
                                       TO  SRH-IDX
               END-IF
           END-PERFORM
      *
      *
           .
       2111-HKNNUM-GET-EXT.
           EXIT.
      *****************************************************************
      *    ����μ����������
      *****************************************************************
       2111-KOHNUM-GET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SRH-KOHNUMMEI
      *
           PERFORM VARYING SRH-IDX FROM    1   BY  1
                   UNTIL ( SRH-IDX >   SRH-KOHNUM-MAX )
               IF    ( SRH-KOHNUM  =   "027" )
                AND  ( SRH-PAYKBN  =   "01" OR "02" OR "03" )
                   IF    ( SRH-KOHNUM  =   SRH-KOHNUML (SRH-IDX) )
                    AND  ( SRH-PAYKBN  =   SRH-PAYKBNL (SRH-IDX) )
                       MOVE    SRH-KOHNUMMEIL (SRH-IDX)
                                       TO  SRH-KOHNUMMEI
                       MOVE    SRH-KOHNUM-MAX
                                       TO  SRH-IDX
                   END-IF
               ELSE
                   IF    ( SRH-KOHNUM  =   SRH-KOHNUML (SRH-IDX) )
                    AND  ( SRH-PAYKBNL (SRH-IDX)   =   SPACE )
                       MOVE    SRH-KOHNUMMEIL (SRH-IDX)
                                       TO  SRH-KOHNUMMEI
                       MOVE    SRH-KOHNUM-MAX
                                       TO  SRH-IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2111-KOHNUM-GET-EXT.
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
      *    �����ݸ����󸡺�����
      *****************************************************************
       900-PTHKNINF-KEY6-SEL-SEC       SECTION.
      *
      *    �����ݸ��ޥ�������
           MOVE    ZERO                TO  FLG-PTHKNINF
           INITIALIZE                      PTHKNINF-REC
           MOVE    SDTCHK05-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    SDTCHK05-PTID       TO  PTHKN-PTID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           .
       900-PTHKNINF-KEY6-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �����ݸ�����FETCH����
      *****************************************************************
       900-PTHKNINF-KEY6-FET-SEC       SECTION.
      *
      *    �����ݸ��ޥ�������
           MOVE    ZERO                TO  FLG-PTHKNINF
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
               INITIALIZE                  PTHKNINF-REC
           END-IF
      *
           .
       900-PTHKNINF-KEY6-FET-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ֹ���󸡺�����
      *****************************************************************
       900-HKNNUM-KEY3-SEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           INITIALIZE                  HKNNUM-REC
           MOVE    SDTCHK05-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    ORCBSD01-SRYYM      TO  HKN-TEKSTYMD
           MOVE    "01"                TO  HKN-TEKSTYMD (7:2)
           MOVE    HKN-TEKSTYMD        TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-KEY3-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ݸ��ֹ����FETCH����
      *****************************************************************
       900-HKNNUM-KEY3-FET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNNUM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC                  =   ZERO )
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-KEY3-FET-EXT.
           EXIT.
      *****************************************************************
      *    ���Ը�����󸡺�����
      *****************************************************************
       900-PTKOHINF-KEY2-SEL-SEC       SECTION.
      *
      *    ���Ը���ޥ�������
           MOVE    ZERO                TO  FLG-PTKOHINF
           INITIALIZE                      PTKOHINF-REC
           MOVE    SDTCHK05-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    SDTCHK05-PTID       TO  PTKOH-PTID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           .
       900-PTKOHINF-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ���Ը������FETCH����
      *****************************************************************
       900-PTKOHINF-KEY2-FET-SEC       SECTION.
      *
      *    ���Ը���ޥ�������
           MOVE    ZERO                TO  FLG-PTKOHINF
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
               INITIALIZE                  PTKOHINF-REC
           END-IF
      *
           .
       900-PTKOHINF-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    ����ϫ�Ҿ��󸡺�����
      *****************************************************************
       900-PTRSIINF-KEY-SEL-SEC        SECTION.
      *
      *    ���Ը���ޥ�������
           MOVE    ZERO                TO  FLG-PTRSIINF
           INITIALIZE                      PTRSIINF-REC
           MOVE    PTHKN-HOSPNUM        TO  PTRSI-HOSPNUM
           MOVE    PTHKN-PTID          TO  PTRSI-PTID
           MOVE    PTHKN-HKNID         TO  PTRSI-HKNID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF ( MCP-RC             =   ZERO )
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
               INITIALIZE                  PTRSIINF-REC
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTRSIINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    �ģ¸���������FHETCH��Ԥ�)
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      *    �ģ¸���������FHETCH�ϹԤ�ʤ�)
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
       910-DBCLOSECURSOR-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
