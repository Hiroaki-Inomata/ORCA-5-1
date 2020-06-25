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
       PROGRAM-ID.                 ORCSSPACHK.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : �ӣУ��ݣ��ңţ������å�
      *  ������            : 
      *  ��������    ��ȼ�        ����
      *  07/07/11    NACL��¿��Ǽ  ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    �ե饰�ΰ�
       01  FLG-AREA.
           03  FLG-ERR             PIC 9(01).
      *
      *    ź���ΰ�
      *01  IDX-AREA.
      *
      *    ����ΰ�
       01  WRK-AREA.
           03  WRK-ERR            PIC X(80).
           03  WRK-ERRMSG         PIC X(200).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ��ѡ��ΰ�
      *****************************************************************
      *
      *****************************************************************
      *    Ϣ�����ΰ�
      *****************************************************************
        LINKAGE                     SECTION.
      *    �����ΰ�
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           .
      *
      *****************************************************************
      *    �硡������
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    SPA-ARE���ͥ����å�
           PERFORM 100-SPANUM-CHK-SEC
      *
           IF      WRK-ERR         NOT =   SPACE
               STRING  "SPA-ARE ERR SPA-MOTOPG ["
                                      DELIMITED  BY  SIZE
                       SPA-MOTOPG     DELIMITED  BY  SPACE
                       "] : SPA-SAKIPG [" 
                                      DELIMITED  BY  SIZE
                       SPA-SAKIPG     DELIMITED  BY  SPACE
                       "] :"          DELIMITED  BY  SIZE
                       WRK-ERR        DELIMITED  BY  "@"
                       LOW-VALUE      DELIMITED  BY  SIZE
                       INTO    WRK-ERRMSG
               END-STRING
               CALL    "cobabort"      USING
                                       WRK-ERRMSG
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    SPA-ARE���ͥ����å�����
      *****************************************************************
       100-SPANUM-CHK-SEC         SECTION.
      *
      *���ŵ��ؼ����ֹ�
           IF    SPA-HOSPNUM(1:2)      NOT NUMERIC
               MOVE    "SPA-HOSPNUM ERR @"   TO  WRK-ERR
           END-IF
      *�±�����
           IF    SPA-HOSPSBT(1:1)      NOT NUMERIC
               MOVE    "SPA-HOSPSBT ERR @"  TO  WRK-ERR
           END-IF
      *���ֹ�
           IF    SPA-PREFNUM(1:2)      NOT NUMERIC
               MOVE    "SPA-PREFNUM ERR @"   TO  WRK-ERR
           END-IF
      *������ֳ���ʬ
           IF    SPA-GAI-JIKANKBN(1:1) NOT NUMERIC
               MOVE    "SPA-GAI-JIKANKBN ERR @"   TO  WRK-ERR
           END-IF
      *�����ޥ���������ʬ
           IF    SPA-DBUPDKBN(1:1)      NOT NUMERIC
               MOVE    "SPA-DBUPDKBN ERR @"   TO  WRK-ERR
           END-IF
      *�¾���
           IF    SPA-BEDSU(1:4)         NOT NUMERIC
               MOVE    "SPA-BEDSU ERR @"   TO  WRK-ERR
           END-IF
      *�¾����ʰ��̡�
           IF    SPA-BEDSUIPN(1:4)      NOT NUMERIC
               MOVE    "SPA-BEDSUIPN ERR @"   TO  WRK-ERR
           END-IF
      *��¾�ե饰
           IF    SPA-HAITAFLG(1:1)      NOT NUMERIC
               MOVE    "SPA-HAITAFLG ERR @"   TO  WRK-ERR
           END-IF
      *̵����ͭ����ʬ�ʣ���̵���ǡ�����ͭ���ǡ�
           IF    SPA-BEDKBN(1:1)      NOT NUMERIC
               MOVE    "SPA-BEDKBN ERR @"   TO  WRK-ERR
           END-IF
      *�¾����ե饰�ʣ����¾�̵���������¾�ͭ���
           IF    SPA-BEDFLG(1:1)      NOT NUMERIC
               MOVE    "SPA-BEDFLG ERR @"   TO  WRK-ERR
           END-IF
      *�ǡ��������ե饰
           IF    SPA-DATACREATEFLG(1:1)      NOT NUMERIC
               MOVE    "SPA-DATACREATEFLG ERR @"   TO  WRK-ERR
           END-IF
      *�ǡ��������ˡ��ʬ
           IF    SPA-DATATEISYUTUKBN(1:1)      NOT NUMERIC
               MOVE    "SPA-DATATEISYUTUKBN ERR @"   TO  WRK-ERR
           END-IF
      *���󴵼ԣɣ�
           IF    SPA-LAST-PTID(1:10)      NOT NUMERIC
               MOVE    "SPA-LAST-PTID ERR @"   TO  WRK-ERR
           END-IF
      *���̣����ϥե饰
           IF    SPA-SURYOZERO-FLG(1:1)   NOT NUMERIC
               MOVE    "SPA-SURYOZERO-FLG ERR @"   TO  WRK-ERR
           END-IF
      *�����ե饰�ʣա��е�ư��
           IF    SPA-SYORI-FLG(1:1)   NOT NUMERIC
               MOVE    "SPA-SYORI-FLG ERR @"   TO  WRK-ERR
           END-IF
      *��¾��
           IF    SPA-LOCK(1:1)   NOT NUMERIC
               MOVE    "SPA-LOCK ERR @"   TO  WRK-ERR
           END-IF
      *���ԣɣ�
           IF    SPA-PTID(1:10)   NOT NUMERIC
               MOVE    "SPA-PTID ERR @"   TO  WRK-ERR
           END-IF
      *��ôΨ
           IF    SPA-FTNRATE(1:3)  NOT NUMERIC
               MOVE    "SPA-FTNRATE ERR @"   TO  WRK-ERR
           END-IF
      *���ջ��֣ɣ�
           IF      SPA-P02-UKEID(1:5)   NOT NUMERIC
               MOVE    "SPA-P02-UKEID ERR @"   TO  WRK-ERR
           END-IF
      *���������ֹ�
           IF      SPA-P02-U02(1:4)  NOT NUMERIC
               MOVE    "SPA-P02-U02 ERR @"   TO  WRK-ERR
           END-IF
      *�ݸ���ʬ�ʣ������̡�����ϫ�ҡ������ա�
           IF    SPA-HKNKBN(1:1)  NOT NUMERIC
               MOVE    "SPA-HKNKBN ERR @"   TO  WRK-ERR
           END-IF
      *�ͻ��ʬ��ϫ���ѡ�
           IF    SPA-RSI-SISIKBN(1:1)  NOT NUMERIC
               MOVE    "SPA-RSI-SISIKBN ERR @"   TO  WRK-ERR
           END-IF
      *����ִ����ģ¤Υ�����  
           IF     SPA-JOB-JOBID(1:7)  NOT NUMERIC
               MOVE    "SPA-JOB-JOBID ERR @"   TO  WRK-ERR
           END-IF
      *
      *�����ֹ湽����������
      *�����ֹ湽����ʬ�ˤ������å�
           EVALUATE    SPA-1009-PTNUMKSIKBN
               WHEN    "1"
                  IF       SPA-1009-JIYKSIKETA(1:2)  NOT NUMERIC
                       MOVE    "SPA-1009-JIYKSIKETA ERR @"
                                                     TO  WRK-ERR
                   END-IF
               WHEN    "2"
      *    ɸ�๽��Ϣ�ֹ���
                  IF       SPA-1009-HJNKSIRENNUMKETA(1:2)  NOT NUMERIC
                       MOVE    "SPA-1009-HJNKSIRENNUMKETA ERR @" 
                                                   TO  WRK-ERR
                   END-IF
               WHEN    "3"
      *    ��ĥ���������
                  IF       SPA-1009-KKCKSIMAEKETA(1:2)  NOT NUMERIC
                       MOVE    "SPA-1009-KKCKSIMAEKETA ERR @"
                                                   TO  WRK-ERR
                   END-IF
      *    ��ĥ����Ϣ�ֹ���
                  IF       SPA-1009-KKCKSIRENNUMKETA(1:2)  NOT NUMERIC
                       MOVE    "SPA-1009-KKCKSIRENNUMKETA ERR @"
                                                   TO  WRK-ERR
                   END-IF
      *    ��ĥ��������
                  IF       SPA-1009-KKCKSIATOKETA(1:2)  NOT NUMERIC
                       MOVE    "SPA-1009-KKCKSIATOKETA ERR @"
                                                   TO  WRK-ERR
                   END-IF
           END-EVALUATE
           .
       100-SPANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *