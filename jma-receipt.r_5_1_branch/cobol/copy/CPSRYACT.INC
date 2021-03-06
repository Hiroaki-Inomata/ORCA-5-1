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
      *-------------------------------------------------*
      *    ���Ź԰٥ޥ���     COPY (CPSRYACT.INC)       *
      *-------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  04.05.00    NACL-¿��Ǽ  09/10/02  �������Ͽ����ɲ�
      ************************************************************
      *01  SRYACT-REC.
           03  SRY-KEY.
      *���ŵ��أɣ�
               05  SRY-HOSPNUM          PIC 9(2).
      *������ʬ
               05  SRY-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  SRY-PTID            PIC  9(10).
      *���Ų�
               05  SRY-SRYKA           PIC  X(02).
      *����ǯ��
               05  SRY-SRYYM           PIC  X(06).
      *���ֹ�
               05  SRY-ZAINUM          PIC  9(08).
      *Ϣ�ֹ�
               05  SRY-RENNUM          PIC  9(02).
      *���ż��̶�ʬ
           03  SRY-SRYSYUKBN           PIC  X(03).
      *���Ŷ�ʬ
           03  SRY-SRYKBN              PIC  X(02).
      *������
           03  SRY-JIHIMONEYTOTAL      PIC  9(07).
      *���Ź԰٥ơ��֥�
           03  SRY-SRYACT-TBL          OCCURS   5.
      *���ť�����
               05  SRY-SRYCD           PIC  X(09).
      *����
               05  SRY-SRYSURYO        PIC  9(05)V9(05).
      *���
               05  SRY-SRYKAISU        PIC  9(03).
      *���������ʬ
      *        1:����̾�����������ࡢ2:�ط������Ȼؼ���3:����������ؼ�
               05  SRY-MEISKYFLG       PIC  X(01).
      *��ư�����ʬ
               05  SRY-AUTOKBN         PIC  X(01).
      *̾�������ֹ�
               05  SRY-INPUTNUM        PIC  9(03).
      *������
               05  SRY-JIHIMONEY       PIC  9(07).
      *������������
               05  SRY-KANSURYO        PIC  9(05)V9(05).
      *ü���ɣ�
           03  SRY-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  SRY-OPID                PIC  X(16).
      *����ǯ����
           03  SRY-CREYMD              PIC  X(08).
      *����ǯ����
           03  SRY-UPYMD               PIC  X(08).
      *��������
           03  SRY-UPHMS               PIC  X(06).
      *
