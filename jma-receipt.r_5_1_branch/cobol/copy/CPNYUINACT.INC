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
      *    �����԰٥ޥ���     COPY (CPNYUINACT.INC)     *
      *-------------------------------------------------*
      *01  NYUIN-SRYACT-REC.
           03  NSRY-KEY.
      *���ŵ��أɣ�
               05  NSRY-HOSPNUM          PIC 9(2).
      *������ʬ
               05  NSRY-NYUGAIKBN       PIC  X(01).
      *���ԣɣ�
               05  NSRY-PTID            PIC  9(10).
      *���Ų�
               05  NSRY-SRYKA           PIC  X(02).
      *����ǯ��
               05  NSRY-SRYYM           PIC  X(06).
      *���ֹ�
               05  NSRY-ZAINUM          PIC  9(08).
      *Ϣ�ֹ�
               05  NSRY-RENNUM          PIC  9(02).
      *���ż��̶�ʬ
           03  NSRY-SRYSYUKBN           PIC  X(03).
      *���Ŷ�ʬ
           03  NSRY-SRYKBN              PIC  X(02).
      *�����۷�
           03  NSRY-JIHIMONEYTOTAL      PIC  9(07).
      *���Ź԰٥ơ��֥�
           03  NSRY-SRYACT-TBL          OCCURS   5.
      *���ť�����
               05  NSRY-SRYCD           PIC  X(09).
      *����
               05  NSRY-SRYSURYO        PIC  9(05)V9(03).
      *���
               05  NSRY-SRYKAISU        PIC  9(03).
      *��ư�����ʬ
               05  NSRY-AUTOKBN         PIC  X(01).
      *̾�������ֹ�
               05  NSRY-INPUTNUM        PIC  9(03).
      *������
               05  NSRY-JIHIMONEY       PIC  9(07).
      *ü���ɣ�
           03  NSRY-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  NSRY-OPID                PIC  X(16).
      *����ǯ����
           03  NSRY-CREYMD              PIC  X(08).
      *����ǯ����
           03  NSRY-UPYMD               PIC  X(08).
      *��������
           03  NSRY-UPHMS               PIC  X(06).
      *
