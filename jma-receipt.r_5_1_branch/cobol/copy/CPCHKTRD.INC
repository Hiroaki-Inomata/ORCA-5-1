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
      *****************************************************************
      *    �����å��ޥ�����       COPY (CPCHKTRD.INC)                   *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  CHKTRD-REC.
           03  CHKTRD-KEY.
               05  CHKTRD-HOSPNUM        PIC 9(2).
      *�����å���ʬ
               05  CHKTRD-CHKKBN         PIC  X(01).
      *���ť�����
               05  CHKTRD-SRYCD          PIC  X(09).
      *ͭ������ǯ����
               05  CHKTRD-YUKOSTYMD      PIC  X(08).
      *ͭ����λǯ����
               05  CHKTRD-YUKOEDYMD      PIC  X(08).
      *�����ɶ�ʬ
               05  CHKTRD-CDKBN          PIC  X(01).
      *Ϣ�ֹ�
               05  CHKTRD-RENNUM         PIC  9(03).
      *Ŭ�ѥ�����
           03  CHKTRD-CD                 PIC  X(9).
      *������ʬ
           03  CHKTRD-NYUGAIKBN          PIC  X(1).
      *Ʊ���������¶�ʬ
           03  CHKTRD-NYUGAIBNDKBN       PIC  X(1).
      *���̡����̥����å���ʬ
           03  CHKTRD-DAYMONTHKBN        PIC  X(1).
      *ü���ɣ�
           03  CHKTRD-TERMID             PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CHKTRD-OPID               PIC  X(16).
      *����ǯ����
           03  CHKTRD-CREYMD             PIC  X(08).
      *����ǯ����
           03  CHKTRD-UPYMD              PIC  X(08).
      *�������� 
           03  CHKTRD-UPHMS              PIC  X(06).
      *�祭�������ѹ���
           03  CHKTRD-UPKEY.
               05  CHKTRD-UPCHKTRDKBN      PIC  X(01).
               05  CHKTRD-UPSRYCD        PIC  X(09).
               05  CHKTRD-UPYUKOSTYMD    PIC  X(08).
               05  CHKTRD-UPYUKOEDYMD    PIC  X(08).
               05  CHKTRD-UPCDKBN        PIC  X(01).
               05  CHKTRD-UPRENNUM       PIC  9(03).
      *
      *�����ѹ���
           03  CHKTRD-SELNAME            PIC  X(200).
           03  CHKTRD-SELCDSYU           PIC X(1).
           03  CHKTRD-SELINPUTCD         PIC X(20).