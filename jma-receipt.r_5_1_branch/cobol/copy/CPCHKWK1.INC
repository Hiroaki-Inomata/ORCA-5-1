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
      *    �����å��ޥ��������ǡ���    COPY (CPCHKWK1.INC)            *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  CHKWK-REC.
      *������ʬ
           03  CHKWK-SYORI-UPDKBN        PIC  X(01).
      *
           03  CHKWK-KEY.
      *���ŵ����ֹ�
               05  CHKWK-HOSPNUM         PIC 9(2).
      *�����å���ʬ
               05  CHKWK-CHKKBN          PIC X(1).
      *���ť�����
               05  CHKWK-SRYCD           PIC X(9).
      *���롼���ֹ��̤���ѡ�
               05  CHKWK-GRPNUM          PIC 9(2).
      *ͭ������ǯ����
               05  CHKWK-YUKOSTYMD       PIC X(8).
      *ͭ����λǯ����
               05  CHKWK-YUKOEDYMD       PIC X(8).
      *�����ɶ�ʬ
               05  CHKWK-CDKBN           PIC X(1).
      *Ŭ�ѥ�����
               05  CHKWK-CD              PIC X(9).
      *ͭ����λǯ����2
           03  CHKWK-YUKOEDYMD2          PIC  X(08).
      *������ʬ��̤���ѡ�
           03  CHKWK-NYUGAIKBN           PIC X(1).
      *Ʊ�����������ʬ��1:�����������Ƚ���Ԥ���
           03  CHKWK-NYUGAIBNDKBN        PIC X(1).
      *������ʬ��1:�������˴����������å���2:����������å���
           03  CHKWK-SYORIKBN            PIC X(1).
      *���顼������0:���顼��9:���顼��8:�����ʤ���
           03  CHKWK-CHKERR              PIC 9(1).
      *�ǡ�����ʬ��1:�ޥ����󶡡�2:�桼����Ͽ��
           03  CHKWK-DATAKBN             PIC X(1).
      *Ϣ�ֹ�ʲ���ɽ����
           03  CHKWK-RENNUM              PIC 9(3).
      *ü���ɣ�
           03  CHKWK-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CHKWK-OPID                PIC  X(16).
      *����ǯ����
           03  CHKWK-CREYMD              PIC  X(08).
      *����ǯ����
           03  CHKWK-UPYMD               PIC  X(08).
      *�������� 
           03  CHKWK-UPHMS               PIC  X(06).