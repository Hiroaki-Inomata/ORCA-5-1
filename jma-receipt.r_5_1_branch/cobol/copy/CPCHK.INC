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
      *    �����å��ޥ���        COPY (CPCHK.INC)                     *
      *****************************************************************
      *****************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *01  CHK-REC.
           03  CHK-KEY.
      *���ŵ����ֹ�
               05  CHK-HOSPNUM         PIC 9(2).
      *�����å���ʬ
               05  CHK-CHKKBN          PIC X(1).
      *���ť�����
               05  CHK-SRYCD           PIC X(9).
      *���롼���ֹ��̤���ѡ�
               05  CHK-GRPNUM          PIC 9(2).
      *ͭ������ǯ����
               05  CHK-YUKOSTYMD       PIC X(8).
      *ͭ����λǯ����
               05  CHK-YUKOEDYMD       PIC X(8).
      *�����ɶ�ʬ
               05  CHK-CDKBN           PIC X(1).
      *Ŭ�ѥ�����
               05  CHK-CD              PIC X(9).
      *������ʬ��̤���ѡ�
           03  CHK-NYUGAIKBN           PIC X(1).
      *Ʊ�����������ʬ��1:�����������Ƚ���Ԥ���
           03  CHK-NYUGAIBNDKBN        PIC X(1).
      *������ʬ��1:�������˴����������å���2:����������å���
           03  CHK-SYORIKBN            PIC X(1).
      *���顼������0:���顼��9:���顼��8:�����ʤ���
           03  CHK-CHKERR              PIC 9(1).
      *�ǡ�����ʬ��1:�ޥ����󶡡�2:�桼����Ͽ��
           03  CHK-DATAKBN             PIC X(1).
      *Ϣ�ֹ�ʲ���ɽ����
           03  CHK-RENNUM              PIC 9(3).
      *ü���ɣ�
           03  CHK-TERMID              PIC  X(16).
      *���ڡ��졼���ɣ�
           03  CHK-OPID                PIC  X(16).
      *����ǯ����
           03  CHK-CREYMD              PIC  X(08).
      *����ǯ����
           03  CHK-UPYMD               PIC  X(08).
      *�������� 
           03  CHK-UPHMS               PIC  X(06).
      *�祭�������ѹ���
           03  CHK-UPDKEY.
               05  CHK-UPHOSPNUM       PIC 9(2).
               05  CHK-UPCHKKBN        PIC X(1).
               05  CHK-UPSRYCD         PIC X(9).
               05  CHK-UPGRPNUM        PIC 9(2).
               05  CHK-UPYUKOSTYMD     PIC X(8).
               05  CHK-UPYUKOEDYMD     PIC X(8).
               05  CHK-UPCDKBN         PIC X(1).
               05  CHK-UPCD            PIC X(9).
           03  CHK-SELNAME             PIC X(200).
           03  CHK-SELCDSYU            PIC X(1).
           03  CHK-SELINPUTCD          PIC X(20).