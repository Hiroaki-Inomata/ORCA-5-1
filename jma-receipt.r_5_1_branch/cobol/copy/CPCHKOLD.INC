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
      *  01.00.01    MCC-ƣ��     02.03.14  �祭�������ѹ��ܤ��ɲ�
      *****************************************************************
      *01  CHK-REC.
           03  CHK-KEY.
               05  CHK-HOSPNUM         PIC 9(2).
      *�����å���ʬ
               05  CHK-CHKKBN          PIC  X(01).
      *���ť�����
               05  CHK-SRYCD           PIC  X(09).
      *ͭ������ǯ����
               05  CHK-YUKOSTYMD       PIC  X(08).
      *ͭ����λǯ����
               05  CHK-YUKOEDYMD       PIC  X(08).
      *�����ɶ�ʬ
               05  CHK-CDKBN           PIC  X(01).
      *Ϣ�ֹ�
               05  CHK-RENNUM          PIC  9(02).
      *Ŭ�ѥ����ɸĿ�
           03  CHK-TEKCDNUM            PIC  9(03).
      *�����ɣ����飱��
           03  CHK-CD-G.
               05  CHK-CD1             PIC  X(09).
               05  CHK-CD2             PIC  X(09).
               05  CHK-CD3             PIC  X(09).
               05  CHK-CD4             PIC  X(09).
               05  CHK-CD5             PIC  X(09).
               05  CHK-CD6             PIC  X(09).
               05  CHK-CD7             PIC  X(09).
               05  CHK-CD8             PIC  X(09).
               05  CHK-CD9             PIC  X(09).
               05  CHK-CD10            PIC  X(09).
      *
           03  CHK-CD-R                REDEFINES   CHK-CD-G.
               05  CHK-CD              PIC  X(09)
                                       OCCURS   10.
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
           03  CHKUP-KEY.
               05  CHKUP-HOSPNUM       PIC  9(02).
               05  CHKUP-CHKKBN        PIC  X(01).
               05  CHKUP-SRYCD         PIC  X(09).
               05  CHKUP-YUKOSTYMD     PIC  X(08).
               05  CHKUP-YUKOEDYMD     PIC  X(08).
               05  CHKUP-CDKBN         PIC  X(01).
               05  CHKUP-RENNUM        PIC  9(02).
      *
      *�����ѹ���
           03  CHK-SELNAME             PIC  X(200).